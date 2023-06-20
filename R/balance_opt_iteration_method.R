#' Balance_optimisation_iteration
#'
#' @param train_data training data set
#' @param ds_iterations downsampling values (vector of numbers:  10 - 100), NA if not using
#' @param smote_iterations smote iterations (vector of numbers: 0.1 - 0.9), NA if not using
#' @param use.neighbours to use all spatial adjoining values, default is FALSE
#' @param fuzz_matrix fuzzy matrix
#' @param mtry mtry from best params
#' @param min_n mtry from best params
#' @param detailed_output TRUE/FALSE if you want to create outputs for theta calculations
#' @param out_dir_detailed default is NA, text filepath which is the output location for detailed_output,
#' @param out_dir location where the balance output files will be stored
#'
#' @importFrom magrittr "%>%"
#' @importFrom foreach foreach
#' @importFrom dplyr mutate filter select rename rowwise group_by distinct top_n add_count
#' @return outputs files directly to folder
#' @export
#'
#' @examples
#' balance_optimisation_iteration(train_data, ds_iterations = c(30, 40, 50), smote_iterations = c( 0.5, 0.6, 0.7), use.neighbours = FALSE,fuzz_matrix = fmat,out_dir = fid$model_inputs0310[2])

balance_optimisation_iteration <- function(train_data = train_data,
                                 ds_iterations = ds_iterations,
                                 smote_iterations = smote_iterations,
                                 mtry = mtry,
                                 min_n = min_n,
                                 fuzz_matrix = fmat,
                                 out_dir = fid$model_inputs0310[2],
                                 use.neighbours = FALSE,
                                 detailed_output = TRUE,
                                 out_dir_detailed = NA){
                                 #extrarun = FALSE,
                                 #extradat = NULL,
                                 #downsample = TRUE,
                                 #ds_iterations = ds_iterations,
                                 #smote = TRUE,
                                 #smote_iterations = smote_iterations,
                                 #use.neighbours = TRUE) {

  # # # # testing lines
#
    # train_data = tdat
    # fuzz_matrix = fmat
    # ds_iterations = NA # c(10,20,30,40,50,60,70,80,90)
    # smote_iterations = 0.5 #c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,0.8, 0.9),
    # mtry = mtry
    # min_n = min_n
    # use.neighbours = TRUE
    # out_dir = outDir
  # create a subfolder to store all balance outputs:

  dir.create(file.path(out_dir, "balance"))

  # downsample and smote options

   if(unique(!is.na(ds_iterations)&!is.na(smote_iterations))){
     print("downsample and smote")

      for(d in ds_iterations){
        #d = ds_iterations[1]
       print(d)

       for(i in smote_iterations){
        # i = smote_iterations[1]
         print(i)

         # set up the parameters for balancing
         downsample_ratio = d  # (0 - 100, Null = 1)
         smote_ratio = i    # 0 - 1, 1 = complete smote

         balance_name = paste0("ds_",downsample_ratio,"_sm_", smote_ratio)

         # training set - train only on pure calls
         ref_dat <- train_data %>%
           dplyr::mutate(mapunit1 = as.factor(mapunit1),
                         slice = as.factor(slice))
         print("Training raw data models...")

         munits <- unique(ref_dat$mapunit1)
         nf_mapunits <- grep(munits, pattern = "_\\d", value = TRUE, invert = TRUE)

         slices <- unique(ref_dat$slice) %>% droplevels()

         # for all slices
         ref_acc <- foreach::foreach(k = levels(slices),.combine = rbind) %do% {

           #k = levels(slices)[2]
           label = paste(d, i, k, sep = "-")
           print(label)
           #print(k)

           #create training set
           ref_train <- ref_dat %>%
             dplyr::filter(!slice %in% k) %>%
             filter(is.na(mapunit2)) %>% # train only on pure calls
             filter(position == "Orig") %>%
             dplyr::select(-id, -slice, -mapunit2,-position, -transect_id)%>%
             droplevels()

           MU_count <- ref_train %>% dplyr::count(mapunit1) %>% filter(n > 10)

           ref_train <- ref_train %>% filter(mapunit1 %in% MU_count$mapunit1)  %>%
             droplevels()

           if (use.neighbours) {
             # test set
             ref_test <- ref_dat %>%
               filter(slice %in% k) %>%
               filter(mapunit1 %in% MU_count$mapunit1) %>%
               #dplyr::select(-id, -slice,-position, -transect_id) %>%
               droplevels()

           }else{

             ref_test <- ref_dat %>%
               filter(slice %in% k) %>%
               filter(mapunit1 %in% MU_count$mapunit1) %>%
               filter(position == "Orig") %>%
               #dplyr::select(-id, -slice,-position, -transect_id) %>%
               droplevels()

           }

           ref_id <- ref_test %>% select(id, mapunit1, mapunit2 )

           null_recipe <-  recipe(mapunit1 ~ ., data = ref_train) %>%
             update_role(tid, new_role = "id variable") %>%
             step_downsample(mapunit1, under_ratio = downsample_ratio) %>%
             step_smote(mapunit1, over_ratio = smote_ratio , neighbors = 4, skip = TRUE)

           randf_spec <- rand_forest(mtry = mtry, min_n = min_n, trees = 151) %>%
             set_mode("classification") %>%
             set_engine("ranger", importance = "permutation", verbose = FALSE)

           pem_workflow <- workflows::workflow() %>%
             workflows::add_recipe(null_recipe) %>%
             workflows::add_model(randf_spec)

           #######################################################
           possibleError <- tryCatch(
            fit(pem_workflow, ref_train),
            error=function(e) e
           )
           if(!inherits(possibleError, "error")){
           #   #REAL WORK

             ref_mod <- possibleError

             oob  <- round(ref_mod$fit$fit$fit$prediction.error, 3)

             #ref_mod <- fit(pem_workflow, ref_train)

             preds <- predict(ref_mod, ref_test)

             pred_all <- cbind(ref_id,.pred_class = preds$.pred_class)

             pred_all <- pred_all %>% mutate(mapunit1 = as.character(mapunit1),
                                             mapunit2 = as.character(mapunit2),
                                             .pred_class = as.character(.pred_class))

             pred_all <- pred_all %>%
               dplyr::mutate(mapunit1 = ifelse(mapunit1  %in% nf_mapunits, "nonfor", mapunit1 ),
                             mapunit2 = ifelse(mapunit2 %in% nf_mapunits, "nonfor", mapunit2) ,
                             .pred_class = ifelse(.pred_class  %in% nf_mapunits, "nonfor", .pred_class ))

             # harmonize factor levels
             pred_all <- harmonize_factors(pred_all)
             pred_all$mapunit2 = as.factor(pred_all$mapunit2)

             print(paste0("generating accuracy metrics for slice:",k))

             if(detailed_output == TRUE){
               saveRDS(pred_all, file.path(out_dir_detailed, paste0("predictions_", k)))
             }

             acc <- acc_metrics(pred_all, fuzzmatrx = fuzz_matrix)%>%
               dplyr::mutate(slice = k,
                             oob = oob)

           }

         } # end of slice loop (downsample only )

        write.csv(ref_acc, file = file.path(out_dir, "balance", paste0("acc_", balance_name,".csv")))

       } # end of smote iteration

     } # end of downsample iteration

   } else if(unique(is.na(smote_iterations)&!is.na(ds_iterations))){
     print("downsample only")

     for(d in ds_iterations){
       #d = ds_iterations[1]
       print(d)
       # set up the parameters for balancing
       downsample_ratio = d  # (0 - 100, Null = 1)
       balance_name = paste0("ds_",downsample_ratio)

       # training set - train only on pure calls
       ref_dat <- train_data %>%
         dplyr::mutate(mapunit1 = as.factor(mapunit1),
                       slice = as.factor(slice))
       print("Training raw data models...")

       munits <- unique(ref_dat$mapunit1)
       nf_mapunits <- grep(munits, pattern = "_\\d", value = TRUE, invert = TRUE)

       slices <- unique(ref_dat$slice) %>% droplevels()

       ref_acc <- foreach::foreach(k = levels(slices),.combine = rbind) %do% {

         #k = levels(slices)[1]
         label = paste(d, k, sep = "-")
         print(label)

         #create training set
         ref_train <- ref_dat %>%
           dplyr::filter(!slice %in% k) %>%
           filter(is.na(mapunit2)) %>% # train only on pure calls
           filter(position == "Orig") %>%
           dplyr::select(-id, -slice, -mapunit2,-position, -transect_id)%>%
           droplevels()

         MU_count <- ref_train %>% dplyr::count(mapunit1) %>% filter(n > 10)

         ref_train <- ref_train %>% filter(mapunit1 %in% MU_count$mapunit1)  %>%
           droplevels()


         if (use.neighbours) {
           # test set
           ref_test <- ref_dat %>%
             filter(slice %in% k) %>%
             filter(mapunit1 %in% MU_count$mapunit1) %>%
             #dplyr::select(-id, -slice,-position, -transect_id) %>%
             droplevels()

           }else{

           ref_test <- ref_dat %>%
             filter(slice %in% k) %>%
             filter(mapunit1 %in% MU_count$mapunit1) %>%
             filter(position == "Orig") %>%
             #dplyr::select(-id, -slice,-position, -transect_id) %>%
             droplevels()

           }

         ref_id <- ref_test %>% select(id, mapunit1, mapunit2 )

         null_recipe <-  recipe(mapunit1 ~ ., data = ref_train) %>%
           update_role(tid, new_role = "id variable") %>%
           step_downsample(mapunit1, under_ratio = downsample_ratio)

         randf_spec <- rand_forest(mtry = mtry, min_n = min_n, trees = 151) %>%
           set_mode("classification") %>%
           set_engine("ranger", importance = "permutation", verbose = FALSE)

         pem_workflow <- workflows::workflow() %>%
           workflows::add_recipe(null_recipe) %>%
           workflows::add_model(randf_spec)

         #######################################################
         possibleError <- tryCatch(
           fit(pem_workflow, ref_train),
           error=function(e) e
         )
         if(!inherits(possibleError, "error")){
           #REAL WORK

           ref_mod <- fit(pem_workflow, ref_train)
           oob  <- round(ref_mod$fit$fit$fit$prediction.error, 3)
           #final_fit <- tune::extract_fit_parsnip(ref_mod)

           preds <- predict(ref_mod, ref_test)

           pred_all <- cbind(ref_id,.pred_class = preds$.pred_class)

           pred_all <- pred_all %>% mutate(mapunit1 = as.character(mapunit1),
                                           mapunit2 = as.character(mapunit2),
                                           .pred_class = as.character(.pred_class))

           pred_all <- pred_all %>%
             dplyr::mutate(mapunit1 = ifelse(mapunit1  %in% nf_mapunits, "nonfor", mapunit1 ),
                           mapunit2 = ifelse(mapunit2 %in% nf_mapunits, "nonfor", mapunit2) ,
                           .pred_class = ifelse(.pred_class  %in% nf_mapunits, "nonfor", .pred_class ))

           # harmonize factor levels
           pred_all <- harmonize_factors(pred_all)
           pred_all$mapunit2 = as.factor(pred_all$mapunit2)

           print(paste0("generating accuracy metrics for slice:",k))

           if(detailed_output == TRUE){
             saveRDS(pred_all, file.path(out_dir_detailed, paste0("predictions_", k)))
           }

           acc <- acc_metrics(pred_all, fuzzmatrx = fuzz_matrix)%>%
             dplyr::mutate(slice = k,
                           oob = oob)

         }# end of error checking loop

       } # end of slice loop (downsample only)

       # extract results from sresults

       write.csv(ref_acc, file = file.path(out_dir, "balance", paste0("acc_", balance_name,".csv")))

     } # end of downsample iteration only loop

   } else if (unique(!is.na(smote_iterations)& is.na(ds_iterations))){
     # smote only
     print("smote only")

       for(i in smote_iterations){
        #i = smote_iterations[1]
         print(i)

         # set up the parameters for balancing
         smote_ratio = i    # 0 - 1, 1 = complete smote
         balance_name = paste0("sm_", smote_ratio)

         # training set - train only on pure calls
         ref_dat <- train_data %>%
           dplyr::mutate(mapunit1 = as.factor(mapunit1),
                         slice = as.factor(slice))

         print("Training raw data models...")

         munits <- unique(ref_dat$mapunit1)
         nf_mapunits <- grep(munits, pattern = "_\\d", value = TRUE, invert = TRUE)

         slices <- unique(ref_dat$slice) %>% droplevels()

         # for all slices
         ref_acc <- foreach::foreach(k = levels(slices),.combine = rbind) %do% {
           #k = levels(slices)[2]
           label = paste(i, k, sep = "-")
           print(label)
           #print(k)

           #create training set
           ref_train <- ref_dat %>%
             dplyr::filter(!slice %in% k) %>%
             filter(is.na(mapunit2)) %>% # train only on pure calls
             filter(position == "Orig") %>%
             dplyr::select(-id, -slice, -mapunit2,-position, -transect_id)%>%
             droplevels()

           MU_count <- ref_train %>% dplyr::count(mapunit1) %>% filter(n > 10)

           ref_train <- ref_train %>% filter(mapunit1 %in% MU_count$mapunit1)  %>%
             droplevels()

           if (use.neighbours) {
             # test set
             ref_test <- ref_dat %>%
               filter(slice %in% k) %>%
               filter(mapunit1 %in% MU_count$mapunit1) %>%
               droplevels()

           }else{

             ref_test <- ref_dat %>%
               filter(slice %in% k) %>%
               filter(mapunit1 %in% MU_count$mapunit1) %>%
               filter(position == "Origin") %>%
               droplevels()

           }

           ref_id <- ref_test %>% select(id, mapunit1, mapunit2 )

           null_recipe <-  recipe(mapunit1 ~ ., data = ref_train) %>%
             update_role(tid, new_role = "id variable") %>%
             step_smote(mapunit1, over_ratio = smote_ratio , neighbors = 4, skip = TRUE)

           randf_spec <- rand_forest(mtry = mtry, min_n = min_n, trees = 151) %>%
             set_mode("classification") %>%
             set_engine("ranger", importance = "permutation", verbose = FALSE)

           pem_workflow <- workflows::workflow() %>%
             workflows::add_recipe(null_recipe) %>%
             workflows::add_model(randf_spec)

           #######################################################
           possibleError <- tryCatch(
             fit(pem_workflow, ref_train),
             error=function(e) e
           )
           if(!inherits(possibleError, "error")){
             #   #REAL WORK
             #tryCatch(
             #ref_mod <- fit(pem_workflow, ref_train)

             ref_mod <- possibleError
             oob  <- round(ref_mod$fit$fit$fit$prediction.error, 3)
             preds <- predict(ref_mod, ref_test)

             pred_all <- cbind(ref_id,.pred_class = preds$.pred_class)

             pred_all <- pred_all %>% mutate(mapunit1 = as.character(mapunit1),
                                             mapunit2 = as.character(mapunit2),
                                             .pred_class = as.character(.pred_class))

             pred_all <- pred_all %>%
               dplyr::mutate(mapunit1 = ifelse(mapunit1  %in% nf_mapunits, "nonfor", mapunit1 ),
                             mapunit2 = ifelse(mapunit2 %in% nf_mapunits, "nonfor", mapunit2) ,
                             .pred_class = ifelse(.pred_class  %in% nf_mapunits, "nonfor", .pred_class ))

             # harmonize factor levels
             pred_all <- harmonize_factors(pred_all)
             pred_all$mapunit2 = as.factor(pred_all$mapunit2)

             print(paste0("generating accuracy metrics for slice:",k))

             if(detailed_output == TRUE){
               saveRDS(pred_all, file.path(out_dir_detailed, paste0("predictions_", k)))
             }

             acc <- acc_metrics(pred_all, fuzzmatrx = fuzz_matrix)%>%
               dplyr::mutate(slice = k,
                             oob = oob)
             #)
           }

         } # end of slice loop (downsample only )

         write.csv(ref_acc, file = file.path(out_dir, "balance", paste0("acc_", balance_name,".csv")))

       } # end of smote iteration

   }

  return(TRUE)

} # end of function

