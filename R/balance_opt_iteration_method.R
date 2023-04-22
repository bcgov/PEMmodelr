#' Balance_optimisation_iteration
#'
#' @param train_data training data set
#' @param ds_iterations downsampling values (vector of numbers:  10 - 100), NA if not using
#' @param smote_iterations smote iterations (vector of numbers: 0.1 - 0.9), NA if not using
#' @param use.neighbours to use all spatial adjoining values, default is FALSE
#' @param fuzz_matrix fuzzy matrix
#' @param out_dir location where the balance output files will be stored
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
                                 use.neighbours = FALSE){
                                 #extrarun = FALSE,
                                 #extradat = NULL,
                                 #downsample = TRUE,
                                 #ds_iterations = ds_iterations,
                                 #smote = TRUE,
                                 #smote_iterations = smote_iterations,
                                 #use.neighbours = TRUE) {

  # # # testing lines
  # train_data =train_data
  # mtry = mtry
  # min_n = min_n
  # ds_iterations <- 0.5
  # smote_iterations <- NA #c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  # fuzz_matrix <- fmat
  # out_dir <- fid$model_inputs0310[2]
  # #

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
         ref_dat <- copy(train_data)
         ref_dat[,mapunit1 := as.factor(mapunit1)]
         ref_dat[,slice := as.factor(slice)]
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

           ### split into train and test based on 5-site slices
           ref_train <- ref_dat[slice != k & position == "Orig",]
           ref_train[,c("id","mapunit2", "position","slice","transect_id","bgc_cat") := NULL]
           low_units <- ref_train[,.(NumUnit = .N), by = .(mapunit1)][NumUnit < 10,]
           ref_train <- ref_train[!mapunit1 %in% low_units$mapunit1,]

           if (use.neighbours) {
             ref_test <- ref_dat[slice == k & !mapunit1 %in% low_units$mapunit1,]
           }else{
             ref_test <- ref_dat[slice == k & !mapunit1 %in% low_units$mapunit1 & position == "Orig",]
           }

           # Q for will do we need to include an id variable here?

           null_recipe <-  recipe(mapunit1 ~ ., data = ref_train) %>%
             update_role(tid, new_role = "id variable") %>%
             step_downsample(mapunit1, under_ratio = downsample_ratio) %>%
             step_smote(mapunit1, over_ratio = smote_ratio , neighbors = 2, skip = TRUE)

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

             ref_mod <- fit(pem_workflow, ref_train)

             final_fit <- tune::extract_fit_parsnip(ref_mod)

             oob  <- round(ref_mod$fit$fit$fit$prediction.error, 3)

             preds <- predict(ref_mod, ref_test)
             pred_all <- cbind(ref_test[,.(id, mapunit1, mapunit2, slice)],
                               .pred_class = preds$.pred_class)

             pred_all <- pred_all %>% mutate(mapunit1 = as.character(mapunit1),
                                             mapunit2 = as.character(mapunit2),
                                             .pred_class = as.character(.pred_class))

             pred_all <- pred_all %>%
               dplyr::mutate(mapunit1 = ifelse(mapunit1  %in% nf_mapunits, "nonfor", mapunit1 ),
                             mapunit2 = ifelse(mapunit2 %in% nf_mapunits, "nonfor", mapunit2) ,
                             .pred_class = ifelse(.pred_class  %in% nf_mapunits, "nonfor", .pred_class ))

             pred_all$mapunit1 <- as.factor(pred_all$mapunit1)
             pred_all$mapunit2 <- as.factor(pred_all$mapunit2)

             print(paste0("generating accuracy metrics for slice:",k))

             acc <- acc_metrics(pred_all, fuzzmatrx = fuzz_matrix)%>%
               dplyr::mutate(slice = k)
            #)
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
       ref_dat <- copy(train_data)
       ref_dat[,mapunit1 := as.factor(mapunit1)]
       ref_dat[,slice := as.factor(slice)]
       print("Training raw data models...")

       munits <- unique(ref_dat$mapunit1)
       nf_mapunits <- grep(munits, pattern = "_\\d", value = TRUE, invert = TRUE)

       slices <- unique(ref_dat$slice) %>% droplevels()

       ref_acc <- foreach::foreach(k = levels(slices),.combine = rbind) %do% {

        # k = levels(slices)[5]
         label = paste(d, k, sep = "-")
         print(label)

         ref_train <- ref_dat[slice != k & position == "Orig",]
         ref_train[,c("id","mapunit2", "position","slice","transect_id","bgc_cat") := NULL]
         low_units <- ref_train[,.(NumUnit = .N), by = .(mapunit1)][NumUnit < 10,]
         ref_train <- ref_train[!mapunit1 %in% low_units$mapunit1,]

         if (use.neighbours) {
           ref_test <- ref_dat[slice == k & !mapunit1 %in% low_units$mapunit1,]
         }else{
           ref_test <- ref_dat[slice == k & !mapunit1 %in% low_units$mapunit1 & position == "Orig",]
         }

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

           final_fit <- tune::extract_fit_parsnip(PEM_rf1)

           oob  <- round(PEM_rf1$fit$fit$fit$prediction.error, 3)

           preds <- predict(ref_mod, ref_test)
           pred_all <- cbind(ref_test[,.(id, mapunit1, mapunit2, slice)],
                             .pred_class = preds$.pred_class)

           pred_all <- pred_all %>% mutate(mapunit1 = as.character(mapunit1),
                                          mapunit2 = as.character(mapunit2),
                                          .pred_class = as.character(.pred_class))

           pred_all <- pred_all %>%
             dplyr::mutate(mapunit1 = ifelse(mapunit1  %in% nf_mapunits, "nonfor", mapunit1 ),
                    mapunit2 = ifelse(mapunit2 %in% nf_mapunits, "nonfor", mapunit2) ,
                    .pred_class = ifelse(.pred_class  %in% nf_mapunits, "nonfor", .pred_class ))

           pred_all$mapunit1 <- as.factor(pred_all$mapunit1)
           pred_all$mapunit2 <- as.factor(pred_all$mapunit2)
           #pred_all$.pred_class <- factor(pred_all$.pred_class,
           #                                levels = levels(pred_all$mapunit1))

           print(paste0("generating accuracy metrics for slice:",k))

           acc <- acc_metrics(pred_all, fuzzmatrx = fuzz_matrix)%>%
             dplyr::mutate(slice = k)

         }

       } # end of slice loop (downsample only )

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

         ref_dat <- copy(train_data)
         ref_dat[,mapunit1 := as.factor(mapunit1)]
         ref_dat[,slice := as.factor(slice)]
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

           ### split into train and test based on 5-site slices
           ref_train <- ref_dat[slice != k & position == "Orig",]
           ref_train[,c("id","mapunit2", "position","slice","transect_id","bgc_cat") := NULL]
           low_units <- ref_train[,.(NumUnit = .N), by = .(mapunit1)][NumUnit < 10,]
           ref_train <- ref_train[!mapunit1 %in% low_units$mapunit1,]

           if (use.neighbours) {
             ref_test <- ref_dat[slice == k & !mapunit1 %in% low_units$mapunit1,]
           }else{
             ref_test <- ref_dat[slice == k & !mapunit1 %in% low_units$mapunit1 & position == "Orig",]
           }

           # Q for will do we need to include an id variable here?

           null_recipe <-  recipe(mapunit1 ~ ., data = ref_train) %>%
             update_role(tid, new_role = "id variable") %>%
             step_smote(mapunit1, over_ratio = smote_ratio , neighbors = 2, skip = TRUE)

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
             ref_mod <- fit(pem_workflow, ref_train)

             final_fit <- tune::extract_fit_parsnip(ref_mod)

             oob  <- round(ref_mod$fit$fit$fit$prediction.error, 3)

             preds <- predict(ref_mod, ref_test)
             pred_all <- cbind(ref_test[,.(id, mapunit1, mapunit2, slice)],
                               .pred_class = preds$.pred_class)


             pred_all <- pred_all %>% mutate(mapunit1 = as.character(mapunit1),
                                             mapunit2 = as.character(mapunit2),
                                             .pred_class = as.character(.pred_class))

             pred_all <- pred_all %>%
               dplyr::mutate(mapunit1 = ifelse(mapunit1  %in% nf_mapunits, "nonfor", mapunit1 ),
                             mapunit2 = ifelse(mapunit2 %in% nf_mapunits, "nonfor", mapunit2) ,
                             .pred_class = ifelse(.pred_class  %in% nf_mapunits, "nonfor", .pred_class ))

             pred_all$mapunit1 <- as.factor(pred_all$mapunit1)
             pred_all$mapunit2 <- as.factor(pred_all$mapunit2)

             print(paste0("generating accuracy metrics for slice:",k))

             acc <- acc_metrics(pred_all, fuzzmatrx = fuzz_matrix)%>%
               dplyr::mutate(slice = k)
             #)
           }

         } # end of slice loop (downsample only )

         write.csv(ref_acc, file = file.path(out_dir, "balance", paste0("acc_", balance_name,".csv")))

       } # end of smote iteration

   }

  return(TRUE)

} # end of function

