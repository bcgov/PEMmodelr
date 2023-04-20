#' Balance_optimisation_iteration
#'
#' @param train_data training data set
#' @param ds_iterations downsampling values (vector of numbers:  10 - 100), NA if not using
#' @param smote_iterations smote iterations (vector of numbers: 0.1 - 0.9), NA if not using
#' @param use.neighbours to use all spatial adjoining values, default is FALSE
#' @param fuzz_matrix fuzzy matrix
#' @param out_dir location where the balance output files will be stored
#'
#' @return
#' @export
#'
#' @examples
#' balance_optimisation_iteration(train_data, ds_iterations = c(30, 40, 50), smote_iterations = c( 0.5, 0.6, 0.7), use.neighbours = FALSE,fuzz_matrix = fmat,out_dir = fid$model_inputs0310[2])

balance_optimisation_iteration <- function(train_data = train_data,
                                 ds_iterations = ds_iterations,
                                 smote_iterations = smote_iterations,
                                 use.neighbours = FALSE,
                                 fuzz_matrix = fmat,
                                 out_dir = fid$model_inputs0310[2]){
                                 #extrarun = FALSE,
                                 #extradat = NULL,
                                 #downsample = TRUE,
                                 #ds_iterations = ds_iterations,
                                 #smote = TRUE,
                                 #smote_iterations = smote_iterations,
                                 #use.neighbours = TRUE) {

  # # testing lines
  # train_data =train_data
  #
  # # extrarun = FALSE
  # # extradat = NULL
  # # downsample = TRUE
  #   #mtry = mtry
  #   #min_n = min_n
  ds_iterations <- NA
  #  #10,
  smote_iterations <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  #  fuzz_matrix <- fmat
  #  out_dir <- fid$model_inputs0310[2]
  #

  # create a subfolder to store all balance outputs:

  dir.create(file.path(out_dir, "balance"))

  # smote_iterations = smote_iterations
   use.neighbours = FALSE

   if(unique(!is.na(ds_iterations)&!is.na(smote_iterations))){
     print("downsample and smote")

      for(d in ds_iterations){
       # d = ds_iterations[1]
       print(d)

       for(i in smote_iterations){
        # i = smote_iterations[1]
         print(i)
         #
         # test if enough data in the slice to use smoting

         # set up the parameters for balancing
         downsample_ratio = d  # (0 - 100, Null = 1)
         smote_ratio = i    # 0 - 1, 1 = complete smote

         balance_name = paste0("ds_",downsample_ratio,"_sm_", smote_ratio)

         # training set - train only on pure calls

         ref_dat <- copy(train_data)
         ref_dat[,mapunit1 := as.factor(mapunit1)]
         ref_dat[,slice := as.factor(slice)]
         print("Training raw data models...")

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

           #randf_spec <- rand_forest(mtry = mtry, min_n = min_n, trees = 151) %>%
           randf_spec <- rand_forest( trees = 151) %>%
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

             #
             # # DOES THIS NEED TO BE ADDED IN ALL THE MODEL BUILDS?
             # # switch out the predicted Nf units for "nonfor" catergory.
             # test.pred <- test.pred %>%
             #   mutate(target = ifelse(as.character(target) %in% nf_mapunits, "nonfor", as.character(target)),
             #          target2 = ifelse(as.character(target2) %in% nf_mapunits, "nonfor", as.character(target2)) ,
             #          .pred_class = ifelse(as.character(.pred_class)  %in% nf_mapunits, "nonfor", as.character(.pred_class)))
             #
             # test.pred <- test.pred %>%  mutate_if(is.character, as.factor)
             #

             preds <- predict(ref_mod, ref_test)
             pred_all <- cbind(ref_test[,.(id, mapunit1, mapunit2, slice)],
                               .pred_class = preds$.pred_class)
             pred_all$mapunit1 <- as.factor(pred_all$mapunit1)
             pred_all$.pred_class <- factor(pred_all$.pred_class,
                                            levels = levels(pred_all$mapunit1))

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
       #d = ds_iterations[3]
       print(d)

       # test if enough data in the slice to use smoting
       # set up the parameters for balancing
       downsample_ratio = d  # (0 - 100, Null = 1)
       balance_name = paste0("ds_",downsample_ratio)

       # training set - train only on pure calls
       ref_dat <- copy(train_data)
       ref_dat[,mapunit1 := as.factor(mapunit1)]
       ref_dat[,slice := as.factor(slice)]
       print("Training raw data models...")

       slices <- unique(ref_dat$slice) %>% droplevels()

       ref_acc <- foreach::foreach(k = levels(slices),.combine = rbind) %do% {

         #k = levels(slices)[1]
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

         # Q for will do we need to include an id variable here?

         null_recipe <-  recipe(mapunit1 ~ ., data = ref_train) %>%
           update_role(tid, new_role = "id variable") %>%
           step_downsample(mapunit1, under_ratio = downsample_ratio)

         #randf_spec <- rand_forest(mtry = mtry, min_n = min_n, trees = 151) %>%
         randf_spec <- rand_forest( trees = 151) %>%
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
           pred_all$mapunit1 <- as.factor(pred_all$mapunit1)
           pred_all$.pred_class <- factor(pred_all$.pred_class,
                                          levels = levels(pred_all$mapunit1))

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

           #randf_spec <- rand_forest(mtry = mtry, min_n = min_n, trees = 151) %>%
           randf_spec <- rand_forest( trees = 151) %>%
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
             pred_all$mapunit1 <- as.factor(pred_all$mapunit1)
             pred_all$.pred_class <- factor(pred_all$.pred_class,
                                            levels = levels(pred_all$mapunit1))

             print(paste0("generating accuracy metrics for slice:",k))

             acc <- acc_metrics(pred_all, fuzzmatrx = fuzz_matrix)%>%
               dplyr::mutate(slice = k)
             #)
           }

         } # end of slice loop (downsample only )

         write.csv(ref_acc, file = file.path(out_dir, "balance", paste0("acc_", balance_name,".csv")))

       } # end of smote iteration

   } else (print("unbalanced"))

   return(TRUE)
} # end of function




#
#
#         # for all slices
#         sresults <- foreach(k = levels(slices)) %do% {
#
#           #k = levels(slices)[5]
#           label = paste(d, i, k, sep = "-")
#           print(label)
#           #print(k)
#           ### split into train and test based on 5-site slices
#
#
#           # training set
#
#           BGC_train <- trDat %>%
#             dplyr::filter(!slice %in% k, position %in% "Orig") %>%
#             # filter(is.na(target2) | target2 == "") %>%
#             dplyr::select(-slice, -transect_id, -position, -id)
#
#           # merge in extra point data
#           if(isTRUE(extrarun)) {
#             print("adding extra points at each model build")
#             BGC_train <- rbind(BGC_train, extradat)
#           }
#           # check if enough data for each class to smote
#
#           MU_count <- BGC_train %>% dplyr::count(target) %>% filter(n < 10) %>%
#             pull(target) %>% droplevels()
#
#           if(length(MU_count) > 0) {
#             print("remove classes with very low training points from smoting test")
#             BGC_train <- BGC_train %>%
#               filter(!target %in% MU_count) %>% droplevels()
#           }
#
#
#           BGC_train <- BGC_train %>%
#             dplyr::select(-target2) %>%
#             drop_na() %>%
#             mutate(target = factor(target),
#                    tid = as.character(tid)) %>%
#             droplevels()
#
#
#           # test set
#           if (isTRUE(use.neighbours)) {
#             BGC_test <- trDat %>% filter(slice %in% k) %>%
#               filter(!target %in% MU_count)
#           } else{ BGC_test <- trDat %>% filter(slice %in% k, position %in% "Orig") %>%
#             filter(!target %in% MU_count)}
#
#           BGC_test_all <- BGC_test # keep for the target2 alt call.
#           BGC_test_transect_no <- length(unique(BGC_test_all$transect_id))
#           BGC_test <- BGC_test %>%
#             dplyr::select(-slice,-target2,-transect_id)
#           #
#           ############### Define test recipes and workflow ###################
#
#           # null_recipe <- balance_optimum(indata = BGC_train,
#           #                                downsample = downsample,
#           #                                downsample_ratio = downsample_ratio,
#           #                                smote= smote,
#           #                                smote_ratio = smote_ratio,
#           #                                update_role(tid, new_role = "id variable"))
#
#
#           null_recipe <-  recipe(target ~ ., data = BGC_train) %>%
#             update_role(tid, new_role = "id variable") %>%
#             step_downsample(target, under_ratio = downsample_ratio) %>%
#             step_smote(target, over_ratio = smote_ratio , neighbors = 2, skip = TRUE)
#
#           randf_spec <- rand_forest(mtry = mtry, min_n = min_n, trees = 151) %>%
#             set_mode("classification") %>%
#             set_engine("ranger", importance = "permutation", verbose = FALSE)
#
#           pem_workflow <- workflow() %>%
#             add_recipe(null_recipe) %>%
#             add_model(randf_spec)
#
#           #######################################################
#           PEM_rf1 <- fit(pem_workflow, BGC_train)
#
#           #final_fit <- pull_workflow_fit(PEM_rf1) # %>%pull(.predictions)
#           final_fit <- extract_fit_parsnip(PEM_rf1)
#
#           oob  <- round(PEM_rf1$fit$fit$fit$prediction.error, 3)
#
#           ######### Predict Test
#           #test_target <- as.data.frame(BGC_test$target) %>% rename(target = 1)
#           test_target <- BGC_test_all %>% dplyr::select(id, target, target2)
#
#           test.pred <-  predict(PEM_rf1, BGC_test)
#           test.pred <- cbind(test_target, test.pred)
#
#           # switch out the predicted Nf units for "nonfor" catergory.
#           test.pred <- test.pred %>%
#             mutate(target = ifelse(as.character(target) %in% nf_mapunits, "nonfor", as.character(target)),
#                    target2 = ifelse(as.character(target2) %in% nf_mapunits, "nonfor", as.character(target2)) ,
#                    .pred_class = ifelse(as.character(.pred_class)  %in% nf_mapunits, "nonfor", as.character(.pred_class)))
#
#           test.pred <- test.pred %>%  mutate_if(is.character, as.factor)
#
#           ###_______________________________________________________
#
#           ###harmonize factor levels
#           targ.lev <- levels(test.pred$target)
#           targ2.lev <- levels(test.pred$target2)
#           pred.lev <- levels(test.pred$.pred_class)
#           levs <- c(targ.lev,targ2.lev, pred.lev) %>% unique()
#           test.pred$target <- factor(test.pred$target, levels = levs)
#           test.pred$target2 <- factor(test.pred$target2, levels = levs)
#           test.pred$.pred_class <- factor(test.pred$.pred_class, levels = levs)
#
#           # output test predictions
#
#           test.pred.out <- test.pred %>% mutate(slice = k)
#           acc.compare <- acc_metrix(test.pred) %>%
#             mutate(slice = k,
#                    transect_no = BGC_test_transect_no,
#                    acc_type = "test_estimate",
#                    oob = oob,
#                    balance = balance_name,
#                    bgc = bec.choose)
#           ##use function to add in theta
#           # acc.theta <- theta_accuracy(acc.compare, theta = 0.5)
#           # acc.compare <- cbind(acc.compare,acc.theta) %>% dplyr::select(unique(colnames(.)))
#           acc.compare <- acc.compare  %>%
#             dplyr::select(bgc, slice, target,balance, trans.sum, trans.tot, pred.tot, acc, kap, everything() )
#           return(list(acc.compare))
#         }
#
#         # extract results from sresults
#         acc_results <- lapply(sresults, function(x) x[[1]])
#         acc <- as.data.frame(rbindlist(acc_results))
#
#         write.csv(acc, file = paste(bal_out_dir, paste0("acc_", balance_name,".csv"),sep = "/"))
#
#
#       } # end of smote iteration
#
#
#
#     }
#
#





##### OLD VERSION

#
# balance_optimum <- function(
    #   indata = BGC_train,
#   downsample = TRUE,
#   downsample_ratio = 100, # (0 - 100, Null = 100)
#   smote = TRUE,
#   smote_ratio = 1    # 0 - 1, 1 = complete smote
# ){
#
#   if(downsample == TRUE & smote == FALSE){
#     print("downsampling")
#
#     #null_recipe <-
#     recipe(target ~ ., data = indata) %>%
#       #update_role(tid, new_role = "id variable") %>%
#       step_downsample(target, under_ratio = downsample_ratio)
#
#
#   } else if(downsample == FALSE & smote == TRUE){
#
#     print("smoting")
#
#    # null_recipe <-
#     recipe(target ~ ., data = indata) %>%
#       #update_role(tid, new_role = "id variable") %>%
#       step_smote(target, over_ratio = smote_ratio, neighbors = 2)
#
#   } else if (downsample == TRUE & smote == TRUE) {
#     print ("downsample and smoting")
#
#     #null_recipe <-
#     recipe(target ~ ., data = indata) %>%
#       #update_role(tid, new_role = "id variable") %>%
#       step_downsample(target, under_ratio = downsample_ratio) %>%
#       step_smote(target, over_ratio = smote_ratio , neighbors = 2)
#
#   }
#
#   else if (downsample == FALSE & smote == FALSE) {
#     print ("raw")
#
#     #null_recipe <-
#     recipe(target ~ ., data = indata)# %>%
#       #update_role(tid, new_role = "id variable")
#   }
#
# }
#
# # testing the output
#
# #null_recipe <- balance_optimum(downsample = FALSE, smote= FALSE)
# #null_recipe <- balance_optimum(downsample = TRUE, smote= TRUE)
# #null_recipe <- balance_optimum(downsample = TRUE, smote= FALSE)
#
# #############################################################
#
# # set up the recipe for the modelling
#
# set_recipe <- function(indata, ds_ratio = NA, sm_ratio = NA) {
#
#   #indata = inmdata_all
#   #ds_ratio = NA
#   #sm_ratio = 40
#
#
#   if(is.na(ds_ratio) & is.na(sm_ratio)){
#     print("basic")
#
#     #null_recipe <-
#     recipe(target ~ ., data = indata) #%>%
#       #update_role(tid, new_role = "id variable")
#
#   } else if(is.na(ds_ratio) & !is.na(sm_ratio)){
#
#     print("smoting")
#
#     #null_recipe <-
#     recipe(target ~ ., data = indata) %>%
#       #update_role(tid, new_role = "id variable") %>%
#       step_smote(target, over_ratio = sm_ratio, neighbors = 2)
#
#   } else if (!is.na(ds_ratio) & is.na(sm_ratio)) {
#     print ("downsample")
#
#     #null_recipe <-
#     recipe(target ~ ., data = indata) %>%
#      # update_role(tid, new_role = "id variable") %>%
#       step_downsample(target, under_ratio = ds_ratio)
#
#   } else if (!is.na(ds_ratio) & !is.na(sm_ratio)) {
#     print ("downsample and smoting")
#
#     #null_recipe <-
#     recipe(target ~ ., data = indata) %>%
#      # update_role(tid, new_role = "id variable") %>%
#       step_smote(target, over_ratio = sm_ratio, neighbors = 2) %>%
#       step_downsample(target, under_ratio = ds_ratio)
#
#   }
#
# }
#
# # set up the recipe for the final modelling
#
# set_final_recipe <- function(indata, ds_ratio = NA, sm_ratio = NA) {
#
#
#   if(is.na(ds_ratio) & is.na(sm_ratio)){
#     print("basic")
#
#     recipe(target ~ ., data = indata)
#
#   } else if(is.na(ds_ratio) & !is.na(sm_ratio)){
#
#     print("smoting")
#
#     recipe(target ~ ., data = indata) %>%
#       step_smote(target, over_ratio = sm_ratio, neighbors = 2)
#
#   } else if (!is.na(ds_ratio) & is.na(sm_ratio)) {
#     print ("downsample")
#
#     recipe(target ~ ., data = indata) %>%
#       step_downsample(target, under_ratio = ds_ratio)
#
#   } else if (!is.na(ds_ratio) & !is.na(sm_ratio)) {
#     print ("downsample and smoting")
#
#     recipe(target ~ ., data = indata) %>%
#       step_smote(target, over_ratio = sm_ratio, neighbors = 2) %>%
#       step_downsample(target, under_ratio = ds_ratio)
#
#   }
#
# }
#
#
#
#
# ######################################################################
# # plot each of the outputs (downsample, smote and ds/ smote combinations)
#
# create_dev_plot <- function(indata) {
#
#   out_data <- indata %>%
#     group_by(balance) %>%
#     summarise(mu_devation = sum(pred.obs.total),
#               mu_var = var(pred.obs.total),
#               mu_mean = mean(pred.obs.total),
#               mu_sd = sd(pred.obs.total))
#
#   # simple plot of accuracy metrics
#   dev_plot <- ggplot(indata, aes(x=target, y=pred.obs.pc)) +
#     geom_bar(stat='identity',  aes(fill = pred.obs.type), width=.5) +
#     coord_flip(ylim =c(-100, 110)) +
#     facet_wrap(~balance)
#
#   # add text
#   dat_text <- out_data
#   dat_text$label <- sprintf(
#     "%s, %s",
#     round(dat_text$mu_devation,0),
#     str_extract(dat_text$balance,"^.{0}")
#   )
#
#   dev_plot_annotate <- dev_plot +
#     geom_text(data = dat_text,
#               size = 3,
#               mapping = aes(x = 8, y = 90, label = label),
#               colour = "blue")
#
#   return(dev_plot_annotate)
# }
#
#
#
# ########################################################################
#
# # Function to run optimising automation
#
# balance_optimisation <- function(trDat , downsample = TRUE, smote = FALSE,
#                                  ds_iterations = c(15, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                                  smote_iterations = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)){
#
#  trDat = trDat2
#  #downsample = FALSE
# #  smote = FALSE
# ## smote_iterations = 0.3
# # ds_iterations = 0.1
#
#   if(downsample == FALSE & smote == FALSE){
#
#     # run the raw model
#     balance_name = "raw"
#
#     # Cross validation loop based on slices
#     slices <- unique(trDat$slice) %>% droplevels()
#
#
#     if(length(slices)<2){
#
#     # switching to transect iteration instead of slices
#
#       trDat_key <- trDat %>%
#         dplyr::select(c(tid)) %>%
#         distinct() %>%
#         mutate(slice = as.factor(seq(1,length(tid),1)))
#
#       trDat <- trDat %>%
#         dplyr::select(-slice) %>%
#         left_join(trDat_key)
#
#       slices <- unique(trDat$slice) %>% droplevels()
#
#     }
#
#
#     # for all slices
#     sresults <- foreach(k = levels(slices)) %do% {
#
#        #k = levels(slices)[1]
#       ### split into train and test based on 5-site slices
#       p
