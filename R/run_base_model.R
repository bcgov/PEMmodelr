#' Run the basic model
#'
#' @param train_data data table containing the training data set
#' @param fuzz_matrix data table with fuzzy metrics
#' @param mtry numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param min_n numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param use.neighbours. if you want to incluse all neighbours in the calculation
#' @param detailed_output OPTIONAL:TRUE/FALSE if you want to output all raw values this is used to determine optimum theta values
#' @param out_dir OPTIONAL: only needed if detailed_output = TRUE. location of filepath there detailed outputs to be stored
#' @return datatable of accuracy metric
#' @export
#' @importFrom foreach "%do%" foreach
#' @importFrom magrittr "%>%"
#' @importFrom ranger ranger
#' @examples
#' run_base_model(train_pts, fuzz_matrix, mtry = 14, min_n = 7, use.neighbours = TRUE)

run_base_model <- function(train_data,
                           fuzz_matrix,
                           mtry = 14,
                           min_n = 7,
                           use.neighbours = TRUE,
                           detailed_output = FALSE,
                           out_dir){

# # # # # # # testing lines:
# train_data = train_data
# fuzz_matrix = fmat
# mtry = mtry
# min_n = min_n
# use.neighbours = FALSE
# detailed_output = TRUE
# out_dir = detailed_outdir
# #
# # # # # end testing lines

  # training set - train only on pure calls
  ref_dat <- train_data %>%
    dplyr::mutate(mapunit1 = as.factor(mapunit1),
                  slice = as.factor(slice))
  print("Training raw data models...")

  munits <- unique(ref_dat$mapunit1)
  nf_mapunits <- grep(munits, pattern = "_\\d", value = TRUE, invert = TRUE)

  slices <- unique(ref_dat$slice) %>% droplevels()

    # check the no of slices
    if(length(slices)<2){ # switching to transect iteration instead of slices

      ref_dat_key <- ref_dat %>%
        dplyr::select(c(tid)) %>%
        distinct() %>%
        mutate(slice = as.factor(seq(1,length(tid),1)))

      ref_dat <- ref_dat %>%
        dplyr::select(-slice) %>%
        left_join(ref_dat_key)

      slices <- unique(ref_dat$slice) %>% droplevels()

    }

    ref_acc <- foreach::foreach(k = levels(slices),.combine = rbind) %do% {

      #k = levels(slices)[3]
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
          filter(position == "Orig") %>%
          droplevels()

      }

      ref_id <- ref_test %>% select(id, mapunit1, mapunit2 )

      null_recipe <-  recipes::recipe(mapunit1 ~ ., data = ref_train) %>%
        recipes::update_role(tid, new_role = "id variable")

      randf_spec <- parsnip::rand_forest(mtry = mtry, min_n = min_n, trees = 151) %>%
        parsnip::set_mode("classification") %>%
        parsnip::set_engine("ranger", importance = "permutation", verbose = FALSE)

      pem_workflow <- workflows::workflow() %>%
        workflows::add_recipe(null_recipe) %>%
        workflows::add_model(randf_spec)

      ref_mod <- parsnip::fit(pem_workflow, ref_train)

      final_fit <- tune::extract_fit_parsnip(ref_mod)

      oob  <- round(ref_mod$fit$fit$fit$prediction.error, 3)

      preds <- predict(ref_mod, ref_test)

      pred_all <- cbind(ref_id,.pred_class = preds$.pred_class)

      pred_all <- pred_all %>% mutate(mapunit1 = as.character(mapunit1),
                                      mapunit2 = as.character(mapunit2),
                                      .pred_class = as.character(.pred_class))

      # switch out the predicted Nf units for "nonfor" catergory.
      pred_all <- pred_all %>%
        dplyr::mutate(mapunit1 = ifelse(mapunit1  %in% nf_mapunits, "nonfor", mapunit1 ),
                      mapunit2 = ifelse(mapunit2 %in% nf_mapunits, "nonfor", mapunit2) ,
                      .pred_class = ifelse(.pred_class  %in% nf_mapunits, "nonfor", .pred_class ))

      # harmonize factor levels
      pred_all <- harmonize_factors(pred_all)
      pred_all$mapunit2 = as.factor(pred_all$mapunit2)

      print(paste0("generating accuracy metrics for slice:",k))

      if(detailed_output == TRUE){
        saveRDS(pred_all, file.path(out_dir, paste0("predictions_", k)))
      }

      acc <- acc_metrics(pred_all, fuzzmatrx = fuzz_matrix) %>%
        dplyr::mutate(slice = k,
                      oob = oob)

     #write.csv(acc, "test2_acc.csv")
    }

  return(ref_acc)

}
