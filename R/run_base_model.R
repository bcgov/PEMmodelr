#' Run the basic model
#'
#' @param train_data data table containing the training data set
#' @param fuzz_matrix data table with fuzzy metrics
#' @param mtry numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param min_n numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param use.neighbours. if you want to incluse all neighbours in the calculation
#' @return datatable of accuracy metric
#' @export
#' @importFrom foreach "%do%"
#' @importFrom magrittr "%>%"
#' @importFrom ranger ranger
#' @examples
#' run_base_model(train_pts, fuzz_matrix, mtry = 14, min_n = 7, use.neighbours = TRUE)

run_base_model <- function(train_data, fuzz_matrix, mtry = 14, min_n = 7, use.neighbours = TRUE){

  # # testing lines:
  # train_data = train_data
  # fuzz_matrix = fmat
  # mtry = 14
  # min_n = 7
  # use.neighbours = TRUE
  # # end testing lines

  ref_dat <- copy(train_data)
  ref_dat[,mapunit1 := as.factor(mapunit1)]
  ref_dat[,slice := as.factor(slice)]
  print("Training raw data models...")

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

    ref_acc <- foreach(k = levels(slices),.combine = rbind) %do% {
      #k = levels(slices)[1]
      ref_train <- ref_dat[slice != k & position == "Orig",]
      ref_train[,c("id","tid","mapunit2", "position","slice","transect_id","bgc_cat") := NULL]
      low_units <- ref_train[,.(NumUnit = .N), by = .(mapunit1)][NumUnit < 10,]
      ref_train <- ref_train[!mapunit1 %in% low_units$mapunit1,]

      if (use.neighbours) {
        ref_test <- ref_dat[slice == k & !mapunit1 %in% low_units$mapunit1,]
      }else{
        ref_test <- ref_dat[slice == k & !mapunit1 %in% low_units$mapunit1 & position == "Orig",]
      }

      ref_mod <- ranger::ranger(mapunit1 ~ ., data = ref_train, mtry = mtry,
                                num.trees = 151, min.node.size = min_n, importance = "permutation")

      preds <- predict(ref_mod, ref_test)
      pred_all <- cbind(ref_test[,.(id, mapunit1, mapunit2, slice)],
                        .pred_class = preds$predictions)
      pred_all$mapunit1 <- as.factor(pred_all$mapunit1)
      pred_all$.pred_class <- factor(pred_all$.pred_class,
                                     levels = levels(pred_all$mapunit1))

      print(paste0("generating accuracy metrics for slice:",k))

      acc <- acc_metrics(pred_all, fuzzmatrx = fuzz_matrix)
    }

    return(ref_acc)

}
