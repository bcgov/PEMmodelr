#' Run final random forest model
#'
#' @param train_data full training dataset
#' @param fuzz_matrix data table with fuzzy metrics
#' @param mtry numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param min_n numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param ds_ratio numeric
#' @param sm_ratio numeric
#' @return parsnip model object
#' @export
#' @examples
#' run_final_model(train_data,fuzz_matrix, mtry = 14, min_n = 7, ds_ratio = NULL,sm_ratio = NULL)

run_final_model <- function(train_data,fuzz_matrix, mtry, min_n, ds_ratio,sm_ratio){

  train_data <- train_data %>% dplyr::select(-fnf, -x, -y)
  ref_dat <- copy(train_data)
  ref_dat[,mapunit1 := as.factor(mapunit1)]
  ref_dat[,slice := as.factor(slice)]
  ref_dat <- ref_dat[position == "Orig",]
  #print("Training raw data models...")

  ref_dat[,c("id","tid","mapunit2", "position","slice","transect_id","bgc_cat") := NULL]
  low_units <- ref_dat[,.(NumUnit = .N), by = .(mapunit1)][NumUnit < 10,]
  ref_train <- ref_dat[!mapunit1 %in% low_units$mapunit1,]


  final_data <- ref_dat

  #set up model params
  randf_spec <- rand_forest(mtry = mtry, min_n = min_n, trees = 200) %>%
    set_mode("classification") %>%
    set_engine("ranger", importance = "permutation", verbose = FALSE)

  best_recipe <-  recipe(mapunit1 ~ ., data = final_data) %>%
    step_downsample(target, under_ratio = downsample_ratio) %>%
    step_smote(target, over_ratio = smote_ratio , neighbors = 10, skip = TRUE)

  pem_workflow <- workflow() %>%
    add_recipe(best_recipe) %>%
    add_model(randf_spec)

  PEM_rf <- fit(pem_workflow, final_data)
  #final_fit <- extract_fit_parsnip(PEM_rf1)
  #saveRDS(PEM_rf, file =file.path(outDir, "final_model.rds")) # output final model

  return(PEM_rf)

}
