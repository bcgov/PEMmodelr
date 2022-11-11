#' Find optimal tuning for rF model
#'
#' Runs a tuneRF function on all training data to determine optimal mtry and min_n.
#' Saves these hyperparameters for use in model building.
#'
#'
#' @param tran_dat is the transect training points
#' @param reduced_var is a  data.frame listing names of reduced features
#'
#' @keywords randomforest, hyperparameter tuning
#' @export
#' ##
indata <- "./temp_data/s1_clean_neighbours_allatts.gpkg"
reduced_covs <- c("aspect", "dah", "swi_twi", "mrvbf")
source("./R/list_mapunits.R")
source("./R/select_pure_training.R")

tictoc::tic()
tran_dat <- terra::vect(indata) %>% as.data.frame()
tictoc::toc()

tran_dat <- select_pure_training(tran_dat)

mapunits_unique <- list_mapunits(tran_dat)

fwrite(target.unique, paste0(AOI_dir, "/fieldmapunits.csv"))


tune_rf <- function(tran_dat, reduced_covs) {
  tran_dat <- terra::vect(indata) %>%
    as.data.frame() %>%
    select_pure_training() %>%
    dplyr::select(mapunit1, slice, all_of(reduced_covs)) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(slice = as.factor(slice), mapunit1 = as.factor(mapunit1)) %>%
    droplevels()

  trees_split <- rsample::initial_split(tran_dat, strata = slice, prop = 1 / 10)
  trees_train <- rsample::training(trees_split)
  trees_test <- rsample::testing(trees_split)

  tune_spec <- parsnip::rand_forest(
    mtry = tune(),
    trees = 10,
    min_n = tune()
  ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("ranger")


  best_recipe <- recipes::recipe(mapunit1 ~ ., data = trees_train) %>%
    recipes::update_role(slice, new_role = "id variable")

  tune_wf <- workflows::workflow() %>%
    workflows::add_recipe(best_recipe) %>%
    workflows::add_model(tune_spec)

  trees_folds <- rsample::vfold_cv(trees_train, v = 10, repeats = 1, strata = slice)

  doParallel::registerDoParallel()
tic()
  tune_res <- tune::tune_grid(
    tune_wf,
    resamples = trees_folds,
    grid = 10
  )
toc()

  best_tune <- select_best(tune_res, metric = "accuracy")

  fwrite(best_tune, file.path(out_dir, "best_tuning.csv"))

  return(best_tune)
}
