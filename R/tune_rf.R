#' Find optimal tuning for rF model
#'
#' Runs a tuneRF function on all training data to determine optimal mtry and min_n.
#' Saves these hyperparameters for use in model building.
#'
#'
#' @param tpt is the transect training points in table format
#' @param output type of output wanted, can be "full" or "best"
#' @param accuracy type (optional) only applies when output =="best". Options are "accuracy" or "roc")
#' @keywords randomforest, hyperparameter tuning
#' @importFrom parsnip rand_forest set_mode set_engine
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select filter mutate
#' @importFrom rsample vfold_cv initial_split training testing
#' @import recipes
#' @export
#' @examples
#' tune_rf(tpt, reduced_covs)
#'


tune_rf <- function(tpt, output = "full", accuracy_type = NULL) {

  # possible add another function to decide output (accuracy, full outputs)
  #tran_dat = trDat

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

  tune_res <- tune::tune_grid(
    tune_wf,
    resamples = trees_folds,
    grid = 10
  )

  if(output == "full") {
    print("returning full tuning outputs")
    out = tune_res

  } else if(output == "best"){

    print("returning best option only")

    if(accuracy_type == "accuracy"){

      out <- select_best(tune_res, metric = "accuracy")

    }
    if(accuracy_type == "roc_auc"){

      out <- select_best(tune_res, metric = "roc_auc")

    }

  }

  return(out)
}
