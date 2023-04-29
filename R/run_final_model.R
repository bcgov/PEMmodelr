#' Run final random forest model
#'
#' @param ref_dat full training dataset
#' @param fuzz_matrix data table with fuzzy metrics
#' @param mtry numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param min_n numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param ds_ratio numeric
#' @param sm_ratio numeric
#' @return parsnip model object
#' @import parsnip
#' @import recipes
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' run_final_model(train_data,fuzz_matrix, mtry = 14, min_n = 7, ds_ratio = NULL,sm_ratio = NULL)

run_final_model <- function(ref_dat,fuzz_matrix, mtry, min_n, ds_ratio = NULL, sm_ratio = NULL){

    #
    # ref_dat= final_data
    # fuzz_matrix = fmat
    # mtry = 14 #mtry
    # min_n = 7 #min_n
    # ds_ratio = NULL
    # sm_ratio = NULL

  # prep data
  ref_dat <- as.data.table(ref_dat)
  ref_dat[,mapunit1 := as.factor(mapunit1)]
  low_units <- ref_dat[,.(NumUnit = .N), by = .(mapunit1)][NumUnit < 10,]
  ref_dat <- ref_dat[!mapunit1 %in% low_units$mapunit1,]
  ref_dat <- ref_dat[complete.cases(ref_dat[, 2:length(ref_dat)]), ]

  #set up model params
  randf_spec <- parsnip::rand_forest(mtry = mtry, min_n = min_n, trees = 200) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("ranger", importance = "permutation", verbose = FALSE)


  # set up downsample and smote options

  if(is.null(ds_ratio) & is.null(sm_ratio)){

    print("no downsample or smoting")

    best_recipe <-  recipes::recipe(mapunit1 ~ ., data = ref_dat)

  }
  if(is.null(ds_ratio) & !is.null(sm_ratio)){

    print("applying smoting")

    best_recipe <-  recipes::recipe(mapunit1 ~ ., data = ref_dat) %>%
      themis::step_smote(mapunit1, over_ratio = sm_ratio , neighbors = 10, skip = TRUE)

  }

  if(!is.null(ds_ratio) & is.null(sm_ratio)){

    print("applying downsample")

    best_recipe <-  recipes::recipe(mapunit1 ~ ., data = ref_dat) %>%
      themis::step_downsample(mapunit1, under_ratio = ds_ratio)

  }
  if(!is.null(ds_ratio) & !is.null(sm_ratio)){

    print("applying downsample and smoting")

    best_recipe <-  recipes::recipe(mapunit1 ~ ., data = ref_dat) %>%
      themis::step_downsample(mapunit1, under_ratio = ds_ratio) %>%
      themis::step_smote(mapunit1, over_ratio = sm_ratio , neighbors = 10, skip = TRUE)

  }

  # set up workflow

  pem_workflow <- workflows::workflow() %>%
    workflows::add_recipe(best_recipe) %>%
    workflows::add_model(randf_spec)

  # run model
  print("running model")

  PEM_rf <- parsnip::fit(pem_workflow, ref_dat)

  return(PEM_rf)

}
