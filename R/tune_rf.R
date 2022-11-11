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

indata <- list.files(file.path(input_pnts_dir), paste0(mtpt,"_att.*.gpkg$"), full.names = TRUE)
#indata <-"../Pem_standards_manuscripts/inputs/s1_clean_pts_att_2021_2.gpkg"

#indata <- list.files(file.path(input_pnts_dir), paste0(mtpt,"_att.*.gpkg$"), full.names = TRUE)
#indata <-"../Pem_standards_manuscripts/inputs/s1_neighbours_att.gpkg"
indata <- "./temp_data/s1_clean_neighbours_allatts.gpkg"
reduced_covs = c("aspect", "dah","swi_twi", "mrvbf")

#indata <-"../Pem_standards_manuscripts/inputs/allextrapts_merged.gpkg"
tic()
tran_dat <- sf::st_read(indata)
toc()
tic()
tran_dat <- terra::vect(indata) %>% as.data.frame
toc()
# tpts_update <- st_read(indata2) %>% dplyr::select(geom, mapunit1)
# tpts2 <- st_join(tpts, tpts_update, by = "geom") %>% mutate(mapunit1 =  mapunit1.y) %>% dplyr::select(-mapunit1.x, mapunit1.y) %>% dplyr::select(mapunit1, everything())
# st_write(tpts2, "../Pem_standards_manuscripts/inputs/s1_clean_pts_att_2021_2.gpkg")
tpts <- tpts %>% filter(!mapunit1 == "") %>% filter(!is.na(mapunit1)) %>% mutate(mapunit2 = replace_na(mapunit2, "")) %>% distinct()
target1.unique <- tpts %>% dplyr::select(mapunit1) %>% distinct
target2.unique <- tpts %>% dplyr::select(mapunit2) %>% distinct %>% dplyr::rename(mapunit1 = mapunit2)
target.unique <- rbind(target1.unique, target2.unique) %>% distinct
fwrite(target.unique, paste0(AOI_dir, "/fieldmapunits.csv"))


tune_rf <- function(tran_dat, reduced_covs) {
  tran_dat <- terra::vect(indata) %>%
      as.data.frame %>%
      dplyr::select(mapunit1, slice, reduced_covs) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(slice = as.factor(slice), mapunit1 = as.factor(mapunit1)) %>%
      droplevels()

      trees_split <- rsample::initial_split(tran_dat, strata = slice, prop = 1/4)
      trees_train <- rsample::training(trees_split)
      trees_test <- rsample::testing(trees_split)

      tune_spec <- rand_forest(
        mtry = tune(),
        trees = 200,
        min_n = tune()) %>%
        set_mode("classification") %>%
        set_engine("ranger")


      best_recipe <-  recipe(mapunit1 ~ ., data = trees_train) %>%
        update_role(slice, new_role = "id variable")

      tune_wf <- workflow() %>%
        add_recipe(best_recipe) %>%
        add_model(tune_spec)

      set.seed(111)
      trees_folds <- vfold_cv(trees_train)

      doParallel::registerDoParallel()

      set.seed(345)
      tune_res <- tune_grid(
        tune_wf,
        resamples = trees_folds,
        grid = 10
      )

      # tune_res
      # best_tune <- show_best(tune_res, metric = "roc_auc")
      # best_tune2 <- show_best(tune_res, metric = "accuracy")

      # tune_res %>%
      #   collect_metrics() %>%
      #   filter(.metric == "roc_auc") %>%
      #   dplyr::select(mean, min_n, mtry) %>%
      #   pivot_longer(min_n:mtry,
      #                values_to = "value",
      #                names_to = "parameter"
      #   ) %>%
      #   ggplot(aes(value, mean, color = parameter)) +
      #   geom_point(show.legend = FALSE) +
      #   facet_wrap(~parameter, scales = "free_x") +
      #   labs(x = NULL, y = "AUC")

      best_tune <- select_best(tune_res, metric = "accuracy")

      fwrite(best_tune, file.path (out_dir, "best_tuning.csv"))

  # Generate Correlations
#   if(isTRUE(corr_plot)){
#   corrplot::corrplot.mixed(cor(subsmpl, use="pairwise.complete.obs"), lower.col="black") #https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
# }

  return(best_tune)
}



