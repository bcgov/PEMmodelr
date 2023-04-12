



predict_map <- function(model, cov, tile_size, out_dir){


  model = fmodel

  covs =


   rf_fit <-extract_fit_parsnip(fmodel)

  # get predictor set:
  rf_features <-
    rf_fit %>%
    extract_fit_engine() %>%
    caret::predictors()  #<- the caret funciton



  PEMprepr::mosaic_covariates


}
