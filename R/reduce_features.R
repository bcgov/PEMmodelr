#' Create list of uncorrelated variables
#'
#' Runs a correlation on subsample of points taken from the subsample_map_raster function and runs a correlation analysis
#' and saves a feature list that is used to reduce the set of variables in the machine learning model.
#'
#'
#' @param subsmpl is the data from subsample_map_raster function
#' @param cutoff set the correlation threshold for removal
#' @param corr_plot output a correlation plot
#'
#' @keywords correlation, feature reduction
#' @export
#' ##

reduce_features <- function(subsmpl, cutoff = 0.90, corr_plot = FALSE) {

  subsmpl <- subsmpl %>% dplyr::select(-any_of(c("x", "y")))

  # Generate Correlations
  if(isTRUE(corr_plot)){
  corrplot::corrplot.mixed(cor(subsmpl, use="pairwise.complete.obs"), lower.col="black") #https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
}
  corr_matrix <-  stats::cor(subsmpl)
  hc <-  caret::findCorrelation(corr_matrix, cutoff = cutoff) # putt any value as a "cutoff"
  #hc <-  sort(hc)
  reduced_covs <-  names(Predictors[,-c(hc)])
  return(reduced_covs)
}

