#' Create list of uncorrelated variables
#'
#' Runs a correlation on subsample of points taken from the subsample_map_raster function and runs a correlation analysis
#' and saves a feature list that is used to reduce the set of variables in the machine learning model.
#' @param subsmpl is the data from subsample_map_raster function
#' @param cutoff set the correlation threshold for removal
#' @param corr_plot output a correlation plot
#'
#' @keywords correlation, feature reduction
#' @import dplyr
#' @import magrittr
#' @importFrom stats cor
#' @importFrom caret findCorrelation
#' @export
#' @examples
#' reduce_features(subsmpl, 0.9, corr_plot = FALSE)
#
reduce_features <- function(subsmpl, cutoff = 0.90, corr_plot = FALSE) {

  #subsmpl <- subsmpl %>% dplyr::select(-any_of(c("x", "y")))
  subsmpl <- data.frame(subsmpl)
  subsmpl <-  subsmpl[complete.cases(subsmpl),]

  # Generate Correlations
  if(isTRUE(corr_plot)){
  corrplot::corrplot.mixed(cor(subsmpl, use="pairwise.complete.obs"), lower.col="black") #https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
}
  corr_matrix <- stats::cor(subsmpl)
  hc <-  caret::findCorrelation(corr_matrix, cutoff = cutoff) # putt any value as a "cutoff"
  #hc <-  sort(hc)
  reduced_covs <- names(subsmpl[,-c(hc)])
  removed_covs <- names(subsmpl[,c(hc)])
  print("the following covariates are removed as highly correlated:")
  print(removed_covs)

  return(reduced_covs)
}

