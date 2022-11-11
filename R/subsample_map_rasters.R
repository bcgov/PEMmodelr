#' Generate systematic subsample of pointsfrom AOI map area
#'
#' Create a subset  full raster stack for an AOI of stand-level predictors for use in the feature reduction functions
#'
#'
#' @param rastfolder is folder location where all predictors are stored
#' @param size the number of samples to generate
#' @param method either "regular" for systematic sampling or "random"

#'
#' @keywords subsample, covariates, predictors, raster
#' @export
#' ##

# setwd("D:/GitHub/PEMsamplr")
# dtm <- ("./temp_data/dem.tif")
# SAGApath <- "C:/SAGA/"
# layers = "all"
# output = "./landscape_covariates"
# sieve_size = 10
# cov_dir = "temp_data"
# res_dir = "landscape_covariates"
# rastfolder = file.path(cov_dir, res_dir)
# size = 100000
# method = "regular"
# xy = TRUE
# na.rm = TRUE
subsample_map_rasters <- function(rastfolder = file.path(cov_dir, res_dir), size = 100000,
                                     method = "regular",
                                     xy = TRUE, na.rm = TRUE) {

cov_list <- list.files(rastfolder, full.names = TRUE)
  covs <- terra::rast(cov_list)
      subsmpl <- terra::spatSample(covs , size = size, method = method, xy = TRUE, na.rm = TRUE) # sample raster
return(subsmpl)
}
