#' Compare two hypercubes
#'
#' Compares the hypercube dimensions of two data sets for either:
#' 1. a space-space comparison where target is an area-of-interest (AOI) and sample is a subsample taken from the same area; or
#' 2. a space-time comparison where target is a future climate for an AOI and sample is an area of historic source climates
#' Creates a hypercube label for each point from bin numbers for each raster
#'
#'
#' @param type either "subsample", "space-space", "space-time"
#' @param target_hypercube is folder location where all predictors are stored; for type space-time = raster for future climates of AOI
#' @param sample_hypercube for subsample a file of points; for space-space and space-time a rastfolder location with comparative rasters
#' @param varlist list of variables to include in hypercube
#' @param sample_type either "number", "density'
#' @param size the number of samples to generate or the density of points in kilometers
#' @param method either "regular" for systematic sampling or "random"
#' @param bins number of bins to create for each raster layer
#' @param graph_compare TRUE/FALSE to generate a ggplot comparing target and sample distibutions for each variable
#' @param return_spatial TRUE/FALSE to generate of 2 layers ina geopackage showing where there is missing space in the target AOI and source areas from the sample
#' @param min_bin the minimum number of points in a bin to be considered adequately sampled
#'
#' @keywords subsample, covariates, predictors, raster
#' @export
#' ##


compare_hypercubes <- function(target_hypercube = file.path(cov_dir, res_dir), sample_hypercube, varlist = "all", size = 100000,
                                     method = "regular", type = "subsample", bins = 10,  minbin = 10, graph_compare = FALSE, return_spatial = FALSE,
                               xy = TRUE, na.rm = TRUE) {

### returns
  ## for each variable the min and max values for variable and the max value for each bin in the AOI
  ## % of AOI area with missed bins
  ## % of AOI area with missed extreme bins (missed bins with either a 1 or 9 in the label)
  ## Kullback-Liebler deficit metric, remove excessive bin sampling then perform KLD

  # cov_list <- list.files(rastfolder, full.names = TRUE)
#   covs <- terra::rast(cov_list)
#       subsmpl <- terra::spatSample(covs , size = size, method = method, xy = TRUE, na.rm = TRUE) # sample raster
# return(subsmpl)
}
