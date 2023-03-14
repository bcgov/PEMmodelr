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

library(data.table)
library(terra)
library(sf)

map <- rast(c("LocalData/rid_level.tif","LocalData/swi_slope.tif",
              "LocalData/tpi.tif","LocalData/twi.tif","LocalData/valley_depth_2.tif"))

thesample <- spatSample(map, size = 10000, method = "regular")
thesample <- na.omit(thesample)
#rnge <- range(map$rid_level)

###testingt
target_hypercube <- map
sample_hypercube <- thesample


compare_hypercubes <- function(target_hypercube, sample_hypercube, varlist = "all",
                                     method = "regular", type = "subsample", bins = 10,  minbin = 1,
                               graph_compare = FALSE, return_spatial = FALSE,
                               xy = TRUE, na.rm = TRUE) { #size = 100000,
    sampDat <- as.data.table(sample_hypercube)
    sampDat <- na.omit(sampDat)
    nsamp <- nrow(sampDat)

    ## split and calculate full raster stack
    target_classified <- terra::sapp(target_hypercube, \(x,...) as.numeric(classify(x,10)))

    concat_fn <- function(...){
      return(as.integer(paste0(...)))
    }
    target_id <- lapp(target_classified,concat_fn)

    target_freq <- as.data.table(freq(target_id)) ##target_freq has counts of all bins
    setorder(target_freq,-count)

    ranges <- minmax(target_hypercube)

  ##split into same bins as full data set
  #i = 1
  ##should update this for efficiency
  sample_binned <- copy(sample_hypercube)
  for(i in 1:ncol(sample_hypercube)){
    sample_binned[[i]] <- (cut(sample_hypercube[[i]],
                                 breaks = seq(ranges[1,i],ranges[2,i],length.out = 11),
                                 labels = F, include.lowest = T) - 1) ##start at 0
  }
  sample_binned <- as.data.table(sample_binned)
  sample_binned[,Code := as.numeric(do.call(paste0,.SD)),.SDcols = names(sample_binned)]

  sample_binned <- na.omit(sample_binned)
  sample_freq <- sample_binned[,.(freq = .N), by = .(Code)]##count

  target_freq[sample_freq, sample_num := i.freq,  on = c(value = "Code")]

  return(target_freq)


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
