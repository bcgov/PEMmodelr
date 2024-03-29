#' Compare two hypercubes
#'
#' Compares the hypercube dimensions of two data sets for either:
#' 1. a space-space comparison where target is an area-of-interest (AOI) and sample is a subsample taken from the same area; or
#' 2. a space-time comparison where target is a future climate for an AOI and sample is an area of historic source climates
#' Creates a hypercube label for each point from bin numbers for each raster
#' @param type either "subsample", "space-space", "space-time"
#' @param target_hypercube is folder location where all predictors are stored; for type space-time = raster for future climates of AOI
#' @param sample_hypercube for subsample a file of points; for space-space and space-time a rastfolder location with comparative rasters
#' @param varlist list of variables to include in hypercube
#' @param sample_type either "number", "density'
#' @param size the number of samples to generate or the density of points in kilometers
#' @param method either "regular" for systematic sampling or "random"
#' @param bins number of bins to create for each raster layer
#' @param bin_width_type bins created by "equalwidth" or "equiprobable"
#' @param graph_compare TRUE/FALSE to generate a ggplot comparing target and sample distibutions for each variable
#' @param return_spatial TRUE/FALSE to generate of 2 layers ina geopackage showing where there is missing space in the target AOI and source areas from the sample
#' @param min_bin the minimum number of points in a bin to be considered adequately sampled
#' @import data.table
#' @importFrom terra sapp
#' @importFrom sf st_drop_geometry
#' @keywords subsample, covariates, predictors, raster
#' @export
#' @examples
#' # example code
#'

# library(data.table)
# library(terra)
# library(sf)
# require(tidyverse)

# map <- rast(c("LocalData/rid_level.tif","LocalData/swi_slope.tif",
#               "LocalData/tpi.tif","LocalData/twi.tif","LocalData/valley_depth_2.tif"))
#
# #thesample <- spatSample(map, size = 10000, method = "regular")
# #thesample <- na.omit(thesample)
# thesample <- st_read(file.path("D:/GitHub/PEMmodelr/LocalData/s1_clean_neighbours_allatts.gpkg"))
#
# ### KIRI build into the function to read the variables in map and use those to select from thesample
# thesample <- as.data.table(thesample) %>% filter(Position == "Orig", slice == 5)  %>% dplyr::select(rid_level, swi_slope, tpi, twi, valley_depth_2)
# rnge <- range(map$rid_level)
#
# ###testingt
# result <- compare_hypercubes(target_hypercube = map, sample_hypercube = thesample, bins = 10)
# i = 1; bins = 10; target_hypercube = map

compare_hypercubes <- function(target_hypercube, sample_hypercube, varlist = "all",
                               method = "regular", type = "subsample", bins = 10, bind_width_type = "equiprobable", minbin = 1,
                               graph_compare = FALSE, return_spatial = FALSE,
                               xy = TRUE) { #size = 100000,

  ## split and calculate full raster stack
  if(bins > 10) warning("This function will probably give incorrect result if bins > 10")
  message("Creating target hypercube...\n")
  target_classified <- terra::sapp(target_hypercube, \(x,...) setValues(x,values(classify(x,bins))))

  concat_fn <- function(...){
    data <- list(...)
    out <- data[[1]]
    for(i in 2:length(data)){
      out <- out + data[[i]]*10^(i-1)
    }
    return(out)
  }
  target_id <- lapp(target_classified,concat_fn)

  target_freq <- as.data.table(freq(target_id)) ##target_freq has counts of all bins

  setorder(target_freq,-count)

  ranges <- minmax(target_hypercube)

  ##split into same bins as full data set
  ##should update this for efficiency
  message("Creating sample hypercube...\n")
  if(inherits(sample_hypercube,"sf")) sample_hypercube <- sf::st_drop_geometry(sample_hypercube)
  sample_hypercube <- as.data.table(sample_hypercube)
  sample_hypercube <- sample_hypercube[,rev(names(target_classified)), with = F]
  sample_hypercube <- na.omit(sample_hypercube)
  sample_binned <- copy(sample_hypercube)
  ranges <- ranges[,names(sample_hypercube)]
  for(i in 1:ncol(sample_hypercube)){
    sample_binned[[i]] <- (cut(sample_hypercube[[i]],
                               breaks = seq(ranges[1,i],ranges[2,i],length.out = bins+1),
                               labels = F, include.lowest = T) - 1) ##start at 0
  }
  sample_binned <- as.data.table(sample_binned)
  sample_binned[,Code := as.numeric(do.call(paste0,.SD)),.SDcols = names(sample_binned)]

  sample_binned <- na.omit(sample_binned)
  sample_freq <- sample_binned[,.(freq = .N), by = .(Code)]##count

  target_freq[sample_freq, sample_num := i.freq,  on = c(value = "Code")]
  missed_bins <- target_freq[is.na(sample_num),value]
  pattern <- paste0(0,"|",bins-1)
  target_freq[,is_extreme := grepl(pattern,value)]
  target_freq[value < 10^(nlyr(target_hypercube)-1), is_extreme := TRUE]
  missed_extremes <- target_freq[is.na(sample_num) & is_extreme,value]

  message("Conducting spatial analysis...\n")
  miss_id <- cells(target_id, missed_bins)
  missex_id <- cells(target_id, missed_extremes)
  rast_extreme <- copy(target_id)
  ##all missed
  target_id[!is.na(target_id)] <- -1
  target_id[miss_id$lyr1] <- 1
  #plot(target_id)
  cells_covered <- as.data.table(freq(target_id))
  miss_perc <- cells_covered[value == 1,count]/sum(cells_covered$count)

  ##extremes
  rast_extreme[!is.na(rast_extreme)] <- -1
  rast_extreme[missex_id$lyr1] <- 1
  #
  cellsex_covered <- as.data.table(freq(rast_extreme))
  missex_perc <- cellsex_covered[value == 1,count]/sum(cellsex_covered$count)
  return(list(percent_miss = miss_perc*100,percent_miss_extreme = missex_perc*100,
              miss_rast = target_id, miss_rast_ex = rast_extreme, var_range = ranges))


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

# require(DataExplorer)
# DataExplorer::create_report(thesample)
#
# thesample$tpi2 <- as.numeric(cut_number(thesample$tpi,10))
# thesample$tpi3 <- as.numeric(cut(thesample$tpi,10))
#
# map2$tpi2 <- as.numeric(cut_number(map2$tpi,10))
# map2$tpi3 <- as.numeric(cut(map2$tpi,10))
# DataExplorer::create_report(map2)

# require(tidyterra)
# ggplot() +
#   geom_spatraster(data = subset(map,1)) +
#   facet_wrap(~lyr) +
#   scale_fill_whitebox_c(
#     palette = "muted",
#     na.value = "white"
#   )
