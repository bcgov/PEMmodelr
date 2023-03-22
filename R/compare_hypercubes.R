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
#' @param graph_compare TRUE/FALSE to generate a ggplot comparing target and sample distibutions for each variable
#' @param return_spatial TRUE/FALSE to generate of 2 layers ina geopackage showing where there is missing space in the target AOI and source areas from the sample
#' @param min_bin the minimum number of points in a bin to be considered adequately sampled
#' @import data.table
#' @import terra
#' @keywords subsample, covariates, predictors, raster
#' @export
#' compare_hypercubes(target_hypercube, sample_hypercube, varlist = "all", method = "regular", type = "subsample", bins = 10,  minbin = 1,graph_compare = FALSE, return_spatial = FALSE,xy = TRUE)
#
# library(data.table)
# library(terra)
# library(sf)
# require(tidyverse)
#
# map <- rast(c("LocalData/rid_level.tif","LocalData/swi_slope.tif",
#               "LocalData/tpi.tif","LocalData/twi.tif","LocalData/valley_depth_2.tif"))
#
# #thesample <- spatSample(map, size = 10000, method = "regular")
# #thesample <- na.omit(thesample)
# thesample <- st_read(file.path("D:/GitHub/PEMmodelr/LocalData/s1_clean_neighbours_allatts.gpkg"))
#
# ### KIRI build into the function to read the variables in map and use those to select from thesample
# thesample <- as.data.table(thesample) %>% filter(Position == "Orig") %>% dplyr::select(rid_level, swi_slope, tpi, twi, valley_depth_2)
# rnge <- range(map$rid_level)
#
# ###testingt
# result <- compare_hypercubes(target_hypercube = map, sample_hypercube = thesample, bins = 9, minbin = 0)
# #
#
# compare_hypercubes <- function(target_hypercube, sample_hypercube, varlist = "all",
#                                method = "regular", type = "subsample", bins = 10,  minbin = 1,
#                                graph_compare = FALSE, return_spatial = FALSE,
#                                xy = TRUE) { #size = 100000,
#
#   ## split and calculate full raster stack
#   cat("Creating target hypercube...\n")
#   target_classified <- terra::sapp(target_hypercube, \(x,...) as.numeric(classify(x,bins)))
#
#   concat_fn <- function(...){
#     return(as.integer(paste0(...)))
#   }
#   target_id <- lapp(target_classified,concat_fn)
#
#   target_freq <- as.data.table(freq(target_id)) ##target_freq has counts of all bins
#   setorder(target_freq,-count)
#
#   ranges <- minmax(target_hypercube)
#
#   ##split into same bins as full data set
#   ##should update this for efficiency
#   cat("Creating sample hypercube...\n")
#   sample_hypercube <- as.data.table(sample_hypercube)
#   sample_hypercube <- na.omit(sample_hypercube)
#   sample_binned <- copy(sample_hypercube)
#   for(i in 1:ncol(sample_hypercube)){
#     sample_binned[[i]] <- (cut(sample_hypercube[[i]],
#                                breaks = seq(ranges[1,i],ranges[2,i],length.out = 11),
#                                labels = F, include.lowest = T) - 1) ##start at 0
#   }
#   sample_binned <- as.data.table(sample_binned)
#   sample_binned[,Code := as.numeric(do.call(paste0,.SD)),.SDcols = names(sample_binned)]
#
#   sample_binned <- na.omit(sample_binned)
#   sample_freq <- sample_binned[,.(freq = .N), by = .(Code)]##count
#
#   target_freq[sample_freq, sample_num := i.freq,  on = c(value = "Code")]
#   missed_bins <- target_freq[is.na(sample_num),value]
#   target_freq[,is_extreme := grepl("0|9",value)]
#   target_freq[value < 10^(nlyr(target_hypercube)-1), is_extreme := TRUE]
#   missed_extremes <- target_freq[is.na(sample_num) & is_extreme,value]
#
#   cat("Conducting spatial analysis...\n")
#   miss_id <- cells(target_id, missed_bins)
#   missex_id <- cells(target_id, missed_extremes)
#   rast_extreme <- copy(target_id)
#   ##all missed
#   target_id[!is.na(target_id)] <- -1
#   target_id[miss_id$lyr1] <- 1
#   plot(target_id)
#   cells_covered <- as.data.table(freq(target_id))
#   miss_perc <- cells_covered[value == 1,count]/sum(cells_covered$count)
#
#   ##extremes
#   rast_extreme[!is.na(rast_extreme)] <- -1
#   rast_extreme[missex_id$lyr1] <- 1
#   #
#   cellsex_covered <- as.data.table(freq(rast_extreme))
#   missex_perc <- cellsex_covered[value == 1,count]/sum(cellsex_covered$count)
#   cat("Done!\n")
#   return(list(percent_miss = miss_perc*100,percent_miss_extreme = missex_perc*100,
#               miss_rast = target_id, miss_rast_ex = rast_extreme, var_range = ranges))
#
#
#   ### returns
#   ## for each variable the min and max values for variable and the max value for each bin in the AOI
#   ## % of AOI area with missed bins
#   ## % of AOI area with missed extreme bins (missed bins with either a 1 or 9 in the label)
#   ## Kullback-Liebler deficit metric, remove excessive bin sampling then perform KLD
#
#   # cov_list <- list.files(rastfolder, full.names = TRUE)
#   #   covs <- terra::rast(cov_list)
#   #       subsmpl <- terra::spatSample(covs , size = size, method = method, xy = TRUE, na.rm = TRUE) # sample raster
#   # return(subsmpl)
# }
