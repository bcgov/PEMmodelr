#' Predict Map Tidy 
#'
#' Takes a tidy model object and associated covariate rasters to generate a thematic map.
#' In order to process the landscape level prediction the input co-variated are tiled
#' and then mosaic'd together at the end.
#'
#' **Action** _is there a bug in tilemaker or with stars causing a xy offset error.
#' Solution is currently a hack.  See `load tile area`.
#'
#' @param model Location of a `mlr` model object (i.e. path to it.)
#' @param cov   A list of raster files.  These will be loaded as a `stars` object
#' @param tilesize Specify the number of pixels in the `x` and `y` directions for the tiles to be generated.  If your computer is mememory limited use a smaller tile (e.g. 500).
#' @param outDir directory for the output file.
#' @keywords predict, landscape
#' @export
#' @examples
#' ### Testing



## libraries  -----
library(dplyr)
library(tidymodels)
library(sf)
library(stars)
library(raster)
library(rgdal)
#library(gdalUtils)
library(GSIF)
#library(pemgeneratr)

#install.packages("GSIF")

# Load GSIF from archive files

#url <- "https://cran.r-project.org/src/contrib/Archive/GSIF/GSIF_0.5-5.tar.gz"
#pkgFile <- "GSIF_0.5-5.tar.gz"
#download.file(url = url, destfile = pkgFile)
#install.packages(c("dismo", "aqp", "plotKML"))
#install.packages(pkgs=pkgFile, type="source", repos=NULL)
# Delete package tarball
#unlink(pkgFile)


#install.packages("FWTools")
library(RSAGA)
#require(meteo)

# Load GSIF from archive files

# url <- "https://cran.r-project.org/src/contrib/Archive/GSIF/GSIF_0.5-5.tar.gz"
# pkgFile <- "GSIF_0.5-5.tar.gz"
# download.file(url = url, destfile = pkgFile)
# install.packages(c("dismo", "aqp", "plotKML"))
# install.packages(pkgs=pkgFile, type="source", repos=NULL, dependencies = TRUE)
# #Delete package tarball
# unlink(pkgFile)


#setwd("/home/rstudio/BEC_DevExchange_Work/")

# read in required functions 
source(here::here('_functions', 'tile_index.R'))

predict_map_tidy <- function(model, cov, tilesize = 500, outDir){
  # 
# model = file.path(outDir, modeltype)
# cov = rast_list
# tilesize = 500
#  outDir = outDir

   
 outtileDir = file.path(outDir,"predicted")

# Adjust names
## This will be used in the loop to rename stars object
n <- basename(cov)
n <- gsub(".tif", "", n)

## Load the model -----
mod <- readRDS(model)
#mod <- readRDS("D:\\GitHub\\PEM_Methods_DevX\\PEM_standards_manuscripts\\models\\paper_all_var\\rF_models\\ESSFmcw\\ESSFmcw_76_noextra_tidy_model.rds")

## Error handle -- model vs. cov -----------
## If names in model features are found in the cov list continue.
## ELSE exit with message
if (length(setdiff(mod$features, n)) != 0) { ## tests if all model features are found in the cov list
  ## On model vs. cov error ---------------
  print("Name mis-match between the model features and the names of the rasters.")
  print("The following raster co-variates are not found in the model features list:")
  print(setdiff(mod$features, n))
} else {
  

  ## create output dir -----------
  ifelse(!dir.exists(outtileDir), dir.create(outtileDir, recursive = TRUE), print("model folder already exists"))
  #dir.create(outDir) ## create output dir -----------
  
  ## create tiles ---------------
  #tiles <- pemgeneratr::tile_index(cov[1], tilesize)
  
  ## alternate -- outside of pemgeneratr
  #ource(here::here('BEC_DevExchange_Work','_functions', 'tile_index.R'))
  # source("./R/tile_index.R")  ## load the tile index function
 
  tiles <- tile_index(cov[1], tilesize)
  
  ## begin loop through tiles -----
  
  ## set up progress messaging
  a <- 0 ## running total of area complete
  ta <- sum(as.numeric(sf::st_area(tiles)))
  
  
  for (i in 1:nrow(tiles)) {    ## testing first 2 tiles       ##nrow(tiles)) {
   # 
   #i = 20 # testing line
    
    t <- tiles[i,]  ## get tile
    print(paste("working on ", i, "of", nrow(tiles)))
    print("...")
    
    ## * load tile area---------
    print("... loading new data (from rasters)...")
    r <- stars::read_stars(cov,
                           RasterIO = list(nXOff  = t$offset.x[1]+1, ## hack -- stars and tile_maker issue??
                                           nYOff  = t$offset.y[1]+1,
                                           nXSize = t$region.dim.x[1],
                                           nYSize = t$region.dim.y[1]), proxy = TRUE)
    
    r_template <- stars::read_stars(cov[1], 
                           RasterIO = list(nXOff  = t$offset.x[1]+1, ## hack -- stars and tile_maker issue??
                                           nYOff  = t$offset.y[1]+1,
                                           nXSize = t$region.dim.x[1],
                                           nYSize = t$region.dim.y[1]), proxy = FALSE)
      
    ## * update names ---------
    #names(r) <- n
    #rnames = gsub(".tif","", basename(r))
    
    ## * convert tile to dataframe ---------
    rsf <- sf::st_as_sf(r, as_points = TRUE)
    
    rr <- gsub(".tif","", names(rsf)) 
    #names(rsf)= c(rnames, "geometry")
    names(rsf)= rr
    
    rsf.df <- as.data.frame(rsf)
    # rsf <- na.omit(rsf)  ## na.omit caused issues
    
    
    ## * Test if tile is empty -------------
    na_table <- as.data.frame(sapply(rsf, function(x) all(is.na(x))))
    ## * determine na counts -- this will be used to restore NA values if tile is run.
    na_table$count <- as.data.frame(sapply(rsf, function(x) sum(is.na(x))))[,1]
    
    ## If any attribute is empty skip the tile
    if(any(na_table[,1] == TRUE)) {
      
      print("Some variables with all NA values, skipping tile...")
      
    } else{
      
      ## * predict ---------
      
      ## * * Managing NA values ----------------
      ## When some of the values are NA change them to zero
      ## * Identify the attribute with the highest number of NA values.
      na_max <- na_table[na_table$count ==  max(na_table$count),]
      na_max <- row.names(na_max[1,]) ## if multiple attributes -- take the first
      ## make a copy of the attribute with the highest number of na values
      rsf_bk <- rsf[,na_max]  ## -- this will be used to restore NA values
      
      ## When some of the values are NA change them to zero -- facilitates pred()
      rsf[is.na(rsf)] <- 0 ## convert NA to zero as the predict function cannot handle NA
      
      print("... modelling outcomes (predicting)...")
      pred <- predict(mod, rsf)
      
      ## Restore NA values
      #pred_dat <- cbind(rsf.df, pred) ## predicted values extracted then changed
      #pred_dat[is.na(rsf_bk[,1]), 1:length(pred_dat)] <- NA ## if originally NA restore NA
      #pred$data <- pred_dat ## values restored to pred -- allows for cbind without issue.
      
      ## * geo-link predicted values ---------
      r_out <- cbind(rsf, pred)
      
      ## layers to keep (i.e. newly predicted layers)
      keep <- setdiff(names(r_out),
                      names(rsf))
      #keep <- keep[-length(keep)] ## drops the last entry (geometry field, not a name)
      
      r_out <- r_out %>% dplyr::select(keep)
      
      
      ## Save the names of the model response -----
      ## The levels are in the multiclass 'response'
      wkey <- 0
      if (wkey == 0)  {
        respNames <- levels(r_out$.pred_class) ## this becomes the dictionary to describe the raster values
        write.csv(respNames, paste(outDir, "response_names.csv",
                                   sep = "/"),
                  row.names = TRUE)
        wkey <- 1 ## Change this value so this small is statement does not execute again.
      }
      
      
      
      ## change the text values to numeric values.
      r_out$.pred_class <- as.numeric(r_out$.pred_class)
      
      ## Set up subdirectories for rastertile outputs
      print("... exporting raster tiles...")
     # for (k in keep) {
    #    dir.create(paste(outDir, k, sep = "/"))
    #  }
      
      
      ## * save tile (each pred item saved) ---------
      #for (j in 1:length(keep)) {
         j <- 1  ## testing
        out <- stars::st_rasterize(r_out[j],
                                   template = r_template)

        #stars::write_stars(out,
        #                   paste0(outDir,"/",
        #                          keep[j], "/",             #sub-directoy
        #                          keep[j], "_", i, ".tif")) #tile name
        
        stars::write_stars(out,
                           paste0(outtileDir,"/", i,".tif")) #tile name
       
      }
      
      ## * report progress -----
      a <- a + as.numeric(sf::st_area(t))
      print(paste(round(a/ta*100,1), "% complete"))
      print("") ## blank line
      
    } ## end if statement -- for when tile is empty
  } ## END LOOP -------------
  print("All predicted tiles generated")
  
  Sys.unsetenv("PROJ_LIB")
 
   ## Mosaic Tiles ---------------
  
  print("Generating raster mosaics")
  #for (k in keep) {
   
    r_tiles <- list.files(outtileDir,
                          pattern = ".tif$",
                          full.names = TRUE)

    ## mosaic
    gdalUtils::mosaic_rasters(gdalfile = r_tiles, ## list of rasters to mosaic
                              dst_dataset = paste0(outDir, "/mosaic.tif"),  #output: dir and filename
                              output_Raster = TRUE,
                              verbose=TRUE) ## saves the raster (not just a virtual raster)
    
  } ### end function



mosaic_map_tidy <- function(outDir){
  
  print("Generating raster mosaics")
  #for (k in keep) {
  outtileDir <-  file.path(outDir,"predicted")
  
  r_tiles <- list.files(outtileDir,
                        pattern = ".tif$",
                        full.names = TRUE)
  
  ## mosaic
  gdalUtils::mosaic_rasters(gdalfile = r_tiles, ## list of rasters to mosaic
                            dst_dataset = paste0(outDir, "/mosaic.tif"),  #output: dir and filename
                            output_Raster = TRUE,
                            verbose=TRUE) ## saves the raster (not just a virtual raster)
  
} ### end function


# 
# 
# #outDir <- "E:/temp/PEM_DATA/LiDAR_AOI/Williams_lake/100mile_2020_LiDAR_DEMs/LAZ4_DEM1m/"
# outDir <- "E:/temp/PEM_DATA/LiDAR_AOI/Williams_lake/100mile_2020_LiDAR_DEMs/mosaic/"
# r_tiles <- list.files(outDir,
#                       pattern = ".tif$",
#                       full.names = TRUE)
# 
# r_tiles <- r_tiles
# library(tictoc)
# tic()
# ## mosaic
# gdalUtils::mosaic_rasters(gdalfile = r_tiles, ## list of rasters to mosaic
#                           dst_dataset = paste0(outDir, "/mosaic_full.tif"),  #output: dir and filename
#                           output_Raster = TRUE,
#                           verbose=TRUE) ## saves the raster (not just a virtual raster)
# toc()
# 
