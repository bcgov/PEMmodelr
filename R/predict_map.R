


# # make tiles (might want to make NA tile )
# ntiles <- makeTiles(template, 500, filename= file.path(fid$model_final[2], "tile_.tif"),  na.rm=FALSE, overwrite = TRUE)
#
#
# # select reduced variables
# reduced_vars <- read.csv(file.path(fid$model_inputs0310[2],  "reduced_covariate_list.csv")) %>% pull()
#
# # get full stack
# rstack <- terra::rast(rast_list)
#
#
# map_bgc <- for(b in bec_zone){
#
#   # read in model
#   model <- readRDS(file.path(fid$model_final[2], mfiles))
#
#   # extract fit
#   rf_fit <- workflows::extract_fit_parsnip(model)
#
#
#   for (i in ntiles)) {
#
#     # get tile
#     i <- ntiles[7]
#
#     t <- terra::rast(file.path(i))  ## read in tile
#     print(paste("working on ", basename(i), "of", length(ntiles)))
#     print("...")
#
#     print("... loading new data (from rasters)...")
#
#     tvec <- terra::as.polygons(t)
#
#     tstack <- terra::extract(rstack, tvec)
#
#
#     # predict
#
#     pred <-  predict(rf_fit, tstack)
#     pred <- pp
#
#
#
#
#     allrasts <- file.path(covdir, res_folder)
#
#
#     tile_size
#     out_dir
#
