


#
#
# devtools::load_all("D:\\PEM_DATA\\PEMprepr")
# devtools::load_all("D:\\PEM_DATA\\PEMsamplr")
# devtools::load_all("D:\\PEM_DATA\\PEMmodelr")
#
# library(PEMprepr)
# library(PEMmodelr)
#
#
# fid <- setup_folders("Deception_AOI")
#
# bec_zones <- as.list(list.files(file.path(fid$model_final[2])))
# mfiles = list.files(file.path(fid$model_final[2]), recursive = TRUE, pattern = "final_model.rds$")
#
#
# #covdir <- fid$cov_dir_1020[2]
# covdir <- "D:\\PEM_DATA\\BEC_DevExchange_Work\\Deception_AOI\\1_map_inputs\\covariates"
# res_folder = "5m"
#
# template <- terra::rast(file.path(covdir, res_folder, "template.tif"))
#
# # get full raster list
# rast_list <- list.files(file.path(covdir, res_folder), pattern = ".tif$", full.names = TRUE)
#
# # select reduced variables
# reduced_vars <- read.csv(file.path(fid$model_inputs0310[2],  "reduced_covariate_list.csv")) %>% pull()
#
# # filter based on covariate for model param
# rast_list <- rast_list[tolower(gsub(".tif", "", basename(rast_list))) %in% (reduced_vars)]
#
#
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
