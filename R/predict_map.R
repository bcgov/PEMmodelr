#' Predict Map from model
#'
#' @param model rf model file
#' @param out_dir location to export predicted tiles
#' @param tile_dir location of template tiles
#' @param tile_size size of tile for processing, default is 500
#' @param rstack spatRast stack of all covars
#' @param probability TRUE or FALSE if probability rasters are to be exported
#'
#' @return TRUE
#' @export
#'
#' @examples
#' predict_map(rf_fit, out_dir = file.path(fid$model_final[2], "ESSF","map_working"), 500, rstack)

predict_map <- function(model, out_dir, tile_size = 500, tile_dir, rstack, probability = FALSE){
  #
  #     bgc = b
  #     model = model
  #     out_dir = file.path(model_dir, b)
  #     tile_size = 500
  #     rstack = rstack
  #     tile_dir = tile_dir
  #     probability = FALSE


  # extract fit
  rf_fit <- workflows::extract_fit_parsnip(model)
  .pred_class<- rf_fit$fit$forest$levels
  respNames <- as.data.frame(.pred_class) %>%
    mutate(pred_no = seq(1:length(.pred_class)))

  write.csv(respNames, file.path(out_dir, "response_names.csv"), row.names = TRUE)

  a <- 0 ## running total of area complete
  ta <- sum(as.numeric(length(ntiles)))

  for (i in ntiles) {

    #i = ntiles[3]
    # get tile and output name
    out_name <- basename(i)

    # create tracking message
    t <- terra::rast(file.path(i))  ## read in tile
    print(paste("working on ", basename(i), "of", length(ntiles)))
    print("... loading data ...")

    #plot(t)

    #check if blank tile
    if(all(is.na(unique(values(t)))) == TRUE){

      print( "Some variables with all NA values, skipping tile...")

    } else {


      # check if t is

      # crop the raster stack to tile extent
      tstack <- crop(rstack, t)
      # convert to dataframe
      rsf <- as.data.frame(tstack)
      # get xy values
      #rsfxy <- terra::crds(tstack)

      # check if all values in columns are NA (ie not in study area)
      na_table <- as.data.frame(sapply(rsf, function(x) all(is.na(x))))

      if(any(na_table[,1] == TRUE)) {

        print("Some variables with all NA values, skipping tile...")

      } else {

        # predict
        pred <- terra::predict(tstack, rf_fit$fit , na.rm = TRUE)

        # write out probability layer
        if(probability == TRUE){
          # check if out_dir exists
          if(!dir.exists(file.path(out_dir, "probability"))){
            dir.create(file.path(out_dir,"probability"))
          } else {
            print("probability dir exists")
          }
          terra::writeRaster(pred, file.path(out_dir,"probability", out_name), overwrite =TRUE)
          print("writing probability tile")
        }

        # write out best class

        pdfxy <- as.data.frame(pred, xy = TRUE, cells = FALSE)
        pdf <- pdfxy %>% select(-x, -y)
        pdfid <-  pdfxy %>% select(x, y)

        best_class <- colnames(pdf)[apply(pdf[,2:length(pdf)] ,1, which.max)]

        r_out <- cbind(pdfid, as.factor(best_class))
        names(r_out) <- c('x','y','.pred_class')

        ## change the text values to numeric values.
        r_out <- left_join(r_out, respNames, by=".pred_class" )
        r_out <- r_out %>% dplyr::select(-".pred_class")

        print("... exporting raster tiles...")

        out <- as_spatraster(r_out, crs = "epsg:3005")

        terra::writeRaster(out, paste0(out_dir,"/",out_name), overwrite = T)
      }
    }

    ## * report progress -----
    a <- a + 1
    print(paste(round(a/ta*100,0), "% complete"))
    print("") ## blank line

  }

  print("All predicted tiles generated")

  r_tiles <- list.files(out_dir,
                        pattern = ".tif$",
                        full.names = TRUE)

  rsrc <- sprc(r_tiles)

  m <- mosaic(rsrc, fun = "min")
  terra::writeRaster(m, paste0(out_dir,"/", "mosaic.tif"))

  return(TRUE)

} # end of function loop


