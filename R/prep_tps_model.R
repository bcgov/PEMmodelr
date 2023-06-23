#' Prepare training points for model
#'
#' @param cleandat datatable training point file
#' @param out_dir text filepath location default is set to location
#' @importFrom stringr str_detect
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter select mutate
#' @return TRUE
#' @export
#'
#' @examples
#' prep_tps_model(cleandat, out_dir = fid$model_inputs0310[2])

# set up the training points by BGCs into a list and add model folders

#cleandat <- read.csv(file.path(fid$model_inputs0310[2], "training_pts.csv"))

prep_tps_model <- function(cleandat, out_dir = fid$model_inputs0310[2]){

    # set up the training points by BGC
    zones <- c(as.character(unique(cleandat$bgc_cat)))

    bgc_pts_subzone <- lapply(zones, function (i){
      #i =  zones[2]
      pts_subzone <- cleandat %>%
        dplyr::filter(stringr::str_detect(tid, as.character(paste0(tolower(i), "_")))) %>%
        droplevels()

      # remove any bec sites series that are not in the bec_zone catergory

      munits <- grep(unique(pts_subzone$mapunit1), pattern = "_\\d",value = TRUE, invert = FALSE)

      diff_bec_mapunits <- grep(munits,pattern = paste0("^",i,"_"), value = TRUE , invert =TRUE)

      pts_subzone <- pts_subzone %>%
        dplyr::filter(!mapunit1 %in% diff_bec_mapunits)

      if(nrow(pts_subzone) == 0){ pts_subzone = NULL} else {ppts_subzone = pts_subzone }
      pts_subzone
    })

    # format names
    names(bgc_pts_subzone) <- zones

    saveRDS(bgc_pts_subzone, file.path(out_dir, "model_input_pts.rds"))



    # Create a folder per BGC and run a check on the training points

    model_bgc <- lapply(names(bgc_pts_subzone), function(xx){

      #xx <- names(bgc_pts_subzone[1])

      alldat = bgc_pts_subzone[[xx]]
      #out_name = names(bgc_pts_subzone[xx])

      # set up output folders
      outDir = file.path(fid$model_draft[2], xx)
      if(!dir.exists(file.path(outDir))){dir.create(file.path(outDir))}

      # set up final model folder
      finalDir = file.path(fid$model_final[2], xx)
      if(!dir.exists(file.path(finalDir))){dir.create(file.path(finalDir))}

      tdat <- alldat %>% dplyr::mutate(slice = factor(slice))
      tdat <- tdat %>%
        dplyr::select(id, fnf, x, y, bgc_cat, mapunit1, mapunit2, position, transect_id, tid, slice, everything())

      tdat_all <- tdat[complete.cases(tdat[ ,12:length(tdat)]),]
      #tdat_centre <- tdat_all %>% dplyr::filter(position %in% "Orig")

      #PEMsamplr::trainingpt_report(tpts = tdat_all, trans = trans, out_dir = outDir)

        })

    return(bgc_pts_subzone)

    }
