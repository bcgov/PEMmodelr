#' Generate a machine learning model using tidy models
#'
#' This function takes in all the data needed to produce machine learning model.
#' Inputs are handed to a RMD report/ script.
#' Outputs include the markdown report, the cross validation object,
#' and a binary model (RDS) that can then be used to predict on new data.
#'
#'
#' @param outDir  Highly recommended to be set as an absolute directory.  This defaults to the project's root directory OR where the RMD script is saved.
#' Additional products generated from the associated `model_gen_tidy.Rmd`` markdown script will also be saved to this dir.
#' @param trDat Is a dataframe that contains the model training data.  The reponse variable should be one of the columns.
#' @param target   The name of the response variable in the traindat data frame.
#' @param rseed    Optional random number seed.
#' @keywords machine-learning, model, report
#' @export
#' @examples
#'
#'

model_gen_tidy <- function(trDat, 
                           target, 
                           target2, 
                           tid, 
                           ds_ratio,
                           sm_ratio,
                           outDir = ".", 
                           mname = "model", 
                           rseed = NA, 
                           infiles = infiles, 
                           mmu = mmu,
                           field_transect = field_transect){
# # # testing : GP
# trDat = inmdata_all
#  target = "target"
#  target2 = "target2"
#  tid = "tid"
#  outDir =  outDir
#  infiles = infiles
#  rseed = 456
#  mmu = mmu
#  mname = mname

   ## create destination folder
  ifelse(!dir.exists(file.path(outDir)),                # if folder does not exist
          dir.create(file.path(outDir)), FALSE)         # create it

  ## error testing ----------------
 # if (sum(is.na(trDat[,target])) > 0) {
#    stop(paste("There are,", sum(is.na(trDat[,target]))  , "NA values in the target:", target))
#  }

  # may need to update here!
#  RMD <- "D:/PEM_DATA/BEC_DevExchange_Work/_functions/model_gen_tidy.Rmd"
 # RMD <- "J:/PEM_DATA/BEC_DevExchange_Work/_functions/model_gen_tidy.Rmd"
  RMD <- "E:/temp/PEM_DATA/BEC_DevExchange_Work/_functions/model_gen_tidy.Rmd"
    #RMD <- "D:/GitHub/PEM_Methods_DevX/_functions/model_gen_tidy.Rmd"

  rmarkdown::render(RMD,              ## where the rmd is located
                    params = list(trDat = trDat,  ## parameters to send to rmarkdown
                                  target = target,
                                  target2 = target2,
                                  tid = tid, 
                                  ds_ratio = ds_ratio,
                                  sm_ratio = sm_ratio,
                                  outDir = outDir,
                                  infiles = infiles,
                                  mmu = mmu,
                                  rseed = rseed,
                                  mname = mname,
                                  field_transect = field_transect),
                    output_dir = outDir)                ## where to save the report
  
  file.rename(paste0(outDir,"/", "model_gen_tidy.html"), paste0(outDir,"/", mname,"_treport.html"))
  ## open the report
  browseURL(paste0(paste0(outDir,"/", mname,"_treport.html")))
}
