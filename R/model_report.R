#' Generate Model summary
#'
#' This function takes in all the data needed to produce machine learning model.
#' Inputs are handed to a RMD report/ script.
#' Outputs include the markdown report, the cross validation object,
#' and a binary model (RDS) that can then be used to predict on new data.
#'
#' @param out_dir  output directory  This defaults to the project's root directory OR where the RMD script is saved.
#' @param tpts  training points cleaned
#' @keywords training data report
#' @import rmarkdown
#' @import tidyr
#' @export
#' @examples
#' trainingpt_report(tpts,  out_dir)

model_report <- function(tpts, out_dir){
  # # # testing : GP
  #tpts =  tdat_all
  #out_dir = outDir

  ## create destination folder
  ifelse(!dir.exists(file.path(out_dir)),
         dir.create(file.path(out_dir)), FALSE)

  RMD <- system.file("rmd_template", "model_report.rmd", package ="PEMmodelr")

  rmarkdown::render(RMD,
                    params = list(tpts = tpts,
                                  out_dir = out_dir),
                    output_dir = out_dir)                ## where to save the report

  #file.rename(paste0(out_dir,"/", "trainingpt_report.html"), paste0(out_dir,"/","field_training_pt_report.html"))
  ## open the report
  browseURL(paste0(paste0(out_dir,"/","model_report.html")))
}
