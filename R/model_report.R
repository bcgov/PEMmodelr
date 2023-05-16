#' Generate Model summary
#'
#' This function takes in all the data needed to produce machine learning model.
#' Inputs are handed to a RMD report/ script.
#' Outputs include the markdown report, the cross validation object,
#' and a binary model (RDS) that can then be used to predict on new data.
#' @param trDat  training points cleaned
#' @param acc_output  model output metrics datatable
#' @param out_dir  output directory  This defaults to the project's root directory OR where the RMD script is saved.
#' @keywords training data report
#' @import rmarkdown
#' @import tidyr
#' @export
#' @examples
#' trainingpt_report(tpts,  out_dir)

model_report <- function(trDat, acc_output, out_dir){

  ## create destination folder
  ifelse(!dir.exists(file.path(out_dir)),
         dir.create(file.path(out_dir)), FALSE)

  RMD <- system.file("rmd_template", "model_report.rmd", package ="PEMmodelr")

  rmarkdown::render(RMD,
                    params = list(trDat = trDat,
                                  acc_output = acc_output,
                                  out_dir = out_dir),
                    output_dir = out_dir)                ## where to save the report

   ## open the report
  browseURL(paste0(paste0(out_dir,"/","model_report.html")))
}
