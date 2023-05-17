#' Generate Final Model summary
#'
#' This function takes in all the data needed to produce machine learning model.
#' Inputs are handed to a RMD report/ script.
#' Outputs include the markdown report, the cross validation object,
#' and a binary model (RDS) that can then be used to predict on new data.
#' @param final_data  final model training point
#' @param final_model  final model object
#' @param  outDir  output directory  This defaults to the project's root directory OR where the RMD script is saved.
#' @keywords final model report
#' @import rmarkdown
#' @import tidyr
#' @export
#' @examples
#' final_model_report(final_data, final_model, outDir)

final_model_report <- function(final_data, final_model, outDir){

  ## create destination folder
  ifelse(!dir.exists(file.path(outDir)),
         dir.create(file.path(outDir)), FALSE)

  RMD <- system.file("rmd_template", "final_model_report.rmd", package ="PEMmodelr")

  rmarkdown::render(RMD,
                    params = list(final_data = final_data,
                                  final_model = final_model,
                                  outDir = outDir),
                    output_dir = outDir)                ## where to save the report

   ## open the report
  browseURL(paste0(paste0(outDir,"/","final_model_report.html")))
}
