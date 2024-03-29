#' Generate theta metrics
#'
#' @param datafolder location of model raw outputs
#' @import foreach
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate filter select rename case_when rowwise group_by distinct
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#' @return dataframe of accuracy results
#' @export
#'
#' @examples
#' generate_theta_metrics(bgcs[1])

generate_theta_metrics = function(datafolder) {

  datafolder = i

  slices <- as.factor( list.files(datafolder, pattern = "prediction_*"))

  if("compiled_theta_results.csv" %in% slices){
    print("compiled theta file already exists, this file will be overwriten")
    slices = slices[-1] %>% droplevels()
  }

  theta_acc <- foreach::foreach(k = levels(slices),.combine = rbind) %do% {
    #k = levels(slices)[1]
    pred_all <- readRDS(file.path(datafolder, k))
    theta_vals <- as.factor(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
    # note if you add 1 and 0 this will interfere with the generate theta threshold calculations

    allthetas <- foreach::foreach(th = levels( theta_vals),.combine = rbind) %do% {
      # th = levels(theta_vals)[1]
      tacc <- acc_metrics(pred_all, fuzzmatrx = fmat, theta = as.numeric(th))  %>%
        dplyr::mutate(theta = th)

    } # end of theta loop

    allthetas <- allthetas %>% dplyr::mutate(slice = k)

  } # end of slice loop

  return(theta_acc)
}

