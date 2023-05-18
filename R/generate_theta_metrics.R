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
#' bgcs <- list.dirs(fid$model_draft[2], recursive = T)
#' bgcs <- bgcs[endsWith(bgcs,"/raw_outputs")]
#' generate_theta_metrics(bgcs[1])

generate_theta_metrics = function(datafolder) {

   # datafolder = i

    slices <- as.factor(list.files(datafolder))

    if("compiled_theta_results.csv" %in% slices){
      print("compiled theta file already exists, this file will be overwriten")
      slices = slices[-1] %>% droplevels()
      }

    theta_acc <- foreach::foreach(k = levels(slices),.combine = rbind) %do% {
    #k = levels(slices)[1]
      pred_all <- readRDS(file.path(datafolder, k))
      theta_vals <- as.factor(c(0.001, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))

      allthetas <- foreach::foreach(th = levels( theta_vals),.combine = rbind) %do% {
        # th = levels(theta_vals)[1]
        tacc <- acc_metrics(pred_all, fuzzmatrx = fmat, theta = as.numeric(th))  %>%
          dplyr::mutate(theta = th)

        } # end of theta loop

      allthetas <- allthetas %>% dplyr::mutate(slice = k)

   } # end of slice loop

  acc <- theta_acc %>%
    dplyr::select(slice, theta, aspat_paf_theta0, aspat_paf_theta.5,
           aspat_paf_theta1,spat_paf_theta0, spat_paf_theta.5,
           spat_paf_theta1 ) %>%
    dplyr::distinct()
  #write.csv(acc, file.path(datafolder, "compiled_theta_results.csv")

  acc2 <- acc  %>%
    tidyr::pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "value") %>%
    dplyr::distinct()

  acc <- acc2 %>%
    dplyr::mutate(type = case_when(
      stringr::str_detect(accuracy_type, "aspat") ~ "aspatial",
      stringr::str_detect(accuracy_type, "spat") ~ "spatial")) %>%
    dplyr::mutate(theta_base = case_when(
      stringr::str_detect(accuracy_type, "theta0") ~ 0,
      stringr::str_detect(accuracy_type, "theta.5") ~ NA,
      stringr::str_detect(accuracy_type, "theta1") ~ 1)) %>%
    dplyr::mutate(theta_final = ifelse(is.na(theta_base), theta, theta_base))

  acc_out <- acc %>% dplyr::select(type, slice, value, theta_final)

return(acc_out)

}
