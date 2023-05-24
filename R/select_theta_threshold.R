#' select_theta_threshold
#'
#' @param allthetas output of the generate theta metric function
#' @param metric text of which metric you wish to use, default = "paf"
#' @importFrom stringr str_detect
#' @importFrom dplyr select distinct mutate
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr "%>%"
#' @return dataframe
#' @export
#'
#' @examples
#' select_theta_threshold(alltheta, "paf")

select_theta_threshold <- function(allthetas,metric = "paf"){

  #allthetas = acc_out
  #metric = "paf"

  mnames = paste0(metric,"_theta")

  noi <-  names(allthetas)[str_detect(names(allthetas),mnames)]

  acc <- allthetas  %>%
    dplyr::select(slice, theta, any_of(noi)) %>%
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

