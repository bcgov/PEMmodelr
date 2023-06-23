#' generate_theta_threshold
#'
#' @param allthetas output of the generate theta metric function
#' @param metric text of which metric you wish to use, default = "paf"
#' @importFrom stringr str_detect
#' @importFrom dplyr select distinct mutate summarise group_by
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr "%>%"
#' @importFrom foreach foreach
#' @return dataframe
#' @export
#'
#' @examples
#' generate_theta_threshold(alltheta)

generate_theta_threshold <- function(allthetas){

  #allthetas = acc_out

  metrics = as.factor(c("p", "pa", "paf"))

  theta_thresh <- foreach::foreach(k = levels(metrics),.combine = rbind) %do% {
    # k = levels(metrics)[1]
    mnames = paste0(k,"_theta")
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
      dplyr::mutate(theta_final = ifelse(is.na(theta_base), theta, theta_base))%>%
      dplyr::select(-theta_base)

    bal_out <- acc %>%
      dplyr::group_by(type, theta_final) %>%
      dplyr::summarise(mean = mean(value),
                       q25 = quantile(value, prob = 0.25),
                       q75 = quantile(value, prob = 0.75)) %>%
      dplyr::mutate(above_thresh = ifelse(q25 <= 0.65, F, T))

    # overall_acc <- ggplot(aes(y = value, x = theta_final), data = acc ) +
    #   geom_boxplot() +
    #   scale_fill_brewer(type = "qual") +
    #   facet_wrap(~type, scales = "free_x")+
    #   geom_hline(yintercept = 0.65,linetype ="dashed", color = "black") +
    #   theme_pem_facet() +
    #   # scale_fill_manual(values=c("grey90", "grey75", "grey50", "grey35","grey10"))+
    #   theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
    #   xlab("Metric") + ylab("Accuracy") +
    #   ylim(0, 1)
    #
    # overall_acc

    bal_out <- bal_out %>% dplyr::mutate(accuracy_type = k)
    #print (paste0("calculated theta threshold for ", k))

  }
  return(theta_thresh)
}
