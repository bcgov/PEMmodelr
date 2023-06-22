
#' Plot Accuracy per mapunit
#'
#' @param acc_output datatable with all results from model run
#' @param type string "spat' for spatial data and "aspat' for aspatial output
#' @importFrom dplyr select mutate
#' @importFrom magrittr "%>%"
#' @import ggplot2
#' @return plot
#' @export
#'
#' @examples
#' acc_plot_mu()
acc_plot_mu <- function(acc_output, type = "spat"){

  if(type == "spat") {

    mu_acc <- acc_output %>%
      dplyr::select(mapunit1, spat_p, spat_pa, spat_paf)

  } else if(type == "aspat"){

    mu_acc <- acc_output %>%
      dplyr::select(mapunit1, aspat_p, aspat_pa)
  }

  mu_long <- mu_acc %>%
    pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "value") %>%
    dplyr::mutate(accuracy_label = sub(".*_", "", accuracy_type))%>%
    dplyr::mutate(valuepc = value*100)

  p4 <- ggplot(aes(y = valuepc, x = mapunit1, fill = accuracy_type), data = mu_long ) +
    geom_boxplot(aes(middle = mean(valuepc))) +
    ggtitle("Mapunit accuracy measures ") +
    xlab("accuracy measure") + ylab("Percent of Accurate calls") +
    # ylim(-0.05, 1)+
    theme_pem_facet()+
    scale_fill_discrete_sequential(palette = "Light Grays")+
    theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

  return(p4)

}
