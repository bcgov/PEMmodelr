
#' Plot accuracy metrics by slice or results type (machine, aspatial, spatial)
#'
#' @param acc_output results output from base of model run
#' @param slice TRUE/FALSE, default is FALSE
#' @importFrom dplyr select mutate
#' @importFrom stringr str_detect str_replace
#' @importFrom magrittr "%>%"
#' @import ggplot2
#' @return plot
#' @export
#'
#' @examples
#' acc_plot_metrics(acc_output, slice = FALSE)
acc_plot_metrics = function(acc_output, slice = FALSE){

  if (slice == TRUE) {

    noi <-  names(acc_output)[str_detect(names(acc_output),"_theta")]

    acc <- acc_output %>%
      dplyr::mutate(slice = factor(slice)) %>%
      dplyr::select(acc_type, slice, acc, kap, oob, all_of(noi))%>%
      distinct() %>%
      dplyr::mutate(inv_oob = 1-oob)

    acc2 <- acc  %>%
      pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "value") %>%
      dplyr::distinct() %>%
      dplyr::mutate(type = dplyr::case_when(
        accuracy_type %in% c("acc", "kap", "inv_oob") ~ "0_machine",
        stringr::str_detect(accuracy_type, "^spat_p_theta") ~ "1_spatial",
        stringr::str_detect(accuracy_type, "aspat_p_theta") ~ "2_aspatial")) %>%
      filter(!is.na(type)) %>% filter(!accuracy_type == "slice") %>%
      mutate(accuracy_type = stringr::str_replace(accuracy_type, "aspat_p_theta_", "\u0398 "),
             accuracy_type = stringr::str_replace(accuracy_type, "^spat_p_theta_", "\u0398 ")) %>%
      mutate(accuracy_type = factor(accuracy_type), type = factor(type))


    acc2 <- acc2 %>%
      mutate(accuracy_type = str_replace(accuracy_type, "theta1", "theta 1"),
             accuracy_type = str_replace(accuracy_type, "theta0", "theta 0"),
             accuracy_type = str_replace(accuracy_type, "aspat_p_theta", "\u0398 "),
             accuracy_type = str_replace(accuracy_type, "aspat_pa_theta", "\u0398 "),
             accuracy_type = str_replace(accuracy_type, "spat_paf_theta", "\u0398 "),
             accuracy_type = str_replace(accuracy_type, "spat_pa_theta", "\u0398 "),
             accuracy_type = str_replace(accuracy_type, "spat_p_theta", "\u0398 "))


    overall_acc <- ggplot(aes(y = value, x = factor(accuracy_type, level = c("kap",  "acc", "inv_oob", "Θ  1", "Θ .5", "Θ  0")), fill = slice), data = acc2 ) +
      geom_boxplot() +
      scale_fill_brewer(type = "qual") +
      facet_grid(~type, labeller = labeller(type =
                                              c("0_machine" = "machine", "1_spatial" = "spatial",
                                                "2_aspatial" = "aspatial")), scales = "free_x")+
      geom_hline(yintercept = 0.65,linetype ="dashed", color = "black") +
      theme_pem_facet() +
      scale_fill_manual(values=c("grey90", "grey75", "grey50", "grey35","grey10"))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
      xlab("Metric") + ylab("Accuracy") +
      ylim(0, 1)

  } else {


    noi <-  names(acc_output)[str_detect(names(acc_output),"_theta")]

    acc <- acc_output %>%
      dplyr::mutate(slice = factor(slice)) %>%
      dplyr::select(acc_type, slice, acc, kap, oob, all_of(noi))%>%
      dplyr::distinct() %>%
      dplyr::mutate(inv_oob = 1-oob)%>%
      dplyr::select(-oob)


    acc_sum_long <- acc %>%
      pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "value") %>%
      dplyr::mutate(type = case_when(
        stringr::str_detect(accuracy_type, "acc") ~ "machine",
        stringr::str_detect(accuracy_type, "kap") ~ "machine",
        stringr::str_detect(accuracy_type, "inv_oob") ~ "machine",
        stringr::str_detect(accuracy_type, "aspat_") ~ "aspatial",
        stringr::str_detect(accuracy_type, "spat_") ~ "spatial")) %>%
      dplyr::mutate(accuracy_type_label = case_when(
        stringr::str_detect(accuracy_type, "acc") ~ "machine",
        stringr::str_detect(accuracy_type, "kap") ~ "machine",
        stringr::str_detect(accuracy_type, "inv_oob") ~ "machine",
        stringr::str_detect(accuracy_type, "_p_") ~ "p",
        stringr::str_detect(accuracy_type, "_pa_") ~ "pa",
        stringr::str_detect(accuracy_type, "_paf_") ~ "paf"))%>%
      mutate(valuepc = value*100)

    acc_sum_long <- acc_sum_long %>%
      rowwise() %>%
      dplyr::mutate(theta = ifelse(type != "machine", str_sub(accuracy_type , start = -1),NA)) %>% #regmatches(acc_sum_long$accuracy_type,regexpr("[0-9]+",acc_sum_long$accuracy_type)), accuracy_type)) %>%
      dplyr::mutate(theta = case_when(
        theta == 5 ~ '0.5',
        accuracy_type == "kap" ~ "kap",
        accuracy_type == "acc" ~ "acc",
        accuracy_type == "inv_oob" ~ "inv_oob",
        .default = theta))

    # # set up order for plots
    acc_sum_long$type_f = factor(acc_sum_long$accuracy_type_label, levels = c("p" ,"pa", "paf"))

    overall_acc <- ggplot(aes(y = valuepc, x = factor(theta, level = c('acc', "kap", "inv_oob","1", "0.5", "0")), fill = type), data = acc_sum_long ) +
      geom_boxplot() +
      facet_wrap(~accuracy_type_label, scales = "free_x", nrow = 1) +
      geom_hline(yintercept = 65,linetype ="dashed", color = "black") +
      ggtitle("Accuracy measures (median + quartiles)") +
      xlab("Metric") + ylab("Accuracy") +
      ylim(-0.05, 100)+
      theme_pem_facet()+
      scale_fill_discrete_sequential(palette = "Light Grays")

  }

  return(overall_acc)

}
