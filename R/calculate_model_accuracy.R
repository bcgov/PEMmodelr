#' Calculate model accuracy metrics
#'
#' Calculates internal ML metrics and PEM specific spatial and proportional metrics.
#' This currently includes: accuracy, mcc, spatial metrics for primary, primary + alternate,
#' and primary + alternate + fuzzy for theta adjusts set at 0, 0.5, 1. Also aspatial proportions
#' for primary+alternate+fuzzy with the same 3 theta settings
#'
#' @param pred_data a data.frame with mapunit1, mapunit2, and .pred columns
#' @param fuzzmatrx is the fuzzy values matrix from the map key giving partial correct points
#' for near misses
#' @param theta the function always returns values for theta 0 and theta 1.
#' The theta setting sets an intermediate theta setting to report efault set to 0.5
#'
#' @keywords accuracy, fuzzy sets, theta
#' @export
#' ##

# library(yardstick)
# library(janitor)
# library(dplyr)
# require(data.table)
#
# pred_data = fread("./temp_data/test_pred.csv")
# fuzzmatrx = fread("./temp_data/fuzzy_matrix_basic_updated.csv")
#   theta = 0.5


report_model_accuracy <- function(pred_data, fuzzmatrx, theta = 0.5) {

  ##1.  Selects max value between primary and secondary calls

  preds = c("id","mapunit1", "mapunit2", ".pred_class")
  pred_data <- pred_data %>% dplyr::select(any_of(preds))
  data1 <- dplyr::left_join(pred_data, fuzzmatrx, by = c("mapunit1" = "target", ".pred_class" = "compare")) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::mutate(p_fuzzval = fuzzval) %>%
    dplyr::select(-fuzzval)
  data2 <- dplyr::left_join(data1, fuzzmatrx, by = c("mapunit2" = "target", ".pred_class" = "compare")) %>%
    dplyr::mutate_if(is.character, as.factor) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(alt_fuzzval = fuzzval)

  ##2. selects the neighbour with max value
  data <- data2 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(pa_fuzzval = max(p_fuzzval, alt_fuzzval)) %>%
    dplyr::group_by(id) %>%
    dplyr::top_n(1, abs(pa_fuzzval)) %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    data.frame() %>%
    dplyr::select(-fuzzval, -alt_fuzzval)

  data <- data %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(
      p_Val = ifelse(mapunit1 == .pred_class, 1, 0),
      alt_Val = ifelse(mapunit2 == .pred_class, 1, 0)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(pa_Val = max(p_Val, alt_Val)) %>%
    dplyr::select(-alt_Val) %>%
    dplyr::mutate_if(is.character, as.factor) %>%
    data.frame() %>%
    dplyr::add_count(mapunit1, name = "trans.tot") %>%
    dplyr::add_count(.pred_class, name = "pred.tot") # %>% group_by(mapunit) %>%
  # dplyr::mutate(pred.tot_pa = sum(pa_Val)) %>% ungroup()

  targ.lev <- as.data.frame(levels(data$mapunit1)) %>%
    dplyr::rename(levels = 1) %>%
    droplevels()
  pred.lev <- as.data.frame(levels(data$.pred_class)) %>%
    dplyr::rename(levels = 1) %>%
    droplevels()
  add.pred.lev <- dplyr::anti_join(pred.lev, targ.lev, by = "levels")
  data <- data %>%
    dplyr::mutate(
      mapunit.new = ifelse(.pred_class %in% add.pred.lev, as.character(.pred_class), as.character(mapunit1)),
      trans.tot.new = ifelse(.pred_class %in% add.pred.lev, 0, trans.tot)
    ) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(trans.tot = trans.tot.new)
  #   mutate(pred.new = ifelse(mapunit.new %in% add.pred.lev, as.character(mapunit), as.character(.pred_class))) %>%
  #     mutate(mapunit = mapunit.new, .pred_class = pred.new)
  ### harmonize factor levels
  targ.lev <- levels(data$mapunit1)
  pred.lev <- levels(data$.pred_class)
  levs <- c(targ.lev, pred.lev) %>% unique()
  data$mapunit <- factor(data$mapunit1, levels = levs)
  data$.pred_class <- factor(data$.pred_class, levels = levs)

  data <- data %>%
    tidyr::drop_na(mapunit) %>%
    dplyr::mutate(no.classes = length(levs)) %>%
    dplyr::select(-pred.tot, -trans.tot.new)

  ### 1)machine learning stats
  data <- harmonize_factors(data)
  acc <- data %>%
    yardstick::accuracy(mapunit1, .pred_class, na_rm = TRUE) %>%
    dplyr::select(.estimate) %>%
    as.numeric() %>%
    round(3)
  mcc <- data %>%
    yardstick::mcc(mapunit1, .pred_class, na_rm = TRUE) %>%
    dplyr::select(.estimate) %>%
    as.numeric() %>%
    round(3)
  # sens <- data %>% sens(mapunit, .pred_class, na_rm = TRUE)
  # spec <- data %>% yardstick::spec(mapunit, .pred_class, na_rm = TRUE)
  # prec <- data %>% precision(mapunit, .pred_class, na.rm = TRUE)
  # recall <- data %>% recall(mapunit, .pred_class, na.rm = TRUE)
  # fmeas <- data %>% f_meas(mapunit, .pred_class, na.rm = TRUE)
  kap <- data %>%
    yardstick::kap(mapunit1, .pred_class, na.rm = TRUE) %>%
    dplyr::select(.estimate) %>%
    as.numeric() %>%
    round(3)
  ### 2) spatial stats
  spatial_acc <- data %>%
    dplyr::mutate(trans.sum = n(), acc = acc, kap = kap) %>%
    ### here the problem is differing number of mapunit vs .pred_class
    dplyr::group_by(mapunit.new) %>%
    # dplyr::mutate(trans.tot = n()) %>%
    # mutate(no.classes = length(unique(mapunit.new))) %>%
    dplyr::mutate(spat_p_correct = sum(p_Val)) %>%
    dplyr::mutate(spat_pa_correct = sum(pa_Val)) %>%
    dplyr::mutate(spat_pf_correct = sum(p_fuzzval)) %>%
    dplyr::mutate(spat_paf_correct = sum(pa_fuzzval)) %>%
    dplyr::select(-id, -mapunit2, -.pred_class, -mapunit, -p_fuzzval, -pa_fuzzval, -p_Val, -pa_Val) %>%
    ungroup() %>%
    distinct() %>%
    dplyr::mutate(spat_p = spat_p_correct / trans.tot) %>%
    dplyr::mutate(spat_pa = spat_pa_correct / trans.tot) %>%
    dplyr::mutate(spat_pf = spat_pf_correct / trans.tot) %>%
    dplyr::mutate(spat_paf = spat_paf_correct / trans.tot) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace(., is.nan(.), 0))) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace(., is.infinite(.), 0))) %>%
    dplyr::mutate(
      spat_p_theta1 = mean(spat_p),
      spat_p_theta0 = sum(spat_p_correct) / trans.sum,
      spat_pa_theta1 = mean(spat_pa),
      spat_pa_theta0 = sum(spat_pa_correct) / trans.sum,
      spat_paf_theta1 = mean(spat_paf),
      spat_paf_theta0 = sum(spat_paf_correct) / trans.sum
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      spat_p_theta_wt = theta * (1 / no.classes) + (1 - theta) * (spat_p_correct / trans.sum),
      spat_pa_theta_wt = theta * (1 / no.classes) + (1 - theta) * (spat_pa_correct / trans.sum),
      spat_paf_theta_wt = theta * (1 / no.classes) + (1 - theta) * (spat_paf_correct / trans.sum),
      spat_p_theta_work = spat_p_theta_wt * spat_p,
      spat_pa_theta_work = spat_pa_theta_wt * spat_pa,
      spat_paf_theta_work = spat_paf_theta_wt * spat_paf
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      spat_p_theta.5 = sum(spat_p_theta_work),
      spat_pa_theta.5 = sum(spat_pa_theta_work),
      spat_paf_theta.5 = sum(spat_paf_theta_work)
    ) %>%
    dplyr::select(-spat_p_theta_wt, -spat_p_theta_work, -spat_pa_theta_wt, -spat_pa_theta_work, -spat_paf_theta_wt, -spat_paf_theta_work) %>%
    dplyr::rename(mapunit = mapunit.new)

  # 3) calculate aspatial metrics (overall and mapunit % correct)
  aspatial_mapunit <- data %>%
    dplyr::select(mapunit) %>%
    dplyr::add_count(mapunit, name = "trans.tot") %>%
    dplyr::distinct() %>%
    dplyr::mutate(trans.sum = sum(trans.tot))
  #
  aspatial_pred <- data %>%
    dplyr::select(.pred_class) %>%
    dplyr::add_count(.pred_class, name = "pred.tot") %>%
    dplyr::distinct()

  aspatial_acc <- left_join(aspatial_mapunit, aspatial_pred, by = c("mapunit" = ".pred_class")) %>%
    dplyr::select(mapunit, trans.tot, pred.tot) %>% # group_by(mapunit) %>%
    # mutate(p_correct = sum(p_Val),
    # pa_correct = sum(pa_Val),
    # paf_correct = sum(pa_fuzzval)) %>%
    # ungroup() %>%
    dplyr::mutate(trans.sum = sum(trans.tot)) %>% # dplyr::select(-p_Val, -pa_Val, -p_fuzzval, -pa_fuzzval) %>% distinct %>%

    # %>%
    mutate(no.classes = length(unique(mapunit))) %>%
    dplyr::mutate(across(where(is.numeric), ~ tidyr::replace_na(., 0))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      aspat_p = min((trans.tot / trans.tot), (pred.tot / trans.tot)),
      aspat_p_wtd = min((trans.tot / trans.sum), (pred.tot / trans.sum))
    ) %>%
    #                 aspat_pa = min((trans.tot/trans.tot),(pa_correct/trans.tot)),
    #                 aspat_pa_wtd= min((trans.tot/trans.sum),(pa_correct/trans.sum))) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace(., is.nan(.), 0))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      aspat_p_theta0 = sum(aspat_p_wtd),
      aspat_p_theta1 = mean(aspat_p)
    ) %>%
    #            aspat_pa_theta0 = sum(aspat_pa_wtd),
    #           aspat_pa_theta1 = mean(aspat_pa)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(aspat_p_theta_wt = theta * (1 / no.classes) + (1 - theta) * (trans.tot / trans.sum)) %>% #
    dplyr::mutate(aspat_p_theta_work = aspat_p_theta_wt * aspat_p_wtd) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace(., is.nan(.), 0))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(aspat_p_theta.5 = sum(aspat_p_theta_wt * aspat_p)) %>%
    dplyr::select(-aspat_p_theta_wt, -aspat_p_theta_work, -no.classes, -trans.sum, -trans.tot) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  #### --- primary plus alternate
  data_pa <- data %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(mapunit = ifelse(mapunit2 == .pred_class, as.character(mapunit2), as.character(mapunit))) %>%
    dplyr::mutate_if(is.character, factor)

  aspatial_mapunit <- data_pa %>%
    dplyr::select(mapunit) %>% # %>% group_by(mapunit) %>%
    dplyr::add_count(mapunit, name = "trans.tot") %>%
    dplyr::distinct()
  #
  aspatial_pred <- data_pa %>%
    dplyr::select(.pred_class) %>%
    dplyr::add_count(.pred_class, name = "pred.tot") %>%
    dplyr::distinct()

  aspatial_acc_pa <- left_join(aspatial_mapunit, aspatial_pred, by = c("mapunit" = ".pred_class")) %>%
    dplyr::select(mapunit, trans.tot, pred.tot) %>% # group_by(mapunit) %>%
    # mutate(p_correct = sum(p_Val),
    # pa_correct = sum(pa_Val),
    # paf_correct = sum(pa_fuzzval)) %>%
    # ungroup() %>%
    dplyr::mutate(trans.sum = sum(trans.tot)) %>% # dplyr::select(-p_Val, -pa_Val, -p_fuzzval, -pa_fuzzval) %>% distinct %>%

    # %>%
    dplyr::mutate(no.classes = length(unique(mapunit))) %>%
    dplyr::mutate(across(where(is.numeric), ~ tidyr::replace_na(., 0))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      aspat_pa = min((trans.tot / trans.tot), (pred.tot / trans.tot)),
      aspat_pa_wtd = min((trans.tot / trans.sum), (pred.tot / trans.sum))
    ) %>%
    #                 aspat_pa = min((trans.tot/trans.tot),(pa_correct/trans.tot)),
    #                 aspat_pa_wtd= min((trans.tot/trans.sum),(pa_correct/trans.sum))) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace(., is.nan(.), 0))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      aspat_pa_theta0 = sum(aspat_pa_wtd),
      aspat_pa_theta1 = mean(aspat_pa)
    ) %>%
    #            aspat_pa_theta0 = sum(aspat_pa_wtd),
    #           aspat_pa_theta1 = mean(aspat_pa)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(aspat_pa_theta_wt = theta * (1 / no.classes) + (1 - theta) * (trans.tot / trans.sum)) %>% #
    dplyr::mutate(aspat_pa_theta_work = aspat_pa_theta_wt * aspat_pa_wtd) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace(., is.nan(.), 0))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(aspat_pa_theta.5 = sum(aspat_pa_theta_wt * aspat_pa)) %>%
    dplyr::select(-aspat_pa_theta_wt, -aspat_pa_theta_work, -no.classes, -trans.sum, -trans.tot) %>%
    dplyr::ungroup() %>%
    distinct() %>%
    dplyr::select(-pred.tot)

  aspatial_acc2 <- dplyr::left_join(aspatial_acc, aspatial_acc_pa, by = "mapunit")




  accuracy_stats <- dplyr::left_join(spatial_acc, aspatial_acc2, by = "mapunit")
  ### calculate paf aspatial statistics
  aspat_fpa_df <- accuracy_stats %>%
    dplyr::select(mapunit, trans.sum, no.classes, trans.tot, pred.tot, spat_p_correct, spat_paf_correct) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      aspat_paf_min_correct = min(trans.tot, pred.tot),
      aspat_paf_extra = spat_paf_correct - spat_p_correct,
      aspat_paf_total = aspat_paf_min_correct + aspat_paf_extra,
      aspat_paf_pred = min((aspat_paf_total / trans.tot), (trans.tot / trans.tot)),
      aspat_paf_pred2 = min((aspat_paf_total / trans.sum), (trans.tot / trans.sum)),
      aspat_paf_unit_pos = min(trans.tot, aspat_paf_total)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(across(where(is.numeric), ~ tidyr::replace_na(., 0))) %>%
    dplyr::mutate(
      aspat_paf_theta0 = sum(aspat_paf_unit_pos / trans.sum),
      aspat_paf_theta1 = mean(aspat_paf_pred)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(aspat_paf_theta_wt = theta * (1 / no.classes) + (1 - theta) * (trans.tot / trans.sum)) %>% #
    dplyr::mutate(aspat_paf_theta_work = aspat_paf_theta_wt * aspat_paf_pred2) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(aspat_paf_theta.5 = sum(aspat_paf_theta_wt * aspat_paf_pred)) %>%
    dplyr::select(-aspat_paf_theta_wt, -aspat_paf_theta_work) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::select(mapunit, aspat_paf_theta0, aspat_paf_theta.5, aspat_paf_theta1)

  accuracy_stats <- dplyr::left_join(accuracy_stats, aspat_fpa_df, by = "mapunit") %>%
    dplyr::select(mapunit, trans.sum, trans.tot, pred.tot, no.classes, everything())
  accuracy_stats
}

# function to calculate the weighted metrics

# weight_by_transect_no <- function(acc) {
#   acc_sum <- acc %>%
#     #     acc_sum <- acc %>%
#     dplyr::filter(acc_type == "test_estimate") %>%
#     mutate(across(ends_with("overall"), ~ .x * 100)) %>%
#     mutate(across(ends_with("meanacc"), ~ .x * 100)) %>%
#     dplyr::select(
#       slice, acc_type, transect_no,
#       aspat_p_overall, aspat_p_meanacc,
#       # aspat_fp_overall,  aspat_fp_meanacc,
#       spat_p_overall, spat_p_meanacc,
#       spat_pf_overall, spat_pf_meanacc,
#       aspat_pa_overall, aspat_pa_meanacc,
#       aspat_fpa_overall, aspat_fpa_meanacc,
#       spat_pa_overall, spat_pa_meanacc,
#       spat_fpa_overall, spat_fpa_meanacc
#     ) %>%
#     distinct()
#
#   # calculate the weighted mean and st dev summary
#   acc_wt_ave <- acc_sum %>%
#     summarise(mutate(across(where(is.numeric), ~ weighted.mean(.x, transect_no, na.rm = FALSE)))) %>%
#     pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "ave_wt") %>%
#     dplyr::filter(!accuracy_type %in% c("slice", "transect_no"))
#
#   acc_wt_sd <- acc_sum %>%
#     summarise(mutate(across(where(is.numeric), ~ sqrt(wtd.var(.x, transect_no, na.rm = FALSE))))) %>%
#     pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "sd_wt") %>%
#     dplyr::filter(!accuracy_type %in% c("slice", "transect_no"))
#
#   acc_wt_sum <- left_join(acc_wt_ave, acc_wt_sd) %>%
#     filter(!accuracy_type == "transect_no")
#
#   return(acc_wt_sum)
# }

