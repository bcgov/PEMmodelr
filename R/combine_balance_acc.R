#' compile balance accuray metrics
#'
#' @param bal_dir directory in which all accuracy metrics are stored
#' @importFrom magrittr "%>%"
#' @importFrom foreach foreach
#' @importFrom dplyr mutate filter group_by summarise rowwise across distinct ungroup
#' @importFrom tidyr gather spread
#' @return datatable with all compiled balance data sets
#' @export
#'
#' @examples
#' combine_balance_outputs(bal_dir = file.path(fid$model_draft[2], "ESSFmc"))

combine_balance_ouputs <- function(bal_dir){
  # testing
  #outDir = file.path(fid$model_draft[2], xx)
  #  end testing

  alldata_list <- list.files(file.path(outDir), full.names = TRUE, pattern = "acc_", recursive = TRUE)

  # remove files with no information
  data_list<- alldata_list[file.info(alldata_list)$size>10]

  aresults <- foreach::foreach(k = data_list, .errorhandling = "pass",.combine = rbind) %do% {
    #k = data_list[2]
    # print(k)
    temp = read.csv(k)
    temp <- temp %>% dplyr::mutate(filename = paste(basename(k)))
    temp
  }

  aresults <- aresults %>%
   # rowwise()%>%
    dplyr::mutate(balance = gsub(".csv","", filename ))

  return(aresults)
}



# function to select best balance options
#' @rdname select_best_acc

select_best_acc <- function(aresults){

  # testing
 aresults <- acc_bgc
  # end testing

  # get best output
  best_balance <- aresults %>%
    dplyr::group_by(balance) %>%
    dplyr::select(balance,aspat_paf_theta1, aspat_paf_theta.5, aspat_paf_theta0, spat_paf_theta1,  spat_paf_theta.5, spat_paf_theta0 ) %>% distinct() %>%
    dplyr::summarise(across(where(is.numeric), mean)) %>%
    dplyr::ungroup()

  # extract raw values
  raw <- best_balance %>%
    dplyr::filter(balance == "acc_base_results")

  # 1) calculate difference between raw and balanced for each metric
  long_raw <- raw %>%
    dplyr::select(-balance) %>%
    tidyr::gather(key = "column", value = "value_1")

  long_bal <- best_balance %>% tidyr::gather(key = "column", value = "value_2", -balance)

  best_balance_difference_all <- long_bal %>%
    dplyr::inner_join(long_raw, by = "column")


  # calculate best overall metric
  best_overall <- best_balance_difference_all %>%
    dplyr::mutate(dif = '-'(value_2, value_1)) %>%
    dplyr::select(balance, column, dif) %>%
    tidyr::spread(key = "column", value = "dif") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(allsum = sum(c_across("aspat_paf_theta.5":"spat_paf_theta1")))

  # select best overall model
  max_overall <- best_overall[which.max(best_overall$allsum),'balance']

  max_raw_overall <- best_balance %>%
    dplyr::filter(balance %in% c(max_overall,"acc_base_results" ))%>%
    dplyr::rowwise() %>%
    dplyr::mutate(allsum = sum(c_across("aspat_paf_theta1":"spat_paf_theta0"))) %>%
    dplyr::select(balance, allsum)

  max_overall <- best_balance %>%
    dplyr::filter(balance %in% max_overall) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value_2 = (sum(c_across("aspat_paf_theta1":"spat_paf_theta0")))/6) %>%
    dplyr::select(balance, value_2) %>%
    dplyr::mutate(column = "overall")

  raw_overall <- best_balance %>%
    dplyr::filter(balance == "acc_base_results") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value_1 = (sum(c_across("aspat_paf_theta1":"spat_paf_theta0")))/6) %>%
    dplyr::select(value_1) %>%
    dplyr::mutate(column = "overall")

  max_raw_overall <- left_join(max_overall, raw_overall, by = "column")


  # 2) select best based on aspat and spat values

   best_balance_as <- best_balance  %>%
     dplyr::rowwise()%>%
     dplyr::mutate(aspatial_sum = (aspat_paf_theta0 +  aspat_paf_theta.5 +  aspat_paf_theta1)/3,
            spatial_sum = ( spat_paf_theta0 +  spat_paf_theta.5 +  spat_paf_theta1)/3)

   # compare these to raw values (here)
   raw_best_balance <-  best_balance_as %>%
     dplyr::filter(balance == "acc_base_results") %>%
     dplyr::select(aspatial_sum, spatial_sum) %>%
     tidyr::gather(key = "column", value = "value_1") %>%
     rbind(long_raw)


   best_balance_as <- best_balance_as %>%
     tidyr::gather(key = "column", value = "value_2", -balance) %>%
     dplyr::group_by(column)%>%
     dplyr::slice(which.max(value_2))


   # check the difference between raw and best

   best_metrics <- best_balance_as %>%
     dplyr::full_join(raw_best_balance , by = "column") %>%
     rbind(max_raw_overall) %>%
     dplyr::rowwise() %>%
     dplyr::mutate(pcdelta = round((value_2 - value_1) *100,1))%>%
     dplyr::rename("maxmetric" = column,
            "max" = value_2,
            "raw" = value_1) %>%
     dplyr::mutate(balance = stringr::str_replace_all(balance, "acc_","")) #%>%

   # split into easy to use columns
   best_metrics <- best_metrics %>%
     dplyr::mutate(ds = case_when(
       stringr::str_detect(balance, "ds") ~ 'ds',
       .default = NA)) %>%
    dplyr::mutate(sm = case_when(
       stringr::str_detect(balance, "sm") ~ 'sm',
       .default = NA)) %>%
     rowwise() %>%
     dplyr::mutate(ds_ratio = ifelse(!is.na(ds), stringr::str_split(balance, "_")[[1]][2],NA),
                   sm_ratio = ifelse(!is.na(sm) & !is.na(ds), stringr::str_split(balance, "_")[[1]][4],NA))%>%
     dplyr::mutate(sm_ratio = ifelse(!is.na(sm) & is.na(ds), stringr::str_split(balance, "_")[[1]][2],sm_ratio))

   return(best_metrics)

  }


