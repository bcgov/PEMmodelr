#' compile balance accuray metrics
#'
#' @param bal_dir directory in which all accuracy metrics are stored
#' @importFrom magrittr "%>%"
#' @importFrom foreach foreach
#' @importFrom dplyr mutate filter group_by summarise rowwise across gather
#' @return datatable with all compiled balance data sets
#' @export
#'
#' @examples
#' combine_balance_outputs(bal_dir = file.path(fid$model_draft[2], "ESSFmc"))

combine_balance_ouputs<- function(bal_dir){
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
  #aresults <- acc_bgc
  # end testing


  # get best output
  best_balance <- aresults %>%
    dplyrr::group_by(balance) %>%
    dplyr::select(balance,aspat_paf_theta1, aspat_paf_theta.5, aspat_paf_theta0, spat_paf_theta1,  spat_paf_theta.5, spat_paf_theta0 ) %>% distinct() %>%
    dplyr::summarise(across(where(is.numeric), mean)) %>%
    ungroup()

  # extract raw values
  raw <- best_balance %>%
    dplyr::filter(balance == "acc_base_results")

  # 1) calculate difference between raw and balanced for each metric
  long_raw <- raw %>%
    dplyr::select(-balance) %>%
    dplyr::gather(key = "column", value = "value_1")

  long_bal <- best_balance %>% dplyr::gather(key = "column", value = "value_2", -balance)

  best_balance_difference_all <- long_bal %>%
    inner_join(long_raw, by = "column")


  # calculate best overall metric
  best_overall <- best_balance_difference_all %>%
    mutate(dif = '-'(value_2, value_1)) %>%
    select(balance, column, dif) %>%
    spread(key = "column", value = "dif") %>%
    rowwise() %>%
    dplyr::mutate(allsum = sum(c_across("aspat_paf_theta.5":"spat_paf_theta1")))

  # select best overall model
  max_overall <- best_overall[which.max(best_overall$allsum),'balance']

  max_raw_overall <- best_balance %>%
    filter(balance %in% c(max_overall,"acc_base_results" ))%>%
    rowwise() %>%
    dplyr::mutate(allsum = sum(c_across("aspat_paf_theta.5":"spat_paf_theta1"))) %>%
    select(balance, allsum)

  max_overall <- best_balance %>%
    filter(balance %in% max_overall) %>%
    rowwise() %>%
    dplyr::mutate(value_2 = sum(c_across("aspat_paf_theta.5":"spat_paf_theta1"))) %>%
    select(balance, value_2) %>%
    mutate(column = "overall")

  raw_overall <- best_balance %>%
    filter(balance == "acc_base_results") %>%
    rowwise() %>%
    dplyr::mutate(value_1 = sum(c_across("aspat_paf_theta.5":"spat_paf_theta1"))) %>%
    select(value_1) %>%
    mutate(column = "overall")

  max_raw_overall <- left_join(max_overall, raw_overall)


  # 2) select best based on aspat and spat values

   best_balance_as <- best_balance  %>%
     rowwise()%>%
     mutate(aspatial_sum = (aspat_paf_theta0 +  aspat_paf_theta.5 +  aspat_paf_theta1)/3,
            spatial_sum = ( spat_paf_theta0 +  spat_paf_theta.5 +  spat_paf_theta1)/3)

   # compare these to raw values (here)
   raw_best_balance <-  best_balance_as %>%
     filter(balance == "acc_base_results") %>%
     select(aspatial_sum, spatial_sum) %>%
     gather(key = "column", value = "value_1") %>%
     rbind(long_raw)


   best_balance_as <- best_balance_as %>%
     gather(key = "column", value = "value_2", -balance) %>%
     group_by(column)%>%
     slice(which.max(value_2))


   # check the difference between raw and best

   best_metrics <- best_balance_as %>%
     full_join(raw_best_balance , by = "column") %>%
     rbind(max_raw_overall) %>%
     rowwise() %>%
     mutate(pcdelta = round((value_2 - value_1) *100,1))%>%
     rename("maxmetric" = column,
            "max" = value_2,
            "raw" = value_1) %>%
     mutate(balance = stringr::str_replace_all(balance, "acc_","")) #%>%
     #mutate(balance = stringr::str_replace_all(balance, "_", " "))

   return(best_metrics)

  }

