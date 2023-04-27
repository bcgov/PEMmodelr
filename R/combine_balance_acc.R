#' compile balance acccuray metrics
#'
#' @param bal_dir directory in which all accuracy metrics are stored
#' @importFrom magrittr "%>%"
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
