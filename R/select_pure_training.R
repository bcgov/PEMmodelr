#' Unique map units from primary and alternate calls
#'
#' Simple function that returns a list of unique map units from mapunit1 and mapunit2 fields
#'
#'
#' @param tran_dat is the transect training points
#'
#' @keywords mapunit
#' @export
#' ##


select_pure_training <- function(tran_dat) {
  pure_tran_dat <- tran_dat %>%
    filter(!mapunit1 == "") %>%
    filter(!is.na(mapunit1)) %>%
    mutate(mapunit2 = replace_na(mapunit2, "")) %>%
    distinct()

  return(pure_tran_dat)
}
