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


list_mapunits <- function(tran_dat) {
  mapunit1_unique <- tran_dat %>%
    dplyr::select(mapunit1) %>%
    distinct()
  mapunit2_unique <- tran_dat %>%
    dplyr::select(mapunit2) %>%
    distinct() %>%
    dplyr::rename(mapunit1 = mapunit2)
  mapunit_unique <- rbind(mapunit1_unique, mapunit2_unique) %>% distinct()
  return(mapunit_unique)
}
