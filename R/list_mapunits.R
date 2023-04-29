#' Unique map units from primary and alternate calls
#'
#' returns a list of unique map units from mapunit1 and mapunit2 fields
#'
#' @param tps dataframe of mapunits
#' @importFrom magrittr "%>%"
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select distinct rename
#' @keywords mapunit
#' @export
#' @examples
#' list_mapunits(tps)
#'
#'
list_mapunits <- function(tps) {

  #tps = mpts
  if(any(class(tps) =="sf")) {

    #print("sf")
    tps <- tps %>%
      sf::st_drop_geometry
  }

  mapunit1_unique <- tps %>%
    dplyr::select(mapunit1) %>%
    dplyr::distinct()
  mapunit2_unique <- tps %>%
    dplyr::select(mapunit2) %>%
    distinct() %>%
    dplyr::rename(mapunit1 = mapunit2)
  mapunit_unique <- rbind(mapunit1_unique, mapunit2_unique) %>%
    distinct() %>%
    pull()

  return(mapunit_unique)
}
