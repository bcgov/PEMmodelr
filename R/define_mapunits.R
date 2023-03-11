#' Define mapunits
#'
#' @param tps sf attribute training points
#' @param mapkey datatable with mapunit key to define field points and attribute to which mapunits will be converted to
#' @param attribute character to define the column name in mapkey that unis will be converted
#' @import sf
#' @importFrom dplyr select left_join rename
#' @return sf attribute with updated mapunit 1 and mapunit 2 fields
#' @export
#'
#' @examples
#' define_mapunits(tps, mapkey,attribute)
#'

define_mapunits <- function(tps, mapkey, attribute) {

 # testing lines
 # tps = allpts[1:1000,1:10]
#  mapkey = mapkey
#  attribute = "BaseMapUnit"


  # match the column for map unit based on key
  mapkeysub <- mapkey %>%
    dplyr::select(FieldCall, attribute)
  names(mapkeysub) = c("FieldCall", "MapUnit")

  outdata <- tpts %>%
    dplyr::left_join( mapkeysub, by = c("mapunit1" = "FieldCall")) %>%
    dplyr::select(-mapunit1) %>%
    dplyr::rename(mapunit1 = MapUnit) %>%
    dplyr::left_join( mapkeysub, by = c("mapunit2" = "FieldCall")) %>%
    dplyr::select(-mapunit2) %>%
    dplyr::rename(mapunit2 = MapUnit)

  outdata <- outdata %>%
    mutate(mapunit1 = ifelse(!is.na(mapunit2) & is.na(mapunit1), mapunit2, mapunit1)) %>%
    mutate(mapunit2 = ifelse((mapunit1 == mapunit2), NA, mapunit2))

  return(outdata)


}

