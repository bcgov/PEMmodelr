#' Filter out mapunits based on minimum number of records
#'
#' @param tpts training data points set
#' @param min_no minimum no of mapunit points (based on Origin position )
#' @importFrom dplyr count filter
#' @importFrom magrittr "%>%"
#' @return dataframe with the filtered mapunits removed
#' @export
#'
#' @examples
#' filter_min_mapunits(tpts, 20)
#'
filter_min_mapunits <- function(tpts, min_no){

  #min_no =100
 # tpts = alldat

  if("position" %in% names(tpts)) {
    otpts <- tpts %>%
      dplyr::filter(position == "Orig")
  } else {
    otpts <- tpts
  }

  MU_count <- otpts %>%
    dplyr::count(mapunit1)

  todrop <- MU_count %>%
    dplyr::filter(n < min_no)

  print("dropping the following mapunits:")
  print(todrop$mapunit1)

  tokeep <- MU_count %>%
      dplyr::filter(n >= min_no)

  mdat <- tpts %>%
    dplyr::filter(mapunit1 %in% tokeep$mapunit1)%>%
    droplevels()

  return(mdat)

}
