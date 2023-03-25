#' Unique map units from primary and alternate calls
#'
#' Function that returns subset of training points with only pure calls (primary calls)
#'
#' @param tps is the transect training points
#' @importFrom dplyr filter mutate distinct
#' @importFrom magrittr "%>%"
#' @keywords mapunit
#' @export
#' @examples
#'
#'select_pure_training(tps)
#

select_pure_training <- function(tps) {

  pure_tran_dat <- tps %>%
    dplyr::filter(!mapunit1 == "") %>%
    dplyr::filter(!is.na(mapunit1)) %>%
    dplyr::mutate(mapunit2 = replace_na(mapunit2, "")) %>%
    dplyr::distinct()

  return(pure_tran_dat)
}
