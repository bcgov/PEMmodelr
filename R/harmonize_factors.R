#' Harmonize factor levels
#'
#' @param target_vs_pred A data frame  with target and .pred_class columns
#' @return data.frame with harmonized factor levels
#' @importFrom magrittr "%>%"
#' @examples
#' harmonize_factors(pred_data)
#' @export

harmonize_factors <- function(target_vs_pred){
  targ.lev <- levels(as.factor(target_vs_pred$mapunit1))
  pred.lev <- levels(as.factor(target_vs_pred$.pred_class))
  levs <- c(targ.lev, pred.lev) %>% unique()
  target_vs_pred$mapunit1 <- factor(target_vs_pred$mapunit1, levels = levs)
  target_vs_pred$.pred_class <- factor(target_vs_pred$.pred_class, levels = levs)
  return(target_vs_pred)
}

