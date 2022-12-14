#' Harmonize factor levels
#'
#' @param target_vs_pred A data frame  with target and .pred_class columns
#' @return data.frame with harmonized factor levels
#'
#' @author
#' @examples
#'

harmonize_factors <- function(target_vs_pred){
  targ.lev <- levels(target_vs_pred$mapunit1)
  pred.lev <- levels(target_vs_pred$.pred_class)
  levs <- c(targ.lev, pred.lev) %>% unique()
  target_vs_pred$mapunit1 <- factor(target_vs_pred$mapunit1, levels = levs)
  target_vs_pred$.pred_class <- factor(target_vs_pred$.pred_class, levels = levs)
  return(target_vs_pred)
}

harmonize_factors_dt <- function(target_vs_pred){
  targ.lev <- levels(as.factor(as.character(target_vs_pred$mapunit1)))
  pred.lev <- levels(as.factor(as.character(target_vs_pred$.pred_class)))
  levs <- unique(c(targ.lev, pred.lev))
  target_vs_pred[,mapunit1 := factor(mapunit1,levels = levs)]
  target_vs_pred[,.pred_class := factor(.pred_class,levels = levs)]
  return(TRUE)
}
