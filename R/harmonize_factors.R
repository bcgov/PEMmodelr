#' Harmonize factor levels
#'
#' @param target_vs_pred A data frame  with target and .pred_class columns
#' @return data.frame with harmonized factor levels
#'
#' @author
#' @examples
#'

harmonize_factors <- function(target_vs_pred){
  targ.lev <- levels(target_vs_pred$target)
  pred.lev <- levels(target_vs_pred$.pred_class)
  levs <- c(targ.lev, pred.lev) %>% unique()
  target_vs_pred$target <- factor(target_vs_pred$target, levels = levs)
  target_vs_pred$.pred_class <- factor(target_vs_pred$.pred_class, levels = levs)
  return(target_vs_pred)
}
