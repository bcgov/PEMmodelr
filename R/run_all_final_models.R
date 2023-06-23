#' Run final model for all BGCs
#'
#' @param bgc_pts_subzone list object containing data points
#' @param best_balance datatable
#' @param reduced_vars list of covariates
#' @param mbal text of the metric to choose options are "raw'", "optimal"
#' @param out_dir text string of output location, default is the file structure
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter select mutate
#' @return TRUE
#' @export
#'
#' @examples
#'run_all_final_models(bgc_pts_subzone, best_balance, reduced_vars,mbal = "overall",out_dir = fid$model_final[2])

run_all_final_models <- function(
    bgc_pts_subzone =  bgc_pts_subzone,
    best_balance = best_balance,
    reduced_vars = reduced_vars,
    mbal = "overall",
    out_dir = fid$model_final[2]){

  if(mbal== "raw") {

    mbaldf <- best_balance %>% dplyr::filter(maxmetric == "overall") %>%
      dplyr::select(bgc, balance, ds_ratio, sm_ratio) %>%
      dplyr::mutate(balance = "raw",
             ds_ratio = NA,
             sm_ratio = NA)
  } else {

    mbaldf <- best_balance %>% dplyr::filter(maxmetric == mbal) %>%
      dplyr::select(bgc, balance, ds_ratio, sm_ratio)

  }


  model_bgc <- lapply(names(bgc_pts_subzone), function(xx) {

    #xx <- names(bgc_pts_subzone[1])

    alldat = bgc_pts_subzone[[xx]]

    outDir = file.path(out_dir, xx)

    final_data <- alldat %>%
      dplyr::filter(position == "Orig") %>%
      dplyr::select(mapunit1, any_of(reduced_vars))

    final_data <-  final_data[complete.cases(final_data[, 2:length(final_data)]), ]

    # testing
    #final_data = as.data.table(final_data)
    bgc_bal = mbaldf %>% filter(bgc == xx)
    ds_ratio = bgc_bal %>% pull(ds_ratio)
    sm_ratio = bgc_bal %>% pull(sm_ratio)


    final_model <- run_final_model(
      final_data,
      mtry = mtry,
      min_n = min_n,
      ds_ratio = ds_ratio,
      sm_ratio = sm_ratio)

    # Output model
    saveRDS(final_model, file.path(outDir, paste0("final_model_",mbal,".rds")))
    final_model_report(bgc_bal, final_data, final_model, outDir)

    })

  return(TRUE)
}

