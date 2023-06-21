#' prepare final accuracy metrics
#'
#' @param bgc_pts_subzone training data prepares as list
#' @param fid object with filepaths
#' @param fmat fuzzy matrix
#' @param mtry best mtry from model set up inputs
#' @param min_n best mtry from model set up inputs
#' @param best_balance output metrics
#' @param final_model_metric metric to assess best balance, default  = "overall"
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#' prep_final_acc_metric(bgc_pts_subzone, fid, fmat, mtry, min_n, best_balance, final_model_metric = "overall")

prep_final_acc_metric <- function(bgc_pts_subzone, fid, fmat, mtry, min_n, best_balance, final_model_metric = "overall") {

  # # # testing lines
  # bgc_pts_subzone = bgc_pts_subzone
  # fid = fid
  # fmat = fmat
  # mtry = min_n
  # min_n = min_n
  # best_balance = best_balance
  # final_model_metric = "overall"
  # # testing lines - end
  #
  model_bgc <- lapply(names(bgc_pts_subzone), function(xx) {

    #xx <- names(bgc_pts_subzone[3])
    alldat = bgc_pts_subzone[[xx]]

    outDir_raw = file.path(fid$model_final[2], xx, "raw")
    ifelse(!dir.exists(file.path(outDir_raw)),
           dir.create(file.path(outDir_raw)), FALSE)

    tdat <- alldat %>% mutate(slice = factor(slice))
    tdat <- tdat %>%
      dplyr::select(id, mapunit1, mapunit2, position,
                    transect_id, tid, slice, any_of(reduced_vars))

    tdat <- tdat[complete.cases(tdat[, 8:length(tdat)]), ]

    # run basic model (no smote and no downsample)
    train_data <- tdat
    train_data <- droplevels(train_data)

    print("run basic neighbours model")

    baseout_neighbours <- run_base_model(
      train_data,
      fuzz_matrix = fmat,
      mtry = mtry,
      min_n = min_n,
      use.neighbours = TRUE,
      detailed_output = FALSE,
      out_dir = outDir_raw)

    write.csv(baseout_neighbours, file.path(outDir_raw, "acc_base_results_neighbours.csv"), row.names = T)

    print("run basic no neighbours model")

    baseout <- run_base_model(
      train_data,
      fuzz_matrix = fmat,
      mtry = mtry,
      min_n = min_n,
      use.neighbours = FALSE,
      detailed_output = FALSE,
      out_dir = outDir_raw)

    write.csv(baseout, file.path(outDir_raw, "acc_base_results_no_neighbours.csv"), row.names = T)

    # extract theta values thresholds

    balance_raw = read.csv(file.path(fid$model_draft[2], xx, "theta_threshold.csv"))

    write.csv(balance_raw, file.path(outDir_raw, "theta_threshold.csv"),row.names = F)



    # extract best balance option
    print("run balance with neighbours model")

    best_balance <- read.csv(file.path(fid$model_inputs0310[2], "best_balancing.csv"))


    best_bgc_balance <- best_balance %>%
      filter(bgc == xx) %>%
      select(bgc, balance,  maxmetric) %>%
      filter(maxmetric == 'overall')%>%
      pull(balance)

    best_bal_file <- read.csv(file.path(fid$model_draft[2],xx, "balance", paste0("acc_", best_bgc_balance,".csv")))
    write.csv( best_bal_file, file.path(outDir_raw, "best_balance_acc_neighbours.csv"),row.names = F)

    # extract best balance option
    print("run balance with neighbours model")

    mbaldf <- best_balance %>% dplyr::filter(maxmetric == "overall") %>%
      select(bgc, balance, ds_ratio, sm_ratio)

    bgc_bal = mbaldf %>% filter(bgc == xx)
    ds_ratio = bgc_bal %>% pull(ds_ratio)
    sm_ratio = bgc_bal %>% pull(sm_ratio)

    bal_model <- balance_optimisation_iteration(
      train_data =  train_data,
      ds_iterations = ds_ratio,
      smote_iterations =  sm_ratio,
      mtry = mtry,
      min_n = min_n,
      fuzz_matrix = fmat,
      out_dir = outDir_raw,
      use.neighbours = FALSE,
      detailed_output = FALSE,
      out_dir_detailed = NA)

  })


  best_bal_file <- read.csv(file.path(outDir_raw,"balance", paste0("acc_", best_bgc_balance,".csv")))
  write.csv( best_bal_file, file.path(outDir_raw, "best_balance_acc_no_neighbours.csv"),row.names = F)

})
return(TRUE)

} # end of function
