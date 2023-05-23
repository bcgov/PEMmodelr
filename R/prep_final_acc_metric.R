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

  # # testing lines
  # bgc_pts_subzone = bgc_pts_subzone
  # fid - fid
  # fmat = fmat
  # mtry = min_n
  # min_n = min_n
  # best_balance = best_balance
  # final_model_metric = "overall"
  # # testing lines - end


  model_bgc <- lapply(names(bgc_pts_subzone), function(xx) {

    #xx <- names(bgc_pts_subzone[1])
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

    write.csv(baseout_neighbours, file.path(outDir_raw, "acc_base_results_neighbours.csv"))

    # print("run basic no neighbours model")
    #
    # baseout <- run_base_model(
    #   train_data,
    #   fuzz_matrix = fmat,
    #   mtry = mtry,
    #   min_n = min_n,
    #   use.neighbours = FALSE,
    #   detailed_output = FALSE,
    #   out_dir = outDir_raw)
    #
    # write.csv(baseout, file.path(outDir_raw, "acc_base_results_no_neighbours.csv"))

    # extract theta values

    print("extract theta value")

    balance_raw = file.path(fid$model_draft[2], xx, "raw_outputs", "compiled_theta_results.csv")
    bal <- read.csv(balance_raw)
    bal_out <- bal %>%
      group_by(type, theta_final)%>%
      summarise(mean = mean(value),
                q25 = quantile(value, prob= 0.25),
                q75 = quantile(value, prob = 0.75)) %>%
      mutate(above_thresh = ifelse(q25 <= 0.65, F, T))

    write.csv(bal_out, file.path(outDir_raw, "theta_threshold.csv"),row.names = F)

    # extract best balance option


    best_bgc_balance <- best_balance %>%
      filter(bgc == xx) %>%
      select( bgc, balance,  maxmetric) %>%
      filter(maxmetric == final_model_metric)%>%
      pull(balance)

    best_bal_file <- read.csv(file.path(fid$model_draft[2],xx, "balance", paste0("acc_", best_bgc_balance,".csv")))
    write.csv(best_bal_file, file.path(outDir_raw, "best_balance_acc.csv"),row.names = F)


  })
  return(TRUE)

} # end of function


