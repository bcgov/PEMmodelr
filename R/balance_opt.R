#' Optimise Training Point Balance
#'
#' @param train_data A data frame of training data with target and covariates
#' @param num_slice Integer - number of slices to use for k-fold cv
#' @param n_iters Integer - number of optimisation iterations
#' @return List with optimal downsample and smote
#' @import data.table
#' @import foreach
#' @import recipe
#' @import themis
#' @import ranger
#' @import ParBayesianOptimization
#'
#' @author Kiri Daust
#' @examples
#'
#' trDat <- st_read("./s1_clean_neighbours_allatts.gpkg")
#' trDat <- as.data.table(st_drop_geometry(trDat))
#' trDat <- trDat[grep("ESSFmc_",mapunit1),]
#' setnames(trDat,old = "mapunit1",new = "target")
#' trDat[,c("CellNum","mapunit2","transect_id","data_type","transition","observer","comments","ID.1") := NULL]
#' trDat <- trDat[,colSums(is.na(trDat)) == 0, with = FALSE]
#' covmat <- cor(trDat[,-(1:5), with = FALSE])
#' covmat[upper.tri(covmat)] <- 0
#' diag(covmat) <- 0
#' trDat <- trDat[,c(rep(TRUE,5),!apply(covmat, 2, function(x) any(abs(x) > 0.9, na.rm = TRUE))),with = FALSE]
#'
#' @export


optimise_balance <- function(train_data,num_slice = 2, n_iters = 4){
  ##function to train model
  train_mod <- function(downsample, smote, trDat = train_data, use.neighbours = T){
    sresults <- foreach(k = num_slice, .combine = 'c') %do% {
      BGC_train <- trDat[slice != k & Position == "Orig",]
      MU_count <- BGC_train[,.(NumUnit = .N), by = .(target)][NumUnit < 10,]
      BGC_train[,target := as.factor(target)]
      BGC_train[,c("ID","tid", "Position","slice") := NULL]

      if (use.neighbours) {
        BGC_test <- trDat[slice == k & !target %in% MU_count$target,]
      }else{
        BGC_test <- trDat[slice == k & !target %in% MU_count$target & Position == "Orig",]
      }

      null_recipe <-  recipe(target ~ ., data = BGC_train) %>%
        step_downsample(target, under_ratio = downsample) %>%
        step_smote(target, over_ratio = smote, neighbors = 2, skip = TRUE) %>%
        prep()

      train_data <- bake(null_recipe,new_data = NULL)
      PEM_rf1 <- ranger(target ~ ., data = train_data, mtry = 26,
                        num.trees = 151, min.node.size = 6, importance = "permutation")

      test.pred <-  predict(PEM_rf1, BGC_test)
      test.pred <- cbind(BGC_test[,.(Position,ID, tid, target)],pred_class = test.pred$predictions)
      test.pred <- as.data.table(test.pred)

      accMet <- test.pred[,.(Correct = if(unique(target) %in% pred_class) 1 else 0),
                          by = .(ID)]
      accVal <- sum(accMet$Correct)/nrow(accMet)
      accVal
    }
    return(list(Score = mean(sresults)))
  }

  ##set bounds
  bounds <- list(
    downsample = c(20,100),
    smote = c(0.2,0.9)
  )

  ##run optimisation
  opt_res <- bayesOpt(
    FUN = train_mod,
    bounds = bounds,
    initPoints = 4,
    iters.n = n_iters,
    iters.k = 1
  )

  return(getBestPars(opt_res))
}
