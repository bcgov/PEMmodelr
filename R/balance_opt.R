#' Optimise Training Point Balance
#'
#' @param train_data A data frame of training data with target and covariates
#' @param num_slice Integer - number of slices to use for k-fold cv
#' @param n_iters Integer - number of optimisation iterations
#' @return List with optimal downsample and smote
#' @import data.table
#' @importFrom foreach "%do%"
#' @import recipes
#' @import themis
#' @importFrom ranger ranger
#' @import ParBayesianOptimization
#' @export
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
# rawDat <- st_read("./PEM_dev/s1_clean_neighbours_allatts.gpkg")
# rawDat <- st_read("D:/PEM_DATA/PEMmodelr/CanyonCreek/10_clean_inputs/30_trainingdata/s1_transect_all_pts_att.gpkg")
# rawDat <- as.data.table(st_drop_geometry(rawDat))
# rawDat <- rawDat[grep("ESSFmc_",mapunit1),]
# rawDat[,c("CellNum","transect_id","data_type","transition","observer","comments","ID.1") := NULL]
# clnDat <- rawDat[,c(rep(T,6),colSums(is.na(rawDat[,-c(1:6), with = F])) == 0), with = FALSE]
# covmat <- cor(clnDat[,-(1:6), with = FALSE])
# covmat[upper.tri(covmat)] <- 0
# diag(covmat) <- 0
# clnDat <- clnDat[,c(rep(TRUE,6),!apply(covmat, 2, function(x) any(abs(x) > 0.9, na.rm = TRUE))),with = FALSE]
# fMat <- fread("./fuzzy_matrix_basic_updated.csv")
# train_data <- clnDat
# train_data <- fread("TestPredict.csv")
#
#opt_res <- optimise_balance(train_data = clnDat, fuzz_matrix = fMat, num_slice = 2)
#  getBestPars(opt_res)

# # test example
# train_data <- trDat %>% filter(mapunit1 %in% c("ICHmc1_01a", "ICHmc1_03","ICHmc1_07" ))
# train_data <- trDat %>% filter(slice %in% c(1,2))
#
# train_data = as.data.table(train_data)
#
#
# opt_res <- optimise_balance(train_data =  train_data,
#                             fuzz_matrix = fMat,
#                             num_slice = 2,
#                             n_iters = 4,
#                             use.neighbours = TRUE,
#                             acc_mets = c("spat_paf_theta.5","aspat_paf_theta.5","spat_paf_theta0"),
#                             mtry = 14,
#                             min_n = 7)

#  getBestPars(opt_res)

optimise_balance <- function(train_data, fuzz_matrix, num_slice = 2, n_iters = 4, mtry = 14,min_n = 7, use.neighbours = TRUE,
                             acc_mets = c("spat_paf_theta.5","aspat_paf_theta.5","spat_paf_theta0")){
 # # # # data lines
 #  train_data <- trDat %>%
 #    filter(slice != 1)#%>%
 #    #dplyr::select(-bgc_cat,-transect_id)
 #
 #  train_data = as.data.table(trDat)
 #  train_data <-  train_data[grep("ICHmc2_",mapunit1),]
 #  train_data <- droplevels(train_data)
 #
 #  fuzz_matrix = fmat
 #  num_slice = 4
 #   n_iters = 4
 #   use.neighbours = TRUE
 #   acc_mets = c("spat_paf_theta.5","aspat_paf_theta.5","spat_paf_theta0")
 #   mtry = 4 #mtry
 #   min_n = 7# min_n
 #   # end data lines

  ref_dat <- copy(train_data)
  ref_dat[,mapunit1 := as.factor(mapunit1)]
  print("Training raw data models...")

  #ref_acc <- foreach(k = 1:num_slice, .combine = rbind) %do% {
  ref_acc <- foreach(k = 2:num_slice, .combine = rbind) %do% {
    ref_train <- ref_dat[slice != k & position == "Orig",]
    ref_train[,c("id","tid","mapunit2", "position","slice","transect_id","bgc_cat") := NULL]
    low_units <- ref_train[,.(NumUnit = .N), by = .(mapunit1)][NumUnit < 10,]
    ref_train <- ref_train[!mapunit1 %in% low_units$mapunit1,]

    if (use.neighbours) {
      ref_test <- ref_dat[slice == k & !mapunit1 %in% low_units$mapunit1,]
     # ref_test <- ref_dat[slice == k,] #& !mapunit1 %in% low_units$mapunit1,]
       }else{
      ref_test <- ref_dat[slice == k & !mapunit1 %in% low_units$mapunit1 & position == "Orig",]
     # ref_test <- ref_dat[slice == k & position == "Orig",]
       }

    # need to add the varaible id in here

    ref_mod <- ranger::ranger(mapunit1 ~ ., data = ref_train, mtry = mtry,
                      num.trees = 151, min.node.size = min_n, importance = "permutation")

    preds <- predict(ref_mod, ref_test)
    pred_all <- cbind(ref_test[,.(id, mapunit1, mapunit2, slice)],
                                    .pred_class = preds$predictions)
    pred_all$mapunit1 <- as.factor(pred_all$mapunit1)
    pred_all$.pred_class <- factor(pred_all$.pred_class,
                                   levels = levels(pred_all$mapunit1))
    # all_units <- unique(c(pred_all$mapunit1,pred_all$.pred_class))
    # pred_all[,`:=`(mapunit1 = factor(mapunit1,levels = all_units),
    #                .pred_class = factor(.pred_class, levels = all_units))]
    print(paste0("generating accuracy metrics for slice:",k))

    acc <- acc_metrics(pred_all, fuzzmatrx = fuzz_matrix)
    acc <- acc[,acc_mets]
    acc
  }

  ref_acc_all <- colMeans(ref_acc)
  ref_acc_fn <- mean(ref_acc_all)

  ##function to train model
  train_mod <- function(downsample, smote, ttrDat = train_data, ref_metrix = ref_acc_fn, UN = use.neighbours){
    #sresults <- foreach(k = 1:num_slice, .combine = rbind) %do% {

    sresults <- foreach(k = 2:num_slice, .combine = rbind) %do% {
      #k = 2
      #ttrDat = train_data
      BGC_train <- ttrDat[slice != k & position == "Orig",]
      MU_count <- BGC_train[,.(NumUnit = .N), by = .(mapunit1)][NumUnit < 10,]
      BGC_train <- BGC_train[!mapunit1 %in% MU_count$mapunit1,]
      BGC_train[,mapunit1 := as.factor(mapunit1)]
      BGC_train[,c("id","tid","mapunit2", "position","slice","transect_id","bgc_cat") := NULL]

      if (UN) {
        BGC_test <- ttrDat[slice == k & !mapunit1 %in% MU_count$mapunit1,]
      }else{
        BGC_test <- ttrDat[slice == k & !mapunit1 %in% MU_count$mapunit1 & Position == "Orig",]
      }

      null_recipe <-  recipe(mapunit1 ~ ., data = BGC_train) %>%
        step_downsample(mapunit1, under_ratio = downsample) %>%
        step_smote(mapunit1, over_ratio = smote, neighbors = 2, skip = TRUE) %>%
        prep()

      train_data <- bake(null_recipe, new_data = NULL)
      #train_data$mapunit1 <- as.factor(as.character(train_data$mapunit1))
      PEM_rf1 <- ranger(mapunit1 ~ ., data = train_data, mtry = mtry,
                        num.trees = 151, min.node.size = min_n, importance = "permutation")

      test.pred <-  predict(PEM_rf1, BGC_test)
      test.pred <- cbind(BGC_test[,.(id, mapunit1, mapunit2, slice)],
                         .pred_class = as.character(test.pred$predictions))
      test.pred <- test.pred[!is.na(test.pred$.pred_class),]

      #harmonize_factors_dt(test.pred)
      mod_acc <- acc_metrics(test.pred,fuzzmatrx = fuzz_matrix)
      mod_acc <- mod_acc[,acc_mets]
      mod_acc
      # accMet <- test.pred[,.(Correct = if(unique(target) %in% pred_class) 1 else 0),
      #                     by = .(ID)]
      # accVal <- sum(accMet$Correct)/nrow(accMet)
    }
    bal_acc <- colMeans(sresults)
    bal_acc_fn <- mean(bal_acc)
    return(list(Score = bal_acc_fn - ref_acc_fn))
  }

  ##set bounds
  bounds <- list(
    downsample = c(20,100),
    smote = c(0.2,0.9)
  )

  print("Starting optimisation...")
  ##run optimisation
  opt_res <- bayesOpt(
    FUN = train_mod,
    bounds = bounds,
    initPoints = 4,
    iters.n = n_iters,
    iters.k = 1
  )

  return(opt_res)
}


