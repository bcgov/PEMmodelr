
# function to determine balancing parameters for recipe preparation. 
# Currently set for smote and or downsample combination. 
# Used to determine optimum balancing requirement for dataset. 
#extrarun = FALSE; use.neighbours = TRUE
balance_optimisation_raw <- function(trDat , extrarun = FALSE, extradat = NULL, downsample = FALSE, smote = FALSE, use.neighbours = TRUE){
  
  trDat = tr
  balance_name = "raw"
  
  
  # Cross validation loop based on slices 
  slices <- unique(trDat$slice) %>% droplevels()
  
  # for all slices
  sresults <- foreach(k = levels(slices)) %do% {
    
   # k = levels(slices)[3]
    ### split into train and test based on 5-site slices
    print(k)
    
    bgc.label = unique(trDat$bgc)
    # training set - train only on pure calls
    BGC_train <- trDat %>% dplyr::filter(!slice %in% k, position %in% "Orig") %>%
      filter(is.na(target2) | target2 == "") %>% dplyr::select(-slice, -bgc, -transect_id, -position, -id) %>%           
      mutate(across("tid", str_replace, " ", "")) %>% 
      mutate(across("tid", str_replace, " ", ""))
    
    
    #if(extra = FALSE) {remove(extra)}
    # merge in extra point data
    if(isTRUE(extrarun)) {
      print("adding extra points at each model build")
      BGC_train <- rbind(BGC_train, extradat)
    }
    
    # check if enough data for each class to smote
       MU_count <- BGC_train %>% dplyr::count(target) %>% filter(n < 10) %>%
      pull(target)
    
    BGC_train <- BGC_train %>%
      dplyr::select(-target2) %>%  drop_na() %>% droplevels() %>% data.frame# %>% mutate(tid = factor(tid)) %>%  droplevels()
    
    if (isTRUE(use.neighbours)) {
      BGC_test <- trDat %>% filter(slice %in% k) %>%
        filter(!target %in% MU_count)
    } else{ BGC_test <- trDat %>% filter(slice %in% k, position %in% "Orig") %>%
      filter(!target %in% MU_count)}

    BGC_test_all <- BGC_test # keep for the target2 alt call. 
    BGC_test_transect_no <- length(unique(BGC_test_all$transect_id))
    BGC_test <- BGC_test %>%
      dplyr::select(-slice,-target2,-transect_id, -position, -bgc)
    # 
    null_recipe <-  recipe(target ~ ., data = BGC_train) %>%
      update_role(tid, new_role = "id variable")
      

    randf_spec <- rand_forest(mtry = mtry, min_n = min_n, trees = 151) %>% 
      set_mode("classification") %>%
      set_engine("ranger", importance = "permutation", verbose = FALSE) 
    
    pem_workflow <- workflow() %>%
      add_recipe(null_recipe) %>%
      add_model(randf_spec)
    
    #######################################################
    PEM_rf1 <- fit(pem_workflow, BGC_train)
    final_fit <- extract_fit_parsnip(PEM_rf1)
        oob  <- round(PEM_rf1$fit$fit$fit$prediction.error, 3)
    
    ######### Predict Test
    test_target <- BGC_test_all %>% dplyr::select(id,target, target2)
    test.pred <-  predict(PEM_rf1, BGC_test)
    test.pred <- cbind(test_target, test.pred) %>% distinct()
    test.pred <- test.pred %>%  mutate_if(is.factor, as.character)# %>% 
### THIS NEEDS TO BE PULLED INTO A FUNCTION OPTION
    convert <- c("X", "A", "W_t", "W", "Sc", "R", "F", "Non_veg","Wat", "Wb")
    
    test.pred <- test.pred %>% mutate(target = ifelse(target %in% convert, "nonfor", target),
                                      target2 = ifelse(target2 %in% convert, "nonfor", target2) ,
                                      .pred_class = ifelse(.pred_class  %in% convert, "nonfor", .pred_class )) 
    
    test.pred <- test.pred %>%  mutate_all(as.factor) %>% distinct()
    ###harmonize levels
    targ.lev <- levels(test.pred$target)
    pred.lev <- levels(test.pred$.pred_class)
    levs <- c(targ.lev, pred.lev) %>% unique()
    test.pred$target <- factor(test.pred$target, levels = levs)
    test.pred$.pred_class <- factor(test.pred$.pred_class, levels = levs)
    # output test predictions
        test.pred.out <- test.pred %>% mutate(slice = k)
    acc.compare <- acc_metrix(test.pred, theta = 0.5) %>%
      mutate(slice = k,
             transect_no = BGC_test_transect_no,
             acc_type = "test_estimate", 
             oob = oob, 
             balance = balance_name,
             bgc = bgc.label)
    ##use function to add in theta  
    # acc.theta <- theta_accuracy(acc.compare, theta = 0.5) 
    # acc.compare <- cbind(acc.compare,acc.theta) %>% dplyr::select(unique(colnames(.)))
    acc.compare <- acc.compare  %>% 
      dplyr::select(bgc, slice, target,balance, trans.sum, trans.tot, pred.tot, acc, kap, everything() )
    return(list(acc.compare))
  }
  
  # extract results from sresults
  acc_results <- lapply(sresults, function(x) x[[1]])
  acc <- as.data.frame(rbindlist(acc_results))
  
  write.csv(acc, file = paste(outDir, paste0("acc_", balance_name,".csv"),sep = "/"))

  #fwrite(acc, file.path(out_dir, mname, bgc.choose, paste0("acc_", balance_name,".csv")))
  
} # end of smote iteration



balance_optimisation2 <- function(trDat , extrarun = FALSE, extradat = NULL, downsample = TRUE, smote = TRUE, use.neighbours = TRUE) {
  
    for(d in ds_iterations){
     #d = ds_iterations[1]
     #print(d)
    
    for(i in smote_iterations){
      
       #i = smote_iterations[1]
       #print(i)
      # 
      # test if enough data in the slice to use smoting 
      
      # set up the parameters for balancing
      downsample_ratio = d  # (0 - 100, Null = 1)
      smote_ratio = i    # 0 - 1, 1 = complete smote
      
      balance_name = paste0("ds_",downsample_ratio,"_sm_", smote_ratio)

        # training set - train only on pure calls

        
      # Cross validation loop based on slices 
      slices <- unique(trDat$slice) %>% droplevels()
      
      # if(length(slices)<2){
      #   
      #   # switching to transect iteration instead of slices
      #   
      #   trDat_key <- trDat %>%
      #     dplyr::select(c(tid)) %>%
      #     distinct() %>%
      #     mutate(slice = as.factor(seq(1,length(tid),1)))
      #   
      #   trDat <- trDat %>%
      #     dplyr::select(-slice) %>%
      #     left_join(trDat_key)
      #   
      #   slices <- unique(trDat$slice) %>% droplevels()
      #   
      # }
      
      
      # for all slices
      sresults <- foreach(k = levels(slices)) %do% {
        
       #k = levels(slices)[3]
        label = paste(d, i, k, sep = "-")
        print(label)
       #print(k)  
        ### split into train and test based on 5-site slices
        
         bgc.label = unique(trDat$bgc)    
        # training set

        BGC_train <- trDat %>% dplyr::filter(!slice %in% k, position %in% "Orig") %>%
          filter(is.na(target2) | target2 == "") %>% dplyr::select(-slice, -bgc,  -transect_id, -position, -id) %>%           
          mutate(across("tid", str_replace, " ", "")) %>% 
          mutate(across("tid", str_replace, " ", ""))      

        # merge in extra point data
        if(isTRUE(extrarun)) {
          print("adding extra points at each model build")
          BGC_train <- rbind(BGC_train, extradat)
        }
        # check if enough data for each class to smote
        
        MU_count <- BGC_train %>% dplyr::count(target) %>% filter(n < 10) %>%
          pull(target) %>% droplevels()
        
        if(length(MU_count) > 0) {
          print("remove classes with very low training points from smoting test")
          BGC_train <- BGC_train %>% 
            filter(!target %in% MU_count) %>% droplevels()
        }
        # 

 
        BGC_train <- BGC_train %>%
          dplyr::select(-target2) %>%  drop_na() %>% mutate(target = factor(target), tid = as.character(tid)) %>%  droplevels()
        
        # test set
        # test set
        if (isTRUE(use.neighbours)) {
          BGC_test <- trDat %>% filter(slice %in% k) %>%
            filter(!target %in% MU_count)
        } else{ BGC_test <- trDat %>% filter(slice %in% k, position %in% "Orig") %>%
          filter(!target %in% MU_count)}

        BGC_test_all <- BGC_test # keep for the target2 alt call. 
        BGC_test_transect_no <- length(unique(BGC_test_all$transect_id))
        BGC_test <- BGC_test %>%
          dplyr::select(-slice,-target2,-transect_id, -bgc)
        # 
        ############### Define test recipes and workflow ###################
        
        # null_recipe <- balance_optimum(indata = BGC_train, 
        #                                downsample = downsample,
        #                                downsample_ratio = downsample_ratio,
        #                                smote= smote, 
        #                                smote_ratio = smote_ratio,
        #                                update_role(tid, new_role = "id variable"))
        
        
        null_recipe <-  recipe(target ~ ., data = BGC_train) %>%
          update_role(tid, new_role = "id variable") %>%
          step_downsample(target, under_ratio = downsample_ratio) %>%
          step_smote(target, over_ratio = smote_ratio , neighbors = 2, skip = TRUE)

        
        
        randf_spec <- rand_forest(mtry = mtry, min_n = min_n, trees = 151) %>% 
          set_mode("classification") %>%
          set_engine("ranger", importance = "permutation", verbose = FALSE) 
        
        pem_workflow <- workflow() %>%
          add_recipe(null_recipe) %>%
          add_model(randf_spec)
        
        #######################################################
        PEM_rf1 <- fit(pem_workflow, BGC_train)
        
        #final_fit <- pull_workflow_fit(PEM_rf1) # %>%pull(.predictions)
        final_fit <- extract_fit_parsnip(PEM_rf1)
        
        oob  <- round(PEM_rf1$fit$fit$fit$prediction.error, 3)
        
        ######### Predict Test
        #test_target <- as.data.frame(BGC_test$target) %>% rename(target = 1)
        test_target <- BGC_test_all %>% dplyr::select(id, target, target2)
        
        test.pred <-  predict(PEM_rf1, BGC_test)
        test.pred <- cbind(test_target, test.pred) %>% 
          mutate_if(is.character, as.factor) %>% droplevels()
        # levels(train.pred$target)
       
        ###______________________ 
        ## for a test convert all predicted non-for groups into a single nonfor unit for accuracy assessment
        
        test.pred <- test.pred %>%  mutate_if(is.factor, as.character)# %>% 
        # mutate(across(c(target, target2, .pred_class),dplyr::recode("X" %in% "nonfor", "A" = "nonfor", "W_t" = "nonfor", 
        #                                                         "W" = "nonfor", "Sc" = "nonfor", "R" = "nonfor", "F" = "nonfor", 
        #                                                         "Non_veg" = "nonfor","Wat" = "nonfor", "Wb" = "nonfor")))
        # 
        convert <- c("X", "A", "W_t", "W", "Sc", "R", "F", "Non_veg","Wat", "Wb")
        
        test.pred <- test.pred %>% mutate(target = ifelse(target %in% convert, "nonfor", target),
                                          target2 = ifelse(target2 %in% convert, "nonfor", target2) ,
                                          .pred_class = ifelse(.pred_class  %in% convert, "nonfor", .pred_class )) 
        
        test.pred <- test.pred %>%  mutate_if(is.character, as.factor)
      ###_______________________________________________________
       
       ###harmonize factor levels
        targ.lev <- levels(test.pred$target)
        targ2.lev <- levels(test.pred$target2)
        pred.lev <- levels(test.pred$.pred_class)
        levs <- c(targ.lev,targ2.lev, pred.lev) %>% unique()
        test.pred$target <- factor(test.pred$target, levels = levs)
        test.pred$target2 <- factor(test.pred$target2, levels = levs)
        test.pred$.pred_class <- factor(test.pred$.pred_class, levels = levs)
        # output test predictions
        
        test.pred.out <- test.pred %>% mutate(slice = k)
        acc.compare <- acc_metrix(test.pred) %>%
          mutate(slice = k,
                 transect_no = BGC_test_transect_no,
                 acc_type = "test_estimate", 
                 oob = oob, 
                 balance = balance_name,
                 bgc = bgc.label)
        ##use function to add in theta  
        # acc.theta <- theta_accuracy(acc.compare, theta = 0.5)
        # acc.compare <- cbind(acc.compare,acc.theta) %>% dplyr::select(unique(colnames(.)))
        acc.compare <- acc.compare  %>% 
          dplyr::select(bgc, slice, target,balance, trans.sum, trans.tot, pred.tot, acc, kap, everything() )
        return(list(acc.compare))
      }
      
      # extract results from sresults
      acc_results <- lapply(sresults, function(x) x[[1]])
      acc <- as.data.frame(rbindlist(acc_results))
      
      write.csv(acc, file = paste(outDir, paste0("acc_", balance_name,".csv"),sep = "/"))
      
      
    } # end of smote iteration
    
  } # end of downsample iteration 
  
}
 #extradat = extra; extrarun = TRUE 
#trDat = trDat_all



