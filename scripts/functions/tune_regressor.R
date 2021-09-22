tune_regressor <- function(prepped_data, symp, paramset, idx, sk, features.outer, 
                           prepped_data.orig=NA, sublist=NA, data=NA) {
    # given a paramset and prepped_data, return a tuned regressor with metrics about its cross validated performance
    # this version of the script will perform CV using caret and will maximize test.cor
    
    # create splits
    splits <- c()
    set.seed(7225) # set seed
    splits <- c(splits, list(createFolds(idx, k=paramset$nfolds)))
    
    # parse regressor type
    regressor.name <- paramset$regressor
    if (regressor.name == 'linear') { 
        regressor <- sk$linear_model$LinearRegression(normalize=FALSE)
        params <- NA
        
    } else if (regressor.name == 'ridge') {
        regressor <- sk$linear_model$Ridge(normalize=FALSE)
        params <- list(alpha=c(0.001, 0.01, 0.1, 0.5, 1, 2, 5, 10))
        
    } else if (regressor.name=='lasso') {
        regressor <- sk$linear_model$Lasso(normalize=FALSE)
        params <- list(alpha=c(0.001, 0.01, 0.1, 0.5, 1, 2, 5, 10))
        
    } else if (regressor.name=='elastic') {
        regressor <- sk$linear_model$ElasticNet(normalize=FALSE, max_iter=10000L)
        params <- list(l1_ratio=c(0.0001, seq(0.1, 0.7, by=0.1), seq(0.75, 0.95, by=0.05), seq(0.96, 1, by=0.01)),
                       alpha=c(0.001, 0.01, 0.1, 0.5, 1, 2, 5, 10))
        
    } else if (regressor.name=='svr_linear') {
        regressor <- sk$svm$SVR(kernel='linear', gamma='scale')
        params <- list(C=c(0.0001, 0.001, 0.01, 0.1, 1, 10),
                       epsilon=c(0.001, 0.01, 0.1, 1, 2, 5, 10))
        
    } else if (regressor.name=='svr_poly') {
        regressor <- sk$svm$SVR(kernel='poly', gamma='scale')
        params <- list(C=c(0.0001, 0.001, 0.01, 0.1, 1, 10),
                       epsilon=c(0.001, 0.01, 0.1, 1, 2, 5, 10))
        
    } else if (regressor.name=='svr_rbf') {
        regressor <- sk$svm$SVR(kernel='rbf', gamma='scale')
        params <- list(C=c(0.0001, 0.001, 0.01, 0.1, 1, 10),
                       epsilon=c(0.001, 0.01, 0.1, 1, 2, 5, 10))
        
    } else {
        stop(paste(regressor.name, 'has not been implemented'))
    }

    # loop over parameters, select the best based on correlation
    params.grid <- expand.grid(params)
    cors <- rep(NA, nrow(params.grid))
    cors_folds <- matrix(nrow=length(splits[[1]]), ncol=nrow(params.grid))
    for (n in 1:nrow(params.grid)) {
        if (!is.na(params)[1]) {
            # set parameters
            for (param in colnames(params.grid)) {
                regressor[[param]] <- params.grid[n,param]
            }
        }
        
        for (split in splits) {
            cors.inner <- c()
            y.scaled <- c()
            y_hat.scaled <- c()
            for (fold in 1:length(split)) {
                train.idx <- unlist(split[-fold])
                test.idx <- split[[fold]]
                
                # select features
                # if this is a PCA run, you will have to re-run PCA on a reduced number of features
                if (paramset$runmode == 'bbs') {
                    print('Regenerating PCA features in inner train data')
                    prepped_data <- generate_pca_features(prepped_data=prepped_data.orig, sublist=sublist, 
                                                          paramset=paramset, symp=symp, data=data, idx=train.idx)
                }
                if (paramset$feature_selection_method == 'none') {
                    features <- prepped_data$allnets
                } else {
                    features <- select_features(prepped_data=prepped_data, symp=symp, paramset=paramset, idx=train.idx)
                }
                
                # create X and y
                X <- as.matrix(prepped_data$fulldata[idx, features])
                y <- c(prepped_data$fulldata[idx, symp])
                
                fit.out <- fit_predict_classifier(regressor=regressor, X=X, y=y, train.idx=train.idx, test.idx=test.idx)
                y.scaled <- c(y.scaled, fit.out$y.test)
                y_hat.scaled <- c(y_hat.scaled, fit.out$y_hat.test)

                # TMP - calculate inner correlation for cors_folds - also assumes that there is only one split
                cors_folds[fold,n] <- cor(fit.out$y.test, fit.out$y_hat.test, method='pearson')
            }
            # calculate correlation
            cors.inner <- c(cors.inner, cor(y.scaled, y_hat.scaled, method='pearson'))
        }
        
        # take mean of correlation
        cors[n] <- mean(cors.inner)
    }
    
    # take the parameters with the highest correlation
    cor.best <- max(cors)
    cors.folds.best <- paste(cors_folds[,cors==cor.best], collapse=',')
    params_best <- params.grid[cors==cor.best,]
    if (ncol(params.grid)==1) {
        names(params_best) <- colnames(params.grid)
        params_best <- as.data.frame(t(params_best))
    }
    if (nrow(params_best) > 1) {
        # if there is a tie, select a random set of params
        params_best <- params_best[sample(nrow(params_best), 1),]
    }
    
    if (!is.na(params)[1]) {
        for (param in colnames(params.grid)) {
            regressor[[param]] <- params_best[,param]
        }
    }
    
    metrics <- data.frame(cor.cv=cor.best, cor.cv.folds=cors.folds.best)

    return(list(regressor=regressor, metrics=metrics, params=params_best))
}