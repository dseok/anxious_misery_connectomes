run_splitsample <- function(prepped_data.orig, data, paramset, sublist, sk) {
    # run split sample validation, with regressor tuning inside
    
    # this version will fit hyperparameters for the training set and test on the testing set
    
    # identify idx
    train.idx <- which(prepped_data$fulldata$sub %in% sublist$sub[sublist$split=='training'])
    test.idx <- which(prepped_data$fulldata$sub %in% sublist$sub[sublist$split=='testing'])
    
    output <- c()
    for (symp in prepped_data$symps) {
        print(symp)
        
        if (paramset$runmode == 'bbs') {
            print('Generating PCA features on training data')
            prepped_data <- generate_pca_features(prepped_data=prepped_data.orig, sublist=sublist, 
                                                  paramset=paramset, symp=symp, data=data, idx=train.idx)
        } else {
            prepped_data <- prepped_data.orig
        }
        
        # select features
        if (paramset$feature_selection_method == 'none') {
            features <- prepped_data$allnets
        } else {
            features <- select_features(prepped_data=prepped_data, symp=symp, paramset=paramset, idx=train.idx)
        }
        
        # hyperparameter tuning - CV
        tune.output <- tune_regressor(prepped_data=prepped_data, symp=symp, paramset=paramset,
                                      idx=train.idx, sk=sk, features.outer=features, 
                                      prepped_data.orig=prepped_data.orig, sublist=sublist, data=data)
        
        # fit on training and predict
        y.scaled <- fit_predict_classifier(tune.output$regressor, 
                                           X=as.matrix(prepped_data$fulldata[,features]), 
                                           y=prepped_data$fulldata[,symp], 
                                           train.idx, test.idx)
        
        # calculate cor.train and cor.test
        cor.train <- cor(y.scaled$y.train, y.scaled$y_hat.train, method='pearson')
        cor.test <- cor(y.scaled$y.test, y.scaled$y_hat.test, method='pearson')
        cor.test.inner <- tune.output$metrics$cor.cv
        # save CV results from all folds
        cor.test.inner.all <- tune.output$metrics$cor.cv.folds

        if (paramset$save_features_predictions) {
            y_actual.train <- paste(y.scaled$y.train, collapse=',')
            y_actual.test <- paste(y.scaled$y.test, collapse=',')
            y_hat.train <- paste(y.scaled$y_hat.train, collapse=',')
            y_hat.test <- paste(y.scaled$y_hat.test, collapse=',')
            
            features <- paste(features, collapse=',')
            
            coefs <- paste(y.scaled$coef, collapse=',')

            # add to output
            output <- rbind(output, 
                            data.frame(cor.test=cor.test, cor.train=cor.train, cor.test.inner=cor.test.inner, cor.test.inner.all=cor.test.inner.all, symp=symp,
                                       y_actual.train=y_actual.train, y_actual.test=y_actual.test,
                                       y_hat.train=y_hat.train, y_hat.test=y_hat.test,
                                       features=features, coefs=coefs))
            
        } else {
            output <- rbind(output, 
                            data.frame(cor.test=cor.test, cor.train=cor.train, cor.test.inner=cor.test.inner, cor.test.inner.all=cor.test.inner.all, symp=symp))
        }
    }
    
    # summarize and output
    for (param in colnames(paramset)) {
        output[,param] <- paramset[[param]]
    }
    return(output)
}
