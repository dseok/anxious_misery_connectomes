fit_predict_classifier <- function(regressor, X, y, train.idx, test.idx) {
    # fit classifier and return metrics on testing
    
    if (length(unique(y))!=2) {
        # scale X and y
        X.train <- as.matrix(scale(X[train.idx,]))
        X_scaler <- list(center=attributes(X.train)$`scaled:center`,
                         scale=attributes(X.train)$`scaled:scale`)
        # scale X.test
        X.test <- as.matrix(X[test.idx,])
        # make sure X.test is oriented properly
        if (length(test.idx)==1 && ncol(X.test)==1) {
            X.test <- t(X.test)
        }
        for (feature in 1:ncol(X.test)) {
            X.test[,feature] <- scale(X.test[,feature],
                                      center=X_scaler$center[feature],
                                      scale=X_scaler$scale[feature])
        }
        
        y.train <- scale(y[train.idx])
        y_scaler <- list(center=attributes(y.train)$`scaled:center`,
                         scale=attributes(y.train)$`scaled:scale`)
        # scale y.test
        y.test <- y[test.idx]
        y.test <- scale(y.test, center=y_scaler$center, scale=y_scaler$scale)
        
        # vectorize y
        y.train <- c(y.train)
        y.test <- c(y.test)
    } else {
        X.train <- X[train.idx,]
        X.test <- as.matrix(X[test.idx,])
        # make sure X.test is oriented correctly
        if (length(test.idx)==1 && ncol(X.test)==1) {
            X.test <- t(X.test)
        }
        y.train <- y[train.idx]
        y.test <- y[test.idx]
    }
    
    # fit
    fit <- regressor$fit(as.matrix(X.train), y.train)
    y_hat.train <- fit$predict(X.train)
    if (length(test.idx) > 0) {
        y_hat.test <- fit$predict(X.test)
    } else {
        y.test <- NA
        y_hat.test <- NA
    }

    if (length(dim(fit$coef_))==1) {
        outcoef <- fit$coef_
    } else {
        outcoef <- fit$coef_[1,]
    }
    
    return(list(y.train=y.train, y_hat.train=y_hat.train,
                y.test=y.test, y_hat.test=y_hat.test,
                coef=outcoef))
}
