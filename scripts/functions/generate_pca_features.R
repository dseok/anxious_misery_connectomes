generate_pca_features <- function(prepped_data, sublist, paramset, symp, data, idx) {
    # perform PCA across all links and subjects. Use netsdata, which uses HC and MDD data
    
    # determine if you should subset features
    if (paramset$feature_selection_method_pca != 'none') {
        # temporarily change feature selection method to the one designated for pca
        paramset$feature_selection_method <- paramset$feature_selection_method_pca
        paramset$feature_selection_nsubs <- paramset$feature_selection_nsubs_pca
        paramset$nfeatures <- paramset$nfeatures_pca
        
        # select features
        features <- select_features(prepped_data, symp, paramset, idx=idx)
        
        # subset prepped_data
        prepped_data$allnets <- features
        prepped_data$fulldata <- prepped_data$fulldata[,c('sub', prepped_data$symps, prepped_data$allnets)]
        prepped_data$netsdata <- prepped_data$netsdata[,c('sub', 'group', prepped_data$allnets)]
    }
    
    # filter by the training data in sublist
    if (paramset$use_hcs_pca==1) {
        trainsubs <- sublist$sub[sublist$split %in% c('training', 'healthy')]
    } else if (paramset$use_hcs_pca==0) {
        trainsubs <- sublist$sub[sublist$split=='training']
    }
    imgvars.mat <- as.matrix(prepped_data$netsdata[,prepped_data$allnets])
    row.names(imgvars.mat) <- prepped_data$netsdata$sub
    
    imgvars.mat.train <- imgvars.mat[row.names(imgvars.mat) %in% trainsubs,]
    imgvars.mat.test <- imgvars.mat[! row.names(imgvars.mat) %in% trainsubs,]
    
    # perform PCA on training subs
    set.seed(1)
    pca.out <- prcomp(imgvars.mat.train, scale=TRUE)
    
    # convert imgvars.mat to rotated solution
    pcavars.mat <- imgvars.mat %*% pca.out$rotation
    pcavars.mat <- scale(pcavars.mat)
    pcavars.df <- cbind(sub=row.names(pcavars.mat), as.data.frame(pcavars.mat))
    rownames(pcavars.df) <- NULL
    
    # merge into fulldata and netsdata
    fulldata <- prepped_data$fulldata
    netsdata <- prepped_data$netsdata
    allnets <- prepped_data$allnets
    symps <- prepped_data$symps
    
    fulldata <- fulldata[,c('sub', symps)]
    netsdata <- netsdata[,c('sub', 'group')]
    
    fulldata <- merge(fulldata, pcavars.df, by.x='sub', by.y='sub')
    netsdata <- merge(netsdata, pcavars.df, by.x='sub', by.y='sub')
    allnets <- colnames(pcavars.df)[-1]
    
    # return
    prepped_data.out <- list(fulldata=fulldata, netsdata=netsdata, allnets=allnets, symps=symps)
}
