select_features <- function(prepped_data, symp, paramset, idx) {
    
    # parse args
    fulldata <- prepped_data$fulldata
    netsdata <- prepped_data$netsdata
    allnets <- prepped_data$allnets
    
    # filter out subjects
    fulldata <- fulldata[idx,]
    netsdata <- netsdata[netsdata$group=='HC' | netsdata$sub %in% fulldata$sub,] # I always keep the full number of HCs
 
    if (paramset$feature_selection_method == 'none') {
        return(allnets)
    } else if (paramset$feature_selection_method %in% c('pearson', 'spearman')) {
        ranks <- calculate_feature_ranks_correlation(fulldata=fulldata, allnets=allnets, symp=symp, 
                                                     method=paramset$feature_selection_method)
    } else {
        stop(paste(paramset$feature_selection_method, 'has not been implemented'))
    }
    
    nets <- names(ranks)[1:paramset$nfeatures]
    return(nets)
}
