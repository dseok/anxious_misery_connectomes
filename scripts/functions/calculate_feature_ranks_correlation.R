calculate_feature_ranks_correlation <- function(fulldata, allnets, symp, method) {
    # calculates ranks for features, sorted by the correlation of each net with the symp
    
    # calculate correlations 
    cors <- cor(fulldata[,symp], fulldata[,allnets], method=as.character(method))[1,]
    cors.abs <- abs(cors)
    
    # sort and return rank
    ranks <- sort(cors.abs, decreasing=TRUE)
    return(ranks)
}
