regress_data <- function(data, sublist) {
    # function that will take imgdata, symptom data (and associated hierarchical clustering files)
    # and return fulldata, the network features and the clinical features that are needed for regression

####### load in data ###
    netvars <- data[['netvars']]
    sympvars <- data[['sympvars']]
    symp.clusters <- data[['symp.clusters']]
    
    demovars <- data[['demovars']]
    motionvars <- data[['motionvars']]
    # merge to metavars
    metavars <- merge(demovars, motionvars, by.x='sub', by.y='sub')
    
    # filter out subjects not in sublist
    sympvars <- sympvars[sympvars$sub %in% sublist$sub,]
    netvars <- netvars[netvars$sub %in% sublist$sub,]
####### generate low-dimensional representation of symptoms ###
    # filter out HCs from sympvars
    sympvars.mdd_idx <- sympvars$sub %in% metavars$sub[metavars$group=='MDD']
    sympvars <- sympvars[sympvars.mdd_idx,]
    
    # loop over clusters
    symps <- names(symp.clusters)
    sympvars.mat <- matrix(nrow=nrow(sympvars),
                           ncol=length(symps),
                           dimnames=list(sympvars$sub, symps))
    for (symp in symps) {
        onitems <- symp.clusters[[symp]]
        sympvars.mat[,symp] <- rowMeans(scale(sympvars[,onitems])) # scale before averaging within cluster
        
    }
    
    sympvars.df <- cbind(sub=row.names(sympvars.mat), as.data.frame(sympvars.mat))
    
    # regress out age and sex from sympvars
    sympvars.df <- merge(sympvars.df, metavars, by.x='sub', by.y='sub')
    sympvars.res <- scale(lm(paste0('cbind(', paste(symps, collapse=', '), ') ~ age + sex'), data=sympvars.df)$residuals)
    
    # merge
    sympvars.df <- cbind(sympvars.df$sub, as.data.frame(sympvars.res))
    colnames(sympvars.df) <- c('sub', symps)
    
####### regress age, sex and motion from networking data ###
    imgvars.df <- netvars
    allnets <- colnames(imgvars.df)[-1]
    
    # regress out age, sex and motion from imgvars
    nsubs <- nrow(imgvars.df)
    imgvars.df <- merge(imgvars.df, metavars, by.x='sub', by.y='sub')
    if (nrow(imgvars.df) != nsubs) {
        stop('ERROR: metavars do not contain the full number of subjects')
    }
    imgvars.df[,allnets] <- 
        lm(paste0('cbind(', paste(allnets, collapse=', '), ') ~ age + sex + meanRelRMS'), data=imgvars.df)$residuals
####### merge ###
    # keep imgvars with group variable - for use in BBS
    netsdata <- imgvars.df[,c('sub', 'group', allnets)]
    # drop metavariables
    imgvars.df[,c('age', 'sex', 'group', 'meanRelRMS')] <- NULL
    
    # merge
    fulldata <- merge(sympvars.df, imgvars.df, by.x='sub', by.y='sub')

###### return ###
    return(list(fulldata=fulldata, netsdata=netsdata, allnets=allnets, symps=symps))
}
