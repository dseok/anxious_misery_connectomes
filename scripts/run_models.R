#!/usr/bin/env Rscript

# Run models and save
rm(list=ls())

##### (1) setup #####
# load packages
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(glmnet)))
suppressMessages(suppressWarnings(library(caret)))
suppressMessages(suppressWarnings(library(data.table)))
suppressMessages(suppressWarnings(library(reticulate)))
suppressMessages(suppressWarnings(library(pROC)))
suppressMessages(suppressWarnings(library(lme4)))

# parse options
option_list <- list(
    # hyperparameters
    make_option(c("--params.path"), action="store", default=NA, type='character',
                help="Path to parameter dataframe")
)
opt <- parse_args(OptionParser(option_list=option_list))

# hyperparameters
params.df <- readRDS(as.character(opt$params.path))

# dataset options
functions.path <- as.character(params.df$functions.path[1])
output.path <- as.character(params.df$output.path[1])
data <- readRDS(as.character(params.df$input.path[1]))

# source functions
for (i in list.files(functions.path, full.names=TRUE)) {
    source(i)
}
# function to return permuted data
return_permuted_data <- function(prepped_data, sim) {
    set.seed(sim)
    prepped_data.sim <- prepped_data
    # permute symp rows
    imgdata <- prepped_data.sim$fulldata[,prepped_data.sim$allnets]
    sympdata <- prepped_data.sim$fulldata[,prepped_data.sim$symps]
    sympdata <- sympdata[sample(1:nrow(sympdata), nrow(sympdata)),]
    
    # recreate fulldata
    prepped_data.sim$fulldata <- cbind(sub=prepped_data.sim$fulldata$sub, sympdata, imgdata)
    return(prepped_data.sim)
}

print(paste('Processing', nrow(params.df), 'hyperparameter combinations'))
print(params.df)
outdata <- data.frame()

for (paramset in 1:nrow(params.df)) {
    print(paste('Processing paramset', paramset))
    
    # read sublist
    sublist <- read.csv(as.character(params.df$sublist.path[paramset]), stringsAsFactors=FALSE)
    
    # save time by not re-regressing out nuisance covariates if you've already performed cleaning
    if (!exists('prepped_data') || params.df$runmode[paramset-1] != params.df$runmode[paramset]) {
        # regress data
        print('Prepping data and regressing out covariates ...')
        prepped_data <- regress_data(data, sublist)
    }

    # import sk
    sk <- import('sklearn')
    
    # attempt model
    tryCatch({
        sim <- params.df$sim[paramset]
        if (sim==0) {
            # perform unpermuted analysis
            prepped_data.orig <- prepped_data
        } else {
            prepped_data.orig <- return_permuted_data(prepped_data, sim)
        }
        
        outdata <- rbind(outdata, run_splitsample(prepped_data.orig=prepped_data.orig, data=data,
                                                  paramset=params.df[paramset,],
                                                  sublist=sublist,
                                                  sk=sk))
    }, error = function(e) {
        print(e)
        print(paste('Skipping paramset', paramset))
    })
    stop()
}

if (nrow(outdata)==0) {
    stop('No hyperparameter sets successfully completed.')
}

##### (6) append info rows #####
outdata <- cbind(outdata, opt)

saveRDS(outdata, output.path)
print('Completed and saved')
