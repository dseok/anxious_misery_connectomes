#!/bin/env Rscript

# submit run_models.R jobs to cluster

rm(list=ls())
data_root <- '..'

# if there is a pre-set of parameters, use that. Otherwise, generate parameters dataframe
preset_params.path <- NA # file.path(data_root, 'paramsets', NA)

if (is.na(preset_params.path)) {
    # define i/o parameters
    output.name <- 'bbs_savefeaturespredictions'

    # define modeling params
    params <- do.call(expand.grid, list(nfeatures=190,
                                            # c(seq(20, 180, by=20), seq(200, 800, by=50), seq(1000, by=200, 38400), 38503), # nfeatures grid
                                            # c(140, 4600, 500, 1200, 3000), # optimal nfeatures for edgewise regression
                                            # 190 # nfeatures for BBS
                                        nfolds=5,
                                        feature_selection_method='none', 
                                        
                                        feature_selection_method_pca='none',
                                        nfeatures_pca='38503',
                                            # c(seq(20, 180, by=20), seq(200, 800, by=50), seq(1000, by=200, 38400), 38503), # nfeatures grid
                                            # 38503 # include all edges in PCA calculation
                                        use_hcs_pca=1,
                                        
                                        regressor='svr_linear',
                                        runmode='bbs',
                                            # 'bbs' # 'edgewise'
                                        sim=0, # 0 to run unpermuted dataset, > 1 to run any number of permutations
                                        save_features_predictions=TRUE))
    param.names <- colnames(params)
    
    # add data parameters (except for output.path, which is assigned below)
    params <- cbind(params, input.path=file.path(data_root, 'data/data_raw.rds'))
    params <- cbind(params, sublist.path=file.path(data_root, 'data/sublist.csv'))
    params <- cbind(params, functions.path=file.path(data_root, 'scripts/functions'))
    
    # save
    saveRDS(params, file.path(data_root, 'paramsets', paste0(output.name, '_params.rds')))
    stop('Paramset saved, please set desired paramset')
}

# set runtime options
njobs <- 500

# directories to assign at runtime
output.dir <- file.path(data_root, 'outputs', output.name )
parentlogdir <- file.path(data_root, 'logs')

# check that current splits don't exist
if (any(grepl('_params.rds', list.files(output.dir)))) {
    stop('There are unconsolidated files. Consolidate and re-run')
}

# prepare output
if (!dir.exists(output.dir)) {
    dir.create(output.dir)
}
logdir <- file.path(parentlogdir, paste0(output.name, '_', format(Sys.time(), format='%y%m%d_%H%M%S')))
dir.create(logdir)

# prepare jobs
jobs.l <- split(1:nrow(params), cut(1:nrow(params), njobs, labels = FALSE)) 

# loop over jobs
for (job in 1:length(jobs.l)) {
    outname <- paste0(output.name, '_split', job)
    output.path <- file.path(output.dir, paste0(outname, '.rds'))

    # save hyperparameters
    onparams <- params[jobs.l[[job]], param.names]
    # add output options
    onparams <- cbind(onparams, output.path)
    # add rownames and save
    rownames(onparams) <- 1:nrow(onparams)
    params.path <- file.path(output.dir, paste0(outname, '_params.rds'))
    saveRDS(onparams, params.path)
    
    # create script file
    scriptfile <- paste0(outname, '.sh')
    # random sleep to help with simultaneous calls of conda environment
    system(paste0('printf \'sleep %s\n\' $(echo "scale=4; $RANDOM/1000" | bc ) > ', scriptfile))
    system(paste0('echo "source activate r_env" >> ', scriptfile))
    
    # append command
    output.path <- file.path(output.dir, paste0(outname, '.rds'))
    system(paste0('echo "./run_models.R --params.path=', params.path, '" >> ', scriptfile))
    
    # submit
    system(paste0('qsub -j y -cwd -o ', file.path(logdir, paste0(scriptfile, '_\\$JOB_ID.o')), ' -l s_vmem=10.0G,h_vmem=10.0G ', scriptfile))
    system(paste0('rm ', scriptfile))
}
