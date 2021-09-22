#!/bin/env Rscript

# Runs stalled jobs (due to conda error)

args <- commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
    stop('Provide an output name')
} else {
    outname <- args[1]
    outdir <- file.path('../outputs', outname)
}

# check that stalled files do exist
results.files <- grep('[0-9].rds', list.files(outdir, full.names=TRUE), value=TRUE)
params.files <- grep('params.rds', list.files(outdir, full.names=TRUE), value=TRUE)
params_roots.files <- gsub('_params', '', params.files)
params.files <- params.files[!params_roots.files %in% results.files]
if (length(params.files)==0) {
    stop('All files have completed')
}

# create logdir
logdir <- file.path('../logs', paste0(outname, '_', format(Sys.time(), format='%y%m%d_%H%M%S'), 'stalled'))
dir.create(logdir)

# run all stalled jobs
for (params.file in params.files) {
    splitname <- gsub('_params', '', basename(params.file))

    # create scriptfile
    scriptfile <- paste0(splitname, '.sh')

    # random sleep to help with simultaneous calls of conda environment
    system(paste0('printf \'sleep %s\n\' $(echo "scale=4; $RANDOM/1000" | bc ) > ', scriptfile))
    system(paste0('echo "source activate r_env" >> ', scriptfile))

    # append command
    output.path <- file.path(outdir, paste0(splitname, '.rds'))
    system(paste0('echo "./run_models.R --params.path=', params.file, '" >> ', scriptfile))

    # submit
    system(paste0('qsub -j y -cwd -o ', file.path(logdir, paste0(scriptfile, '_\\$JOB_ID.o')), ' -l h_vmem=20.0G,s_vmem=20.0G ', scriptfile))
    system(paste0('rm ', scriptfile))
}
