#!/bin/env Rscript

# Runs stalled jobs (due to conda error), based on an input list of splits

args <- commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
    stop('Provide an output name')
} else {
    outname <- args[1]
    splits.file <- args[2]
    outdir <- file.path('../outputs', outname)
    splits <- read.delim(splits.file, header=FALSE)
}

# make params.files and check that they exist
params.files <- paste0('../outputs/', outname, '/', outname, '_split', splits$V1, '_params.rds')
if (any(!sapply(params.files, file.exists))) {
    stop('Some files do not exist!')
}

# create logdir
logdir <- file.path('../logs', paste0(outname, '_', format(Sys.time(), format='%y%m%d_%H%M%S'), 'stalled_manual'))
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
    system(paste0('qsub -j y -cwd -o ', file.path(logdir, paste0(scriptfile, '_\\$JOB_ID.o')), ' -l h_vmem=10.0G,s_vmem=10.0G ', scriptfile))
    system(paste0('rm ', scriptfile))
}
