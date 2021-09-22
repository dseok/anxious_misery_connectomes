#!/bin/env Rscript

# clean up CCA results

args <- commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
	stop('Provide an output name')
} else {
	outname <- args[1]
	outdir <- file.path('../outputs', outname)
}

outdata.path <- file.path(outdir, paste0(outname, '_output.rds'))
onfiles.all <- list.files(outdir, full.names=TRUE)[grepl('split[1-9]', list.files(outdir))]
onfiles.params <- grep('params.rds$', onfiles.all, value=TRUE)
onfiles <- onfiles.all[! onfiles.all %in% onfiles.params]
# filter to only include completed sets of params
onfiles.params <- gsub('.rds$', '_params.rds', onfiles)

if (!file.exists(onfiles[1])) {
	stop('No new files')
}

# load output
if (file.exists(outdata.path)) {
	outdata <- readRDS(outdata.path)
	print(paste('nrows of original:', nrow(outdata)))
} else {
	outdata <- data.frame()
}

# loop over onfiles
for (file in onfiles) {
	ondata <- readRDS(file)
	print(file)
	outdata <- rbind(outdata, ondata)
	print(paste('nrows of new file:', nrow(outdata)))
}

outdata <- outdata[!duplicated(outdata),]

saveRDS(outdata, outdata.path)

# delete onfiles
sapply(onfiles, file.remove)
sapply(onfiles.params, file.remove)
