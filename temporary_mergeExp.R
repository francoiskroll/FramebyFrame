# mergeRAWs
tic()

ffpaths <- c('~/Dropbox/FbyFdemo/220531_14_RAWs.csv',
             '~/Dropbox/ZFAD/210907_PSEN2/210907_13_RAWsadjustedv2.csv')

ffs <- lapply(ffpaths, function(pth) {
  importRAWs(pth)
})

# if parallel boxes, first zhrs same
# but can't assume that
# if two different experiments:
# different start time
# different total duration

# 1-- take latest start time zhrs
# 2-- take earliest end time
# will still be different number of frames... I do not think we can just align them
# best would to match as close as possible the zhrs? but would not want to break bouts either...
# maybe just align start and stop and report on discrepancy

### what is the latest start zhrs? ###
# this is the start of each experiment:
staz <- unlist(lapply(ffs, function(ff) {
  ff$zhrs[1]
}))
# latest start is:
sta <- max(staz)
cat('\t \t \t \t >>> Latest START is',  sta,'hours after zt0 \n')

### what is the earliest stop zhrs? ###
# this is the stop of each experiment:
stoz <- unlist(lapply(ffs, function(ff) {
  ff$zhrs[nrow(ff)]
}))
# earliest stop is:
sto <- min(stoz)
cat('\t \t \t \t >>> Earliest STOP is',  sto,'hours after zt0 \n')

### make every experiment start and stop at the same time ###
cat('\t \t \t \t >>> To make every experiment start and stop at the same Zeitgeber time: \n')
ffs <- lapply(1:length(ffs), function(f) {
  nrowbf <- nrow(ffs[[f]])
  ffc <- subset(ffs[[f]], zhrs>=sta & zhrs<=sto)
  nrowaf <- nrow(ffc)
  cat('\t \t \t \t \t trimmed', nrowbf-nrowaf,'frames from experiment', f,'\n')
  return(ffc)
})

# there may still be a small discrepancy in number of frames
nros <- unlist(lapply(ffs, nrow))
cat('\t \t \t \t >>> After trimming, maximum difference in number of frames between two experiments is **' , max(abs(diff(nros))) , '** frames \n')

# there is no good solution re how to remove these surplus frames
# any ways of doing it will shift slightly one experiment compared to another
# will remove that many frames at the end (this is arbitrary, we do not know where the "surplus frames" are)
# so, make sure every experiment has the same number of frames as the experiment with the minimum number of frames:

cat('\t \t \t \t >>> To make every experiment the same number of frames: \n')
ffs <- lapply(1:length(ffs), function(f) {
  nrowbf <- nrow(ffs[[f]])
  ffc <- ffs[[f]][1:min(nros)]
  nrowaf <- nrow(ffc)
  cat('\t \t \t \t \t removed last', nrowbf-nrowaf,'frames of experiment', f,'\n')
  return(ffc)
})

cat('\t \t \t \t >>> Stitching the experiments together \n')
# get the zhrs column from each experiment
zhcols <- as.data.frame(sapply(ffs, function(ff) {
  ff$zhrs
}))
cat('\t \t \t \t >>> Worst time difference between two frames aligned together is',
    round(max(as.numeric((apply(zhcols, 1, diff))))*60*60, 2), 'seconds \n')

# delete the timestamps columns from every experiment except the first
ffs[2:length(ffs)] <- lapply(ffs[2:length(ffs)], function(ff) {
  # where is the first column of actual data?
  firstf <- which(grepl("^f+[[:digit:]]", colnames(ff)))[1]
  # drop everything before that column
  return(ff[,firstf:ncol(ff)])
})

# now ready to actually stitch together
ffm <- dplyr::bind_cols(ffs)

# done
# but we will need to worry about fX
# e.g. if merge two experiments from box1, we will have f1 twice

return(ffm)
