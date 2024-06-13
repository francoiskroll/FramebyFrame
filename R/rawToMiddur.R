#####################################################
# ~ FramebyFrame: conversion frame-by-frame data to middur data ~
#
#
# Francois Kroll 2023
# francois@kroll.be
#####################################################

# main use of this is to prepare middur file to be used in predPharma Shiny app

# was a standalone script, up to rawToMiddur_v4

# see howMiddur.R for the background
# this is where I figured out how Viewpoint's algorithm works exactly to calculate middur from frame-by-frame delta-pixel data


# function ffColtoMid -----------------------------------------------------
# function to convert one column of frame-by-frame delta-pixel data to middur

# contains the central algorithm for the conversion
# the algorithm works as:
# 1 minute = 60 seconds * 25 (typically) frames-per-second = 1500 frames
# from a count of 1500 frames, remove any frame that is
# below or equal to freezing threshold (typically 3); e.g. 1000 frames are between 0 and 3 delta-pixel, count is now 500 frames
# equal to or above burst threshold (typically 200); e.g. 10 frames are > 200, count is now 490
# each frame is 1/frames-per-second (typically 25), i.e. ~ 0.04 seconds
# so multiply number of frames by 0.04 seconds to get time spent above freezing/below burst

# Note; comparing Viewpoint's middur and the one calculated here on one example fish gives r = 0.9999749
# I cannot find where the last tiny discrepancy is, probably some rounding difference

#' Title
#'
#' @param ffcol
#' @param fps
#' @param freezing
#' @param burst
#'
#' @return
#'
#' @examples
ffColtoMid <- function(ffcol, fps, freezing, burst) {

  minsplits <- seq(0, nrow(ffcol), 60 * fps) # if fps = 25, then 0, 1500, 3000, etc
  # in theory splits should be same for every column in a RAWs, but it is fast so does not matter if repeat operation every column
  # in function rawtoMiddur, ensured that we have a number of rows multiple of 1500, so do not need to manually add last row to splits

  # preallocate midcol data
  midcol <- rep(NA, length(minsplits)-1)

  # now populate midcol

  # initiate a progress bar
  pro <- txtProgressBar(min=0, max=length(midcol), initial=0, style=3, width=50)

  for(mi in 1:(length(minsplits)-1)) {

    start <- minsplits[mi]+1 # so 1, 1501, 3001, etc.
    stop <- minsplits[mi+1] # so 1500, 3000, 6000, etc.

    # so chunks of data are
    # row 1 -- row 1500
    # row 1501 -- row 3000
    # row 3001 -- row 6000
    # etc.

    # key conversion algorithm, see explanations above

    midcol[mi] <-   round(
      (1/fps) *
        (nrow(ffcol[start:stop,]) -
           length(which(ffcol[start:stop,] <= freezing)) - # note here < or =
           length(which(ffcol[start:stop,] >= burst)))
      , 3) # note here > or =

    setTxtProgressBar(pro, mi) # advance progress bar
  }

  close(pro) # terminate the progress bar
  return(midcol)
}







# function rawToMiddur ----------------------------------------------------

# main function to convert a RAWs to middur data
# usage rawtoMiddur(ffpath, freezing=3, burst=200)
# ffpath = where to find frame-by-frame data
# freezing = Viewpoint tracking parameter, minimum delta-pixel to be considered an active frame
# default is 3 px
# burst = Viewpoint tracking parameter, maximum delta-pixel to be considered an active frame (above this is considered abnormally high)
# default is 200 px
# DATA = TRUE or FALSE, whether to export DATA file or not
# default is TRUE

# expects first column to be time (and named 'time'), then all columns after that to be frame-by-frame delta-pixel data (typically 1 + 96 columns)

# will output automatically to YYMMDD_BX_middurfromRaw.csv

# if necessary, will crop up to 1-minute of data at the beginning of the experiment to have number of frames divisible by 1500 (1-minute worth of frames)

# I checked first 20 values of one fish middur converted from RAW vs middur from DATA.txt
# (DATA.txt is from VpFormatter.R, which takes Zebralab file as input, so Viewpoint calculations of middur)
# values are virtually identical, only difference is decimal place or maybe shifted by a couple of frames

#' Title
#'
#' @param ffpath
#' @param freezing
#' @param burst
#' @param exportPath
#'
#' @return
#' @export
#'
#' @examples
rawToMiddur <- function(ffpath,
                        freezing=3,
                        burst=200,
                        exportOrNo=FALSE) {

  # import frame-by-frame data
  ff <- data.table::fread(ffpath)

  # calculate frame-rate
  fps <- round(1/mean(diff(ff$exsecs)))

  # sanity checks for user
  cat('\t \t \t \t Frame rate is ***', fps, '*** frames-per-second \n')
  cat('\t \t \t \t Experiment lasted ***', round(nrow(ff)/fps/60/60,0), '*** hours \n')

  # make sure number of frames is divisible by 1500 (1-minute worth of frames)
  # deleting a few frames at the end of the experiment if necessary

  # below; option to trim from start of the experiment
  # cat('\t \t \t \t Trimming first ***', round((nrow(ff) %% 1500)/fps,0), '*** seconds of data to make it divisible by 1500 \n')
  # ff <- ff[- (1:(nrow(ff) %% 1500)) ,]

  nrowstoomany <- nrow(ff) %% (fps*60) # fps*60 is number of frames in one minute

  # if there are some rows to trim
  if(nrowstoomany!=0) {
    cat('\t \t \t \t Trimming last ***', round(nrowstoomany/fps,0), '*** seconds of data to make experiment a round number of minutes \n')
    rows2del <- (nrow(ff) - (nrowstoomany-1)) : nrow(ff)
    ff <- ff[- rows2del ,]
  }

  # now the number of rows in ff is a multiple of 1500
  # check this
  if(nrow(ff)%%(fps*60) != 0) stop('\t \t \t \t >>> Error rawtoMiddur: number of rows not a multiple of 1500, even after trimming \n')

  # how many time columns do we have?
  timecols <- which(grepl("^f+[[:digit:]]", colnames(ff)))[1] - 1
  # above finds first column which is called fX (usually f1 or f97)

  # convert frame-by-frame data to middur
  mid <- data.table::data.table(sapply( (timecols+1) : ncol(ff),
                                        function(ci) { # ci = column index
                                          # message looks like >>> converting f3   (3/96)
                                          cat('\t >>> converting', colnames(ff)[ci], '  (', ci-timecols, '/', ncol(ff)-timecols, ') \n')
                                          ffColtoMid(ff[,..ci], fps, freezing, burst)
                                        }))

  # put back column names (they get lost above)
  colnames(mid) <- colnames(ff)[ (timecols+1) : ncol(ff)]

  # re timepoints: below first puts back time as seconds, then uses Zebralab XLS file to put time in number of hours after day0 9AM
  # first back seconds first is thus useless, but I will leave it if ever useful in the future

  # place back time columns
  # there are multiple options here
  # I think what makes most sense is to take the timepoint of the latest frame of each minute
  tsrows <- seq((fps*60), nrow(ff), (fps*60)) # gives the last row of each minute, so row 1500, 3000, etc. We will use the time of each of these frames
  # we have one timestamp extra, drop it

  if(length(tsrows) != nrow(mid))
    stop('\t \t \t \t >>> Error: ', length(tsrows), ' timestamps but ', nrow(mid), ' rows after middur conversion, something wrong. \n')


  ### v3 was using an older version of RAWs.csv format which only had time in seconds as time column
  # now we have the zth column integrated in RAWs.csv, so we do not have to worry about Zeitgeber times here
  # can just add back the time columns to middur data

  # for each minute bin, we take the time information of the last frame
  # e.g. first minute lasts from frame 1 to 1501, so we take information of frame 1501
  mid <- cbind(ff[tsrows, 1:timecols], mid)

  # conversion DONE
  # if need to add CLOCK column like DATA files from Jason, can check rawToMiddur_v3

  # export, if required
  if(exportOrNo) {
    # prepare file name
    # get the YYMMDD_BX component
    ymdb <- substr(basename(ffpath), 1, 9)
    # we want name to be YYMMDD_BX_middur.csv
    exportPath <- paste0(dirname(ffpath), whatSlash(ffpath), ymdb, '_middur.csv')

    cat('\t \t \t \t >>> Wrote', exportPath, '\n')

    write.csv(mid, file=exportPath, row.names=FALSE)
  }

  # return too, if need to use within R
  return(mid)

}
