### Frame-by-Frame package ###
### activity parameters function ###

# below are functions which compute activity parameters
# i.e. parameters describing the overall activity of each fish
# without having to distinguish each active bout, contrary to activeBoutParameters
# these functions run on frame-by-frame deltapx data for all time windows / all fish, stored in the list dn


# these parameters/functions are:

# percentageTimeActive(...)
# the % of time (% of the whole day or % of the whole night) that each fish spent active

# totalDeltaPx(...)
# the sum of all deltapx for each fish and each time window



# function activityPercentageTimeActive_onefish(...) ----------------------

# ffc = one column of frame-by-frame deltapx
# one column i.e. data for one fish during one window (e.g. day1)

#' Title
#'
#' @param ffc
#' @param zhc
#' @param bin_nsecs
#' @param fps
#'
#' @return
#'
#' @examples
activityPercentageTimeActive_onefish <- function(ffc,
                                                 zhc,
                                                 bin_nsecs,
                                                 fps) {

  # count number of active frames in ffc
  return( 100 * (sum(ffc > 0) / length(ffc)) )
  # above: ffc > 0 will return TRUE (frame was > 0 px) / FALSE (frame was 0 px)
  # sum() will count all the TRUE
  # divide by how many frames in this window to get proportion, multiply by 100 to get percentage

}



# function activityTotalPx_onefish(...) -----------------------------------

#' Title
#'
#' @param ffc
#' @param zhc
#' @param bin_nsecs
#' @param fps
#'
#' @return
#'
#' @examples
activityTotalPx_onefish <- function(ffc,
                                    zhc,
                                    bin_nsecs,
                                    fps) {

  return( sum(ffc)/1000000 )
  # we simply sum all the deltapx for this fish
  # and divide by 1 million to return results in millions of px

  # Note, no normalisation here, so do not compare night vs day directly as they are different durations
  # alternative would be e.g. mean total deltapx/hour but difficult to read

}



# function activitySunsetStartle_onefish(...) -----------------------------

# note, we do not look here whether we are given day or night data
# will simply calculate parameter on any ffc given
# activityParameter() then takes care of removing the day results

# SunsetStartle is currently defined as the maximum deltapx during the 3 seconds after the light transition

#' Title
#'
#' @param ffc
#' @param zhc
#' @param bin_nsecs
#' @param fps
#'
#' @return
#'
#' @examples
activitySunsetStartle_onefish <- function(ffc,
                                          zhc,
                                          bin_nsecs,
                                          fps) {

  # ***
  nsecs <- 3 # 3-second window after transition defined here
  # ***

  return( max(ffc[1:fps*nsecs]) )
  # fps*nsecs: e.g. 25 frames-per-second * 3 seconds = 75 frames
  # take the first that many frames from ffc, e.g. the first 3 seconds' worth of data
  # return the max deltapx during that period

}



# function activitySlope_onefish(...) -------------------------------------

# ffc = one column of frame-by-frame deltapx
# one column i.e. data for one fish during one window (e.g. day1)

#' Title
#'
#' @param ffc
#' @param zhc
#' @param bin_nsecs
#' @param fps
#'
#' @return
#'
#' @examples
activityRegression_onefish <- function(ffc,
                                       zhc,
                                       bin_nsecs,
                                       fps) {

  ### bin size in frames ###
  bin <- round(bin_nsecs * fps)


  ### shift zhc so starts at 0 ###
  # first zhc should be 0 as we are looking for the intercept at the light transition
  zhc <- zhc - zhc[1]
  # now we have hours after start of the window (+ 10 minute as we trimmed the start)


  ### delete the first 10 min of data ###
  # the issue is the startle response, especially at night
  # big increase in activity biases too much the regression
  # 10 min is somewhat arbitrary, but looking at data binned in 10 min (bin_nsecs=10*60), only the first bin is incredibly high
  # even if there is no startle response at day, probably better to apply to both
  trimn <- round(10*60*fps)
  ffc <- ffc[-(1:trimn)]
  zhc <- zhc[-(1:trimn)] # shifting zhc first allows to have zhc start at 0.167 here
  # so intercept will actually reflect intercept with 0 = start of the transition, not 0 = start of the transition + 10 min


  ### bin ffc ###
  # single frames are mostly 0 so regression is not particularly meaningful when calculated on frame-by-frame
  # trim a few frames at the start to have number of rows multiple of bin size
  # number of rows we need to remove is:
  nrows2remove <- length(ffc) %% bin

  # can be up to `bin_nsecs` of data, e.g. 10 min
  # so worried that if we trim from the start we might impact the regression
  # remove them at random from the data
  rowi2remove <- sample(1:length(ffc), nrows2remove)

  # if we need to remove some rows,
  if(length(rowi2remove)>0) {
    # remove these rows from ffc
    ffc <- ffc[-rowi2remove]

    # remove these rows from zhc
    zhc <- zhc[-rowi2remove]
  }

  # check this
  # remainder of division should be 0
  if(length(ffc) %% bin != 0) stop('\t \t \t \t Error: number of frames not a multiple of bin size even after trimming some \n')
  if(length(zhc) %% bin != 0) stop('\t \t \t \t Error: number of zhrs timepoints not a multiple of bin size even after trimming some')

  # now bin
  ffb <- colSums(matrix(ffc, nrow=bin))

  # make a small dataframe with zhrs
  zh2use <- seq(from=bin, to=length(ffc), by=bin) # we take the nth zhrs of each bin
  ffd <- as.data.frame(cbind(zhc[zh2use], ffb))
  colnames(ffd) <- c('zhrs', 'binpx')

  ### calculate regression ###
  bzfit <- lm(binpx ~ zhrs, data=ffd) # fit of bin px vs zhrs

  # bit barbaric, but will return intercept_slope as string so can be stored in cell of pa
  return(paste(as.character(bzfit[[1]][1]) , as.character(bzfit[[1]][2]), sep='_'))
}




# function activityFractalDim_onefish(...) --------------------------------

# ffc = one column of frame-by-frame deltapx
# one column i.e. data for one fish during one window (e.g. day1)

# ! zhc & bin_nsecs settings are not used, only to be consistent with other activity parameters
# to repeat (it is confusing): bin_nsecs here does not decide the binning window used before measuring fractal dimension

#' Title
#'
#' @param ffc
#' @param zhc
#' @param bin_nsecs
#' @param fps
#'
#' @return
#'
#' @examples
activityFractalDim_onefish <- function(ffc,
                                       zhc,
                                       bin_nsecs,
                                       fps) {

  # ! exception: in empty wells, frame-by-frame data might be all 0 or only a few positive values
  # in that case, return NA (in theory it is 1.0 if all same value, but not meaningful to calculate on empty wells anyways)
  # here, arbitrary threshold: if less than 20 positive values return NA
  # it seems to throw an error when calculating the fractal dimension on only a few positive values
  # maximum number of positive values while still throwing an error I have seen is 13
  if(length(which(ffc>0)) < 20) { return(NA) }

  # now normal run:

  fps <- round(fps) # as will use to count number of rows here

  ### step0: trim first 10 min of data ###
  # to avoid any strong effect of light transition, especially startle response at night
  trimn <- round(10*60*fps)
  ffc <- ffc[-(1:trimn)]

  ### step1: bin in 1-sec bins ###
  # remove surplus rows to get number of rows multiple of fps
  ntoomany <- length(ffc)%%fps
  if(ntoomany>0) {
    fcrop <- ffc[-(1:ntoomany)]
  } else {
    fcrop <- ffc
  }
  # check this
  if(length(fcrop)%%fps != 0) stop('\t \t \t \t >>> Error activityFractalDim_onefish: number of frames not a multiple of bin size, even after trimming \n')

  # now bin
  fb <- colSums(matrix(fcrop, nrow=fps)) # *** binning set to 1 second here ***

  ### step2: smooth in 1-min bins ###
  fs <- unlist(data.table::frollmean(fb, n=120, hasNA=TRUE, na.rm=TRUE)) # *** smoothing set to 1 minute here ***
  fs <- fs[-(1:120)]

  ### step3: measure fractal dimension ###
  return(as.numeric(fractaldim::fd.estimate(fs, methods='boxcount')$fd)) # *** method boxcount to measure fractal dimension ***
  # https://www.mssanz.org.au/MODSIM97/Vol%201/Breslin.pdf (really nice paper to read)
  # recommended 'variation' but made no sense when testing it; it was not reacting to binning or smoothing
  # boxcount was behaving as expecting, e.g. decreasing with increasing smoothing
}


