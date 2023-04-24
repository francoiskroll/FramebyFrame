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
#' @export
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
#' @export
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
#' @export
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
#' @export
#'
#' @examples
activityFractalDim_onefish <- function(ffc,
                                       zhc,
                                       bin_nsecs,
                                       fps) {

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

  # do this within a tryCatch because fd.estimate may return Error in 1:qv : NA/NaN argument
  # this happens when number of active frames is low but not 0
  # previous version of activityFractalDim_onefish had a "minimum number of active frames" threshold to return NA when below threshold
  # but it was difficult to find a threshold which was working for every situation
  # alternative solution is to simply return NA if fd.estimate could not run
  tryCatch(
    {
      # try to calculate & return fractal dim
      fdim <- as.numeric(fractaldim::fd.estimate(fs, methods='boxcount')$fd) # ***
      return(fdim)
    },
    # if an error occurs, return NA
    error=function(e) {
       # cat('\t \t \t \t \t could not calculate fractal dimension, returning NA \n')
      # message above interferes with fishy progress bar in activityParameter
      return(NA)
    }
  )

  # *** method boxcount to measure fractal dimension
  # https://www.mssanz.org.au/MODSIM97/Vol%201/Breslin.pdf (nice paper to read)
  # recommended 'variation' but made no sense when testing it; it was not reacting to binning or smoothing
  # boxcount was behaving as expecting, e.g. decreasing with increasing smoothing
}


# function activityCompressibility ----------------------------------------

#' Title
#'
#' @param ffc
#' @param zhc
#' @param bin_nsecs
#' @param fps
#'
#' @return
#' @export
#'
#' @examples
#'
activityCompressibility_onefish <- function(ffc,
                                            zhc,
                                            bin_nsecs,
                                            fps) {

  # turn ffc into a character vector
  # e.g. 0 0 5 into "0" "0" "5"
  # (easier to perform the compression on characters)
  ffch <- as.character(as.vector(ffc))

  # compress the vector using gzip
  fzip <- memCompress(from=ffch, type='gzip')

  # by how much did we compress it?
  # we can calculate how much smaller the compressed sequence is compared to the original sequence
  # e.g. 0.25 would mean the compressed sequence is only 25% the length of the original sequence (i.e. we compressed it 4x)
  # but this would mean a low number is more compressive, which may be counterintuitive?
  # calling this parameter "repetitiveness" does not change in the direction
  # alternative is to calculate 1 - this ratio, e.g. 0.75 would mean we reduced the length of the sequence by 75%
  # we will use this, I think makes more sense
  return ( 1 - (length(fzip) / length(ffch)) )
  # e.g. sequence is 100,000 frames and its length becomes 500 after compression
  # so compressed sequence is 0.005 (0.5%) of original
  # i.e. we reduced the sequence length by 99.5%
}



# function activitySunsetStartle(...) -------------------------------------

# read comments in activityParameter.R
# exception to the framework of calculating parameters by time window/by fish
# we want last n values of previous day/night and first n values of current day/night
# so better to calculate with all of dn as input

# note, we do not look here whether we are given day or night data
# will calculate the parameter on any ffc given
# activityParameter() then takes care of removing the day results

# SunsetStartle is currently defined as the maximum deltapx during the 3 seconds after the light transition

# dn: all frame-by-frame data splitted in a list by day/night

# win: window index given by the sapply in activityParameter

# fps: frames-per-second

# behaviourParameter gives us all ff data of one experiment and dn, i.e. ff split by day/night or woi

#' Title
#'
#' @param ff
#' @param dn
#'
#' @return
#' @export
#'
#' @examples
activitySunsetStartle <- function(ff,
                                  dn) {

  ### defining here how big of a window we consider
  # let us say 1 min before transition up to 1 min after transition
  nsecs <- 60
  ###

  # chunk of code below originally taken from activityParameter.R
  # logic is similar, we loop through time windows, and within each we loop through fish
  # but we need access to full data ff

  # (how many time columns?)
  # need this number below so we do not process the time columns as actual data
  timecols <- which(grepl("^f+[[:digit:]]", colnames(dn[[1]])))[1] - 1

  # ** FIRST sapply to loop through time window, typically night0, day1, ...
  pal <- sapply( 1:length(dn), function(win) {

    cat('\n \t \t \t \t >>>', toupper(names(dn)[win]), ' \n')

    ### average framerate of this window?
    fps <- averageFramerate(dn[[win]]$exsecs) # will print fps to Console too

    # therefore, how many frames should we look before/after transition to find the sunsetStartle?
    nfras <- nsecs*fps

    ### prepare a dataframe of ff data which is last n frames of previous time window up to first n frames of this time window
    # essentially as if we were zooming on the light transition in the big RAWs.csv file
    # to do this, we simply append together end of previous time window and start of current time window

    # we need to know start of this window in frame index
    # unfortunately do not have frame index as a column, but we can get the exsecs from dn and match to find the row in full ff
    staf <- which(ff$exsecs==dn[[win]]$exsecs[1]) # will give row (frame) index of first frame of current time window

    # now prepare dataframe (read above)
    # I think we can assume here we will always have a couple of minutes of data before this transition, even if first day/night or woi transition?
    ffwt <- as.data.frame( ff[ (staf-nfras) : (staf+nfras) , ] )
    # ffwt is for ff around window transition

    ### set-up a progress bar
    # how many fish are we about to calculate sunsetStartle for?
    nfis <- ncol(ffwt)-timecols # number of columns in the data, minus the time columns
    prg <- txtProgressBar(min=0, max=nfis, style=3, char='><> ')

    # sapply to loop through fish
    sapply((timecols+1):ncol(ffwt),
           function(fic) {
             # fic for fish column, will loop e.g. column #4, #5, ... (assuming first three columns are timestamps)

             # calculate the activity parameter for this fish
             # using 'one fish' version of the parameter function
             # we give it
             # ffc = frame-by-frame data for that time window/fish, ffc for frame-by-frame column
             # cat('\t \t \t \t \t >>> well', colnames(dnw)[fic], ' \n')
             # which fish are we at?
             setTxtProgressBar(prg, fic-timecols) # update progress bar
             # fic-timecols will give which fish we are at

             # now we simply return the maximum delta-pixel of the timecourse zoomed in on the window transition (ffwt created above)
             return( max(ffwt[,fic]) )
           })

  })
  # overarching (** first) sapply gives 'pal' directly as a dataframe columns = day/night rows = fish

  # make sure it is a dataframe
  pal <- as.data.frame(pal)

  # add column names as day/night or woi
  colnames(pal) <- names(dn)

  # put back fishid as first column
  # take them from column names of dn
  pal <- pal %>%
    add_column(fish=colnames(dn[[1]])[(timecols+1):ncol(dn[[1]])], .before=1)
  row.names(pal) <- NULL

  # ! activitySunsetStartle is only calculated on nights
  # it would have been more intuitive not to calculate it on day data
  # but easier to calculate parameter on every window, then switch day results to NA now
  cat('\t \t \t \t >>> activitySunsetStartle is only meaningful for nights, switching day results to NA... \n')

  cols2NA <- which(substr(colnames(pal), 1, 3)=='day')

  if(length(cols2NA)>0) {
    pal[,cols2NA] <- NA
  }

  # return pal
  # should have the same format as created by e.g. activityParameter
  return(pal)

}
