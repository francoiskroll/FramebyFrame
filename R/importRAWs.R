###################################################
# ~~~ FramebyFrame package ~~~

# functions to import RAWs.csv

# Francois Kroll 2022
# francois@kroll.be
###################################################

# function averageFramerate(...) ------------------------------------------

#' Calculates average framerate
#' @description Calculates average framerate from a series of timestamps in seconds.
#'
#' @param tsecs Timestamps in seconds
#'
#' @return Average framerate
#' @export
#'
#' @examples averageFramerate(c(0, 0.04, 0.08))
#' ## returns 25 frames-per-second

averageFramerate <- function(tsecs) {

  fps <- 1/mean(diff(tsecs))
  cat('\t \t \t \t >>> Frame rate is ***', round(fps, 2), '*** frames-per-second \n')

  # return fps
  return(fps)
}



# function findLightTransitionFrame(...) ----------------------------------

# small function to find frame where a given light transition occured
# used in function importAddTimetoRAWs()

# zth = Zeitgeber durations (i.e. number of hours since day0 9AM), typically a column of frame-by-frame RAWs data
# transitionHour = transition to look for, usually e.g. 14 or 24 or 38 or 48, etc.

# returns the frame just before the light transition (usually = row number in frame-by-frame RAWs data)
# e.g. if looking for first sunset = 14 zth hours
# may find frame zth = 13.99999 (Case1)
# in that case it returns that frame
# or may find frame zth = 14.00001 or frame with exactly zth = 14 (Case2)
# in that case, assumes this frame is the frame just after the transition and it returns the frame just before

#' Finds light transition frame
#' @description Finds frame index at which a given light transition occurred.
#'
#' @param Zeitgeberdurations Series of timestamps in 'Zeitgeber durations', i.e. number of hours since (typically) 9 AM on day 0.
#' @param transitionHour Time in Zeitgeber duration of the light transition to search for.
#'
#' @return Frame index.
#'
#' @examples

findLightTransitionFrame <- function(Zeitgeberdurations, transitionHour) {

  tfra <- which.min(abs(Zeitgeberdurations - transitionHour)) # transition frame

  if (Zeitgeberdurations[tfra] >= transitionHour) { # this is Case2

    tfra <- tfra - 1

    # check we are now before the transition
    if (Zeitgeberdurations[tfra] >= transitionHour)
      stop('\t \t \t \t >>> Something unexpected: the frame preceding the closest frame to the transition is still after the transition \n')

    return(tfra)

  } else {
    return(tfra)
  }

}



# function importRAWs(...) ------------------------------------------------

# importRAWs is a simpler version of an old function called importAddTimetoRAWs
# adding the timestamps to the frame-by-frame data was moved to vpSorter
# this has the benefit of not asking the user constantly for the Zebralab XLS file

# importRAWs simply does a few checks and records the data in Environment so we do not have to import RAWs.csv constantly
# if data already in Environment, it skips full import

#' Title
#'
#' @param ffpath
#'
#' @return
#'
#' @examples

importRAWs <- function(ffpath) {

  # check we are given a ffpath that makes sense
  if(substrEnding(ffpath, 4) != '.csv')
    stop('\t \t \t \t >>> Error: did you pick the correct file? It is not .csv \n')

  #### first; do we actually need to import? ####
  # if importRAWs(...) was ran already, frame-by-frame data is already in Environment

  # name of the object will be ff_name of the file (w/o .csv)
  # ff for frame-by-frame, to label it as imported through importRAWs
  rawname <- afterLastSlash(ffpath) # everything after last slash of ffpath, e.g. 210927_12_RAWs.csv
  ffname <- paste0('ff_', substr(rawname, 1, nchar(rawname)-4)) # name of the object if it was imported already, e.g. ff_210927_12_RAWs
  # nchar(rawname)-4 deletes the last 4 characters, which should be .csv

  # look in Environment (ls() lists all objects) if ffname is there
  if(length(which(ls(envir=.GlobalEnv)==ffname))==1) { # if it is there already
    cat('\t \t \t \t >>> Frame-by-frame data was imported already, skipping full import... \n \n')
    ff <- get(ffname) # copy existing object to ff

  ######

  } else { # if it is not there

    cat('\t \t \t \t >>> Frame-by-frame data not imported yet, proceeding to full import... \n')
    # then proceed with importing RAWs.csv properly

    #### import frame-by-frame data ####
    ff <- data.table::fread(ffpath)

    # detect number of wells on the plate
    # look at name of last column in ff
    nwells <- readr::parse_number(colnames(ff)[ncol(ff)])

    # if that number is above 97, we are looking at box2 data
    # so subtract 96 from the number
    if (nwells > 96) {nwells <- nwells - 96}

    # note, this is also correct if e.g. a 24-well plate is used
    # Zebralab (at least systems in Rihel lab) calls first well of second box 97 regardless of the plate format

    cat('\t \t \t \t >>> Detected *', nwells,'* -wells plate \n')

    fps <- averageFramerate(ff$exsecs)
    cat('\t \t \t \t >>> Experiment lasted ***', round(nrow(ff)/fps/60/60,0), '*** hours \n')


    #### return RAW ####

    # ! before, save it to Environment so can skip full import next time importRAWs(...) is called

    # ffname created at the start, it is the filename with ff_ in front, e.g. ff_210927_12_RAWs
    assign(ffname, ff, envir=.GlobalEnv) # will store ff in variable ffname

  }


  # now return ff
  # outside of else{} because
  # if was in Environment already, we copied the object to ff
  # if not in Environment already, we proceeded to full import as ff
  # either way, we return ff
  return(ff)

}
