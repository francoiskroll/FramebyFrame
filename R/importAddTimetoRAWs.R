###################################################
# ~~~ FramebyFrame package ~~~

# function importAddTimetoRAWs
# and smaller helper functions

# Francois Kroll 2022
# francois@kroll.be
###################################################


# function importAddTimetoRAWs(...) to import frame-by-frame data in RAWs.csv
# and add time columns & find light transitions (writing YYMMDD_BX_lights.csv) while doing so

# also smaller helper functions averageFramerate(...) and findLightTransitionFrame(...)


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
#' @export
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



# function importAddTimetoRAWs --------------------------------------------

# zebpath = path to Zebralab XLS file
# ffpath = path to frame-by-frame RAWs.csv file

# function will import RAWs.csv file and add time columns to it
# then will calculate light transitions and save them to YYMMDD_BX_lights.csv (in same folder as RAWs.csv)

#' Imports RAWs.csv
#'
#' @param ffpath Path to frame-by-frame data, typically ..._RAWs.csv.
#' @param zebpath Path to Zebralab file, typically YYMMDD_BX.xls.
#' @param dayduration Duration of day in hours. Default: 14.
#'
#' @return Dataframe containing frame-by-frame data with added timestamp columns.
#' @export
#'
#' @examples
importAddTimetoRAWs <- function(ffpath,
                                zebpath,
                                dayduration=14) {

  # check we are given a ffpath that makes sense
  if(substrEnding(ffpath, 4) != '.csv')
    stop('\t \t \t \t >>> Error: did you pick the correct file? It is not .csv \n')

  # first; do we actually need to import? -----------------------------------
  # if importAddTimetoRAWs(...) was ran already, frame-by-frame data is already in Environment

  # name of the object will be ff_name of the file (w/o .csv)
  # ff for frame-by-frame, to label it as imported through importAddTimetoRAWs
  rawname <- afterLastSlash(ffpath) # everything after last slash of ffpath, e.g. 210927_12_RAWs.csv
  ffname <- paste0('ff_', substr(rawname, 1, nchar(rawname)-4)) # name of the object if it was imported already, e.g. ff_210927_12_RAWs
  # nchar(rawname)-4 deletes the last 4 characters, which should be .csv

  # look in Environment (ls() lists all objects) if ffname is there
  if(length(which(ls(envir=.GlobalEnv)==ffname))==1) { # if it is there already
    cat('\t \t \t \t >>> Frame-by-frame data was imported already, skipping full import... \n \n')
    ff <- get(ffname) # copy existing object to ff


  } else { # if it is not there
    cat('\t \t \t \t >>> Frame-by-frame data not imported yet, proceeding to full import... \n')
    # then proceed with importing RAWs.csv properly
    # import frame-by-frame data ----------------------------------------------
    ff <- fread(ffpath)

    # column named `time` is a bad idea, creates conflicts later with a function called time
    colnames(ff)[1] <- 'exsecs' # number of seconds after experiment started

    # detect number of wells on the plate
    # look at name of last column in ff
    nwells <- parse_number(colnames(ff)[ncol(ff)])

    # if that number is above 97, we are looking at box2 data
    # so subtract 96 from the number
    if (nwells > 96) {nwells <- nwells - 96}

    cat('\t \t \t \t >>> Detected *', nwells,'* -wells plate \n')

    # check format looks correct
    # detect number
    if(ncol(ff)!=nwells+1) stop('\t \t \t \t >>> Error: why _RAWs.csv file does not have number of wells + 1 columns? \n')


    # calculate frame-rate ----------------------------------------------------

    fps <- averageFramerate(ff$exsecs)

    cat('\t \t \t \t >>> Experiment lasted ***', round(nrow(ff)/fps/60/60,0), '*** hours \n')


    # -------------------------------------------------------------------------

    # for timepoints: will add both full timestamps (best estimate) + zth (i.e. number of hours since day0 9AM)
    # will also output light transitions in frame # (row #), as Vp_Extract.m

    # will follow same logic as Vp_Extract.m to find transitions, i.e. find closest frame (in zth) to 14, 24, etc.


    # import first row of Zebralab file ---------------------------------------

    # the Zebralab XLS file has a different encoding whether it is from the older or newer version of the Zebralab software
    # and the encoding decides which import function we should use
    # I cannot find a solution to tell the encoding in advance,
    # so previously each command was asking whether zebDeprecatedFormat was TRUE or FALSE
    # a better solution to avoid bothering the user is simply to try one import command, and switch to the other if it does not work:

    usefRead <- TRUE # by default use fread
    # except if fread does not work
    tryCatch( { result <- fread(zebpath) },
              error = function(e) {usefRead <<- FALSE},
              silent=TRUE)

    # read first row of Zebralab's XLS file
    if (usefRead) {
      zebfi <- fread(zebpath)[1,]
    } else {
      zebfi <- read.delim(zebpath, fileEncoding='UCS-2LE', header=TRUE, nrow=1)
    }
    # zebfi = ZebraLab XLS file's first row


    # get t0 full timestamp ---------------------------------------------------

    startts <- paste(zebfi$stdate, zebfi$sttime) # start timestamp, e.g. 28/01/2021 10:27:35
    # ! assumes first timestamp is correct. ViewPoint have made serious errors about this; carefully check
    # especially if Replay; first timestamp is stupidly taken from the computer clock when you start the Replay, not from the raw file
    startts <- dmy_hms(startts) # convert in lubridate format eg. 2021-01-28 10:27:35 UTC


    # add full timestamps to frame-by-frame data ------------------------------

    # will now assume first frame timestamp in RAW csv is = first timestamp in ZebraLab XLS file
    # and write all the frame timestamps as full date/time eg. 2021-01-28 10:27:35 UTC
    ff <- ff %>%
      add_column(fullts = startts+dseconds(ff$exsecs), .before='exsecs') # add full timestamp column
    # takes first timestamp then add to it any number of seconds written in the frame-by-frame data
    # e.g. at 3000th frame, 2021-03-16 19:54:49 + 120 seconds = 2021-03-16 19:56:49


    # add Zeitgeber durations -------------------------------------------------

    # i.e. number of hours since first day0 9AM
    startdate <- date(ff$fullts[1]) # get startdate of the experiment = date of the first timepoint

    ff <- ff %>%
      add_column(zhrs = as.numeric(difftime(ff$fullts, ymd_hms(paste(startdate, '09:00:00')), units='hours')), .before='exsecs')
    # zeitgeber is time difference in hours since first 9AM

    # tell user about timestamps so can check they look correct
    cat('\t \t \t \t >>> First timestamp is ***', as.character(ff$fullts[1]), '*** \n')
    cat('\t \t \t \t >>> Last timestamp is ***', as.character(ff$fullts[nrow(ff)]), '*** \n \n')


    # find light transitions --------------------------------------------------

    # sunsets and sunrises times (in number of hours since day0 sunrise at 9AM) for 10 days
    suns <- c(rbind(seq(dayduration, 720, 24), seq(24, 720, 24)))
    # typically dayduration = 14
    # so will go 14, 24, 38, 48 etc.

    # take only the ones we will find in the data
    # round down last timepoint (indeed, if e.g. experiment stops at 71.9 hours, there is no 72 hours sunrise)
    lastz <- floor(max(ff$zhrs))
    # experiment should always start between 0+ and before 14 so should not matter how we round start
    firstz <- floor(min(ff$zhrs))

    suns <- intersect(suns, firstz:lastz) # will give the sunsets/sunrises we expect to encounter in the data

    # find the transition frames
    # using function findLightTransitionFrame(...) from above

    if (length(suns) != 0) { # if no light transition (e.g. experiment lasted just a few hours), should skip this step
      # preallocate small dataframe transition number / sunset or sunrise / frame # / full timestamp / Zeitgeber duration / seconds of experiment
      ltcolnms <- c('transition_num', 'sunset_or_sunrise', 'frame', 'full_timestamp', 'zhrs', 'exsecs') # light transitions column names
      lt <- as.data.frame(matrix(nrow=length(suns), ncol=length(ltcolnms))) # lt for light transitions
      colnames(lt) <- ltcolnms
      lt$full_timestamp <- ymd_hms(NA)

      for (s in 1:length(suns)) {

        # fill in transition number
        lt[s, 'transition_num'] <- s

        # fill in sunset or sunrise
        # if index in suns is even (2, 4, ...): sunrise; if index in suns is odd (1, 3, ...): sunset
        if (s %% 2 == 0) {
          lt[s, 'sunset_or_sunrise'] <- 'sunrise'
        } else if (s %% 2 != 0) {
          lt[s, 'sunset_or_sunrise'] <- 'sunset'
        }


        # find frame number of transition
        tfra <- findLightTransitionFrame(ff$zhrs, round(suns[s])) # transition frame

        # fill it in
        lt[s, 'frame'] <- tfra

        # fill in full timestamp
        lt[s, 'full_timestamp'] <- ff[tfra, 'fullts']

        # fill in Zeitgeber duration
        lt[s, 'zhrs'] <- ff[tfra, 'zhrs']

        # fill in number of seconds since start of the experiment
        lt[s, 'exsecs'] <- ff[tfra, 'exsecs']

        # give a quick check to user
        cat('\t \t \t \t >>> Light transition #', s,
            ': ', toupper(lt[s, 'sunset_or_sunrise']), ' at frame ', tfra,
            ', clock = ', as.character(lt[s, 'full_timestamp']),
            ', Zeitgeber duration = ', lt[s, 'zhrs'], '\n',
            sep='')

      }

      # write light transitions
      outpath <- paste0(beforeLastSlash(ffpath), beforeLastUnderscore(afterLastSlash(ffpath)), '_lights.csv')
      fwrite(lt, file=outpath)
    } # closes if suns is not length 0


    # return RAW with time columns --------------------------------------------

    # ! before, save it to Environment so can skip full import next time importAddTimetoRAWs(...) is called

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
