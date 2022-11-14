### Frame-by-Frame package ###
### sleep parameters ###



# function hoursSleep_onefish(...) ----------------------------------------

# zzc = one column of sleep booleans (TRUE = this frame was part of a nap / FALSE = this frame was not part of a nap)

#' Title
#'
#' @param zzc
#' @param fps
#'
#' @return
#' @export
#'
#' @examples
sleepHours_onefish <- function(zzc,
                               fps) {

  # to obtain time asleep, we simply sum the TRUE frames
  # in backstage, TRUE is represented as 1 and FALSE is represented as 0, so we can simply sum the booleans
  # this will give us total time spent asleep in number of frames
  # to convert into hours, we use frame-rate (fps)
  return(sum(zzc) / fps / 60 / 60)

}

# Note; another way to think about total time asleep would be in proportion (%) of day or night
# + would make it possible to compare day vs night
# (cannot easily compare with total time spent asleep as night and day have different durations)
# - total time spent asleep in hours is nicely intuitive

# will keep total time spent asleep for now
# not making comparisons day vs night anyways

# can easily add it as a separate parameter in the future



# function sleepNumNaps_onefish(...) --------------------------------------

# zzc = one column of sleep booleans (TRUE = this frame was part of a nap / FALSE = this frame was not part of a nap)
# note, fps is not used here but kept for consistency with other sleep parameters (namely sleepHours_onefish)

#' Title
#'
#' @param zzc
#' @param fps
#'
#' @return
#' @export
#'
#' @examples
sleepNumNaps_onefish <- function(zzc,
                                 fps) {

  # to count the number of sleep bouts (aka naps)
  # we simply diff (successive difference) the sleep booleans
  # ! diff works as N+1 - N
  # TRUE - TRUE = 0 = fish stays asleep
  # TRUE - FALSE = 1 = fish falls asleep
  # FALSE - FALSE = 0 = fish stays awake
  # FALSE - TRUE = -1 = fish wakes up

  # below: counting number of times fish falls asleep, i.e. number of 1s
  return(length(which(diff2(zzc)==1)))

}




# function sleepLatency_onefish(...) --------------------------------------

# zzc = one column of sleep booleans (TRUE = this frame was part of a nap / FALSE = this frame was not part of a nap)

#' Title
#'
#' @param zzc
#' @param fps
#'
#' @return
#' @export
#'
#' @examples
sleepLatency_onefish <- function(zzc,
                                 fps) {

  # sleepLatency is simply the duration after light transition until the first nap
  # we are given here sleep booleans for one window at a time
  # so we simply record the frame index of the first TRUE and convert into minutes

  # find earliest TRUE
  fni <- which(zzc)[1] # first nap, frame index

  # Note, if fish never slept during that time window, fni becomes NA and will not get plotted in the scatter plot
  # in the sleep latency survival curve, they will somewhat get plotted as the curve will not reach 0
  # we may want to change this in the future or during plotting to label these fish somehow

  # we convert frame index in minutes
  latmin <- (fni / fps / 60) # latency in minutes

  # ready to return
  return(latmin)
}




# sleepNapDuration(...) ---------------------------------------------------

# zzc = one column of sleep booleans (TRUE = this frame was part of a nap / FALSE = this frame was not part of a nap)

#' Title
#'
#' @param zzc
#' @param fps
#'
#' @return
#' @export
#'
#' @examples
sleepNapDuration_onefish <- function(zzc,
                                     fps) {

  # first convert sleep booleans to sleep transitions
  # by diff the sleep booleans
  # (! diff works as N+1 - N)
  # TRUE, TRUE >> TRUE - TRUE >> 1 - 1 = 0, i.e. fish stays asleep
  # TRUE, FALSE >> FALSE - TRUE >> 0 - 1 = -1, i.e. fish wakes up
  # FALSE, TRUE >> TRUE - FALSE >> 1 - 0 = 1, i.e. fish falls asleep
  # FALSE, FALSE >> FALSE - FALSE >> 0 - 0 = 0, i.e. fish stays awake

  # exception: it is possible the fish was 'always asleep'
  # in this case, sleepNapDuration should be NA as this is meaningless data
  # (this only happens with empty wells)
  if (length(which(!zzc))==0) { # i.e. how many not (!) TRUE, i.e. how many frames not asleep
    return(NA)
  }

  # note, we will assume the fish was awake at frame 1 of the window
  # we will 'cheat' and manually make this the case
  # this handles a very rare case (if anything it probably occurs only in empty wells) where the very first 1500 frames are inactive
  # so first boolean in zzc is directly TRUE, which makes the following steps uselessly complicated
  # we will replace the first value of the zzc we receive to FALSE
  # in the worst case scenario, we have artificially decreased the duration of this sleep bout (assuming it is genuine) by 1 frame i.e. 0.04 sec. Not a really big deal...

  zzc[1] <- FALSE

  # now work on transitions
  ztrs <- diff2(zzc) # ztrs for zzz transitions

  # exception: it is possible the fish did not have a single full nap during that time window
  # a single full nap i.e. at least one 1 and one -1 in ztrs
  # in this case, mean nap duration is NA, as "we could not measure"
  if( length(which(ztrs==1))==0 | length(which(ztrs==-1))==0 ) {
    return(NA) # return call will stop execution of the whole function
  }
  # note, I think this may be returned for empty wells
  # e.g. if there was a positive px value at the start, then 'sleep' started, but never 'woke up' because there was never another positive px value
  # so trusting user here to have labelled correctly empty wells

  # note, the next stage will be much more convenient if we only filmed complete sleep bouts
  # i.e. starting with the fish falling asleep (+1) and finishing with the fish waking up (-1)

  # make it so the video starts at the start of this fish's first nap
  # (switch to NA the frames before that)
  # ! it is possible that the video already starts exactly at the start of a nap by chance; if so do not do anything
  # ! diff2 puts a NA at the start, so look at first non-NA value
  ztrsnoNA <- ztrs[!is.na(ztrs)]
  if(ztrsnoNA[1]!=1) { # if video does not start at the start of a nap
    ztrs[1: (match(1, ztrs)-1) ] <- NA
    # match() finds first '1' in ztrs, replace with NA from frame 1 to frame just before
    # i.e. gives ..., NA, NA, NA, 1, ...
  }

  # make it so the video stops just after this fish finishes its last nap (when it wakes up)
  # it is like we are manually stopping the camera just after the end of a nap
  # ! it is possible that the video already stops exactly at the stop of a nap by chance; if so do not do anything
  if(ztrs[length(ztrs)]!=-1) { # if video does not stops at the stop of a nap
    ztrs[(max(which(ztrs==-1))+1) : length(ztrs)] <- NA
    # max(which(col==-1)), i.e. index of last '-1' in ztrs
    # from that frame up to the end, replace by NA
    # so video always stops with -1, i.e. the end of a nap
  }

  # check this just to be safe
  # take only non-NA values
  ztrsnoNA <- ztrs[!is.na(ztrs)]

  # check first non-NA value is 1
  if(ztrsnoNA[1] != 1)
    stop('\t \t \t \t>>> Error: even after edits, data does not start at the start of an active bout for fish', fi, '\n')

  # check last non-NA value is -1
  if(ztrsnoNA[length(ztrsnoNA)] !=-1)
    stop('\t \t \t \t>>> Error: even after edits, data does not end at the stop of an active bout for fish', fi, '\n')

  # note, the above is obviously not perfectly accurate as we are modifying the data
  # but we are measuring the duration of each nap within a window
  # so it makes sense to only take 'round' naps, i.e. that start and finish during that window
  # we do not want to consider naps overlapping light transitions anyway
  # because the transition will usually wake up the fish, making it an unnatural duration

  # second, convert sleep transitions to start/stop indices
  # will be a list with two elements:
  # START indices, i.e. the frame index of each start of a nap
  # STOP indices, i.e. the frame index of each stop of a nap
  saof <- list( saf=which(ztrs==1), sof=which(ztrs==-1) )
  # saf for START frames
  # sof for STOP frames

  # thanks to the small edits above, there should now be same number of STARTs and STOPs
  if (length(saof$saf) != length(saof$sof))
    stop('\t \t \t \t >>> Error: not the same number of nap STARTs and nap STOPs \n')

  # can also check that at each position, STOP index is always *after* START index
  check <- unique(saof$sof > saof$saf) # will compare each position and return TRUE or FALSE, here take unique
  if (! (length(check)==1 & check[1]==TRUE)) # check there is only one unique element and it is TRUE (i.e. were all TRUE before unique)
    stop ('\t \t \t \t >>> Error: some nap STOPs are *after* nap STARTs \n') # if not the case (!), throw error

  # which makes index of saf/sof convenient because to get e.g.
  # START frame and STOP frame of that fish's 56th nap is simply:
  # START frame is saf[56] and STOP frame is sof[56]
  # can picture it as dataframe where each row is a nap and there are two columns: START index and STOP index
  # consequently, doing the difference STOP - START gives how many frames every nap lasted

  nadur <- saof$sof - saof$saf # nadur for nap durations

  # we convert durations in frames to minutes using fps
  nadurmin <- (nadur / fps / 60)

  # and we return the mean nap duration in minutes
  return(mean(nadurmin))
}
