### Frame-by-Frame package ###
### active bout parameters, one fish functions ###

# the following functions are given data for one time window/one fish, namely;
# ffc = frame-by-frame data, ffc for frame-by-frame column
# aic = active/inactive transitions, aic for active/inactive column
# and they calculate a parameter for that one time window/one fish
# each time one of the functions below is ran, it returns ONE function
# usually the mean parameter of all active bouts

# for example, activeboutMean(...) is ran for day1, fish5
# it loops through all the active bouts that fish5 did during day1
# for each active bout, it calculates the mean deltapx of that active bout
# e.g. deltapx values for active bout #1593 are 1 5 9 3 1 >> mean = 3.8 px
# then it returns the mean of all these active bout means
# e.g. during day1, the average bout of fish5 was 9.2 px

# smaller helper function transitionsToActiveBoutStartStop(...)
# it is called by one activeboutParameter_onefish() function
# giving it one time window / one fish's active/inactive transitions computed by framesToActiveInactiveTransitions()
# it simply records the frame positions where the fish started/stopped active bouts

# function transitionsToActiveBoutStartStop(...) --------------------------

# given one column of active/transitions, i.e. a vector of 0 / -1 / 1
# returns START frame index and STOP frame index of each active bout
# useful for single active bout parameters, such as activeBoutLength and activeBoutMean
#' Title
#'
#' @param colAI
#'
#' @return
#' @export
#'
#' @examples
transitionsToActiveBoutStartStop <- function(colAI) { # column of active/inactive transitions

  # make sure we are being given a simple vector
  colAI <- as.vector(unlist(colAI))

  colnoNA <- colAI[!is.na(colAI)] # colAI without NAs
  # (first value is typically NA because of diff from framesToActiveInactiveTransitions(...))

  # make it so the video starts at the start of this fish's first active bout
  # (switch to NA the frames before that)
  # ! it is possible that the video already starts exactly at the start of an active bout by chance; if so do not do anything
  if(colnoNA[1]!=1) { # if video does not start at the start of an active bout
    colAI[1: (match(1, colAI)-1) ] <- NA
    # match() finds first '1' in colAI, replace with NA from frame 1 to frame just before
  }

  # make it so the video stops just after this fish finishes its last active bout
  # it is like we are manually stopping the camera just after the end of a swimming bout
  # ! it is possible that the video already stops exactly at the stop of an active bout by chance; if so do not do anything
  if(colnoNA[length(colnoNA)]!=-1) { # if video does not stop at the stop of an active bout
    colAI[(max(which(colAI==-1))+1) : length(colAI)] <- NA
    # max(which(col==-1)), i.e. index of last '-1' in colAI
    # from that frame up to the end, replace by NA
    # so video always stops with -1, i.e. the end of a swimming bout
  }

  # check this just to be safe
  # take only non-NA values
  colnoNA <- colAI[!is.na(colAI)]

  # check first non-NA value is 1
  if(colnoNA[1] != 1)
    stop('\t \t \t \t>>> Error: even after edits, data does not start at the start of an active bout for fish', fi, '\n')

  # check last non-NA value is -1
  if(colnoNA[length(colnoNA)] !=-1)
    stop('\t \t \t \t>>> Error: even after edits, data does not end at the stop of an active bout for fish', fi, '\n')

  # note, the above is obviously not perfectly accurate as we are modifying the data
  # but the worst case scenario is losing one bout at the start and one bout at the end
  # which is negligible during a 14-hr day or 10-hr night

  # find all the START transitions, i.e. frames at which an active bout starts, i.e. where = 1
  # find all the STOP transitions, i.e. frames at which an active bout stops, i.e. where = -1
  # put those in a list to be returned
  saof <- list( saf=which(colAI==1), sof=which(colAI==-1) )
  # saf for START frames
  # sof for STOP frames

  # thanks to the small edits above, there should now be same number of STARTs and STOPs
  if (length(saof$saf) != length(saof$sof))
    stop('\t \t \t \t >>> Error: not the same number of active bout STARTs and active bout STOPs \n')

  # which makes index of saf/sof convenient because to get e.g. START frame and STOP frame of that fish's 56th active bout is simply:
  # START frame is saf[56] and STOP frame is sof[56]
  # additionally, START and STOP frames directly match frames in frame-by-frame timecourse dn
  # so something like dn[[win]] [ saf[56] : sof[56] , FISH] will give deltapx of active bout 56, e.g. 2 9 17 8 1

  return(saof)
}


###
# series of activeboutParameter_onefish functions -------------------------
###


# function activeboutLength_onefish(...) ----------------------------------

# given, for one fish and one time window,
# its frame-by-frame data, ffc for frame-by-frame column
# its active/inactive transition frames, aic for active/inactive transitions column
# function loops through active bouts, takes the number of frames each active bout lasted
# then the mean active bout length and returns that
# e.g. active bout is 1 5 9 3 1 >> length = 5
# it does this for every active bout for one fish/one time window, and returns the mean of all lengths
# so returns mean length in number of frames, we cannot convert to seconds here as we only have one column of data
# will do in function activeBoutParameter(...)

#' Title
#'
#' @param ffc
#' @param aic
#' @param fps
#'
#' @return
#' @export
#'
#' @examples
activeboutLength_onefish <- function(ffc,
                                     aic,
                                     fps) {

  # ! exception
  # if there is no active bout (happens with empty well), then we should return NA
  # in aic, 1 means larva starts moving, -1 means larva stops moving
  # we need at least one start and one stop to have a meaningful result
  if( length(which(aic==1))==0 & length(which(aic==-1))==0 ) {
    cat('\t \t \t \t \t \t 0 complete active bout, returning NA \n')
    return(NA)
  }

  # Note, we do not need actually frame-by-frame data (argument ffc) here
  # as we can simply calculate how long each active bout lasted from the START/STOP positions
  # but will keep for consistency with other parameters

  # find this fish's START/STOP frames
  # i.e. for each active bout, frame index of START and frame index of STOP
  # using function transitionsToActiveBoutStartStop(...)

  saof <- transitionsToActiveBoutStartStop(aic)
  # function returns a list saof for START/STOP frames
  # saof$saf are all the START positions, saof$sof are all the STOP positions

  # contrary to other active bout parameters, we do not need to loop through every active bout here
  # simply calculating the difference between the STOP positions and the START positions will give active bout lengths
  las <- saof$sof - saof$saf # las for active bout lengths

  # check there is no NA in las
  if(sum(is.na(las))!=0)
    stop('\t \t \t \t >>> Some NA in vector which stores the lengths of each active bout \n')

  # calculate mean active bout length
  lam <- mean(las)

  # now convert mean active bout length from frames to seconds, using fps
  lam <- lam/fps

  # now return the mean active bout length, for that time window and that fish
  return(lam)

}


# function activeboutMean_onefish(...) ------------------------------------

# given, for one fish and one time window,
# its frame-by-frame data, ffc for frame-by-frame column
# its active/inactive transition frames, aic for active/inactive transitions column
# function loops through active bouts, calculates the mean deltapx of each active bout
# then the mean active bout mean deltapx and returns that

#' Title
#'
#' @param ffc
#' @param aic
#' @param fps
#'
#' @return
#' @export
#'
#' @examples
activeboutMean_onefish <- function(ffc,
                                   aic,
                                   fps) {

  # ! exception
  # if there is no active bout (happens with empty well), then we should return NA
  # in aic, 1 means larva starts moving, -1 means larva stops moving
  # we need at least one start and one stop to have a meaningful result
  if( length(which(aic==1))==0 & length(which(aic==-1))==0 ) {
    cat('\t \t \t \t \t \t 0 complete active bout, returning NA \n')
    return(NA)
  }

  # find this fish's START/STOP frames
  # i.e. for each active bout, frame index of START and frame index of STOP
  # using function transitionsToActiveBoutStartStop(...)

  saof <- transitionsToActiveBoutStartStop(aic)
  # function returns a list saof for START/STOP frames
  # saof$saf are all the START positions, saof$sof are all the STOP positions

  # ** THIRD sapply to loop through the active bouts of that fish

  # preallocate vector which will store mean deltapx of each active bout
  mdpa <- rep(NA, length(saof$saf))
  # ! much better for speed to preallocate all NA rather than have a vector that grows at each iteration

  # sapply loops through START frames
  # active bout index will go active bout #1, active bout #2, etc.
  invisible(sapply(1:length(saof$saf), function(a) {

    # for each active bout, get its deltapx data from dn
    dpa <- as.vector(unlist(ffc[ saof$saf[a]:(saof$sof[a]-1)])) # deltapx points for that one active bout
    # the way we built saf/sof makes it convenient as they match the actual frame-by-frame data in dn

    # check that there are only positive deltapx values
    if(unique(dpa>0) != TRUE)
      stop('\t \t \t \t >>> Error: there is something else than only positive deltapx values in \n')

    # add the mean deltapx of this active bout to vector mdpa
    mdpa[a] <<- mean(dpa)
  }))

  # check there is no more NA in mdpa
  if(sum(is.na(mdpa))!=0) stop('\t \t \t \t >>> Some NA in vector which stores the mean of each active bout \n')
  # now return the mean active bout mean, for that time window and that fish
  return(mean(mdpa))

}


# function activeboutSum_onefish(...)  ------------------------------------

# given, for one fish and one time window,
# its frame-by-frame data, ffc for frame-by-frame column
# its active/inactive transition frames, aic for active/inactive transitions column
# function loops through active bouts, calculates the sum of the deltapx for each active bout
# then the mean active bout total deltapx and returns that

#' Title
#'
#' @param ffc
#' @param aic
#' @param fps
#'
#' @return
#' @export
#'
#' @examples
activeboutSum_onefish <- function(ffc,
                                  aic,
                                  fps) {

  # ! exception
  # if there is no active bout (happens with empty well), then we should return NA
  # in aic, 1 means larva starts moving, -1 means larva stops moving
  # we need at least one start and one stop to have a meaningful result
  if( length(which(aic==1))==0 & length(which(aic==-1))==0 ) {
    cat('\t \t \t \t \t \t 0 complete active bout, returning NA \n')
    return(NA)
  }

  # find this fish's START/STOP frames
  # i.e. for each active bout, frame index of START and frame index of STOP
  # using function transitionsToActiveBoutStartStop(...)

  saof <- transitionsToActiveBoutStartStop(aic)
  # function returns a list saof for START/STOP frames
  # saof$saf are all the START positions, saof$sof are all the STOP positions

  # ** THIRD sapply to loop through the active bouts of that fish

  # preallocate vector which will store mean deltapx of each active bout
  sdpa <- rep(NA, length(saof$saf))
  # ! much better for speed to preallocate all NA rather than have a vector that grows at each iteration

  # sapply loops through START frames
  # active bout index is a, will go active bout #1, active bout #2, etc.
  invisible(sapply(1:length(saof$saf), function(a) {

    # for each active bout, get its deltapx data from dn
    dpa <- as.vector(unlist(ffc[ saof$saf[a]:(saof$sof[a]-1)])) # deltapx points for that one active bout
    # the way we built saf/sof makes it convenient as they match the actual frame-by-frame data in dn

    # check that there are only positive deltapx values
    if(unique(dpa>0) != TRUE)
      stop('\t \t \t \t >>> Error: there is something else than only positive deltapx values in \n')

    # add the sum deltapx of this active bout to vector mdpa
    sdpa[a] <<- sum(dpa)
  }))

  # check there is no more NA in mdpa
  if(sum(is.na(sdpa))!=0) stop('\t \t \t \t >>> Some NA in vector which stores the sum of each active bout \n')
  # now return the mean active bout mean, for that time window and that fish
  return(mean(sdpa))

}



# function activeboutStd_onefish(...) -------------------------------------

# given, for one fish and one time window,
# its frame-by-frame data, ffc for frame-by-frame column
# its active/inactive transition frames, aic for active/inactive transitions column
# function loops through active bouts, calculates the standard deviation of the deltapx points for each active bout
# then the mean active bout standard deviation and returns that
# e.g. active bout is 1 5 9 3 1 >> std = 3.35
# it does this for every active bout for one fish/one time window, and returns the mean of every all standard deviations

#' Title
#'
#' @param ffc
#' @param aic
#' @param fps
#'
#' @return
#' @export
#'
#' @examples
activeboutStd_onefish <- function(ffc,
                                  aic,
                                  fps) {

  # ! exception
  # if there is no active bout (happens with empty well), then we should return NA
  # in aic, 1 means larva starts moving, -1 means larva stops moving
  # we need at least one start and one stop to have a meaningful result
  if( length(which(aic==1))==0 & length(which(aic==-1))==0 ) {
    cat('\t \t \t \t \t \t 0 complete active bout, returning NA \n')
    return(NA)
  }

  # find this fish's START/STOP frames
  # i.e. for each active bout, frame index of START and frame index of STOP
  # using function transitionsToActiveBoutStartStop(...)

  saof <- transitionsToActiveBoutStartStop(aic)
  # function returns a list saof for START/STOP frames
  # saof$saf are all the START positions, saof$sof are all the STOP positions

  # ** THIRD sapply to loop through the active bouts of that fish

  # preallocate vector which will store std of each active bout's deltapx points
  stddpa <- rep(NA, length(saof$saf)) # stddpa for standard deviation of delta px of active bouts
  # ! much better for speed to preallocate all NA rather than have a vector that grows at each iteration

  # sapply loops through START frames
  # active bout index is a, will go active bout #1, active bout #2, etc.
  invisible(sapply(1:length(saof$saf), function(a) {

    # for each active bout, get its deltapx data from dn
    dpa <- as.vector(unlist(ffc[ saof$saf[a]:(saof$sof[a]-1)])) # deltapx points for that one active bout
    # the way we built saf/sof makes it convenient as they match the actual frame-by-frame data in dn

    # check that there are only positive deltapx values
    if(unique(dpa>0) != TRUE)
      stop('\t \t \t \t >>> Error: there is something else than only positive deltapx values in \n')

    # add the mean deltapx of this active bout to vector mdpa
    stddpa[a] <<- sd(dpa) # ! core is here, standard deviation of the deltapx values of that active bout
  }))

  # for other active bout parameters, we check here that vector does not contain NA
  # however standard deviation of a single value is undefined, so gives NA, e.g. sd(1)
  # so active bouts lasting one frame will output NA; this is correct and we should leave it this way

  # now return the mean active bout mean, for that time window and that fish
  return(mean(stddpa, na.rm=TRUE)) # return the mean standard deviation for this time window/fish
  # note, na.rm=TRUE, see above

}



# function activeboutMin_onefish(...) -------------------------------------

# given, for one fish and one time window,
# its frame-by-frame data, ffc for frame-by-frame column
# its active/inactive transition frames, aic for active/inactive transitions column
# function loops through active bouts, takes the minimum of the deltapx points for each active bout
# then the mean active bout minimum and returns that
# e.g. active bout is 1 5 9 3 1 >> min = 1
# it does this for every active bout for one fish/one time window, and returns the mean of all minimums

#' Title
#'
#' @param ffc
#' @param aic
#' @param fps
#'
#' @return
#' @export
#'
#' @examples
activeboutMin_onefish <- function(ffc,
                                  aic,
                                  fps) {

  # ! exception
  # if there is no active bout (happens with empty well), then we should return NA
  # in aic, 1 means larva starts moving, -1 means larva stops moving
  # we need at least one start and one stop to have a meaningful result
  if( length(which(aic==1))==0 & length(which(aic==-1))==0 ) {
    cat('\t \t \t \t \t \t 0 complete active bout, returning NA \n')
    return(NA)
  }

  # find this fish's START/STOP frames
  # i.e. for each active bout, frame index of START and frame index of STOP
  # using function transitionsToActiveBoutStartStop(...)

  saof <- transitionsToActiveBoutStartStop(aic)
  # function returns a list saof for START/STOP frames
  # saof$saf are all the START positions, saof$sof are all the STOP positions

  # ** THIRD sapply to loop through the active bouts of that fish

  # preallocate vector which will store std of each active bout's deltapx points
  mindpa <- rep(NA, length(saof$saf)) # stddpa for standard deviation of delta px of active bouts
  # ! much better for speed to preallocate all NA rather than have a vector that grows at each iteration

  # sapply loops through START frames
  # active bout index is a, will go active bout #1, active bout #2, etc.
  invisible(sapply(1:length(saof$saf), function(a) {

    # for each active bout, get its deltapx data from dn
    dpa <- as.vector(unlist(ffc[ saof$saf[a]:(saof$sof[a]-1)])) # deltapx points for that one active bout
    # the way we built saf/sof makes it convenient as they match the actual frame-by-frame data in dn

    # check that there are only positive deltapx values
    if(unique(dpa>0) != TRUE)
      stop('\t \t \t \t >>> Error: there is something else than only positive deltapx values in \n')

    # add the minimum deltapx of this active bout to vector mindpa
    mindpa[a] <<- min(dpa) # ! core is here, minimum of the deltapx values of that active bout
  }))

  # check there is no more NA in mindpa
  if(sum(is.na(mindpa))!=0)
    stop('\t \t \t \t >>> Some NA in vector which stores the minimum of each active bout \n')

  # now return the mean active bout minimum, for that time window and that fish
  return(mean(mindpa)) # return the mean minimum for this time window/fish
  # note, na.rm=TRUE, see above

}



# function activeboutMax_onefish(...) -------------------------------------

# given, for one fish and one time window,
# its frame-by-frame data, ffc for frame-by-frame column
# its active/inactive transition frames, aic for active/inactive transitions column
# function loops through active bouts, takes the minimum of the deltapx points for each active bout
# then the mean active bout maximum and returns that
# e.g. active bout is 1 5 9 3 1 >> max = 9
# it does this for every active bout for one fish/one time window, and returns the mean of all maximums

#' Title
#'
#' @param ffc
#' @param aic
#' @param fps
#'
#' @return
#' @export
#'
#' @examples
activeboutMax_onefish <- function(ffc,
                                  aic,
                                  fps) {

  # ! exception
  # if there is no active bout (happens with empty well), then we should return NA
  # in aic, 1 means larva starts moving, -1 means larva stops moving
  # we need at least one start and one stop to have a meaningful result
  if( length(which(aic==1))==0 & length(which(aic==-1))==0 ) {
    cat('\t \t \t \t \t \t 0 complete active bout, returning NA \n')
    return(NA)
  }

  # find this fish's START/STOP frames
  # i.e. for each active bout, frame index of START and frame index of STOP
  # using function transitionsToActiveBoutStartStop(...)

  saof <- transitionsToActiveBoutStartStop(aic)
  # function returns a list saof for START/STOP frames
  # saof$saf are all the START positions, saof$sof are all the STOP positions

  # ** THIRD sapply to loop through the active bouts of that fish

  # preallocate vector which will store std of each active bout's deltapx points
  maxdpa <- rep(NA, length(saof$saf)) # maxdpa for standard deviation of delta px of active bouts
  # ! much better for speed to preallocate all NA rather than have a vector that grows at each iteration

  # sapply loops through START frames
  # active bout index is a, will go active bout #1, active bout #2, etc.
  invisible(sapply(1:length(saof$saf), function(a) {

    # for each active bout, get its deltapx data from dn
    dpa <- as.vector(unlist(ffc[ saof$saf[a]:(saof$sof[a]-1)])) # deltapx points for that one active bout
    # the way we built saf/sof makes it convenient as they match the actual frame-by-frame data in dn

    # check that there are only positive deltapx values
    if(unique(dpa>0) != TRUE)
      stop('\t \t \t \t >>> Error: there is something else than only positive deltapx values in deltapx data of an active bout.\n')

    # add the minimum deltapx of this active bout to vector mindpa
    maxdpa[a] <<- max(dpa) # ! core is here, maximum of the deltapx values of that active bout
  }))

  # check there is no more NA in mindpa
  if(sum(is.na(maxdpa))!=0)
    stop('\t \t \t \t >>> Some NA in vector which stores the maximum of each active bout.\n')

  # now return the mean active bout maximum, for that time window and that fish
  return(mean(maxdpa)) # return the mean maximum for this time window/fish

}


# function activeboutNum_onefish(...) -------------------------------------

# given, for one fish and one time window,
# its frame-by-frame data, ffc for frame-by-frame column
# its active/inactive transition frames, aic for active/inactive transitions column
# function counts the number of active bouts and returns this value

#' Title
#'
#' @param ffc
#' @param aic
#' @param fps
#'
#' @return
#' @export
#'
#' @examples
activeboutNum_onefish <- function(ffc,
                                  aic,
                                  fps) {

  # ! exception
  # if there is no active bout (happens with empty well), then we should return 0
  # in aic, 1 means larva starts moving, -1 means larva stops moving
  if( length(which(aic==1))==0 & length(which(aic==-1))==0 ) {
    return(0)
  }

  # Note, we do not actually need frame-by-frame deltapx data here (ffc)
  # as we are simply counting the number of active bouts
  # but easier to leave so all activeBoutParameter_onefish functions are called in the same way

  # find this fish's START/STOP frames
  # i.e. for each active bout, frame index of START and frame index of STOP
  # using function transitionsToActiveBoutStartStop(...)

  saof <- transitionsToActiveBoutStartStop(aic)
  # function returns a list saof for START/STOP frames
  # saof$saf are all the START positions, saof$sof are all the STOP positions

  # Note, contrary to most other active bout parameters, we do not need to loop through active bouts here
  # we can simply count the number of START positions, i.e. the number of times the fish initiated an active bout
  return(length(saof$saf))

}
