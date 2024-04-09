#' Title
#'
#' @param ffpath
#' @param sentinel
#' @param genopath
#' @param onlyGrp
#' @param zhstartsd
#' @param zhstopsd
#' @param px_thr
#' @param nframes_thr
#' @param extendBefore
#' @param extendAfter
#' @param replaceBy
#'
#' @return
#' @export
#'
#' @examples
replacePulses <- function(ffpath,
                          sentinel,
                          genopath=NA,
                          onlyGrp=NA,
                          zhstartsd,
                          zhstopsd,
                          px_thr=2,
                          nframes_thr=2,
                          extendBefore=2,
                          extendAfter=2,
                          replaceBy=0
                          ) {

  ff <- importRAWs(ffpath=ffpath)

  ffc <- ff[,..sentinel]

  # for each pulse, we will take the maximum activity as center of that pulse
  # take every transition from inactive to active

  aic <- diff(ffc>0) # essentially replicates function framesToActiveInactiveTransitions
  # diff: difference between successive TRUE/FALSE
  # ! second element minus first element
  # possibilities are:
  # TRUE , TRUE = 1 - 1 = 0, i.e. fish continues moving
  # TRUE , FALSE = 0 - 1 = -1, i.e. fish STOPS moving
  # FALSE , TRUE = 1 - 0 = 1, i.e. fish STARTS moving
  # FALSE , FALSE = 0 - 0 = 0, i.e. fish continues NOT moving

  # diff above will skip first row, so ais will always lose one row
  # solution so we can match rows (frames) between ff and ais is to add a row of NA on top
  aic <- c(NA, aic)

  # then we can use function transitionsToActiveBoutStartStop as it is
  saof <- transitionsToActiveBoutStartStop(colAI=aic)
  # function returns a list saof for START/STOP frames
  # saof$saf are all the START positions, saof$sof are all the STOP positions

  # then we copy activeboutMax_onefish, but we stop before making the overall average
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

  # lapply to loop through START frames
  # active bout index is a, will go active bout #1, active bout #2, etc.
  puldet <- lapply(1:length(saof$saf), function(a) {

    # goal is to return a named vector pulse #, start frame index, stop frame index, maximum frame index, maximum frame deltapx
    # so, for the active bout (pulse) we are looking at, get
    # start frame index
    start_frame <- saof$saf[a]
    stop_frame <- saof$sof[a]-1

    # for each active bout, get its deltapx data from dn
    dpa <- as.vector(unlist(ffc[start_frame:stop_frame])) # deltapx points for that one active bout
    # the way we built saf/sof makes it convenient as they match the actual frame-by-frame data in dn

    # check that there are only positive deltapx values
    if(unique(dpa>0) != TRUE)
      stop('\t \t \t \t >>> Error: there is something else than only positive deltapx values in deltapx data of an active bout.\n')

    # start of pulse in fullts, zhrs, exsecs
    start_zhrs <- as.numeric(ff[start_frame,'zhrs'])
    start_exsecs <- as.numeric(ff[start_frame,'exsecs'])

    # stop of pulse in fullts, zhrs, exsecs
    stop_zhrs <- as.numeric(ff[stop_frame,'zhrs'])
    stop_exsecs <- as.numeric(ff[stop_frame,'exsecs'])

    # duration of pulse in frames?
    nframes <- length(dpa)

    # record maximum deltapx
    max_px <- max(dpa)

    # which frame index is the maximum px?
    max_frame <- start_frame + which.max(dpa) - 1

    # return named vector
    return(c(pulsi=a,
             start_frame=start_frame,
             start_zhrs=start_zhrs,
             stop_frame=stop_frame,
             stop_zhrs=stop_zhrs,
             stop_exsecs=stop_exsecs,
             nframes=nframes,
             max_px=max_px,
             max_frame=max_frame))
  })
  puldet <- as.data.frame(do.call(rbind, puldet))

  cat('\t \t \t \t >>> Detected', nrow(puldet), 'pulses\n')


  # filter detected pulses --------------------------------------------------

  ### only keep those within SD period
  puldet <- puldet %>%
    filter(start_zhrs>zhstartsd) %>%
    filter(stop_zhrs<zhstopsd)

  ### remove pulses below a certain deltapx threshold
  puldet <- puldet %>%
    filter(max_px>=px_thr)

  ### remove pulses below a certain duration
  puldet <- puldet %>%
    filter(nframes>=nframes_thr)

  ### reset the pulse indices
  puldet$pulsi <- 1:nrow(puldet)

  cat('\t \t \t \t >>> After filtering,', nrow(puldet), 'pulses left\n')

  ### export detected pulses
  expfolder <- parentFolder(paths=ffpath,
                            upn=1,
                            slash=whatSlash(ffpath))

  dtbx <- paste(strNthSplit(afterLastSlash(ffpath), split='_', nth=1),
                strNthSplit(afterLastSlash(ffpath), split='_', nth=2),
                sep='_')

  filenm <- paste0(dtbx, '_pulses.csv')

  write.csv(puldet, paste0(expfolder, whatSlash(ffpath), filenm), row.names=FALSE)


  # exclude those frames from dataset ---------------------------------------

  cat('\t \t \t \t >>> Replacing the corresponding frames\n')

  # collect all the frames we want to exclude
  # we loop through the pulses and take start frame to stop frame, possibly with extension on each side
  sdframes <- unlist(apply(puldet, 1, function(ro) {
    (ro['start_frame']-extendBefore) : (ro['stop_frame']+extendAfter)
  }))

  # replace those frames in ff data
  # only for wells in `onlyGrp`
  # or all
  timecols <- which(grepl("^f+[[:digit:]]", colnames(ff)))[1] - 1
  ffsd <- data.table::copy(ff)

  if(is.na(onlyGrp)) {
    ffsd[sdframes, (timecols+1):ncol(ffsd)] <- replaceBy
  } else {
    geno <- importGenotype(genopath=genopath)
    wells2edit <- as.vector(unlist(select(geno, all_of(onlyGrp))))
    wells2edit <- wells2edit[!is.na(wells2edit)] # remove any NA
    wells2edit <- sprintf('f%i', wells2edit)
    # now replace those values by replaceBy
    ffsd[sdframes, (wells2edit) := replaceBy]
  }

  # for example, pulse #37 lasts 20 frames, from 4216530 to 4216549
  # f2 is 4, 41, 3, 7, etc.
  # after edit, f2 is 0, 0, 0, 0, etc.

  # write new RAWs.csv
  filenm <- paste0(strNthSplit(afterLastSlash(ffpath), split='\\.', nth=1), 'pulEx.csv')

  cat('\t \t \t \t >>> Exporting new RAWs.csv to', filenm,'\n')

  data.table::fwrite(ffsd,
                     paste0(expfolder, whatSlash(ffpath), filenm),
                     row.names=FALSE) # RAWs for raw sorted
  # = all raw data in one csv file
  # nframes rows x 97 columns (time in seconds + 96 wells)
}
