### Frame-by-Frame package ###
### sleep from frames ###

# main function sleepFromFrames(...) detects inactive bouts longer than 1 minute in frame-by-frame deltapx data

# full explanation:

# Rihel lab usually uses middur parameter from xls results file written by Zebralab
# middur is duration by minute (seconds/minute) spent above (or equal) Freezing parameter
# and below (or equal) Burst parameter
# framerate is typically 25 frames-per-second, so 25 frames * 60 seconds = 1500 frames in each minute
# data for each frame =
# number of pixels that changed intensity (pixel gray value > Sensitivity threshold, typically 20) vs previous frame
# Freezing is usually 3 deltapx / Burst is usually 200 deltapx
# for example, during minute 5 of tracking, fish 1 spent 1000 frames between 4--199 deltapx
# and 500 frames between 0 and 3 deltapx
# middur for this fish/that minute = 1500 - 500 = 1000 frames = 40 seconds/minute
# Rihel lab typically considers any minute (or more) spent at middur < 0.1 seconds/minute as sleep

# This approach works well and is easy to compute on the minute-binned data
# but a disadvantage is that it arbitrarily cuts periods of inactivity
# e.g. if fish is inactive for 30 seconds;
# then data switches to the next minute;
# then fish is inactive for another 30 seconds
# then middur will not be < 0.1 for a full minute,
# as the period of inactivity was artificially splitted between two minute bins
# therefore, some sleep bouts are missed with this period
# which is not a serious issue as the error is random and does not impact comparisons
# or in other words, each fish loses approximately the same number of sleep bouts

# nonetheless, a better solution is to use the frame-by-frame deltapx data
# and scan for periods of inactivity longer than a minute
# beyond not missing periods of inactivity,
# it has the extra advantage of not relying on Freeze and Burst thresholds.
# this makes the sleep amount unit easier (simply > 1 minute of deltapx = 0)
# and it ensures more consistency, as not all users set Freeze/Burst the same way


# function detectNaps(...) ------------------------------------------------

# converts frame-by-frame deltapx data into sequences of booleans

# TRUE = this frame was part of a nap

# FALSE = this frame was not part of a nap
# which can either be because the frame was active (deltapx > 0)
# or because it was inactive but the fish had moved sometimes in the (60*fps)-1 (typically 1499) preceding frames

# ffpath = full path to frame-by-frame deltapx data, typically YYMMDD_BX_RAWs.csv
# zthr_min = minimum number of minutes of inactivity for a period to be considered sleep
# recommended to leave as default 1 minute, cf. Prober et al, 2006

#' Title
#'
#' @param ffsource
#' @param woi
#' @param zthr_min
#' @param inaThr
#'
#' @return
#'
#' @examples
detectNaps <- function(ffsource,
                       dtbx=NA,
                       woi=NA,
                       zthr_min=1,
                       inaThr=0) {

  # did sleep detection settings changed since last time? -------------------

  # same logic for each parameter:
  # we ask: is there an object in Environment by the name e.g. last__woi?
  # if yes, is it the same as the current setting e.g. woi?
  # if no, then record that we need to start nap detection again
  # will use newDetection below

  # assume that user did not change the detection settings:
  newDetection <- FALSE
  # except if:
  if('last__woi' %in% ls(envir=.GlobalEnv)) {
    if (! identical(last__woi, woi)) {
      cat('\t \t \t \t >>> detectNaps: sleep detection setting woi was changed since last run! \n')
      newDetection <- TRUE
    }
  }

  if('last__zthr_min' %in% ls(envir=.GlobalEnv)) {
    if (! identical(last__zthr_min, zthr_min)) {
      cat('\t \t \t \t >>> detectNaps: sleep detection setting zthr_min was changed since last run! \n')
      newDetection <- TRUE
    }
  }

  if('last__inaThr' %in% ls(envir=.GlobalEnv)) {
    if (! identical(last__inaThr, inaThr)) {
      cat('\t \t \t \t >>> detectNaps: sleep detection setting inaThr was changed since last run! \n')
      newDetection <- TRUE
    }
  }


  # import frame-by-frame data ----------------------------------------------

  # detectNaps(...) is used both for making sleep traces and to calculate sleep parameters
  # for sleep traces: we want continuous frame-by-frame data, i.e. all of _RAWs.csv
  # for sleep parameters: we want the frame-by-frame data split in day/night windows, called `dn`

  # accordingly,
  # ffsource can either be
  # ffpath, i.e. path to _RAWs.csv
  # or
  # dn, i.e. ff split in a list of day/night windows by splitFramesbyDayNight(...)

  # if ffsource is a string, will assume we are given the path to _RAWs.csv
  # note, importRAWs skips import if was loaded already
  if (is.character(ffsource)) {

    if (substrEnding(ffsource, 4)=='.csv') { # need to put this as second If otherwise it checks the entire dataframes when given `dn`
      cat('\n \t \t \t \t >>> Running on complete timecourse...\n')
      ffordn <- importRAWs(ffpath=ffsource)

      # rest will be much simpler if put ff in a list with one element so we can run same commands whether using ff or dn
      ffordn <- list(ffordn)
      names(ffordn) <- 'all'
    }

    # if not, assume we are given `dn`
  } else {

    cat('\n \t \t \t \t >>> Running on frame-by-frame data split by time window...\n')
    ffordn <- ffsource # this is now dn

    # do we actually need to run detectNaps in full?
    # not if it was already ran on the same `dn` data during this R session
    # if object was recorded in Environment at the end of detectNaps() previously, it is called dnz_YYMMDD_BX

    # for the object's name, we want to get to dnz_YYMMDD_BX
    # we recorded YYMMDD_BX as dtbx prior in behaviourParameter
    dnzname <- paste0('dnz_', dtbx)

    # now, is there an object by the name of dnzname?
    # look in Environment (ls() lists all objects) if dnzname is there
    if(length(which(ls(envir=.GlobalEnv)==dnzname))==1 & !newDetection) { # if there is an object by this name AND sleep detection settings were not changed
      cat('\t \t \t \t >>> detectNaps() was ran on these data already with same detection settings, skipping detection... \n')
      return(get(dnzname)) # return the object already in Environment and we are done!
      # return call will stop execution of the entire function

    } else { # if it is not in Environment already
      cat('\t \t \t \t >>> detectNaps not ran on these data previously with same detection settings, proceeding to full nap detection... \n')
    }

  }


  # how many time columns ---------------------------------------------------

  # find how many time columns we have; i.e. how many columns before first fish data
  # below tests if string looks like f + integer
  # first true will be first data column, minus 1 gives number of time columns
  timecols <- which(grepl("^f+[[:digit:]]", colnames(ffordn[[1]])))[1] - 1


  # calculate framerate -----------------------------------------------------
  fps <- averageFramerate(ffordn[[1]]$exsecs)
  cat('\t \t \t \t >>> Frame rate is ***', round(fps, 2), '*** frames-per-second \n')

  # convert zthr_min from minutes to frames
  zthr <- round(zthr_min * 60 * fps) # typically 1 * 60 * 25 = minimum 1500 frames inactive before we call it sleep


  # rolling sum to find naps ------------------------------------------------

  ### big lapply to loop through windows ###
  # fdz will be a list that looks like ffordn
  # if given dn, it will be one element for each time window
  # if given ff, it will be one element for the entire experiment
  # in either case, dataframe stored at each slow is
  # time columns, then one column per fish
  # each row is a frame
  # each cell is TRUE = frame was part of a sleep bout // FALSE = frame was not part of a sleep bout


  fdz <- lapply(1:length(ffordn), function(win) { # note, if given ff then win is just the entire experiment

    cat('\n \t \t \t \t >>> Detecting naps for', names(ffordn)[win], 'data \n')

    ### smaller sapply to loop through fish ###
    # and do rolling sum etc. for each

    # set-up a progress bar
    nfis <- ncol(ffordn[[win]])-timecols # number of columns in the data, minus the time columns
    prg <- txtProgressBar(min=0, max=nfis, style=3, char='><> ')

    zzz <- data.table::data.table(sapply( (timecols+1):ncol(ffordn[[win]]) , function(w) {

      # cat('\t \t \t \t \t \t >>> detecting naps for well', colnames(ffordn[[win]])[w], '\n')

      # which fish are we at?
      setTxtProgressBar(prg, w-timecols) # update progress bar
      # w-timecols will give which fish we are at

      # get the data for this window, this fish
      fwf <- ffordn[[win]][,..w] # frame-by-frame data for one Window, one Fish

      # apply inactivity threshold if required
      # if() below is not necessary (when setting to 0, will not change the data) but might save a bit of time to genuinely skip that step when inaThr is set to 0
      if(inaThr > 0) {
        fwf[which(fwf>0 & fwf <= inaThr)] <- 0 # i.e. we replace by 0 any frame below inaThr (fwf>0 is not necessary but I find clearer)
        # so that in the rolling sum below they count as inactive
        # note it is below or equal
        # e.g. inaThr is 3, all px 1, 2, 3 are converted to 0
      }

      # `roll` stores rolling sum results for one window, one fish
      roll <- unlist(data.table::frollsum(fwf, n=zthr, fill=NA, algo='fast', align='right', na.rm=FALSE))
      # frollsum from data.table
      # align='right' means preceding rows are taken in the sum
      # e.g. if window is at frame 15 and zthr is 10, it covers rows 5 to 15
      # this is why the first `zthr` rows cannot be computed (e.g. for row 5, cannot go from row -5 to row 5)
      # so fill=NA, i.e. fills the first rows with NAs
      # for some reason it stores result in a list, hence unlist()

      # why do we do rolling sum?
      # we calculate, for every frame, the sum of the preceding 1500 frames
      # for one frame,
      # if that sum is a positive number: it means some frame in the preceding 1500 was positive
      # if that sum is null: it means the preceding 1500 were null,
      # i.e. the fish was inactive for at least the full preceding minute, i.e. it was asleep
      # so next all we have to do is replace any null frame by TRUE (= asleep) and any positive frame by FALSE (= not asleep)

      # ***
      # about algo='fast':
      # there are two options: 'fast' or 'exact'
      # 'fast' can generate tiny numbers (1e-14) when summing many 0s
      # 'exact' does not make this error, but takes much longer to run
      # solution is to keep 'fast' but simply test if sum is < 1 px
      # logic is: the 'quieter' 1-min possible is sum of all 0 except one frame at 1 px
      # so sum will be = 1
      # anything below 1 must represent these errors
      # can read here https://rdatatable.gitlab.io/data.table/reference/froll.html

      # now convert into asleep/not asleep booleans (see above for logic)
      # if sum of preceding `zthr` rows is 0, larva has been asleep for at least the entire preceding `zthr_min` (usually 1 min)
      # if sum of preceding `zthr` rows is positive, larvae has been active sometimes in the preceding `zthr_min` (usually 1 min)
      roll <- roll < 1
      # i.e. is the frame 0? (for every frame)
      # read why not ==0 above ***

      # how to interpret
      # 0 (now TRUE) represents 'frame was part of a nap',
      # i.e. at this frame, the fish had been inactive for more than `zthr` (typically 1500 frames, 1 min)
      # > 0 (now FALSE) represents 'frame was not part of a nap',
      # i.e. the fish had been active sometimes in the preceding `zthr` (typically 1500 frames, 1 min)

      # ! replace previous 'zthr' frames to TRUEs, retroactively
      # currently: first TRUE is NOT when the fish fell asleep, it is when the inactivity period has lasted for 1 minute
      # accordingly, definition should be 'retroactive',
      # i.e. the sleep bout starts when the fish stopped being active, not when it has been inactive for 1 minute already
      # so now we need to detect the first TRUE of each sleep bout, and replace the previous `zthr` (typically 1500 frames) by TRUE
      # diff to detect the transitions to TRUE (diff2 will give correct positions, see statsUtilities)
      retrost <- which(diff2(roll)==1) - (zthr-1) # these are the retroactive starts, i.e. when the fish first became inactive

      retroframes <- lapply(retrost, function(r) {
        r:(r+zthr-1) # for each real start, this gives the frames to replace
      })
      # we store this in a big list, where each element stores the retroactive frames for one sleep bout
      # note, retroactive frames include the first TRUE
      # we pool together all the frame indices to replace (unlist)
      # and finally replace in roll
      roll[unlist(retroframes)] <- TRUE

      # and we can replace any NA left at the start with FALSE
      # indeed, if they were part of a sleep bout they would have been replaced by TRUE
      # e.g. at frame 1503 fish has been inactive for 1 minute, so now the start of its data looks like NA, NA, NA, TRUE, ..., TRUE
      roll[is.na(roll)] <- FALSE

      return(roll)

    }))
    # fish-by-fish sapply above will return zzz, a data.table columns = 96 fish x rows = frames, elements are TRUE/FALSE

    # now back to the big lapply looping through windows

    # we need to add back the time columns
    fdzw <- cbind( ffordn[[win]][,1:timecols] , zzz ) # fdz for ff or dn (one window), zzz version (i.e. booleans asleep or not)

    # and bring back the column names
    colnames(fdzw) <- colnames(ffordn[[win]])

    # done, we return fdzw
    # and lapply will automatically add it to the list
    return(fdzw)

  })

  # put back the names on the list elements
  names(fdz) <- names(ffordn)


  # return result -----------------------------------------------------------

  # first, record the sleep detection settings we just used
  # so next time we can compare if user changed them
  assign('last__woi', woi, envir=.GlobalEnv)
  assign('last__zthr_min', zthr_min, envir=.GlobalEnv)
  assign('last__inaThr', inaThr, envir=.GlobalEnv)

  # data-wise, we now have the list fdz
  # if input was ff it has only one element which is the entire timecourse; we should return that one element as a dataframe
  # if input was dn it has one element per time window; we should return the whole list

  # as in the beginning, we will guess what was the input
  if (is.character(ffsource)) { # if string, assume we were given path to _RAWs.csv, so ff

    # return the full timecourse, remember we put it in a list of a single element, so return that one element
    return(fdz[[1]])

    # if not, check the object we were given was indeed called dn to be safe
  } else if (substitute(ffsource)=='dn') {

    # before returning, we will record object in Environment
    # so that next time it is called we can skip ahead and not run detectNaps() again
    assign(dnzname, fdz, envir=.GlobalEnv) # will store fdz in variable `dnzname`
    # dnzname was created at the beginning of detectNaps() when checking if object existed already in Environment

    return(fdz) # return the whole list
  }
} # and done!



# function summarySleepCourse(...) ----------------------------------------

# given frame-by-frame deltapx data,
# calculates binned sleep timecourse
# it calls function detectNaps() which returns for each fish a series of
# FALSE: this fish was not asleep at this frame
# TRUE: this fish was asleep at this frame (precisely: had been inactive for 1 minute or more)
# sleepFromFrames(...) counts the number of TRUE within epochs, typically 10 minutes
# e.g. fish had 3000 TRUE (asleep) frames from 10th minute up to 20th minute
# and converts these values to minutes, e.g. 3000 TRUE frames is 2 minutes asleep (assuming 25 frames-per-second)
# it writes this binned sleep timecourse to YYMMDD_BX_sleep.csv to used by other functions

# ffpath = full path to frame-by-frame data (typically YYMMDD_BX_RAWs.csv)
# genopath = full path to genotype file (typically YYMMDD_BXgenotype.txt)
# zthr_min = minimum number of minutes to call a period of inactivity 'sleep'; usually 1 minute
# epoch_min = epoch in minutes
# used as a more intuitive metric of sleep overtime; time asleep in each epoch
# usually 10 minutes; as in number of minutes asleep per 10 minutes
# writeSleepdata = whether or not to write sleep data
# it is a CSV file, columns = fish, rows = epochs, data = number of minutes spent asleep in `epo_min`-epochs (typically 10-min)
# filename will be YYMMDD_BX_sleep.csv (if ffpath was written correctly)

#' Title
#'
#' @param ffpath
#' @param genopath
#' @param zthr_min
#' @param inaThr
#' @param epo_min
#' @param smoothOrNo
#' @param smooth_npoints
#'
#' @return
#'
#' @examples
#' @importFrom data.table :=
summarySleepCourse <- function(ffpath,
                               genopath,
                               zthr_min,
                               inaThr,
                               epo_min=10,
                               smoothOrNo=FALSE,
                               smooth_npoints) {


  # check some settings -----------------------------------------------------

  # if smoothing is off, make sure smooth_npoints = 0
  if (! smoothOrNo) { smooth_npoints <- 0 }



  # did we run this already? ------------------------------------------------
  # build the filename it would be under
  # i.e. zzztc_YYMMDD_BX_epoXX_binXX

  ztcname <- paste0('zzztc_',
                    afterLastSlash(ffpath),
                    '_epo', epo_min,
                    '_smo', smooth_npoints)

  # ! also need to check the user did not change the sleep detection settings
  # assume the user did not change the settings:
  newDetection <- FALSE
  # and change if needed:
  if('last__zthr_min' %in% ls(envir=.GlobalEnv)) {
    if (! identical(last__zthr_min, zthr_min)) {
      cat('\t \t \t \t >>> summarySleepCourse: sleep detection setting zthr_min was changed since last run! \n')
      newDetection <- TRUE
    }
  }

  if('last__inaThr' %in% ls(envir=.GlobalEnv)) {
    if (! identical(last__inaThr, inaThr)) {
      cat('\t \t \t \t >>> summarySleepCourse: sleep detection setting inaThr was changed since last run! \n')
      newDetection <- TRUE
    }
  }

  # check if object is already in Environment
  # look in Environment (ls() lists all objects) if there
  if(length(which(ls(envir=.GlobalEnv)==ztcname))==1 & !newDetection) { # if it is there already and user did not change the sleep detection settings
    cat('\t \t \t \t >>> Sleep timecourse was calculated with same settings already, skipping... \n \n')
    return(get(ztcname)) # shortcut: return directly the object
  }
  # if not, then will proceed below

  # switch to frame asleep/awake with function detectNaps(...) --------------

  ffz <- detectNaps(ffsource=ffpath,
                    zthr_min=zthr_min,
                    inaThr=inaThr)



  # calculate framerate -----------------------------------------------------

  fps <- averageFramerate(ffz$exsecs)

  # convert epo_min from minutes to frames
  epo <- epo_min * 60 * round(fps)


  # how many time columns? --------------------------------------------------

  # i.e. how many columns before first fish data
  # below tests if string looks like f + integer
  # first true will be first data column, minus 1 gives number of time columns
  timecols <- which(grepl("^f+[[:digit:]]", colnames(ffz)))[1] - 1


  # sum in epochs -----------------------------------------------------------
  # by summing within bin
  # ! actual binning, not rolling window
  # eg. epo is 10min >> minute 1 to 10; 11 to 20 etc.

  # number of rows need to divisible by bin size (epoch)
  # can crop from the start, as habituation period not usually analysed anyway
  cat('\t \t \t \t >>> Trimming first ***', round((nrow(ffz) %% epo)/fps,0),
      '*** seconds of data to make it divisible by size of bin \n')

  ffzcrop <- ffz[- (1:(nrow(ffz) %% epo)) ,]
  # modulo %% gives 'surplus' of division, eg. 10 %% 3 = 1 (as divide 9 / 3, and left 1)
  # so from fr, trim the rows before surplus + 1 (= trim from row1 to row surplus+1)

  cat('\t \t \t \t >>> Summing in bins of ***', round(epo/fps/60,1), '*** minutes each \n')

  # binning

  # bin asleep/awake frames data
  ffzb <- data.table::data.table(apply(ffzcrop[, (timecols+1) : ncol(ffzcrop) ], # skip time columns
                           2,
                           function(x) colSums(matrix(x, nrow=epo), na.rm=TRUE)))
  # colSums is the core of the function: it sums asleep/awake frames within epo, typically 10 min * 60 sec * 25 fps
  # TRUE is represented as 1, FALSE as 0, so summing gives the number of TRUE, i.e. the number of frames spent asleep

  cat('\t \t \t \t >>> Binning reduced number of timepoints from ***', nrow(ffz), '*** to ***', nrow(ffzb), '*** \n')


  # putting back timestamps
  # I think what makes most sense is to take each bin'th value from ffzcrop
  # that means each timestamp = the timestamp of the latest frame of that bin
  # e.g. a bin that contains frame1 to 100 will get the timestamp of frame100

  tsrows <- seq(epo, nrow(ffzcrop), epo) # which rows to take the latest frame's timestamp from
  # check it makes sense
  if(length(tsrows) != nrow(ffzb)) stop('\t \t \t \t >>> Error: Number of timestamps does not match the number of rows after binning')

  ffzb <- cbind(ffzcrop[tsrows, 1:timecols ], ffzb) # take first few time columns

  # each datapoint is now the number of frames spent asleep in that epoch (bin)
  # allowed values are
  # 0 = fish was constantly not-asleep during that epoch
  # or any positive integer below epoch size
  # e.g. epoch is 15000 frames (10 minutes) and zthr is 1500 frames (1 minute)
  # fish could be asleep for 1 minute (below would not be called sleep) up to 10 minutes (cannot be asleep for 11 minute during 10 minutes)
  # ! values below 1 minute (1500 frames) are also possible, they indicate a 'nap' that was splitted between two 10-minute bin
  # e.g. fish is asleep 15 seconds at the end of a 10-minute epoch; then it switches to the next 10-minute epoch; and it is asleep for another 50 seconds
  # >> fish was inactive for a continuous 15 + 50 seconds = 65 seconds, so called sleep; but appears as 15 seconds/10 minute then 50 seconds/10 minute in data

  # cf. above, check there is no value over epo
  if (sum(ffzb[, (timecols+1) : ncol(ffzb)] > epo) != 0)
    stop('\t \t \t \t Error >>> After binning in epochs, some values are larger than the epoch size, which does not make sense \n')


  # convert frames to minutes -----------------------------------------------

  # time spent asleep within epoch is now in frames; which is not the most convenient unit
  # convert it in minutes
  ffzb[, (timecols+1) : ncol(ffzb) ] <- ffzb[, (timecols+1) : ncol(ffzb) ] / round(fps) / 60


  # write sleep data --------------------------------------------------------
  # so can be used by someone who would want to plot themselves

  # what would be the best way to label the genotypes for this person?
  # I do not know if Python or MatLab would usually work by pivotting to long format too
  # maybe second row is good for now
  # returning fXX_grp colnames would require the person to split the strings
  # with second row, can always just delete it and assign genotype themselves if not convenient

  # are we looking at box1 or box2 data?
  # check first fish ID in txf to tell whether looking at box1 or box2
  # (only really a question when running parallel boxes)

  firstID <- colnames(ffzb)[which(grepl("^f+[[:digit:]]", colnames(ffzb)))[1]]
  # regex above looks for column names which are f + digit and takes the first one

  if (firstID == 'f1') {
    boxnum <- 1
    cat('\t \t \t \t >>> Detected BOX1 data \n')
  } else if (firstID == 'f97') {
    boxnum <- 2
    cat('\t \t \t \t >>> Detected BOX2 data \n')
  } else {
    stop('\t \t \t \t >>> Error when detecting which box was ran. First fish ID is not f1 or f97 as expected, but', firstID, '\n')
  }

  # prepare genotype file
  geno <- importGenotype(genopath=genopath)
  geno <- fillGenotype(geno=geno, boxnum=boxnum)

  # add groups as second row
  # assignGenotype will return NA for time columns, which is what we want
  genorow <- as.character(assignGenotype(colnames(ffzb), geno=geno))

  # now add this row as first one (under column name)
  ffzbexp <- rbind(genorow,
                   as.data.frame(ffzb))

  # build filename
  zzztcpath <- paste0(beforeLastSlash(ffpath) , 'zzztc_', beforeLastUnderscore(afterLastSlash(ffpath)) ,
                      '_epo', epo_min, '.csv')

  cat('\t \t \t \t >>> Writing sleep timecourse before smoothing as',  afterLastSlash(zzztcpath), '\n')
  cat('\t \t \t \t \t in folder',  beforeLastSlash(zzztcpath), '\n')
  cat('\t \t \t \t \t filename is zzztc _ YYMMDD _BX _ epo `epoch in minutes` .csv', '\n')

  # export
  data.table::fwrite(ffzbexp, file=zzztcpath)


  # how many time columns? --------------------------------------------------
  # in ffzb
  # find how many time columns we have; i.e. how many columns before first fish data
  # below tests if string looks like f + integer
  # first true will be first data column, minus 1 gives number of time columns
  timecols <- which(grepl("^f+[[:digit:]]", colnames(ffzb)))[1] - 1


  # smooth if needed --------------------------------------------------------

  if (smoothOrNo) {

    # by rolling average

    cat('\t \t \t \t >>> Smoothing with rolling average of window size ***', smooth_npoints, '*** rows \n')

    # smoothing
    # smoothing with rolling average will give back nrow - (npoints - 1)
    # eg. smooth 10 datapoints with npoints = 3, you get 8 datapoints back
    # correct way to deal with zth column: delete first npoints - 1 timepoints
    # eg. https://machinelearningmastery.com/moving-average-smoothing-for-time-series-forecasting-python/

    # copy zzz to zs = zzz smoothed
    # zzz left unedited
    zs <- data.table::copy(ffzb) # note here copy(), a data.table quirk
    # if we simply copy like zs <- ffzb,
    # data.table saves memory by not actually copying the object but simply pointing to the original
    # so if we now modify zs, zzz is also modified (zs is only a pointer)
    # by saying copy(), we explicitly tell data.table to *really* copy it

    # smooth each well's data and keep it aside
    smo <- data.table::data.table(sapply( (timecols+1) : ncol(zs), function(w){ # skip time columns

      cat('\t \t \t \t \t \t >>> smoothed well', colnames(zs)[w], ' \n')

      unlist(data.table::frollmean(zs[, ..w], n=smooth_npoints, hasNA=TRUE, na.rm=TRUE))}))


    # replace data in zs with the smoothed from above
    zs[, ( (timecols+1) :ncol(zs)) := smo]

    # delete smo
    rm(smo)

    # first `npoints` rows will be NA (expected from rolling average)
    # I think best is to trim them, or risk being handled as 0 when summing in bins
    cat('\t \t \t \t >>> Trimming first', smooth_npoints-1, 'rows \n')
    zs <- zs[- (1 : (smooth_npoints-1)) , ]

    cat('\t \t \t \t >>> Smoothing done \n')

  } else {
    cat('\t \t \t \t >>> No smoothing \n')
    # keep zzz as zs
    zs <- copy(ffzb)
  }



  # record in Environment ---------------------------------------------------
  # so can skip if ran again
  assign(ztcname, zs, envir=.GlobalEnv) # will store zs in variable ztcname
  # ztcname was built at the beginning


  # return zs ---------------------------------------------------------------
  return(zs)

}
