# summaryActivityCourse(...) ----------------------------------------------

#' Title
#'
#' @param ffpath
#' @param genopath
#' @param smoothOrNo
#' @param smooth_nsecs
#' @param binOrNo
#' @param bin_nsecs
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom data.table :=

summaryActivityCourse <- function(ffpath,
                                  genopath,
                                  smoothOrNo=TRUE,
                                  smooth_nsecs=30*60,
                                  binOrNo=TRUE,
                                  bin_nsecs=10*60) { # opens function


  # check some settings -----------------------------------------------------

  # if smoothing is off, make sure smooth_nsecs = 0
  if (! smoothOrNo) { smooth_nsecs <- 0 }

  # if binning is off, make sure bin_nsecs = 0
  if (! bin_nsecs) { bin_nsecs <- 0 }

  # so that filename we write matches settings


  # did we run this already? ------------------------------------------------
  # build the filename it would be under
  # i.e. acttc_YYMMDD_BX_smoXX_binXX.csv

  # below: acttc_YYMMDD_BX_smoXX_binXX
  tcname <- paste0('acttc_',
                   substr(afterLastSlash(ffpath), 1, 9),
                   '_smo', smooth_nsecs,
                   '_bin', bin_nsecs)

  # below: i.e. acttc_YYMMDD_BX_smoXX_binXX.csv
  tccsv <- paste0('acttc_',
                  substr(afterLastSlash(ffpath), 1, 9),
                  '_smo', smooth_nsecs,
                  '_bin', bin_nsecs,
                  '.csv')

  # full path
  tcpath <- paste0(beforeLastSlash(ffpath), tccsv)

  # first check if object is already in Environment
  # look in Environment (ls() lists all objects) if there
  if(length(which(ls(envir=.GlobalEnv)==tcname))==1) { # if it is there already
    cat('\t \t \t \t >>> Activity timecourse was calculated with same settings already, skipping... \n \n')
    return(get(tcname)) # shortcut: return directly the object
  }

  # if not in Environment, check also on drive
  # could be there if was ran during a previous R session

  # fullpath it would be under is tcfpath, built above
  # list all files in ffpath folder
  # all files in folder
  allfiles <- list.files(beforeLastSlash(ffpath))

  # if tcpath is in the folder
  if (tccsv %in% allfiles) {

    cat('\t \t \t \t >>> Summary timecourse with same settings found in folder, will use it as shortcut... \n \n')

    # import it
    fsbin <- data.table::fread( list.files(beforeLastSlash(ffpath), full.names=TRUE)[which(allfiles==tccsv)] )
    # as fsbin, i.e. final result of function

    # remove genotype row we added
    fsbin <- fsbin[-1,]

    # because of genotype row, every data column was converted to string
    # correct this
    fcol1 <- which(grepl("^f+[[:digit:]]", colnames(fsbin)))[1] # position of first data column
    # convert these columns to numeric and replace in fsbin
    fsbin[,fcol1:ncol(fsbin)] <- as.data.frame(sapply(fsbin[,fcol1:ncol(fsbin)], as.numeric))

    # and return it
    return(fsbin)
  }


  # import frame-by-frame data ----------------------------------------------
  ff <- importRAWs(ffpath=ffpath)


  # calculate framerate -----------------------------------------------------
  fps <- averageFramerate(ff$exsecs)
  cat('\t \t \t \t >>> Experiment lasted ***', round(nrow(ff)/fps/60/60,0), '*** hours \n')


  # how many time columns? --------------------------------------------------

  # find how many time columns we have; i.e. how many columns before first fish data
  # below tests if string looks like f + integer
  # first true will be first data column, minus 1 gives number of metadata columns
  timecols <- which(grepl("^f+[[:digit:]]", colnames(ff)))[1] - 1


  # smooth if needed --------------------------------------------------------

  if (smoothOrNo) {

    # by rolling average

    # number of rows of rolling window for smoothing
    npoints <- round(smooth_nsecs * fps)

    cat('\t \t \t \t >>> Smoothing with rolling average of window size ***', npoints, '*** rows \n')

    # smoothing
    # smoothing with rolling average will give back nrow - (npoints - 1)
    # eg. smooth 10 datapoints with npoints = 3, you get 8 datapoints back
    # correct way to deal with zth column: delete first npoints - 1 timepoints
    # eg. https://machinelearningmastery.com/moving-average-smoothing-for-time-series-forecasting-python/

    # smooth each well's data and keep it aside
    smo <- data.table::data.table(sapply(4:ncol(ff), function(w){ # 4:ncol() because first 3 columns are timestamps

      cat('\t \t \t \t \t >>> smoothing well', colnames(ff)[w], '\n')

      unlist(data.table::frollmean(ff[, ..w], n=npoints, hasNA=TRUE, na.rm=TRUE))}))

    # copy fr to frs = fr smoothed
    # replace data with the smoothed from above
    ffs <- data.table::copy(ff) # note here copy(), a data.table quirk
    # if we simply copy like ffs <- ff,
    # data.table saves memory by not actually copying the object but simply pointing to the original
    # so if we now modify ffs, ff is also modified (ffs is only a pointer)
    # by saying copy(), we explicitly tell data.table to *really* copy it
    ffs[, ( (timecols+1) :ncol(ffs)) := smo]

    # delete smo
    rm(smo)

    # first `npoints` rows will be NA (expected from rolling average)
    # I think best is to trim them, or risk being handled as 0 when summing in bins
    cat('\t \t \t \t >>> Trimming first', npoints-1, 'rows \n')
    ffs <- ffs[-(1:(npoints-1)),]

    cat('\t \t \t \t >>> Smoothing done \n')

  } else {
    cat('\t \t \t \t >>> No smoothing \n')
    ffs <- ff
  }

  # either way, do not need ff anymore
  rm(ff)
  gc()



  # bin if needed -----------------------------------------------------------

  if (binOrNo) {

    # by summing within bin

    # number of rows inside each bin for binning
    bin <- bin_nsecs * round(fps) # i.e. how many frames in each bin

    # number of rows need to divisible by bin size
    # in most cases, probably best to crop from the start as they will usually be hands etc.
    cat('\t \t \t \t >>> Trimming first ***', round((nrow(ffs) %% bin)/fps,0),
        '*** seconds of data to make it divisible by size of bin \n')

    # if we need to crop, do it now
    if( (nrow(ffs) %% bin) != 0) {
      fscrop <- ffs[- (1:(nrow(ffs) %% bin)) ,]
    } else {
      fscrop <<- data.table::copy(ffs)
    }
    # modulo %% gives 'surplus' of division, eg. 10 %% 3 = 1 (as divide 9 / 3, and left 1)
    # so from ffs, trim the rows before surplus + 1 (= trim from row1 to row surplus+1)

    # need to do within an If condition, because if modulo result is 0 (i.e. we do not need to crop)
    # it would still remove one row

    cat('\t \t \t \t >>> Downsampling in bins of ***', round(bin_nsecs/60,1), '*** minutes each \n')

    # binning
    fsbin <- data.table::data.table(apply(fscrop[,-(1:timecols)], # only bin actual data, ignore first column will are fullts, zhrs, exsecs)
                              2,
                              function(x) colSums(matrix(x, nrow=bin), na.rm=TRUE))) # ? frollsum from data.table could do the job faster?

    cat('\t \t \t \t >>> Binning reduced number of timepoints from ***', nrow(ffs), '*** to ***', nrow(fsbin), '*** \n')

    # putting back timestamps
    # I think what makes most sense is to take each bin'th value from fscrop
    # that means each timestamp = the timestamp of the latest frame of that bin
    # e.g. bin containing frame1 to 100 will get the timestamp of frame100

    tsrows <- seq(from=bin, to=nrow(fscrop), by=bin) # which rows to take the latest frame's timestamp from
    # check it makes sense
    if(length(tsrows) != nrow(fsbin))
      stop('\t \t \t \t >>> Error: Number of timestamps does not match the number of rows after binning \n')


    fsbin <- cbind(fscrop[tsrows, 'fullts'], fscrop[tsrows, 'zhrs'], fscrop[tsrows, 'exsecs'], fsbin)

  } else {
    cat('\t \t \t \t >>> No binning \n')
    fsbin <- ffs
  }


  # either way, do not need ffs anymore
  rm(ffs)
  gc()



  # export smoothed/binned timecourse ---------------------------------------
  # so can be used by someone who would want to plot themselves

  # what would be the best way to label the genotypes for this person?
  # I do not know if Python or MatLab would usually work my pivotting to long format too
  # maybe second row is good for now
  # returning fXX_grp colnames would require the person to split the strings
  # with second row, can always just delete it and assign genotype themselves if not convenient

  # are we looking at box1 or box2 data?
  # check first fish ID in txf to tell whether looking at box1 or box2
  # (only really a question when running parallel boxes)

  firstID <- colnames(fsbin)[which(grepl("^f+[[:digit:]]", colnames(fsbin)))[1]]
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
  genorow <- as.character(assignGenotype(colnames(fsbin), geno=geno))

  # now add this row as first one (under column name)
  fsbinexp <- rbind(genorow,
                    as.data.frame(fsbin))

  # write fsbinexp
  # full path was built above
  cat('\t \t \t \t >>> Writing smoothed/binned activity timecourse as', afterLastSlash(tcpath),'\n')
  cat('\t \t \t \t \t in folder', beforeLastSlash(tcpath) ,'\n')
  cat('\t \t \t \t \t filename is acttc _ YYMMDD _ BX _ smo `seconds smoothing` _ bin `seconds binning` . csv \n')
  data.table::fwrite(fsbinexp, tcpath)


  # save fsbin to Environment -----------------------------------------------
  # so can skip if ran again

  assign(tcname, fsbin, envir=.GlobalEnv) # will store fsbin in variable tcname


  # return fsbin ------------------------------------------------------------
  return(fsbin)

} # closes function
