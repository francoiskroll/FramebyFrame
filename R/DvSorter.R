###################################################
# ~~~ FramebyFrame package ~~~

# dvSorter

# function dvSorter to sort raw txt files from DanioVision
# for the most part, the function is a simpler version of vpSorter as DanioVision seems not to have ordering errors

# Francois Kroll 2024
# francois@kroll.be
###################################################

# function dvSorter(...) --------------------------------------------------

#' Sorts Raw DanioVision .txt Files
#'
#' @param ffDir
#' @param zt0
#' @param date0
#' @param time0
#' @param dayduration
#' @param pxwell
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom tibble add_column
#' @importFrom dplyr inner_join

dvSorter <- function(ffDir,
                     zt0='09:00:00',
                     date0=NA,
                     time0=NA,
                     dayduration=14,
                     pxwell=8280.75) {

  tictoc::tic()

  # check rawoutput folder (ffDir) is correct
  if(!dir.exists(ffDir))
    stop('\t \t \t \t >>> Error vpSorter: cannot find directory ', ffDir, 'please check carefully the path given.\n
    \t \t \t \t Maybe the path is correct but the formatting is not:\
         \t \t \t -- on Windows, a correct path would look like: C:\\\\Desktop\\\\myExperiment\\\\210907_01_myExp_rawoutput (the double \\\\ is an unfortunate R quirk)\
         \t \t \t -- on Mac, a correct path would look like: ~/Desktop/myExperiment/210907_01_myExp_rawoutput\
         \t \t \t Using the package here() to write the paths would simplify your life. Please find instructions in the FramebyFrame README. \n')

  # give zt0
  if (!is.character(zt0)) stop('\t \t \t \t Error vpSorter: something wrong about zt0 setting, give e.g. zt0="09:00:00" for 9 AM \n')

  # dayduration cannot be above 24
  if (!is.numeric(dayduration) | dayduration > 24)
    stop('\t \t \t \t Error vpSorter: something wrong about dayduration setting, give a number below 24, e.g. dayduration=14 \n')

  # ffDir: on Windows, pressing Tab key automatically writes the directory as ".../" but we need it to finish by "\\" or "\\\\"
  # function whatSlash in pathUtilities was written for this situation
  # if the last character is / and we are on Windows, replace by \\\\:
  if(substrEnding(ffDir, 1)=='/') {
    ffDir <- paste0( paste0(strsplit(ffDir, '')[[1]][1:(nchar(ffDir)-1)], collapse='') , whatSlash(ffDir))
    # if on Mac: will replace by / by /, i.e. no change
    # if on Windows: will replace by / by \\\\

    # also, if there is no slash at the end, add one. That will avoid another error
  } else if(! substrEnding(ffDir, 1) %in% c('/', '\\')) { # \\ is actually \, first \ is escape
    ffDir <- paste0( paste0(strsplit(ffDir, '')[[1]][1:nchar(ffDir)], collapse='') , whatSlash(ffDir))
    # if on Mac: will add /
    # if on Windows: will add \\\\
  }

  ### about paths;
  # here, we assume experiment is in folder YYMMDD_BX_...
  # folder name alone is:
  fdir <- levelUpPath(ffDir, upn=1, slash=whatSlash(ffDir))
  # so date_box should be:
  dtbx <- substr(fdir, 1, 9) # date box, e.g. 210218_11

  # build export file name:
  expfolder <-  parentFolder(ffDir, upn=1, slash=whatSlash(ffDir))
  # export folder = folder containing the folder with the raw files (assuming this is a good folder to put the output)

  # ffDir should include one file per well, each time with the complete experiment
  cat('\t \t \t >>> Sorting raw DanioVision files in directory', ffDir, '\n')

  ### FORMATTING
  # check formatting on just the first file
  # so we do not waste time doing it for each file individually
  cat('\t \t \t >>> Formatting check. \n')
  dvnames <- list.files(ffDir)
  # ! will likely import in the order file 1, 10, ... instead of file 1, 2, ..., so:
  dvnames <- naturalsort::naturalsort(dvnames)
  # note, should not matter in which order we import files as each contain the complete experiment

  # if a file is opened in Excel, there might be some hidden files like ~$...
  # make sure we do not try to import those
  dvnames <- dvnames[!startsWith(dvnames, '~$')]
  # also, there might be other stuff in the folder, so we will only import files that start with "Track"
  # that seems to be the standard name for DV files
  dvnames <- dvnames[startsWith(dvnames, 'Track')]

  ## now to import the first file:
  dvpath1 <- paste0(ffDir, dvnames[1])
  # first row of file gives number of header rows
  nheadrows <- as.numeric(read.csv(dvpath1, fileEncoding='UTF-16', sep=';', nrows=1, header=FALSE)[,2])
  cat('\t \t \t \t \t >>>', nheadrows,'header rows \n')

  # how many columns should we prepare?
  ncols <- max(count.fields(dvpath1, sep=';', blank.lines.skip=TRUE), na.rm=TRUE)

  # so now to import all header rows:
  dvhead <- read.csv(dvpath1, fileEncoding='UTF-16', sep=';', nrows=nheadrows, header=FALSE, col.names=sprintf('V%i', 1:ncols), fill=TRUE)

  # we get column names from header rows
  # it is the row starting with Trial time
  dvcols <- as.character(dvhead[which(dvhead$V1=='Trial time'),])

  # prepare which columns we should skip & which we should import
  # saves time when importing all the data
  # assume we skip all of them:
  colskips <- rep('NULL', ncols)
  # then change the ones we want to import
  # only ones we want to import are Recording time & Activity
  colskips[ which(dvcols %in% c('Recording time', 'Activity')) ] <- 'numeric'

  # where is well number written?
  # looking for header row "Arena ID"
  # some DV formats use coordinates e.g. E2 for "Arena name"
  # but "Arena ID" seems to always be a number
  wellrow <- which(dvhead$V1=='Arena ID')

  ## START TIME of the experiment
  # if user gave it manually, process it now
  if(!is.na(date0) & !is.na(time0)) {
    cat('\t \t \t >>> User gave date0 & time0 manually. \n')
    startts <- paste(date0, time0)
    startts <- lubridate::dmy_hms(startts)

  # if user did not give it, assume we take it from the DV file header
  } else {
    cat('\t \t \t >>> Reading experiment start time from the header of the first DanioVision file. \n')
    # in header rows, we are given the start time
    # ! this is when the user presses start but not necessarily when recording starts, if I understand correctly
    start0 <- dvhead[which(dvhead$V1=='Start time'), 2]
    # process it with lubridate
    start0 <- lubridate::dmy_hms(start0)

    # then we need to add 'recording after'
    delay <- lubridate::hms ( dvhead[which(dvhead$V1=='Recording after'), 2] )
    # lubridate's hms will process e.g. "+ 3:40:09.1" into "3H 40M 9.1S"

    # add delay to start0:
    startts <- start0 + delay

  }
  cat('\t \t \t \t >>> experiment start time is', as.character(startts), '\n')


  ### IMPORT ALL DATA
  # we checked formatting above so are ready to go ahead
  # we want to import each file one by one in a list of (typically) 96 slots, one per well
  # each file is the complete experiment for one well
  cat('\t \t \t >>> Importing all data. \n')
  pw <- lapply(1:length(dvnames), function(i) {
    cat('\t \t \t \t >>> imported file #', i, '/', length(dvnames), '\n')
    wed <- read.csv(paste0(ffDir, dvnames[i]), fileEncoding='UTF-16', sep=';', comment.char='"', skip=nheadrows, header=FALSE, col.names=dvcols, fill=TRUE,
                    colClasses=colskips)
  })
  # pw is per well, matches variable name in vpSorter
  # not importing all columns thanks to colClasses cuts import time by ~ 50%

  # add well names to the list
  # we know from formatting check which row has "Arena ID"
  # so Arena ID from each file
  wellids <- sapply(1:length(dvnames), function(i) {
    # read Arena ID header row
    arenaid <- read.csv(paste0(ffDir, dvnames[i]), fileEncoding='UTF-16', sep=';', skip=wellrow-1, nrows=1, header=FALSE, fill=TRUE)
    # extract just the well ID, e.g. 3
    # +1 because it starts counting at 0
    return( as.integer(arenaid[1,2])+1 )
  })
  # add the well IDs as names of the list
  names(pw) <- sprintf('f%i', wellids)


  # join data from each well by "Recording time"
  cat('\t \t \t >>> Joining data from every well in one dataframe. \n')
  bx <- pw %>%
    purrr::reduce(inner_join, by='Recording.time')
  # bx is for box data, matches variable name in vpSorter

  # edit column names
  colnames(bx) <- c('exsecs', names(pw))

  # removing pw now might help a bit with memory
  rm(pw)
  gc()


  ### now ready to convert activity data
  # will use a separate function that we can apply through columns
  cat('\t \t \t >>> Converting %Activity to pixel count. \n')

  # how many time columns?
  # I think guaranteed to be one at this stage, but to be safe
  timecols <- which(grepl("^f+[[:digit:]]", colnames(bx)))[1] - 1

  bxDV <- as.data.frame(sapply( (timecols+1):ncol(bx) , function(f) {

    cat('\t \t \t \t >>> converting well #', colnames(bx)[f],'/', ncol(bx)-timecols,'\n')

    return( convertDV(wed=bx[,f],
                      pxwell=pxwell) )
  }))

  # put back column names
  colnames(bxDV) <- colnames(bx)[(timecols+1):ncol(bx)]

  # put back time column
  bx <- bxDV %>%
    add_column(exsecs=bx$exsecs, .before=1)

  rm(bxDV)
  gc()

  #### add full clock timestamps to data

  cat('\t \t \t >>> Calculating full timestamps for each frame. \n')

  # we will write all the frame timestamps as full date/time eg. 2021-01-28 10:27:35 UTC
  bx <- bx %>%
    add_column(fullts = startts + lubridate::dseconds(bx$exsecs) , .before='exsecs') # add full timestamp column
  # takes first timestamp then add to it any number of seconds written in the frame-by-frame data
  # e.g. at 3000th frame, 2021-03-16 19:54:49 + 120 seconds = 2021-03-16 19:56:49

  # tell user about timestamps so can check they look correct
  cat('\t \t \t \t >>> First timestamp is ***', as.character(bx$fullts[1]), '*** \n')
  cat('\t \t \t \t >>> Last timestamp is ***', as.character(bx$fullts[nrow(bx)]), '*** \n \n')


  #### add Zeitgeber durations
  cat('\t \t \t >>> Calculating Zeitgeber durations, i.e. time in hours since 9AM day0. \n')

  # i.e. number of hours since first day0 9AM
  startdate <- lubridate::date(bx$fullts[1]) # get startdate of the experiment = date of the first timepoint

  bx <- bx %>%
    add_column(zhrs = as.numeric(difftime(bx$fullts, lubridate::ymd_hms(paste(startdate, zt0)), units='hours')),
               .before='exsecs')
  # Zeitgeber is time difference in hours since first ZT0


  #### find light transitions ####

  # sunsets and sunrises times (in number of hours since day0 sunrise at ZT0) for 10 days
  suns <- c(rbind(seq(dayduration, 720, 24), seq(24, 720, 24)))
  # typically dayduration = 14
  # so will go 14, 24, 38, 48 etc.

  # take only the ones we will find in the data
  # round down last timepoint (indeed, if e.g. experiment stops at 71.9 hours, there is no 72 hours sunrise)
  lastz <- floor(max(bx$zhrs))
  # experiment should always start between 0+ and before 14 so should not matter how we round start
  firstz <- floor(min(bx$zhrs))

  suns <- intersect(suns, firstz:lastz) # will give the sunsets/sunrises we expect to encounter in the data

  # find the transition frames
  # using function findLightTransitionFrame(...) from above

  if (length(suns) != 0) { # if no light transition (e.g. experiment lasted just a few hours), should skip this step
    # preallocate small dataframe transition number / sunset or sunrise / frame # / full timestamp / Zeitgeber duration / seconds of experiment
    ltcolnms <- c('transition_num', 'sunset_or_sunrise', 'frame', 'full_timestamp', 'zhrs', 'exsecs') # light transitions column names
    lt <- as.data.frame(matrix(nrow=length(suns), ncol=length(ltcolnms))) # lt for light transitions
    colnames(lt) <- ltcolnms
    lt$full_timestamp <- lubridate::ymd_hms(NA)

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
      tfra <- findLightTransitionFrame(bx$zhrs, round(suns[s])) # transition frame

      # fill it in
      lt[s, 'frame'] <- tfra

      # fill in full timestamp
      lt[s, 'full_timestamp'] <- bx[tfra, 'fullts']

      # fill in Zeitgeber duration
      lt[s, 'zhrs'] <- bx[tfra, 'zhrs']

      # fill in number of seconds since start of the experiment
      lt[s, 'exsecs'] <- bx[tfra, 'exsecs']

      # give a quick check to user
      cat('\t \t \t \t >>> Light transition #', s,
          ': ', toupper(lt[s, 'sunset_or_sunrise']), ' at frame ', tfra,
          ', clock = ', as.character(lt[s, 'full_timestamp']),
          ', Zeitgeber duration = ', lt[s, 'zhrs'], '\n',
          sep='')

    }

    # write light transitions
    outpath <- paste0(expfolder, whatSlash(ffDir), dtbx, '_lights.csv')
    data.table::fwrite(lt, file=outpath)
  } # closes "if suns is not length 0"


  ### remove NA frames
  # DV sometimes skip frames but it fills them with NA
  # I checked a few examples and it seems that if a frame is NA for one well, it is always NA for every well
  # so we can simply delete NA frames, which will be the same as Zebralab's framerate not being perfectly stable
  # to be safe, we will check every well for NAs
  # how many time columns?
  cat('\n \t \t \t >>> Removing NA frames. \n')
  timecols <- which(grepl("^f+[[:digit:]]", colnames(bx)))[1] - 1
  naframes <- apply(bx[,timecols:ncol(bx)], 2, function(col) {
    which(is.na(col))
  })
  # we get a list, one slot for each well, each slot lists NA frame indices for this well
  # we can simply pool all the indices and keep unique:
  naframes <- unique(as.vector(unlist(naframes)))

  # we now delete the NA frames
  bx <- bx[-naframes,]

  ### export as YYMMDD_BX_RAWs.csv
  exportname <- paste0(expfolder, whatSlash(ffDir), dtbx, '_RAWs.csv') # RAWs for raw sorted

  # now write export
  cat('\n \t \t \t >>> Exporting all frame-by-frame data from into', exportname, '\n')

  data.table::fwrite(bx,
                     exportname,
                     row.names=FALSE)
  # = all raw data in one csv file

  tocout <- tictoc::toc(quiet=TRUE) # toc output

  fps <- 1/mean(diff(bx$exsecs))
  cat('\t \t \t >>> Sorted around', round(nrow(bx)/fps/60/60, 1), 'hours of data in',
      round(as.numeric(tocout[[2]] - tocout[[1]])/60/60, 2), 'hours')

  ### we are done!


}



# function convertDV(...) -------------------------------------------------

# small helper function to convert one well's data from DanioVision %activity to delta pixel

# wed is one column of DV data from dataframe where columns = wells, rows = frames
# pxwell is number of pixels in one well
# for a 96-well plate: 8280.75 px (source: https://github.com/TahneeMa/DanioBehaviour)
# this is essentially saying that each well is a square 91 px * 91 px, so area is 91^2 = 8281 px

# DV data give % of pixels within each well which changed intensity between this frame and the previous one
# Viewpoint format is pixel count
# so DV does Activity% = (px_moved / px_well) * 100
# where px_well is total number of pixels in each well
# so to get pixel count, we want:
# px_moved = (px_total * Activity%) / 100
# so formula is
# px_moved = (8280.75 * Activity%) / 100

#' Title
#'
#' @param wed
#' @param pxwell
#'
#' @return
#' @export
#'
#' @examples

convertDV <- function(wed,
                      pxwell=8280.75) {

  # make sure we are given a simple vector of numeric
  wed <- as.numeric(unlist(wed))

  # now convert using formula
  wed <- sapply(wed, function(x) {
    return( floor((x * pxwell)/100) )
  })

  # unit represents number of pixels so I think it should be an integer
  # I think it makes sense to round down (floor) because e.g. 0.77 px is really below detection limit for 1 px, so 0
  return(wed)
}
