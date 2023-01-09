###################################################
# ~~~ FramebyFrame package ~~~

# VpSorter

# function VpSorter to sort raw xls files

# Francois Kroll 2022
# francois@kroll.be
###################################################


# VpSorter was previously a script, which went up to v15
# so the present function counts as v16

# -------------------------------------------------------------------------

# VERSION HISTORY
# (you probably do not need to read this)

# v0: imports all the raw xls files and bind them together
# can be 100+ Gb. Changing Mac/Windows/R settings allow R to load everything thanks to virtual RAM
# but then runs into a hard limit of maximum number of rows (I think ~ 2.1 billion rows max; eg. matrix(nrow=2150000000, ncol=1) is too big)
# so; maximum number of files that can be handled (1M rows/file) is 2,000,000,000 / 1,000,000 = 2000 files

# v1: box by box
# easy way to cut number of rows by half is to process each box separately
# in practice: only import from xls files rows for well 1>>96, then 97>>192
# ! had to delete bit where it compares frame rate between boxes, probably a good idea to write fps per well so can manually check, can even draw a quick plot
# ! same, had to delete bit where it compares timepoints between boxes; may be think of a way to still check this?

# v2: less writing all data
# moved some operations to initial fread's lapply
# 1- only reads type==101 (v1 was excluding those later, which was writing again all the data)
# 2- converts time to seconds (again, easier to do it on file while it is being imported than later)
# 3- already teases apart by well and append to list pw; so when done reading the files pw is already created
# replaced all write.csv to fwrite (data.table version, much faster)

# v3: mostly improved 3- from above
# while reading the data, it builds pw as a list of list
# 96 elements = one per well
# each is a list of number of files elements
# so goes: well1: data from file1, file2, ...; well2: data from file1, file2, ...
# (problem with v2 was that for each new file,
# it was appending its data to the well dataframe in pw, so it was getting incredibly slow after a thousand or so files)

# v4: reads from zip archive of xls files

# v5
# option to skip splitting data in LD and free-running folders
# for cases where only LD
# or simply want to get all the fixed raw files in one folder
# testMode option; if ON, only process the first 4 files as a test before running it on all of the xls files

# v6
# see section fix time column
# now uses column abstime to sync all timepoints

# v7
# writes a Markdown log file
# see function SpeaknRecord
# batchMode to run script through Vp_Sorter_BATCH (source() calls)

# v8
# from v6: fix time column by doing a complete inner_join of time columns by abstime
# it appears it is too much to ask for for full experiments, does not throw error but gets stuck for many hours, I guess memory saturates
# alternative solution: only inner_join times of well 1 and well 96, should always give the same result

# v9
# throwing new memory errors, improved a little bit last part about finding the row which corresponds to the transition to the free-running experiment

# v10
# for LD + free-running experiments; exports all data in CSV, clears memory, then re-imports only LD, exports raw xls files for LD; and same for free-running data
# avoids keeping all data in tall format

# v11
# added FORMATTING CHECKS section below to allow some flexibility re formatting of the xls files.
# There are at least two different formats of the xls files depending on the lab/system, even in the new Zebralab
# also changed slightly the code importing each xls file so it skips rows that says 'BACK_LIGHT'. I think these appear when Chronolog is ON in Zebralab.
# (see 'v11: found some rows[...]' for complete explanation)

# v12
# improved FORMATTTING CHECKS section
# read.table to import the first file should be more flexible before we know its formatting
# code then looks in the column names of the first file for the columns we want (abstime, time, etc.)
# then when importing all the files we 'select' the columns we want (select=cols2take) instead of dropping the ones we do not want

# v13
# deleted many things to make it simpler, including
# test mode
# processing LD and free running separately
# (lab will probably not use those functions so better to keep it simple)
# made it agnostic to number of wells, tested with 24-well plate
# ! will assume same type of plate in both boxes

# v14
# adapted for boxes in Fish Facility (previous Zebrabox model)

# v16
# converted VpSorter to a function within the FramebyFrame package



# -------------------------------------------------------------------------



# function to print to console as cat() + record to a Markdown logfile
#' Speak and Record to Log File
#'
#' @param ...
#' @param header
#' @param warning
#'
#' @return
#'
#' @examples

SpeaknRecord <- function(..., header=0, warning=FALSE) {
  # eg. SpeaknRecord('hey', 'ho')

  # if logfile does not exist yet, create it
  if(!exists('logfile')) {
    # preallocate as an empty dataframe
    logfile <- as.data.frame(matrix(ncol=1, nrow=0))
  }

  # now add the entry to the logfile, i.e. add a row to the dataframe
  if(header==0){
    record <- paste(..., '  ') # need to add 2 spaces to finish the line
    logfile <<- rbind(logfile, record) # record 'hey ho' to logfile
  } else if(header==1){
    record <- paste('#', ...) # add Markdown header 1 (biggest title)
    logfile <<- rbind(logfile, record)
  } else if(header==2){
    record <- paste('##', ...) # add Markdown header 1 (biggest title)
    logfile <<- rbind(logfile, record)
  } else if(header==3){
    record <- paste('###', ...) # add Markdown header 1 (biggest title)
    logfile <<- rbind(logfile, record)
  } else if(header==4){
    record <- paste('###', ...) # add Markdown header 1 (biggest title)
    logfile <<- rbind(logfile, record)
  } else if(header==5){
    record <- paste('###', ...) # add Markdown header 1 (biggest title)
    logfile <<- rbind(logfile, record)
  } else{
    cat('\t \t \t \t >>> SpeaknRecord: header can only be 0, 1, 2, 3, 4, 5 \n')
  }

  speech <- paste('\t \t \t \t >>> ', ..., '\n') # prepare message to send to console

  if(warning==FALSE){
    cat(speech) # send message to console
  } else if(warning==TRUE){
    message(speech)
  }
}


# -------------------------------------------------------------------------

# Which generation of boxes did you use?

# 1 if boxes in fish facility
# 2 if boxes in Rockefeller basement

# Running two boxes in parallel or not?
# if two boxes in parallel, twoBoxMode <- TRUE
# if a single box, twoBoxMode <- FALSE
# precisely, the question is: do raw .xls files contain data from two boxes or a single one?


# Which box of the pair are you running?
# ! only 1 or 2 here, as in first or second box
# Do you have only one box? Then boxnum <- 1

# Do you want to export the sorted .xls files?
# It will export the _RAWs.csv file which is what you need for FramebyFrame package
# but you may want to also get the sorted .xls files, i.e. as if Zebralab did not make the errors
# they can be given to Vp_Extract.m from Marcus Ghosh, for example

# -------------------------------------------------------------------------

# INPUT

# create an experiment folder, named YYMMDD_BOX1_BOX2_XXX
# (preferably on D: drive as there is more space)
# eg. 210302_12_13_PSEN1
# in the experiment folder, place the ZebraLab XLS results file
# it should be called YYMMDD_BOX1_BOX2_XXX.xls
# eg. 210302_12_13_PSEN1.xls
# if raw xls files are zipped, unzip them
# they should all be in one folder, typically called YYMMDD_BOX1_BOX2_XXX_rawoutput; this folder should be inside the experiment folder created above
# eg. in folder 210302_12_13_PSEN1, there is folder 210302_12_13_PSEN1_rawoutput, which contains all the raw xls files

# Note; Vp_Sorter assumes time column in xls files is in microseconds (i.e. ~ 400000 microseconds/frame if 25 frames-per-second)


# -------------------------------------------------------------------------

# DESCRIPTION

# in raw xls files, rows should be well 1 to 192 from timepoint t, then well 1 to 192 for timepoint t+1, etc.
# However, ZebraLab makes ordering mistakes, where it does not respect well order 1 >> 192
# Vp_Extract.m (from Marcus Ghosh) deals with this by deleting windows where the order is not respected.
# This solution is satisfactory with the previous model of the Zebrabox as errors usually happen only at the beginning and end of experiments.
# It does not work with the newer model of the Zebrabox because it makes errors continuously.
# The correct well 1 >> 192 is almost never respected, the rows are basically shuffled.
# Present script Vp_Sorter correctly re-orders the data by looking at the timestamps. It does not throw any data.

# Note; Vp_Sorter runs on one box at a time. This makes it easier on memory.

# Note; Vp_Sorter currently *only* handles data from the newer Zebralab software, it does not work on data from the previous model.
# for data from the previous model (boxes 1-9 in the Rihel lab), please continue using Vp_Extract.m

# Note; Vp_Sorter was created to handle 1+-week experiments where the first 3 days would be LD cycles, followed by 4-5 days of free-running conditions
# for standard LD experiments, you can easily turn this option OFF below, but keep this in mind as it explains some of the comments/code

# Are you running this on another Windows machine than the Rihel lab common computer?
# I would suggest trying the script first, then if it is throwing memory errors (typically 'cannot allocate a vector of size...');
# there are some Windows settings I suggest changing and that should help avoiding them
# find them in comments below (look for 'memory intensive step')


# -------------------------------------------------------------------------

# OUTPUT

# Vp_Sorter will create:

# 1- RAW CSV file with all frame-by-frame data
# each column = one well, each row = one timepoint

# 2- raw xls files re-written, with correct order (well 1 >> 192), in a folder called rawoutputFixed
# the 'fixed' raw xls files can then be input to Vp_Extract.m

# 3- a README Markdown log file
# it is useful to read how the run went; it also gives quality check information such as frame-rate or number of hours of data



#' Sorts RAW .xls Files
#'
#' @param ffDir
#' @param boxGen
#' @param twoBoxMode
#' @param boxnum
#' @param exportXlsOrNo
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom tibble add_column
#' @importFrom dplyr inner_join

vpSorter <- function(ffDir,
                     zebpath=NA,
                     boxGen=2,
                     twoBoxMode=TRUE,
                     boxnum=1,
                     zt0='09:00:00',
                     date0=NA,
                     time0=NA,
                     dayduration=14,
                     exportXlsOrNo=FALSE) {


  #### check settings make sense ####
  # vpSorter takes a while to run so need to be thorough so we do not throw an error after the user waited for an hour

  # about start of the experiment,
  # not giving enough information:
  if (is.na(zebpath)) {
    if(is.na(date0)) stop('\t \t \t \t >>> Error vpSorter: if not giving zebpath, you will need to enter manually date0 as DD/MM/YYYY, e.g. date0="28/01/2021" \n')
    if(is.na(time0)) stop('\t \t \t \t >>> Error vpSorter: if not giving zebpath, you will need to enter manually time0 as HH:MM:SS, e.g. time0="10:27:35" \n')
  }

  # giving too much information:
  if (!is.na(date0) & !is.na(time0)) {
    if(!is.na(zebpath)) {
      message('\t \t \t \t Warning vpSorter: you should not give date0 and time0 if you have a Zebralab XLS file to be used as source for start date/time. Will trust what the Zebralab XLS file says. \n')
      date0 <- NA
      time0 <- NA
    }
  }

  # boxGen is 1 or 2
  if (! boxGen %in% c(1, 2)) stop('\t \t \t \t Error vpSorter: boxGen can only be 1 or 2, e.g. boxGen=2 \n')

  # twoBoxMode is logical
  if (!is.logical(twoBoxMode)) stop('\t \t \t \t Error vpSorter: twoBoxMode can only be TRUE or FALSE, e.g. twoBoxMode=TRUE \n')

  # give zt0
  if (!is.character(zt0)) stop('\t \t \t \t Error vpSorter: something wrong about zt0 setting, give e.g. zt0="09:00:00" for 9 AM \n')

  # dayduration cannot be above 24
  if (!is.numeric(dayduration) | dayduration > 24)
    stop('\t \t \t \t Error vpSorter: something wrong about dayduration setting, give a number below 24, e.g. dayduration=14 \n')

  # exportXlsOrNo is logical
  if (!is.logical(exportXlsOrNo)) stop('\t \t \t \t Error vpSorter: exportXlsOrNo can only be TRUE or FALSE, e.g. exportXlsOrNo=FALSE \n')


  #### folder with xls files ####

  # ask user where the folder with all the xls files is
  cat('\t \t \t \t >>> Sorting RAW .xls files in directory', ffDir, '\n')

  # prepare export paths
  # folder name alone is:
  fdir <- levelUpPath(ffDir, upn=1, slash=whatSlash(ffDir))

  # find date_box:
  if (boxnum==1){
    dtbx <- substr(fdir, 1, 9) # date box for box1, eg. 210218_11
  } else if (boxnum==2){
    dtbx <- paste(substr(fdir, 1, 6), # date box for box2, eg. 210218_11
                  substr(fdir, 11, 12), sep='_')
  } else {
    stop('\t \t \t \t >>> Box number can only be 1 or 2. Did you write something else? \n')
  }

  SpeaknRecord('EXPERIMENT', dtbx, header=1)

  expfolder <-  parentFolder(ffDir, upn=1, slash=whatSlash(ffDir))
  # export folder = folder containing the folder with the raw files (assuming this is a good folder to put the output)


  #### open first xls file to check formatting ###

  SpeaknRecord('FORMATTING CHECK', header=3)

  # let us start by opening the first xls file to check the formatting
  # important as Viewpoint is never consistent with the formatting between systems
  # below only supports differences I have seen, unfortunately it would likely not support a format I have never seen

  xlsnames <- list.files(path=ffDir) # get the names of all the xls to read
  # edit 12/04/2022 -- used to be okay without naturalsort but now does not import in the right order (it goes file 1, 10, ... instead of file 1, 2, ...), so
  xlsnames <- naturalsort::naturalsort(xlsnames)

  # quick check: we should not have anything else than .xls or .XLS files in there -- check this
  if(all(substrEnding(xlsnames, 4)!='.xls') | all(substrEnding(xlsnames, 4)!='.XLS'))
    stop('\t \t \t \t >>> Error: there is something else than just .xls files in this folder, check and run again \n')

  # import the first xls file
  fi1 <- read.table(paste0(ffDir, xlsnames[1]), fill=TRUE, header=TRUE)

  ### which are the columns we need? ###

  # 1- column abstime
  colabstime <- which(colnames(fi1)=='abstime')
  if(length(colabstime)==0)
    stop('\t \t \t \t >>> Error: could not find column abstime \n')
  SpeaknRecord('Selecting column abstime at position', colabstime)

  # 2- column time
  coltime <- which(colnames(fi1)=='time')

  if(length(coltime)==0)
    stop('\t \t \t \t >>> Error: could not find column time \n')
  SpeaknRecord('Selecting column time at position', coltime)

  # 3- column type
  coltype <- which(colnames(fi1)=='type')

  if(length(coltype)==0)
    stop('\t \t \t \t >>> Error: could not find column type \n')
  SpeaknRecord('Selecting column type at position', coltype)

  # 4- column location
  collocation <- which(colnames(fi1)=='location')

  if(length(collocation)==0)
    stop('\t \t \t \t >>> Error: could not find column location \n')
  SpeaknRecord('Selecting column location at position', collocation)

  # 5- column data1
  coldata1 <- which(colnames(fi1)=='data1')

  if(length(coldata1)==0)
    stop('\t \t \t \t >>> Error: could not find column data1 \n')
  SpeaknRecord('Selecting column data1 at position', coldata1)

  # the indices of the columns we want:
  cols2take <- c(colabstime, coltime, coltype, collocation, coldata1)


  ### how is location column formatted? ###
  # I have seen C001--C192 (option1) or c1--c192 (option2)
  # what is the first character?
  locfirstchar <- strsplit(fi1$location[1], '')[[1]][1]

  # how many characters?
  # minimum number of characters will allow us to differentiate option1 and option2
  # as if option1: always 4, if option2: between 2 and 4
  locnchar <- min(nchar(fi1$location))

  # how many wells in each box?
  # can tell by number of unique well IDs, then divide by 2 as we get well IDs of box1 and box2 wells together
  # ! if given data for only one box, this will be wrong
  # there is no simple way to guess whether we are given data for a single box or two from the raw .xls files alone
  # for example, we could confuse two 48-well plates with a single box with a 96-well plate
  # hence, asked user at the top whether 'twoBoxMode'; if yes, then divide number of unique well IDs by two
  nwells <- length(sort(unique(readr::parse_number(fi1$location))))

  if (twoBoxMode) {
    nwells <- nwells/2
  }

  SpeaknRecord('Detected *', nwells, '* - well plate')

  # additionally, note Zebralab starts second box at well 97 regardless of number of wells

  # differentiate between option1 and option2:
  if (locfirstchar=='C' & locnchar==4) { # if option1

    SpeaknRecord('Locations are written CXXX, e.g. C096')

    # set the locations accordingly
    if (boxnum==1){
      SpeaknRecord('Running BOX1 so expecting C001, C002, ...')
      locs=sprintf('C%0.3d', 1:nwells) # Box1 locations = C001 >> C096 if 96 wells, or C001 >> C024 if 24 wells, etc.
    } else if (boxnum==2) {
      SpeaknRecord('Running BOX2 so expecting C097, C098, ...')
      locs=sprintf('C%0.3d', 97:(97+nwells-1)) # Box2 locations = C097 >> C192 if 96 wells, or C097 >> C120 if 24 wells, etc.
    } else {
      stop('\t \t \t \t >>> Error: Box number can only be 1 or 2. Did you write something else? \n')
    }

  } else if (locfirstchar=='c' & locnchar==2) {

    SpeaknRecord('Locations are written c..., e.g. c96')

    # set the locations accordingly
    if (boxnum==1){
      SpeaknRecord('Running BOX1 so expecting c1, c2, ...')
      locs=sprintf('c%i', 1:nwells) # Box1 locations = c1 >> c96
    } else if (boxnum==2) {
      SpeaknRecord('Running BOX2 so expecting c97, c98, ...')
      locs=sprintf('c%i', 97:(97+nwells-1)) # Box2 locations = c97 >> c192
    } else {
      stop('\t \t \t \t >>> Error: Box number can only be 1 or 2. Did you write something else? \n')
    }

  } else {

    stop('\t \t \t \t >>> Error: in the xls files, expecting location column to be formatted as C001, C002, ... or c1, c2, ... \n')

  }

  # note, in comments below will usually assume 96 wells but now locs created above actually represents number of wells
  # i.e. not always 96


  ### what is the time unit? ###
  # Viewpoint is not consistent, sometimes seconds sometimes microseconds (?)
  unitcheck <- min(fi1$time[which(fi1$time!=0)]) # in first file, what is the smallest value that is not 0?

  # if microseconds and fps ~ 25, should be ~ 40000
  # will leave large margin to allow from 10 fps (~ 100,000) up to 50 fps (~ 20,000)
  if (unitcheck > 18000 & unitcheck < 102000) {
    SpeaknRecord('Time column seems to be in microseconds, will divide by 1M to convert into seconds')
    timeconv <- 1000000 # i.e. we need to divide by 1M to get seconds
    # if seconds and fps ~ 25, should be ~ 0.04
    # will leave large margin to allow from 10 fps (~ 0.1) up to 50 fps (~ 0.02)
  } else if (unitcheck > 0.01 & unitcheck < 0.11) {
    SpeaknRecord('Time column seems to be in seconds, no conversion needed')
    timeconv <- 1 # i.e. we already have seconds (divide by 1 does not do anything)
  } else {
    stop('\t \t \t \t >>> Did not recognise time unit \n')
  }

  # we do not need fi1 anymore, delete it
  rm(fi1)


  #### import and append all the xls files ####

  # notes below likely to be important only for very long 200+ hours, but will keep advice if needed for future

  # memory intensive step ahead, default R/Windows/Mac settings will throw errors
  # on Windows (common Rihel lab computer: i7 4-core + 16 Gb RAM + 2 drives, C: = SSD 236Gb & D: = HDD 1.81Tb)
  # current settings that seem to work:

  # 1- increase memory in R settings file
  # open Notepad as administrator
  # File > Open..., file C: / Program Files / R / R-xxx / etc / Rprofile.site (bottom right, All files if you cannot see it)
  # add line:
  # invisible(utils::memory.limit(128000))
  # close R and start again, can run memory.limit() in Console to check it is reading it correctly
  # ! if multiple versions of R co-existing, make sure you change the Rprofile.site of the correct one (folder name xxx above should be different)

  # 2- increase allowed size of page file
  # (from what I understand this is to allow more virtual RAM,
  # i.e. actually being written on the disk but treated as RAM by computer)
  # Control Panel >> System and Security >> System >> Change Settings (right) >> Advanced tab >> Performance: Settings...
  # >> Virtual Memory: Change...
  # unticked Automatically...
  # for both D: drive & C: drive, tick Custom size, Initial size = 16 / Maximum size = 64000
  # Set, then restart computer

  tictoc::tic()

  pw <- vector(mode='list',length=length(locs)) # pre-allocate list of 96 elements (one per well)
  pw <- lapply(pw, function(w){
    vector(mode='list', length=length(xlsnames)) # in each of these 96 elements, pre-allocate a list of number of files elements
  })

  rowcount <- as.integer(0) # total number of rows, will use it to check everything makes sense after

  SpeaknRecord('DATA IMPORT', header=3)

  lapply(1:length(xlsnames), function(fi) { # fi for file index; xli for list of xls
    SpeaknRecord('Importing file #', fi, 'of', length(xlsnames))
    fif <- data.table::setkey( data.table::fread(paste0(ffDir, xlsnames[fi]), select=cols2take, fill=TRUE, integer64='numeric', colClasses=c(type='character')),
                  location, type) [.(locs, '101')]
    # might be ok to change to integer64='integer64' here, but seems to make only a very slight different in memory
    # fread(fi) = read that xls file and add it to the xli list
    # setkey = set column 'location' as key (key is data.table concept, works a bit like row names so you can filter rows based on it)
    # setkey = set column 'type' as second key
    # [.(locs = filters on 'location' key, takes only location that are in locs (if box1: C001 >> C096; if box2: C097 >> C192)
    # 101)] = filters on 'type' key, only takes = 101
    # (v1: was excluding anything != 101, but subset by negation does not work as well)
    fif$time <- fif$time/timeconv
    # times used to be in seconds (old VP system), now in microseconds
    # convert back in seconds, easier to read and Vp_Extract.m expects it to calculate frame rate (fps)
    # v1: was dividing all columns on big dataframe with all of data, which was hard on memory
    # v2: now doing it file by file

    # v11: found some rows where data1 says 'BACK_LIGHT' and location A01-1, A02-1, etc.
    # this also shifts columns and type column stores well number
    # which in consequence changes format of type column as character (as not only 71/101 anymore)
    # (usual miserable Viewpoint formatting)
    # i.e. indexing on the location column, [., (locs)] above, works ok; it takes only expected well numbers so will exclude rows A01-1
    # the problem is indexing on the type column, it needs to know whether it looks through numbers (101, 71) or strings ('101', 'c1', ...)
    # (clever) solution is to convert type column into character by default when important (see colClasses=c('location'))
    # then filter to keep only '101', so whenever it is 'c1' (when BACK_LIGHT rows appear), it will get thrown out

    # take from this file all the data of well w and put the values in slot w of the list
    lapply(1:length(locs), function(w) {  # pw = per well
      pw[[w]][[fi]] <<- fif[location==locs[w], c('abstime', 'time', 'data1')]
    })

    # keep track of how many rows of data we are importing
    rowcount <<- rowcount + nrow(fif)

  })
  # pw is a list of lists
  # one element per well
  # within each, one element per file

  # convert to a simpler list where each element is one well
  pw <- lapply(1:length(pw), function(w){
    SpeaknRecord('Binding data from well #', w, 'of', length(pw))
    data.table::rbindlist(pw[[w]])}) # bind all the file list, eg. in well1, there are file1, file2, ..., bind the files
  # note: lapply(pw, rbindlist) would give the same result, only benefit of above is to print progress

  # total number of rows (splitted in different wells in pw) should be same as total number of rows in xls files
  if(!identical(rowcount, sum(unlist(lapply(pw, function(x) {nrow(x)})))))
    stop('\t \t \t \t >>> Error: Something wrong when appending all the xls files together... \n')


  #### remove errors ####
  # this only applies to earlier model of Zebrabox

  if (boxGen==1) {

    SpeaknRecord('ERROR REMOVAL', header=3)
    SpeaknRecord('Zebrabox generation 1, removing some errors...')

    pw <- lapply(1:length(pw), function(w) {

      SpeaknRecord('Error removal well', w)

      # take that well's data
      wed <- pw[[w]]

      # error 1: correct ones as zeros
      # will assume that correct way of dealing with this is to minus 1 all the data
      SpeaknRecord('\t older Zebralab gives 0s as 1s, correct this...')
      wed$data1 <- wed$data1 - 1

      # error 2 = duplicated datapoints
      # i.e. same abstime and time

      # which rows have duplicated abstime?
      dupabstime <- which(duplicated(wed$abstime))

      # which rows have duplicated time?
      duptime <- which(duplicated(wed$time))

      # any cases of duplicated abstime but not time
      if( ! all( dupabstime %in% duptime ) ) stop('\t \t \t \t >>> Error: Some duplicated abstime do not have duplicated time \n')

      # or vice-versa?
      if( ! all( duptime %in% dupabstime ) ) stop('\t \t \t \t >>> Error: Some duplicated time do not have duplicated abstime \n')

      # if ok, intersect both
      dups <- intersect(dupabstime, duptime)

      # if we found any duplicates, remove them
      if (length(dups)>0) {
        SpeaknRecord('\t removing', length(dups), 'duplicated abstime/time rows')
        return( wed[-dups,] )
      } else {
        SpeaknRecord('\t no duplicated abstime/time rows')
        return( wed )
      }

    })

  }


  #### quality checks ####

  SpeaknRecord('QUALITY CHECK', header=3)

  # check that all timestamps are chronological for every well
  if (!all(sapply(pw, function(wed){all(diff(wed$time)>0)})))
    stop('\t \t \t \t >>> For at least one well, the timestamps are not all chronological \n')
  # wed for well data

  # check frame rate
  SpeaknRecord('Calculating average frame rate per well...')
  fpspw <- sapply(pw, function(w) {1/mean(diff(w$time))}) # fps per well

  if(max(fpspw) - min(fpspw) > 1) {
    SpeaknRecord('Can proceed; but all wells do not have the same frame rate (minimum 1 fps difference)', warning=TRUE)
  }

  if (isTRUE(all.equal(min(fpspw), max(fpspw), 0.02))) { # if all wells have same fps (if around 25fps, then it is 25 fps +- 0.5 fps)
    SpeaknRecord('Frame rate is ***', round(fpspw[1],2), '*** frames-per-second')
    fps <- fpspw[1]
    1
  } else {
    message('\t \t \t \t >>> Problem with frame rate. It appears to be different between wells, \n
          Frame rate for each well = \n')

    SpeaknRecord('Problem with frame rate. It appears to be different between wells', warning=TRUE)
    message('\t \t \t \t >>> Frame rate per well = \n')
    print(fpspw)
    fps <- fpspw[1] # ! this is completely arbitrary and will be inaccurate for many wells
  }
  # ! above is meant to catch errors with frame rate, not to deal with cases where frame rate is not consistent
  # accordingly, below assumes frame rate is the same for every well


  # how many data points for each well?

  SpeaknRecord('The well with the MINIMUM number of timepoints has ***', min(sapply(pw, nrow)),
               '*** datapoints, i.e. around ***', round(min(sapply(pw, nrow)) / fps / 60 / 60, 1), '*** hours of data')

  SpeaknRecord('The well with the MAXIMUM number of timepoints has ***', max(sapply(pw, nrow)),
               '*** datapoints, i.e. around ***', round(min(sapply(pw, nrow)) / fps / 60 / 60, 1), '*** hours of data')

  SpeaknRecord('The largest difference between two wells is ***', max(sapply(pw, nrow)) - min(sapply(pw, nrow)),
               '*** datapoints, i.e. around ***', round((max(sapply(pw, nrow)) - min(sapply(pw, nrow))) / fps, 1), '*** seconds of data')

  # are timepoints common between well?
  # ! each box sampled separately (even when started at same time)
  # in other words: timepoints are not usually common between boxes, but are common within each box

  comts <- length(Reduce(intersect, lapply(pw, function(wed) {wed$abstime}))) # number of timepoints shared by every well; by looking at abstime

  SpeaknRecord('***', round( comts / max(sapply(pw, nrow)) * 100, 3), # round(...) = percentage of timepoints which are common between every well
               '% *** of timepoints are common between wells of', paste0('BOX', boxnum))

  if(comts < 99) stop('\t \t \t \t >>> Error: less than 99% of timepoints are shared between all wells. Something wrong. \n')


  #### fix time column ####

  SpeaknRecord('PROCESSING', header=3)

  SpeaknRecord('Fixing time column...')

  # not always needed
  # there is a specific error where two abstime can match one time (or two very close times) or vice-versa
  # eg. well 1 to 84: abstime 466483627 / time 80557
  # then well 85 to 96: 466523503 / time 80017 (which looks like same frame as 80557?)
  # but abstime 466523503 is also shared with well 1 to 84 with time 120433
  # i.e. trusting abstime or trusting time would not pool the same datapoints in a single frame; which one to believe?
  # from what I understand, the goal of abstime to sync timepoints, so will trust this
  # from the cases I have seen (example above) abstime is consistent, while time is not; which is another argument to use abstime to sync

  # Note: at the end of the day, if the 'video' of some fish is shifted by 1 frame compared to the 'video' of the other fish, it will not affect the results

  # essentially, need to make sure one abstime is always = one time
  # strategy is:
  # 1- ignore data for now; inner_join abstime/time columns, by abstime
  bxt <- lapply(pw[c(1, length(pw))], # box times; joining time columns of well1 and 96; would ideally be just lapply(pw, function...); see explanations below
                function(wed){wed[,1:2]}) %>%
    purrr::reduce(inner_join, by='abstime')
  # ideally, one would do the exact same inner_join as below (when joining data)
  # i.e. inner_join of all (96) time columns by abstime
  # but on full 7+ week experiments it seems to be too much to ask, it gets stuck
  # instead, will join only time column of well1 and time column of well96, by their abstime
  # I think it should always give the same result
  # one (imperfect) way to check is to confirm that number of rows of the common time column we find below is = number of rows after joining all the data by abstime after; see below

  # gives abstime + 1 time column per well; all synced by abstime
  # if just well1 and 96: abstime / time well1 / time well2

  # 2- record times of well 96 (last column) as the correct ones
  # relatively arbitrary; but error seems to arise because of a false start at the beginning of the experiment;
  # so I think makes sense to take latest well as common reference
  bxt <- bxt[,ncol(bxt), with=FALSE]


  #### join ####

  SpeaknRecord('Stitching', paste0('BOX', boxnum), 'data from every well in a single dataframe...')

  # 3- ignore time now; inner_join all abstime/data columns, by abstime
  # (joining by abstime like above so should give the same rows)
  bx <- lapply(pw,
               function(wed){wed[,c(1,3)]}) %>%
    purrr::reduce(inner_join, by='abstime')

  colnames(bx) <- c('abstime', sprintf('f%i', as.integer(substr(locs, 2, 99))))
  # as.integer(substr(locs...): from C001, C002, ... >> 1, 2, ...
  # then sprintf to add f for fish, so >> f1, f2, f3, ...

  # gives abstime + 1 column per well of its data; all synced by abstime

  # 3- replace it by synced times of well 96 found above
  # (abstime only used to sync, will not use it again)
  if(nrow(bxt) != nrow(bx)) stop('\t \t \t \t >>> Error: Number of common times is not equal to the number of rows after joining the data \n')
  bx$abstime <- bxt
  colnames(bx)[1] <- 'exsecs' # note, previously this column was called 'time' but this is vague
  # exsecs is number of seconds since start of the experiment

  # checks
  SpeaknRecord('Checking the number of rows of each dataframe...')

  if (nrow(bx) != comts)
    stop ('\t \t \t \t >>> Number of rows in merged dataframe looks wrong \n')

  rm(pw)
  rm(bxt)

  ## Note when reading this in the future: abstime is only used to sync things together then thrown away
  # the exsecs column in the final RAWs.csv is the Viewpoint time column (seconds or microseconds after experiment started), not the abstime column


  #### export sorted xls files ####
  # if required

  if (exportXlsOrNo) {

    SpeaknRecord('EXPORT XLS FILES', header=3)

    # bx to long format
    SpeaknRecord('Expanding', paste0('BOX', boxnum), 'data to long format...')

    bxlg <- bx %>%
      stats::setNames(., c('exsecs', locs)) %>% # changes column names
      pivot_longer(cols=-exsecs, # i.e. all columns except time
                   names_to='location',
                   values_to='data1') %>%
      add_column(abstime=0, .before='exsecs') %>%
      add_column(channel=0, .after='time') %>%
      add_column(type=101, .after='channel')

    # there should now be number of timepoints * 96 rows
    if(nrow(bx) * nwells != nrow(bxlg))
      stop('>>>', paste0('BOX', boxnum), ': Something suspicious when converting to long format')


    #### export in files of 1M rows each ####
    nroweach <- 1000000

    # create the folders which will contain the raw xls files
    SpeaknRecord('Creating folder to store rawoutput XLS files...')

    dir.create(paste0(expfolder, 'rawoutputFixed'))
    dir.create(paste0(expfolder, paste0('rawoutputFixed', whatSlash(ffDir)), 'box', boxnum)) # inside, create folder box1 or box2

    splits <- c()
    # v2 alternative, write 1M rows at a time, avoids splitting first, which re-writes all of the data again
    if (nrow(bxlg) <= nroweach) { # i.e. if only 1 file to write
      splits <- c(0, nrow(bxlg))
    } else {
      splits <- seq(0, nrow(bxlg), nroweach) # eg. bxlg has 3.2M rows and nroweach = 1M, gives 1M, 2M, 3m
      splits[length(splits)+1] <- nrow(bxlg)
    }

    for (i in 1:(length(splits)-1)){
      SpeaknRecord('Writing file #', i, 'of', length(splits)-1)
      fname <- paste0(file.path(file.path(expfolder, 'rawoutputFixed', fsep=''),
                                paste0('box', boxnum), fsep=whatSlash(ffDir)), whatSlash(ffDir), dtbx, '_raw_', i, '.xls')
      data.table::fwrite(bxlg[(splits[i]+1):splits[i+1],], file=fname, sep='\t', row.names=FALSE, quote=FALSE)
      # eg. bxlg has 3.2M rows >>> splits = 0, 1M, 2M, 3M, 3.2M (5 elements)
      # iteration1; file1 = row 1 to 1,000,000
      # iteration2; file2 = row 1,000,001 to 2,000,000
      # iteration3; file3 = row 2,000,001 to 3,000,000
      # iteration4; file4 = row 3,000,001 to 3,200,000
    }

    # do not need bxlg anymore
    rm(bxlg)
    gc()

  } else {

    SpeaknRecord('Exporting sorted .xls files is OFF')

  }



  #### add times ####

  # we now have object bx as clean dataframe with all frame-by-frame data
  # each row is one frame, each column is one well
  # now we add timestamps

  # will add both full timestamps (date and clock time, or at least the best estimate) + zth (i.e. number of hours since day0 ZT0)
  # will also output light transitions in frame # (row #)

  # to find transitions we find closest frame (in zth) to 14, 24, etc.
  # (14, 24, etc. depending on setting dayduration, but we assume total day length is 24 hours)

  ### to know start time of the experiment, we need import first row of Zebralab file
  # unfortunately there is no information in the RAW .csv files that relates to the actual clock value
  # to leave it accessible to users which do not have a Zebralab file (e.g. if from different system)
  # we also have optional date0 and time0 settings, so below we only import Zebralab file if it is given

  # the Zebralab XLS file has a different encoding whether it is from the older or newer version of the Zebralab software
  # and the encoding decides which import function we should use
  # I cannot find a solution to tell the encoding in advance,
  # so previously many commands were asking whether zebDeprecatedFormat was TRUE or FALSE
  # a better solution to avoid bothering the user is simply to try one import command, and switch to the other if it does not work:

  if (!is.na(zebpath)) {
    usefRead <- TRUE # by default use fread
    # except if fread does not work
    tryCatch( { result <- data.table::fread(zebpath) },
              error = function(e) {usefRead <<- FALSE},
              silent=TRUE)

    # read first row of Zebralab's XLS file
    if (usefRead) {
      zebfi <- data.table::fread(zebpath)[1,]
    } else {
      zebfi <- read.delim(zebpath, fileEncoding='UCS-2LE', header=TRUE, nrow=1)
    }
    # zebfi = ZebraLab XLS file's first row

    # get t0 full timestamp
    startts <- paste(zebfi$stdate, zebfi$sttime) # start timestamp, e.g. 28/01/2021 10:27:35

  } else if (is.na(zebpath) & !is.na(date0) & !is.na(time0)) {
    startts <- paste(date0, time0)
  }

  # ! we assume here that first timestamp is correct. ViewPoint have made serious errors about this; carefully check
  # especially if Replay; first timestamp is (stupidly) taken from the computer clock when you start the Replay, not from the raw file
  startts <- lubridate::dmy_hms(startts) # convert in lubridate format eg. 2021-01-28 10:27:35 UTC


  #### add full clock timestamps to frame-by-frame data

  # will now assume first frame timestamp in RAW csv is = first timestamp in ZebraLab XLS file
  # and write all the frame timestamps as full date/time eg. 2021-01-28 10:27:35 UTC
  bx <- bx %>%
    add_column(fullts = startts + lubridate::dseconds(bx$exsecs) , .before='exsecs') # add full timestamp column
  # takes first timestamp then add to it any number of seconds written in the frame-by-frame data
  # e.g. at 3000th frame, 2021-03-16 19:54:49 + 120 seconds = 2021-03-16 19:56:49


  #### add Zeitgeber durations

  # i.e. number of hours since first day0 9AM
  startdate <- lubridate::date(bx$fullts[1]) # get startdate of the experiment = date of the first timepoint

  bx <- bx %>%
    add_column(zhrs = as.numeric(difftime(bx$fullts, lubridate::ymd_hms(paste(startdate, zt0)), units='hours')),
               .before='exsecs')
  # zeitgeber is time difference in hours since first ZT0

  # tell user about timestamps so can check they look correct
  cat('\t \t \t \t >>> First timestamp is ***', as.character(bx$fullts[1]), '*** \n')
  cat('\t \t \t \t >>> Last timestamp is ***', as.character(bx$fullts[nrow(bx)]), '*** \n \n')


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


  #### export ####

  # write
  SpeaknRecord('Exporting all frame-by-frame data from', paste0('BOX', boxnum), 'into', paste0(expfolder, whatSlash(ffDir), dtbx, '_RAWs.csv'))

  data.table::fwrite(bx,
                     paste0(expfolder, whatSlash(ffDir), dtbx, '_RAWs.csv'),
                     row.names=FALSE) # RAWs for raw sorted
  # = all raw data in one csv file
  # nframes rows x 97 columns (time in seconds + 96 wells)

  ### might help with memory:
  gc()


  #### report end ####

  tocout <- tictoc::toc(quiet=TRUE) # toc output

  SpeaknRecord('Sorted around', round(nrow(bx)/fps/60/60, 1), 'hours of data in',
               round(as.numeric(tocout[[2]] - tocout[[1]])/60/60, 2), 'hours')


  #### write log file ####

  write.table(logfile,
              paste0(expfolder, whatSlash(ffDir), dtbx, '_VpSorterlog.md'),
              row.names=FALSE, col.names=FALSE, quote=FALSE)


  # just in case...
  gc()


}

