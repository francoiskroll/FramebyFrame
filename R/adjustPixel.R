#####################################################
# ~ ZFAD: adjustPixel function ~
#
# function to adjust delta-pixel data to adjust for size or pigmentation difference
# was originally written for PSEN2 experiment to adjust for fainter pigmentation of PSEN2 larvae
#
# Francois Kroll 2023
# francois@kroll.be
#####################################################

# from pigmentBias.R: psen2 do seem fainter under Zebrabox camera than scr
# serves as a calibration of sort
# here: adjust the data based on this, i.e. substract a few px from the scr data

# starting point for this code will be export module from VpSorter.R


# function adjustPixel ----------------------------------------------------

# scale up or down frame-by-frame data of a group of fish

# usage adjustPixel(rawpath, grp, scale, round=down, export=TRUE)

# rawpath = where the RAWs.csv file is

# grp = group of fish on which scaling needs to be applied
# ! needs to be one of the groups in genotype file

# scale = ratio to use to scale data of these fish
# e.g. 0.5 = divide delta-pixel data for these fish by 0.5
# e.g. 2.0 = double delta-pixel data for these fish

# export = TRUE or FALSE (default TRUE)
# export=TRUE means export raw xls files (1M rows each), will export in rawoutputAdjusted folder
# export=FALSE means do not export raw xls files (only big time x fish RAWs csv)

# round = up or down (by default down)
# delta-pixel values are integers, but multiplying them by scale will not always give an integer
# so need to round the values, here decide whether to round up (e.g. 2.8 becomes 3) or down (e.g. 2.8 becomes 2)
# round() would round to integer, e.g. 2.8 becomes 3 but 2.2 becomes 2; no currently implemented
# Note; I think it makes more sense to round down (hence the default) because single pixels are turned on or not
# or in other words, e.g. 0.9 pixel (for example a pixel activated but below grey value sensitivity threshold) is written as 0 pixel in the data

# ! by default output for the RAWs csv is YYMMDD_BX_RAWsadjusted.csv
# ! make sure to have genotype file in same folder, its name should be YYMMDD_BXgenotype.txt

# examples
# adjustPixel(here(210907_12_RAWs.csv), 0.5, 'scr', export=TRUE)
# will give the same as above: adjustPixel(here(210907_12_RAWs.csv), 0.5, 'scr')

# below: not sure if need to import other things from data.table? the .SDcols system may need to be imported

#' Title
#'
#' @param grp
#' @param scale
#' @param ffpath
#' @param genopath
#' @param round
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr %>% select all_of
#' @importFrom data.table :=
adjustPixel <- function(ffpath,
                        genopath,
                        grpL,
                        grpS=NA,
                        scale=NA,
                        round='down',
                        dayduration=14) {


  ### find scaling ratio using scaleFromStartle
  # if not given by user
  if(is.na(scale)) {
    scale <- scaleFromStartle(ffpath=ffpath,
                              genopath=genopath,
                              grpL=grpL,
                              grpS=grpS,
                              dayduration=dayduration)
  }


  # import & get info -------------------------------------------------------

  # import RAWs csv
  fr <- data.table::fread(file=ffpath)

  # import genotype file (by guessing its name)
  geno <- importGenotype(genopath) # importGenotype() is sourced from genotypeUtilities.R

  # extract a few info for paths
  expfolder <- dirname(ffpath) # gets the experiment folder, i.e. the folder where the RAW file is
  dtbx <- substr(basename(ffpath), 1, 9) # gets YYMMDD_BX, e.g. 210907_12
  bxnum <- strNthSplit(dtbx, '_', 2) # gets box number, e.g. 12


  # modify genotype of box2 -------------------------------------------------
  # if box2; genotype is 1 >> ..., but RAWs data is 97 >> ...
  # below detects if RAWs data is of box2, if yes add 96 to all genotype entries

  # (how many time columns?)
  timecols <- which(grepl("^f+[[:digit:]]", colnames(fr)))[1] - 1

  firstfish <- colnames(fr)[timecols+1] # first fX column name should be name of first fish, either f1 if box1 or f97 if box2

  if (firstfish == 'f97') { # look at last fish, if it is f97 then we have box2 data
    geno <- geno + 96 # and we need to add 96 to all genotype entries
  }

  # find fish we need to scale ----------------------------------------------
  # i.e. fish belonging to grp
  fish2scale <- as.vector(unlist(select(geno, all_of(grpL)))) # take fish in grp, those are the ones we need to scale up or down
  fish2scale <- fish2scale[!is.na(fish2scale)] # remove any NA
  fish2scale <- sprintf('f%i', fish2scale) # add f to them, so becomes f2, f4, ...


  # scale their data --------------------------------------------------------
  # below, data.table approach for speed
  # idea is: .SDcols = i , j
  # I think along the lines of: take columns i; and we are modifying j
  # := is modify in place; i.e. we do not expressely save the variable with <-
  # lapply goes through the columns fish2scale (I think .SD says that), multiply by scale for each
  if (round=='down') {
    fr[, .SDcols=fish2scale[], fish2scale[] :=
         lapply(.SD, function(x) floor(x * scale)), ]
  } else if (round=='up') {
    fr[, .SDcols=fish2scale[], fish2scale[] :=
         lapply(.SD, function(x) ceiling(x * scale)), ]
  }


  # output adjusted data ----------------------------------------------------
  # to YYMMDD_BX_RAWsadjusted.csv
  cat('\t \t \t \t >>> Writing', paste0(expfolder, whatSlash(ffpath), dtbx, '_RAWsadjusted.csv'), '... \n')
  data.table::fwrite(fr,
                     paste0(expfolder, whatSlash(ffpath), dtbx, '_RAWsadjusted.csv'),
                     row.names=FALSE)


  ### here, deleted chunk to export .xls files
  # can find in previous version of adjustPixel (before it was a FramebyFrame function)



}


# function scaleFromStartle(...) ------------------------------------------

# function will calculate parameter activitySunsetStartle
# and return a scaling ratio which can be used in adjustPixel
# approach was originally developed for experiment 210907_PSEN2 to cancel potential bias due to fainter pigmentation of psen2 f0 knockouts

#' Title
#'
#' @param ffpath
#' @param genopath
#' @param grpL
#' @param grpS
#' @param dayduration
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr

scaleFromStartle <- function(ffpath,
                             genopath,
                             grpL,
                             grpS,
                             dayduration=14) {


  ### calculate parameter activitySunsetStartle
  # note, easier than asking user to give parameter table as input
  sut <- behaviourParameter(parameter='activitySunsetStartle',
                            ffpath=ffpath,
                            genopath=genopath,
                            skipNight0=FALSE,
                            dayduration=dayduration)


  ### calculate maximum across nights
  # take the largest sunset startle of the 3 nights
  # assumption being that this is the closest we can get to the real size/darkness of each larva in pixels

  # which are the night columns?
  nicols <- colnames(sut)[startsWith(colnames(sut), 'night')]

  if(length(nicols)==0)
    stop('\t \t \t \t >>> Error scaleFromStartle: need at least activitySunsetStartle for at least one night in order to calculate scaling ratio. \n')

  cat('\t \t \t \t >>> For each larva, taking maximum sunsetStartle across all nights. \n')
  sut <- sut %>%
    mutate(maxSt=apply(sut, 1, function(row) {
      max(as.numeric(row[nicols]))
    }))


  ### keep only the two groups given by the user
  sut <- sut %>%
    filter(grp %in% c(grpL, grpS))


  ### compare with t-test and tell user
  cat('\t \t \t \t >>> t-test on maximum sunsetStartle', grpL, 'vs', grpS, ': \n')
  print( t.test(maxSt ~ grp, data=sut) )


  ### ratio of means is the scaling ratio
  scar <- mean(sut[which(sut$grp==grpS), 'maxSt']) / mean(sut[which(sut$grp==grpL), 'maxSt'])

  cat('\t \t \t \t >>> Scaling ratio =', scar, '\n')

  return(scar)

}
