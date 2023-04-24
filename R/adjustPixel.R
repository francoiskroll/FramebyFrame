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

#' Title
#'
#' @param rawpath
#' @param grp
#' @param scale
#' @param round
#' @param export
#'
#' @return
#' @export
#'
#' @examples
adjustPixel <- function(rawpath, grp, scale, round='down', export=TRUE) {


  # import & get info -------------------------------------------------------

  # import RAWs csv
  fr <- fread(file=rawpath)

  # import genotype file (by guessing its name)
  genopath <- paste0(substr(afterLastSlash(rawpath), 1, 9), 'genotype.txt')
  geno <- importGenotype(genopath) # importGenotype() is sourced from genotypeUtilities.R

  # extract a few info for paths
  expfolder <- beforeLastSlash(rawpath) # gets the experiment folder, i.e. the folder where the RAW file is
  dtbx <- substr(afterLastSlash(rawpath), 1, 9) # gets YYMMDD_BX, e.g. 210907_12
  bxnum <- afterLastUnderscore(dtbx) # gets box number, e.g. 12


  # modify genotype of box2 -------------------------------------------------
  # if box2; genotype is 1 >> 96, but RAWs data is 97 >> 192
  # below detects if RAWs data is of box2, if yes add 96 to all genotype entries
  lastfish <- colnames(fr)[ncol(fr)] # last column name should be name of last fish, either f96 if box1 or f192 if box2

  if (lastfish == 'f192') { # look at last fish, if it is 192 then we have box2 data
    geno <- geno + 96 # and we need to add 96 to all genotype entries
  }

  # find fish we need to scale ----------------------------------------------
  # i.e. fish belonging to grp
  fish2scale <- as.vector(unlist(select(geno, grp))) # take fish in grp, those are the ones we need to scale up or down
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
  fwrite(fr,
         paste0(expfolder, dtbx, '_RAWsadjusted.csv'),
         row.names=FALSE)


  # write xls files (if needed) ---------------------------------------------

  # if we need to export the raw xls files again for Vp_Extract.m
  if (export==TRUE) {

    # convert data to long Viewpoint format
    cat('\t \t \t \t >>> Converting adjusted data to long format \n')
    frl <- fr %>%
      pivot_longer(cols=-time, # i.e. all columns except time
                   names_to='location',
                   values_to='data1') %>%
      add_column(abstime=0, .before='time') %>%
      add_column(channel=0, .after='time') %>%
      add_column(type=101, .after='channel')

    # there should now be number of timepoints * 96 rows
    if(nrow(fr) * 96 != nrow(frl))
      stop('>>> Something suspicious when converting adjusted data to long format \n')

    # split into small xls files and export
    nroweach <- 1000000

    # create the folders which will contain the raw xls files
    cat('\t \t \t \t >>> Creating folders to store adjusted xls files \n')
    dir.create(paste0(expfolder, 'rawoutputAdjusted'))
    dir.create(paste0(expfolder, 'rawoutputAdjusted/', 'box', bxnum)) # inside, create folder e.g. box12

    splits <- c()
    # v2 alternative, write 1M rows at a time, avoids splitting first, which re-writes all of the data again
    if (nrow(frl) <= nroweach) { # i.e. if only 1 file to write
      splits <- c(0, nrow(frl))
    } else {
      splits <- seq(0, nrow(frl), nroweach) # eg. bxlg has 3.2M rows and nroweach = 1M, gives 1M, 2M, 3m
      splits[length(splits)+1] <- nrow(frl)
    }

    for (i in 1:(length(splits)-1)){
      cat('\t \t \t \t Writing BOX', bxnum, 'file #', i, 'of', length(splits)-1, '\n')
      fname <- paste0(file.path(file.path(expfolder, 'rawoutputAdjusted', fsep=''),
                                paste0('box', bxnum), fsep='/'), '/', dtbx, '_raw_', i, '.xls')
      fwrite(frl[(splits[i]+1):splits[i+1],], file=fname, sep='\t', row.names=FALSE, quote=FALSE)
      # eg. bxlg has 3.2M rows >>> splits = 0, 1M, 2M, 3M, 3.2M (5 elements)
      # iteration1; file1 = row 1 to 1,000,000
      # iteration2; file2 = row 1,000,001 to 2,000,000
      # iteration3; file3 = row 2,000,001 to 3,000,000
      # iteration4; file4 = row 3,000,001 to 3,200,000
    }
  }
}
