# importGenotype ----------------------------------------------------------
# small command to import .txt genotype file

#' Title
#'
#' @param genopath
#'
#' @return
#'
#' @examples
importGenotype <- function(genopath) {
  if( substrEnding(genopath, 3) != 'txt') stop('\t \t \t \t >>> Genotype file does not finish by .txt \n')

  if(!file.exists(genopath)) stop('\t \t \t \t Error: file', genopath, ' does not exist! \n')

  geno <- read.delim(genopath, header=TRUE, skip = 1, na.strings=c("","NA"))
  return(geno)
}


# fillGenotype ------------------------------------------------------------
# makes sure genotype file contains all fish, adding an 'excluded' column if necessary

# Note, boxnum is required here as genotype always given 1>>96 regardless of box1 or box2

#' Title
#'
#' @param geno
#' @param boxnum
#'
#' @return
#'
#' @examples
fillGenotype <- function(geno,
                         boxnum=1) {

  genall <- as.vector(unlist(geno)) # pool all the fish in genotype file
  genall <- sort(genall[!is.na(genall)]) # remove any NA & sort

  # check we have a sensical number of fish
  if (length(genall)==0) stop('\t \t \t \t >>> There is no fish ID in this genotype file. \n')
  if (length(genall) > 96) stop('\t \t \t \t >>> More than 96 fish IDs in this genotype file, is this right? \n')
  cat('\t \t \t \t >>> Found N = ', length(genall), 'fish IDs in genotype file \n')

  # check fish IDs are given as 1 to 96
  if ( ! identical(unique(genall %in% 1:96), TRUE) ) stop('\t \t \t \t >>> Some fish IDs are not within 1--96. Maybe you gave fish IDs of second box as 97--192? Please change them to 1--96. \n')

  # check no duplicates in genotype file
  if(sum(duplicated(genall))!=0) stop('\t \t \t \t >>> Some wells are duplicated in genotype file. \n')

  # any fish missing from genotype file? if yes -- add them to excluded column in genotype file
  exclu <- (1:96) [which(! 1:96 %in% genall)] # i.e. which of 1, 2, 3, ..., 96 is not in genotype file

  if (length(exclu) != 0) {
    # preallocate excluded column as all NA
    geno$excluded <- NA
    # add any excluded fish
    geno[1:length(exclu), 'excluded'] <- exclu

    # now all fish should be present in genotype file
    genall <- as.vector(unlist(geno)) # pool all the wells in genotype file
    genall <- sort(genall[!is.na(genall)]) # remove any NA & sort

    # check looks good
    if( length((1:96) [which(! 1:96 %in% genall)]) != 0 )
      stop('\t \t \t \t >>> Still some wells missing from genotype file even after adding excluded column \n')
  }

  # if second box, add 96 to the all the fish IDs
  if (boxnum==2) {
    geno <- geno + 96
  }

  # now ready to return
  return(geno)
}


# pivotLongAssignGenotype -------------------------------------------------
# pivots time x fish data into long format and assigns genotypes in a group column

# v2: handles multiple time columns
# new argument timecols = how many time columns
# ! assumes these are the first columns
# e.g. timecols = 1 means first column is timestamps
# e.g. timecols = 3 means first three columns are timestamps
# by default, 1
# (should be ok to not issue a new version as default = 1, so should be compatible with calls in older scripts)

# txf below stands for time x fish
# i.e. data should be first column = time (can be hours or seconds or else)
# followed by 96 columns, one per fish
#' Title
#'
#' @param txf
#' @param genopath
#'
#' @return
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom tibble add_column
#' @importFrom dplyr mutate

pivotLongAssignGenotype <- function(txf,
                                    genopath) {

  # how many time columns?
  # find how many time columns we have; i.e. how many columns before first fish data
  # below tests if string looks like f + integer
  # first true will be first data column, minus 1 gives number of metadata columns
  timecols <- which(grepl("^f+[[:digit:]]", colnames(txf)))[1] - 1

  # add genotypes to column names which are fXX
  # e.g. f01 becomes f01_het
  txf <- addGenotypefXColnames(df=txf,
                               genopath=genopath)

  # names of time columns?
  timenms <- colnames(txf)[1:timecols]

  # pivot long
  txfl <- txf %>%
    pivot_longer(cols=-all_of(timenms),
                 names_to='fish',
                 values_to='px')
  # fish will be like fXX_grp, e.g. f01_het

  # then split fish into fish + group
  # replace fish in fish column
  # add group column
  txfl <- txfl %>%
    add_column(grp=afterFirstUnderscore(txfl$fish), .after='fish') %>%
    mutate(fish=beforeFirstUnderscore(txfl$fish))

  return(txfl)

}



# addGenotypefXColnames(...) ----------------------------------------------

# add genotype information to fXX colnames
# input: a dataframe where some columns' names are f01, f02, etc.
# function will change them to f01_grp, f02_grp
# it does not care about what other columns are
# it will just find columns which are f + digit and change them

#' Title
#'
#' @param df
#' @param genopath
#'
#' @return
#'
#' @examples
addGenotypefXColnames <- function(df,
                                  genopath) {

  # are we looking at box1 or box2 data?
  # check first fish ID in txf to tell whether looking at box1 or box2
  # (only really a question when running parallel boxes)

  firstID <- colnames(df)[which(grepl("^f+[[:digit:]]", colnames(df)))[1]]
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


  # import and fill genotype file
  # about 'fill genotype file': all wells are in the data, but all wells are not necessarily included in the genotype file
  # solution: add an excluded group in genotype file, which will contain any wells not included in any group
  # this is done by fillGenotype function
  # fillGenotype will also convert 1>>96 to 97>>192 if box2 data
  geno <- importGenotype(genopath)
  geno <- fillGenotype(geno, boxnum=boxnum)

  # which column names are fXX?
  fxinds <- which(grepl("^f+[[:digit:]]", colnames(df))) # indices of fx columns, e.g. col4, col5, col6

  # actual fXX of these columns
  fxs <- colnames(df)[fxinds] # e.g. f01, f02, f03

  # give these column names to assignGenotype so we get the corresponding groups
  fxgrps <- assignGenotype(fs=fxs, geno=geno) # e.g. scr, ko, ko

  # create fXX_grp, e.g. f01_scr, f02_ko, f03_ko
  fxnewcols <- paste(fxs, fxgrps, sep='_')

  # make sure one new name for each fXX column
  if(length(fxinds) != length(fxnewcols))
    stop('\t \t \t \t Error: in addGenotypefXColnames() not one new column name for each fXX column \n')

  # set them as new column names, for the columns that were fXX (i.e. at indices fxinds)
  colnames(df)[fxinds] <- fxnewcols

  # return the df
  return(df)

}


# assignGenotype ---------------------------------------------------------

# assignGenotype takes a vector of fish IDs as input (in the form f1, f2, f3, ...)
# for each, it looks up its genotype in the genotype file (geno) and returns its genotype
# eg. f1, f1, f2 >>> scr, scr, ko

#' Title
#'
#' @param fs
#' @param geno
#'
#' @return
#'
#' @examples
assignGenotype <- function(fs, geno) {
  sapply(fs,
         function(f){ # wn = well name
           colnames(geno)[which(geno==substr(f, 2, 99), arr.ind=TRUE)[2]]
         })
}
# details
# substr(): strips the f from column names, f1, f2, ... >> 1, 2, ..., so can match vs genotype file
# which(arr.ind=TRUE): find that well number in the genotype file (arr.ind=TRUE gives row and column coordinate, then [2] takes column number)
# colnames(geno): get the column name of that element
# eg. f12 >> look for 12 in genotype file >> it is in column 2 >> column 2 name is SCR
# any fish which has now grp = NA was not mentioned in genotype file, so it must have been empty or excluded
