###################################################
# ~~~ FramebyFrame package ~~~

# Legacy fingerprint

# calculate old version of fingerprint from a .mat file obtained from SCRAP.m

# Francois Kroll 2022
# francois@kroll.be
###################################################


# note, I confirmed with exp PSEN2 210907_13 that below generates exactly same fingerprint as code I received as example Clustering2.m


# function getFromMat(...) ------------------------------------------------

# small function to help get the data from the ludicrous data structure we get from the .mat file
# parameter = parameter name to extract
# meanstd = mean or std
# will return 4 values: night1, day1, night2, day2
# this matches order of fingerprints in drugDb.csv
#' Title
#'
#' @param mat
#' @param parameter
#' @param nights
#' @param days
#' @param meanstd
#' @param grpNum
#'
#' @return
#' @export
#'
#' @examples
getFromMat <- function(mat,
                       parameter,
                       nights,
                       days,
                       meanstd,
                       grpNum) {

  c(mat[,,1]$summarytable[,,1][[meanstd]][,,1][[parameter]][,,1]$night[[grpNum]][[1]][nights[1]],
    mat[,,1]$summarytable[,,1][[meanstd]][,,1][[parameter]][,,1]$day[[grpNum]][[1]][days[1]],
    mat[,,1]$summarytable[,,1][[meanstd]][,,1][[parameter]][,,1]$night[[grpNum]][[1]][nights[2]],
    mat[,,1]$summarytable[,,1][[meanstd]][,,1][[parameter]][,,1]$day[[grpNum]][[1]][days[2]])

}



# function legacyFingerprint(...) -----------------------------------------

#' Title
#'
#' @param matPath
#' @param conGrp
#' @param treGrp
#' @param nights
#' @param days
#'
#' @return
#' @export
#'
#' @examples
legacyFingerprint <- function(matPath,
                              conGrp,
                              treGrp,
                              nights=c(2,3),
                              days=c(2,3)) {


  ### read .mat file ###
  mat <- readMat(matPath)$geno

  # means and standard deviations are already calculated by SCRAP.m
  # first, build a dataframe control group mean / control group std / treatment group mean / treatment group std
  # with each row being one parameter
  # will make it simple to calculate Z-scores


  ### extract genotype names ##
  genonms <- unlist(mat[,,1]$name)

  # check genotypes given by user are in there
  if(!all(c(conGrp, treGrp) %in% genonms))
    stop('\t \t \t \t >>> Could not find control and/or treatment group in this dataset \n')

  # find the number of each group
  conNum <- as.numeric(which(genonms==conGrp))
  treNum <- as.numeric(which(genonms==treGrp))


  ### preallocate dataframe ###
  legparams <- c('sleep',
                 'sleepBout',
                 'sleepLength',
                 'sleepLatency',
                 'averageActivity',
                 'averageWaking')

  legpr <- rep(rep(legparams, rep(4, length(legparams)))) # this repeats 4 times each parameter, so sleep sleep sleep sleep; sleepBout ...
  dns <- as.vector(replicate(length(legparams), c('night1', 'day1', 'night2', 'day2'))) # writes all the days/nights

  # prepare data.frame:
  # fd for fingerprint dataframe
  fd <- data.frame(win=dns, parameter=legpr)

  # add uparam column
  # e.g. day1_sleepLength
  fd <- fd %>%
    mutate(uparam=paste(win, parameter, sep='_'), .before=1)


  ### fill in dataframe ####
  # accessing the data in the mat file below is really trial and error and checking in MATLAB I am accessing the right values
  # seems like code added a 4th night even though it does not exist, parameters are usually 0 for this night
  # but sometimes a small value (e.g. 0.0213) which I suppose is an artefact from somewhere...

  # here, could code a check that day/night given by user are full (~ 10 or 14 hours) by looking at middur data

  # sapply below to loop through parameters
  fd <- fd %>%

    add_column(meanCon= as.vector(sapply(legparams, function(lp) {
      getFromMat(mat=mat,
                 parameter=lp,
                 nights=nights,
                 days=days,
                 meanstd='mean',
                 grpNum=conNum)
    }))) %>%

    add_column(stdCon= as.vector(sapply(legparams, function(lp) {
      getFromMat(mat=mat,
                 parameter=lp,
                 nights=nights,
                 days=days,
                 meanstd='std',
                 grpNum=conNum)
    }))) %>%

    add_column(meanTre= as.vector(sapply(legparams, function(lp) {
      getFromMat(mat=mat,
                 parameter=lp,
                 nights=nights,
                 days=days,
                 meanstd='mean',
                 grpNum=treNum)
    }))) %>%

    add_column(stdTre= as.vector(sapply(legparams, function(lp) {
      getFromMat(mat=mat,
                 parameter=lp,
                 nights=nights,
                 days=days,
                 meanstd='std',
                 grpNum=treNum)
    })))

  # add last two parameters manually
  fd <- fd %>%

    add_row(uparam='daymean_averageWaking',
            win='daymean',
            parameter='averageWaking',
            meanCon=as.numeric(mat[,,1]$summarytable[,,1]$mean[,,1]$averageWaking[,,1]$daymean[[conNum]][[1]]),
            stdCon=as.numeric(mat[,,1]$summarytable[,,1]$std[,,1]$averageWaking[,,1]$daymean[[conNum]][[1]]),
            meanTre=as.numeric(mat[,,1]$summarytable[,,1]$mean[,,1]$averageWaking[,,1]$daymean[[treNum]][[1]]),
            stdTre=as.numeric(mat[,,1]$summarytable[,,1]$std[,,1]$averageWaking[,,1]$daymean[[treNum]][[1]])) %>%

    add_row(uparam='nightmean_averageWaking',
            win='nightmean',
            parameter='averageWaking',
            meanCon=as.numeric(mat[,,1]$summarytable[,,1]$mean[,,1]$averageWaking[,,1]$nightmean[[conNum]][[1]]),
            stdCon=as.numeric(mat[,,1]$summarytable[,,1]$std[,,1]$averageWaking[,,1]$nightmean[[conNum]][[1]]),
            meanTre=as.numeric(mat[,,1]$summarytable[,,1]$mean[,,1]$averageWaking[,,1]$nightmean[[treNum]][[1]]),
            stdTre=as.numeric(mat[,,1]$summarytable[,,1]$std[,,1]$averageWaking[,,1]$nightmean[[treNum]][[1]]))


  ### calculate fingerprint ####
  fd <- fd %>%
    mutate(zsco=(meanTre-meanCon)/stdCon)


  ### add grp column ###
  # maybe useful if we end up appending multiple fingerprints
  fd <- fd %>%
    add_column(grp=treGrp, .after='parameter')

  ### return fingerprint table ###
  return(fd)

}
