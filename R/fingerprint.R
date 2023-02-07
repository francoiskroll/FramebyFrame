### Frame-by-Frame package ###
### fingerprint ###


# function calculateFingerprint(...) --------------------------------------

#' Title
#'
#' @param paDir
#' @param controlGrp
#' @param mergeExp1
#' @param mergeExp2
#' @param mergeExp3
#' @param mergeExp4
#' @param singleFish
#' @param grporder
#' @param skipNight0
#' @param avgDayNight
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom tibble add_column
#' @importFrom dplyr summarise_at
#' @importFrom dplyr group_by

calculateFingerprint <- function(paDir,
                                 controlGrp,
                                 mergeExp1=NA,
                                 mergeExp2=NA,
                                 mergeExp3=NA,
                                 mergeExp4=NA,
                                 singleFish=FALSE,
                                 grporder=NA,
                                 skipNight0=FALSE,
                                 avgDayNight=TRUE) {


  # import all parameter dataframes in paDir --------------------------------

  # should import all files in paDir
  # but to be safe, will take only .csv files which start with activity or activebout or sleep
  # (as user may have put other stuff in the folder like plots)

  flis <- which(substrEnding(list.files(paDir), 4) == '.csv') # indices of files we will probably want to import from paDir

  # from those, keep only those which start with activity or activebout or sleep
  flis <- flis[which(    substr( list.files(paDir)[flis] , 1, nchar('activity') ) == 'activity' |
                           substr( list.files(paDir)[flis] , 1, nchar('activebout') ) == 'activebout' |
                           substr( list.files(paDir)[flis] , 1, nchar('sleep') ) == 'sleep')]

  # paths we want are therefore
  papaths <- list.files(paDir, full.names=TRUE)[flis]

  # now ready to import all parameter tables
  psl <- paramReadPivot(pa=papaths,
                        grporder=grporder,
                        skipNight0=skipNight0)
  # psl for ParameterS Long format

  # note, v1 was averaging day1 and day2 / night1 and night2 datapoints for each larva at this stage
  # v2: I think it is preferable to calculate Z-score for day1 / Z-score for day2, then average the Z-scores to get Z score for day
  # I think better because v2 takes into account the spread of controls on day1 and spread of controls on day2 separately

  # add unique parameter column (see explanations below)
  psl <- psl %>%
    add_column(uparam=paste(psl$win, psl$parameter, sep='_'), .before=1)

  # from now on, 'parameter' refers to one parameter during one window,
  # e.g. day1 activeboutLength, night1 activeboutLength, day2 activeboutLength are all different parameters
  # added column uparam 'unique parameter' above to make this clear


  # prepare the control dataframe -------------------------------------------

  # we will need to Z-score each datapoint to the control mean
  # to do this, we first need to calculate the mean & sd of the controls for each experiment, each day/night, each uparam

  cat('\t \t \t \t >>> Calculating mean and standard deviation of controls during each day/night... \n')

  con <- psl %>%
    subset(grp %in% controlGrp) %>% # ***
    group_by(uparam, parameter, win, date, box, date_box) %>%
    summarise_at(vars(param),
                 list(
                   con_mean= ~ mean(., na.rm=TRUE),
                   con_sd= ~ sd(., na.rm=TRUE),
                   con_median= ~ median(., na.rm=TRUE),
                   con_mad=~ mad(., na.rm=TRUE),
                   con_n= ~ length(.)
                 ))
  # ***using %in% here instead of == allows multiple control groups
  # (useful if not consistent between experiments how control group is called, e.g. both wt and scr)

  # we now have, for each experiment, each day/night, each uparam, the mean & sd of all control larvae


  # Z-score all datapoints --------------------------------------------------
  # Z-score to controls, whose means/sds are found in con dataframe
  # ! Z-score to own controls, i.e. controls for the same date_box; as ideally they are from the same clutch
  # also same day/night and same uparam

  cat('\t \t \t \t >>> Z-scoring parameters to controls in same date_box... \n')


  # adding new column to psl called paz for parameters Z-scored to controls
  psl$paz <- apply(psl, 1, function(row) {

    # which uparam did we get?
    r_uparam <- as.character(row['uparam'])

    # which experiment did we get? i.e. date_box
    r_datebox <- as.character(row['date_box'])

    # so controls' mean for this uparam and datebox is
    conmean <- con[which(con$uparam==r_uparam & con$date_box==r_datebox), 'con_mean']

    # so controls' sd for this uparam and datebox is
    consd <- con[which(con$uparam==r_uparam & con$date_box==r_datebox), 'con_sd']

    # now we have all we need to Z-score this datapoint
    # using Zscore() from statsUtilities
    return(zScore(
      datapoint=row['param'],
      mean=conmean,
      sd=consd
    ))
  })

  # note, above also Z-scores the control larvae (to themselves), which is on purpose
  # so we can choose to plot them later


  # average days and nights together ----------------------------------------
  # if required, now is the time to obtain one Z-score for both days / one Z-score for both nights

  if (avgDayNight) {

    cat('\t \t \t \t >>> For each parameter and each larva, averaging its days Z-scores together and its nights Z-scores together \n')
    psm <- psl %>%
      group_by(parameter, date, box, fish, grp, daynight, date_box, daynight_grp, date_box_daynight_grp) %>% # **
      summarise_at(vars(paz),
                   list(
                     pazm= ~ mean(., na.rm=TRUE) # ***
                   )) %>% # pazm for mean of parameter z-scores
      add_column(uparam=paste(.$daynight, .$parameter, sep='_'), .before='parameter')

    # re-create uparam column
    # which now will be e.g. day_activeboutLength
    psm <- psm


    # if we are not averaging days and nights
  } else {
    # we still make the format match so continue as normal below
    psm <- psl
    colnames(psm)[which(colnames(psm)=='paz')] <- 'pazm'
  }
  # ** we essentially group by everything except any factor that gives window information
  # in other words, we keep window info 'hidden' so that, for each experiment, parameter, fish,
  # the days datapoints end up together in the same bag getting averaged
  # and the nights datapoints end up together in the same bag getting averaged

  # *** the na.rm here means that the result will not always be the mean of two Z-scores
  # in rare cases, we have a situation e.g. f11 had 0 sleep bout during night1 then 5 sleep bouts during night2
  # its sleep latency for night1 is thus NA (as no sleep bout) and say its sleep latency for night2 is 15 min
  # Z-score for night1 will be NA and Z-score for night2 will be say 2
  # result of mean will give 2, as we remove the NA
  # the alternative here would be to give NA for this larva that only has a single datapoint, but I think better to use the data we have

  # for the standard Rihel lab experiment (and w/ skipNight0 TRUE), this halves the number of rows in psl
  # as day1/day2 becomes just day and night1/night2 becomes just night


  # merge experiments, if required ------------------------------------------
  # user may want to merge sets of experiments, e.g. single clutch was tracked in two boxes
  # now is a good time as we calculated Z-scores within box, which should help controlling for technical variability
  # we can do so by editing date_box so that experiments are merged as one
  if (!is.na(mergeExp1[1]) | !is.na(mergeExp2[1]) | !is.na(mergeExp3[1]) | !is.na(mergeExp4[1])) {

    # put mergeExps in a list
    poL <- list(mergeExp1, mergeExp2, mergeExp3, mergeExp4)

    # loop through the list and edit date_box of experiments to pool
    # e.g. mergeExp1 is 220906_16 and 220906_17; we edit all to mergeExp_1
    for(p in 1:length(poL)) {

      if (is.na(poL[[p]][1])) next

      cat('\t \t \t \t >>> Merging fingerprints of experiments', poL[[p]], ' \n')

      # say we merge two boxes that have larvae as f1>>f96
      # merging the experiments would now make it look like f1 in exp1 is same animal as f1 in exp2, as they would share same date_box
      # so give fish from the experiments we are merging a full tag to correct this
      psm$fish <- as.character(psm$fish)
      psm[which(psm$date_box %in% poL[[p]]), 'fish'] <- paste(unlist(psm[which(psm$date_box %in% poL[[p]]), 'date_box']),
                                                              unlist(psm[which(psm$date_box %in% poL[[p]]), 'fish']),
                                                              sep='_')
      # this is not a worry when not merging as column date_box keeps track that f1 exp1 is different than f1 exp2

      # now find all rows which belong to experiments to merge and edit date_box
      psm$date <- as.character(psm$date)
      psm[which(psm$date_box %in% poL[[p]]), 'date'] <- 'mergeExp'

      psm$box <- as.integer(as.character(psm$box))
      psm[which(psm$date_box %in% poL[[p]]), 'box'] <- p # i.e. 1, 2, etc.
      psm[which(psm$date_box %in% poL[[p]]), 'date_box'] <- paste('mergeExp', p, sep='_') # i.e. mergeExp_1, mergeExp_2, etc.

    }
  }

  # Z-scores to fingerprint -------------------------------------------------

  if (!singleFish) {

    cat('\t \t \t \t >>> Summarising Z-scores into clutch fingerprints... \n')

    # fgp for fingerprint
    fgp <- psm %>%
      group_by(uparam, parameter, date, box, grp, date_box) %>%
      summarise_at(vars(pazm),
                   list(
                     mean= ~ mean(., na.rm=TRUE),
                     sd= ~ sd(., na.rm=TRUE),
                     sem= ~ sem_narm(.),
                     median= ~ median(., na.rm=TRUE),
                     mad= ~ mad(., na.rm=TRUE),
                     nfis= ~ length(.)
                   ))

    # add back a couple of composite columns

    ### add date_box_grp column
    fgp <- fgp %>%
      add_column(date_box_grp=paste(fgp$date, fgp$box, fgp$grp, sep='_'), .after='date_box')

    ### remake daynight or window column

    # we left option to average Z-scores of days together/nights together or not
    # accordingly, uparam is e.g. day_activeboutLength etc. or day1_activeboutLength etc.
    # make again the lost daynight or win column here
    # we will call it period so we stay agnostic as to whether it is day/night or day1/night1/etc.
    pers <- unlist(lapply(strsplit(fgp$uparam, '_'), function(up) {up[1]}))

    fgp <- fgp %>%
      add_column(period=pers, .after='parameter')

    # now building on it, make a composite column period_date_box_grp
    fgp <- fgp %>%
      add_column(date_box_period_grp=paste(fgp$period, fgp$date, fgp$box, fgp$grp, sep='_'), .after='date_box_grp')

    # if we keep Z-scores of single larvae
  } else {
    fgp <- psm
    # replace 'daynight' to 'period' in column names so matches above
    colnames(fgp) <- sub('daynight', 'period', colnames(fgp))
    ### add date_box_grp column
    fgp <- fgp %>%
      add_column(date_box_grp=paste(fgp$date, fgp$box, fgp$grp, sep='_'), .after='date_box') %>%
      ### add date_box_grp_fish column
      add_column(date_box_grp_fish=paste(fgp$date, fgp$box, fgp$grp, fgp$fish, sep='_'), .before='parameter')
  }


  ### remove day_activitySunsetStartle and day_sleepLatency if present
  # as these parameters are not defined during the day
  # but we kept it (with NAs) to have consistent number of columns
  # could also be e.g. dayX_activitySunsetStartle
  # so below: takes uparam that starts with 'day' and finishes with 'activitySunsetStartle' or 'sleepLatency'
  rows2del <- which(substr(fgp$uparam, 1, nchar('day'))=='day' &
                      ( substrEnding(fgp$uparam, nchar('activitySunsetStartle'))=='activitySunsetStartle' |
                          substrEnding(fgp$uparam, nchar('sleepLatency'))=='sleepLatency' ))

  if(length(rows2del)>0) {
    fgp <- fgp[-rows2del,]
  }



  # save fingerprint to drive -----------------------------------------------
  # will write in folder where bhvparams directory is
  expdir <- parentFolder(paDir, 1, whatSlash(paDir))
  write.csv(fgp, paste0(expdir, whatSlash(paDir), 'fingerprint.csv'), row.names=FALSE)



  # return ------------------------------------------------------------------
  return(fgp)

}




# function fingerprintSimilarity(...) -------------------------------------

# calculates a similarity score to compare fingerprints
# it does pairwise comparisons of all fingerprints

#' Title
#'
#' @param fgp
#' @param metric
#' @param simScore
#' @param grporder
#' @param removeControl
#' @param controlGrp
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr %>%
#' @importFrom tibble add_column
#' @importFrom dplyr group_by

fingerprintSimilarity <- function(fgp,
                                  metric='mean',
                                  simScore,
                                  grporder=NA,
                                  removeControl=FALSE,
                                  controlGrp) {

  # check simScore
  if(! simScore %in% c('cosine', 'correlation', 'euclidean'))
    stop('\t \t \t \t >>> Error fingerprintSimilarity: does not support this similarity score, similarity scores currently supported are: cosine, correlation, euclidean \n')

  # check metric
  if(! metric %in% c('mean', 'median'))
    stop('\t \t \t \t >>> Error fingerprintSimilarity: does not support this metric. Metrics currently supported are: mean and median \n')

  # import fgp, if needed ---------------------------------------------------

  # if we are given a string, assume it is a path
  # if not, assume we are given fgp object directly
  if(is.character(fgp)) {

    if(!file.exists(fgp)) stop('\t \t \t \t >>> Could not find file', fgp, ': please check the path. \n')
    if(!endsWith(fgp, '.csv')) stop('\t \t \t \t >>> Path does not end with .csv, please check. \n')

    fgp <- read.csv(fgp)
  }


  # if removing controls, do it now
  if(removeControl) {
    fgp <- fgp %>%
      filter(! grp %in% controlGrp)
  }

  # if user did not give grporder, we rank alphabetically
  if (is.na(grporder[1])) {
    grporder <- sort(unique(fgp$grp))
  }

  # if any group present in data is not in grporder, we should exclude it
  grp2del <- unique(fgp$grp)[!unique(fgp$grp) %in% grporder]
  fgp <- fgp %>%
    filter(! grp %in% grp2del)

  # now can order the grp column
  fgp$grp <- factor(fgp$grp, levels=grporder)

  # will calculate pairwise similarities

  # need to pivot to wide format
  # if clutch fingerprints;
  # each column is a date_box_grp (i.e. one group during one experiment), each row is a uparam, and each cell the mean Z-score
  # or in other words, where each column represents one clutch's fingerprint
  # if single larvae fingerprints;
  # each column is date_box_grp_fish (i.e. one larva), each row is a uparam, and each cell the Z-score

  # we can tell which type of fingerprints we were given by checking if there is a 'fish' column
  if('fish' %in% colnames(fgp)) {
    fgw <- fgp %>%
      group_by(date_box_grp) %>%
      pivot_wider( id_cols=uparam,
                   names_from=date_box_grp_fish,
                   values_from=pazm)

    # if not, we assume we were given clutch fingerprints
  } else {

    # record the order we want for the date_box_grp
    # this is based on grporder, which we used already to rank grp
    fgord <- as.vector(unlist(unique(fgp[with(fgp, order(grp)), 'date_box_grp']))) # fingerprint order

    if (metric=='mean') {
      fgw <- fgp %>%
        group_by(date_box_grp) %>%
        pivot_wider( id_cols=uparam,
                     names_from=date_box_grp,
                     values_from=mean)
    } else if (metric=='median') {
      fgw <- fgp %>%
        group_by(date_box_grp) %>%
        pivot_wider( id_cols=uparam,
                     names_from=date_box_grp,
                     values_from=median)
    }

    # each column of fgw is one fingerprint
    # now put the columns of fgw in the order we want
    fgw <- fgw[,c(1, match(fgord, colnames(fgw)))]

  }


  # now calculations depend on the score chosen
  if(simScore=='cosine') {
    # calculate pairwise cosine similarities between fingerprints (columns)
    fsim <- as.data.frame(lsa::cosine(as.matrix(fgw[2:ncol(fgw)])))
  } else if (simScore=='correlation') {
    fsim <- as.data.frame(cor(as.matrix(fgw[2:ncol(fgw)]), method='pearson'))
  } else if (simScore=='euclidean') {
    fsim <- as.matrix(dist( t(fgw[2:ncol(fgw)]) , 'euclidean'))
  }

  # print the similarity matrix
  # only if clutch fingerprint, if larva fingerprint it is too big
  if(! 'fish' %in% colnames(fgp)) {
    cat('\t \t \t \t >>> Pairwise *', simScore , '* between fingerprints \n')
    print(fsim)
  }

  # convert to tibble
  # ! conversion delete row.names which we need to add as first column, so keep them aside
  rownms <- row.names(fsim)
  fsim <- tibble::as_tibble(fsim)

  # avoid using rownames
  if('fish' %in% colnames(fgp)) {
    fsim <- fsim %>%
      add_column(date_box_grp_fish=rownms, .before=1)
    row.names(fsim) <- NULL
  } else {
    fsim <- fsim %>%
      add_column(date_box_grp=rownms, .before=1)
    row.names(fsim) <- NULL
  }


  ### export the pairwise similarity scores ###
  # will write in folder where bhvparams directory is
  expdir <- parentFolder(paDir, 1, whatSlash(paDir))
  write.csv(fgp, paste0(expdir, whatSlash(paDir), 'fingerprint.csv'), row.names=FALSE)

  # return cosine similarity matrix
  invisible(fsim) # same as return() but without printing to Console

}
