### Frame-by-Frame package ###
### behaviour parameter ###

# main function is behaviourParameter(...)


# function splitFramesbyDayNight(...) -------------------------------------

# path = path to any file in the experiment folder that is YYMMDD_...
# ! assumes importAddTimetoRAWs(...) was run at some point, so wrote YYMMDD_BX_lights.csv with information about light transitions

#' Title
#'
#' @param tc
#' @param path
#'
#' @return
#'
#' @examples
splitFramesbyDayNight <- function(tc,
                                  path) {

  # guess path to light transitions info and import file
  folderpath <- beforeLastSlash(path)
  filename <- paste0(substr(afterLastSlash(path), 1, 9), '_lights.csv')
  suns <- read.csv(paste0(folderpath, filename)) # suns for sunrises/sunsets
  # Note; transitions are meant to be 'frame just before'

  # preallocate a list based on light transitions, each element is one *complete* day or night (all the data rows for that one day or night)
  # standard experiment would be day1 / night1 / day2 / night2

  # preallocate the list dn for days/nights
  dn <- vector(mode='list', length=length(diff(suns$frame))) # days/nights; number of complete day or night is like doing diff() on e.g. the frame numbers

  # put appropriate names
  # will assume started experiment during the day (i.e. between 9AM and 11PM)
  # so we can simply go day0, night0, day1, night1, etc.
  dn_nms <- c(rbind(sprintf('night%i', 0:30), sprintf('day%i', 1:31))) # names for a one-month long experiment
  # ! assumes starts during day; finishes during day
  # goes night0, day1, night1, day2, etc.; so easy to exclude night0
  # typically gives day1 / day2 + night1 / night2 as experimental design

  # just take the first few according to how many we need
  names(dn) <- dn_nms[1:length(dn)]

  # find light transitions in row index of timecourse
  # note; if using timecourse is frame-by-frame data, below should give same as `frame` column already suns
  # but has the advantage of generalising if using sleep file as timecourse data

  suns$rowi <- sapply(suns$zhrs, function(zh) {
    return(findLightTransitionFrame(Zeitgeberdurations=tc$zhrs, transitionHour=round(zh))) # zh can be 13.9999, rounding will turn it into 14
    # as we want to look for the frame just before 14, not the frame just before 13.999
  })

  # fill the list
  # for each window, we take from row just after transition until row just before transition
  for(w in 1:length(dn)) {
    dn[[w]] <- tc[ (suns[w, 'rowi'] + 1) : (suns[w+1, 'rowi']) , ]
  }

  # return the list
  return(dn)

}



# function splitFramesbyWoi(...) ------------------------------------------
# split frame-by-frame data by windows of interest
# partly copies splitFramesbyDayNight, but here user tells us windows of interest
# woi should be c(start1, end1, start2, end2, ...)
# each start or end in format YYYY-MM-DD HH:MM:SS

#' Title
#'
#' @param tc
#' @param woi
#'
#' @return
#'
#' @examples
splitFramesbyWoi <- function(tc,
                             woi) {

  # woi should be an even number of elements as start/end, start/end, etc.
  if (length(woi)%%2 !=0) stop('\t \t \t \t >>> Error splitFramesbyWoi: please give start and stop timestamps of each window of interest.
                               Accordingly, woi should have an even number of timestamps. \n')

  # convert woi timestamps to zth, i.e. number of hours since 9AM on day0
  woiz <- as.numeric(unlist(sapply(woi, function(ti) {
    lubridate::difftime( lubridate::ymd_hms(ti), lubridate::ymd_hms(paste(lubridate::date(tc$fullts[1]), '09:00:00')), units='hours')
  })))

  # check that woi timestamps are actually within the experiment
  if(!all(woiz > min(tc$zhrs))) stop('\t \t \t \t >>> Error splitFramesbyWoi: some woi timestamps are before the start of the experiment. Please check. \n')
  if(!all(woiz < max(tc$zhrs))) stop('\t \t \t \t >>> Error splitFramesbyWoi: some woi timestamps are after the end of the experiment. Please check. \n')

  # find 'transition frames', i.e. frame just before each woi timestamp
  # for this, can use function findLightTransitionFrame
  woif <- sapply(woiz, function(wz) {
    findLightTransitionFrame(Zeitgeberdurations=tc$zhrs, transitionHour=wz)
  })

  # will store transition frames in a small dataframe, will make it simpler to slice the data below
  # the start frames are at even indices & the stop frames are at odd indices
  wfd <- data.frame(start=woif[seq(1, length(woif), 2)], stop=woif[seq(2, length(woif), 2)])
  # make sure that every stop is after start
  if(!all(wfd$stop > wfd$start)) stop('\t \t \t \t >>> Error splitFramesbyWoi: some stop frames are before its start frame \n')
  # each row of wfd represents one window of interest, with its start frame and its stop frame

  # preallocate a list based on woi transitions, each element is one *complete* woi (all the data rows for that one woi)
  # will call list dn for daynight to stick with nomenclature when analysing full days/nights but here each element is one woi
  dn <- vector(mode='list', length=length(woi)/2) # divided by two because each woi is defined by start and stop
  # add names, will simply call woi1, woi2, ...
  names(dn) <- sprintf('woi%i', 1:(length(woi)/2))

  # fill the list
  # for each window, we take from row just after transition until row just before transition
  for(w in 1:length(dn)) {
    dn[[w]] <- tc[ (wfd[w, 'start'] + 1) : (wfd[w, 'stop']) , ]
  }

  # return the list
  return(dn)

}


# function paramReadPivot(...) --------------------------------------------
# internal function, not for end user
# read parameter dataframes into list `paL` / rbindlist / pivot
# has to be done in multiple functions, so move these tasks here
# input = parameter dataframe path(s) or object(s), created by behaviourParameter()
# output = combined dataframe in long format, to be used by function e.g. paramSummary or ggParameter

#' Title
#'
#' @param pa
#' @param grporder
#' @param skipNight0
#' @param poolExp1
#' @param poolExp2
#' @param poolExp3
#'
#' @return
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom tibble add_column
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter
paramReadPivot <- function(pa,
                           grporder=NA,
                           skipNight0,
                           poolExp1=NA,
                           poolExp2=NA,
                           poolExp3=NA) {

  # what did we get as input?
  # allow 2 options for pa: path(s) to .csv parameters or object(s) in Environment
  # if given a string that finishes with .csv, assume path(s), else assume object(s)
  # preallocate list where we will store the pa(s)
  # (effectively re-doing the list from behaviourParameter()
  # but want to keep what is being recorded in Environment/saved to drive intuitive)
  paL <- vector(mode='list', length=length(pa))

  # ! exception: if given only one pa directly as a dataframe, import it directly
  if(is.data.frame(pa)) {
    paL[[1]] <- pa
  } else {

    # fill the list
    for (i in 1:length(pa)) {
      # if pa is a string finishing by .csv, assume we need to import from path
      if (substrEnding(pa[i], 4)=='.csv') {
        paL[[i]] <- data.table::fread(pa[i])

        # else, if pa is a string, assume we were given the object's name in Environment
      } else if (is.character(pa[i])) {
        paL[[i]] <- get(pa[i])
      }

    }
  }


  # rbind the list
  # source is recorded through date and box columns
  par <- data.table::rbindlist(paL) # par for parameter rbind-ed

  # pivot to long format
  pal <- par %>%
    pivot_longer(cols=-c(parameter, date, box, fish, grp),
                 names_to='win',
                 values_to='param')

  ### prepare day/night column
  # simply says 'day' or 'night'
  # ! if analysing by woi, write 'woi' instead
  daynight <- as.character(sapply(pal$win, function(x) {
    if(substr(x, 1, 3) == 'day') {return('day')}
    if(substr(x, 1, 3) == 'nig') {return('night')}
    if(substr(x, 1, 3) == 'woi') {return('woi')}
  }))
  # add the day/night column
  pal <- pal %>%
    add_column(daynight=daynight, .before='win')

  ### if user wants to pool clutches, do it now
  if (!is.na(poolExp1[1]) | !is.na(poolExp2[1]) | !is.na(poolExp3[1])) {

    # put poolExps in a list
    poL <- list(poolExp1, poolExp2, poolExp3)

    # create composite column date_box
    pal <- pal %>%
      add_column(date_box=paste(pal$date, pal$box, sep='_'), .before='param')

    # loop through the list with the experiments to pool
    for(p in 1:length(poL)) {

      if (is.na(poL[[p]][1])) next

      cat('\t \t \t \t >>> Pooling experiments', poL[[p]], 'together \n')

      # now find all rows which belong to experiments to pool and edit date and box
      pal$date <- as.character(pal$date)
      pal[which(pal$date_box %in% poL[[p]]), 'date'] <- 'pool'
      pal[which(pal$date_box %in% poL[[p]]), 'box'] <- p # so will be 1 or 2 or 3 depending on the poolExp
    }

    # now delete column date_box
    pal$date_box <- NULL
    # below will re-create the date_box column
    # result will be pool_1; pool_2; pool_3
  }

  ### create a bunch of composite columns
  # will be useful later when combining data in different ways for summary or plots
  # e.g. date_box_win_grp can be 210927_12_day0_scr
  pal <- pal %>%
    # add composite column date + box, i.e. labelling individual experiments
    add_column(date_box=paste(pal$date, pal$box, sep='_'), .before='param') %>%
    # add composite column date + box + win, i.e. labelling individual windows of individual experiments
    add_column(date_box_win=paste(pal$date, pal$box, pal$win, sep='_'), .before='param') %>%
    # add composite column day/night + grp, i.e. ignoring which experiment is which
    add_column(daynight_grp=paste(pal$daynight, pal$grp, sep='_'), .before='param') %>%
    # add composite column win + grp, i.e. ignoring which experiment is which
    add_column(win_grp=paste(pal$win, pal$grp, sep='_'), .before='param') %>%
    # add composite column date + box + day/night + grp, e.g. 210927_12_night_scr
    add_column(date_box_daynight_grp=paste(pal$date, pal$box, pal$daynight, pal$grp, sep='_'), .before='param') %>%
    # add composite column date + box + window + grp, e.g. 210927_12_day0_scr
    add_column(date_box_win_grp=paste(pal$date, pal$box, pal$win, pal$grp, sep='_'), .before='param')

  # remove any excluded fish
  nexclu <- length(which(par$grp=='excluded'))

  if(nexclu>0) {
    cat('\t \t \t \t >>> Removing N =', nexclu, 'excluded wells \n')
    pal <- pal %>%
      filter(grp!='excluded')
  }


  # check grporder
  # and exclude any genotype not mentioned
  # check not given a genotype twice (or more)

  if (!is.na(grporder[1])) { # this is all IF we are given a grporder, otherwise just skip
    if (length(which(duplicated(grporder)))!=0)
      stop('\t \t \t \t >>> Error: genotype(s) more than once in grporder. Give each genotype once. \n')

    # check we are not given a genotype that is not in parameter
    if(length(which(! grporder %in% unique(pal$grp)!=0)))
      stop('\t \t \t \t >>> Error: genotype *', grporder[which(! grporder %in% unique(pal$grp))],
           '* is not present in parameter data. \n')

    # if any grp present in data is not given in grporder, assume we need to remove it
    if(length(which(! unique(pal$grp) %in% grporder!=0))) {

      excludegeno <- unique(pal$grp)[which(! unique(pal$grp) %in% grporder!=0)]

      cat('\t \t \t \t >>> Removing all fish of group(s)', excludegeno, '... \n')

      pal <- pal %>%
        filter(! grp %in% excludegeno)

    }
  }


  # order factors

  ### date ###
  # convert to factor, no need to worry about order for now
  pal$date <- factor(pal$date)

  ### box ###
  # convert to factor, no need to worry about order for now
  pal$box <- factor(pal$box)

  ### fish ###
  # convert to fish, no need to worry about order for now
  pal$fish <- factor(pal$fish)

  ### group ###
  # either we order alphabetically (default) or the user told us the order
  if(is.na(grporder[1])) {
    # if not told anything; converting to factors will do alphabetically by default
    pal$grp <- factor(pal$grp)
  } else {
    # else, user told us the order
    pal$grp <- factor(pal$grp, levels=grporder)
  }

  ### daynight ###
  # will always do day > night for now
  # skips this if analysing by woi
  # when analysing by woi, daynight column is all 'woi' so no need for factors
  if(pal$daynight[1] != 'woi') {
    pal$daynight <- factor(pal$daynight, levels=c('day', 'night'))
  }

  ### order win ###
  # want do day1, day2, ... then night0, night1, night2, ...
  # while keeping it flexible for shorter/longer experiments

  # take unique win
  # and order it so as above
  # which luckily is simply alphabetical order (as days will be before nights)
  pal$win <- factor(pal$win, levels=sort(unique(pal$win)))

  ### daynight_grp ###
  # read explanations below
  pal$daynight_grp <-
    factor(pal$daynight_grp,
           levels= as.vector(unlist(unique(pal[with(pal, order(daynight, grp)), 'daynight_grp']))) )

  ### win_grp ###
  # read explanations below
  pal$win_grp <-
    factor(pal$win_grp,
           levels= as.vector(unlist(unique(pal[with(pal, order(win, grp)), 'win_grp']))) )

  ### date_box_daynight_grp ###
  # below achieves something like:
  # day_box1_grp1 > day_box1_grp2 > day_box2_grp1 > day_box2_grp2
  # night_box1_grp1 > night_box1_grp2 > night_box2_grp1 > night_box2_grp2
  # trick is to think about grouping levels:
  # we first want to separate all day vs night, then within each box1 vs box2, etc.
  # note, below will also be correct when we were given a grporder!
  # indeed, order by grp will follow the levels we set above
  pal$date_box_daynight_grp <-
    factor(pal$date_box_daynight_grp,
           levels= as.vector(unlist(unique(pal[with(pal, order(daynight, box, grp)), 'date_box_daynight_grp']))) )
  # about levels=...: we sort the rows by daynight first, box second, grp third
  # then unique of the date_box_daynight_grp column gives us the group in right order

  ### date_box_win_grp ###
  # similar logic, but now win is higher level (see e.g. plots used in PhD thesis)
  # achieves something like:
  # box1_day1_grp1 > box1_day1_grp2 > box1_day2_grp1 > box1_day2_grp2
  # box2_day1_grp1 > box2_day1_grp2 > box2_day2_grp1 > box2_day2_grp2
  # box1_night1_grp1 > box1_night1_grp2 > box1_night2_grp1 > box1_night2_grp2
  # box2_night1_grp1 > box2_night1_grp2 > box2_night2_grp1 > box2_night2_grp2
  pal$date_box_win_grp <-
    factor(pal$date_box_win_grp,
           levels= as.vector(unlist(unique(pal[with(pal, order(win, box, grp)), 'date_box_win_grp']))) )


  # should we skip night0?
  # for standard experiments 2 days / 2 nights we often want to skip night0
  if (skipNight0) {
    pal <- pal %>%
      filter(win != 'night0')

    if (nrow(pal)==0) stop('\t \t \t \t >>> No complete day or night left after removing Night 0! \
                            \t \t \t Did you have only one night? Then you should turn OFF skipNight0, as skipNight0=FALSE. \n')
  }

  # pal ready to be returned
  return(pal)

}


# function paramSummary(...) ----------------------------------------------

# receives parameter dataframe(s) as input
# and prints some summary statistics

#' Title
#'
#' @param pa
#' @param skipNight0
#' @param poolExp1
#' @param poolExp2
#' @param poolExp3
#' @param poolExps
#' @param poolWins
#'
#' @return
#'
#' @examples
#' @importFrom dplyr summarise_at
#' @importFrom dplyr group_by
paramSummary <- function(pa,
                         skipNight0=FALSE,
                         poolExp1=NA,
                         poolExp2=NA,
                         poolExp3=NA,
                         poolExps=FALSE,
                         poolWins=FALSE) {

  pal <- paramReadPivot(pa=pa,
                        skipNight0=skipNight0,
                        poolExp1=poolExp1,
                        poolExp2=poolExp2,
                        poolExp3=poolExp3)

  # tell user summary stats, including final Ns
  cat('\n \n \t \t \t \t >>> SUMMARY STATS... \n')

  # group by, from less resolution (more pooling) to more resolution (less pooling)
  if (poolExps & poolWins) {
    pal <- pal %>%
      group_by(daynight_grp)
  } else if (poolExps & !poolWins) {
    pal <- pal %>%
      group_by(win_grp)
  } else if (!poolExps & poolWins) {
    pal <- pal %>%
      group_by(date_box_daynight_grp)
  } else if (!poolExps & !poolWins) {
    pal <- pal %>%
      group_by(date_box_win_grp)
  } else {
    stop('\t \t \t \t >>> Something wrong with pooling code in paramSummary(). \n')
  }

  # print summary
  pal %>%
    summarise_at(vars(param),
                 list(
                   mean= ~ mean(., na.rm=TRUE),
                   sd= ~ sd(., na.rm=TRUE),
                   sem= ~ sem_narm(.),
                   npoints= ~ length(.)
                 )) %>%
    print()

}



# function behaviourParameter(...) ----------------------------------------

# overarching function for behaviour parameters

#' Title
#'
#' @param parameter
#' @param ffpath
#' @param genopath
#' @param woi
#' @param zthr_min
#' @param inaThr
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom tibble add_column

behaviourParameter <- function(parameter,
                               ffpath,
                               genopath,
                               woi=NA,
                               zthr_min=1,
                               inaThr=0,
                               dayduration=14) {


  # do some checks/tell user ------------------------------------------------

  # check parameter name

  if (! parameter %in% allparameters) {
    print(allparameters)
    stop('\t \t \t \t >>> Error behaviourParameter: *', parameter, '* is not one of the supported parameters.\
         \t \t \t See available parameters above. \n')
  }

  # if analysing by woi, cannot calculate activityTransitionDelta
  if (!is.na(woi[1]) & parameter=='activityTransitionDelta')
    stop('\t \t \t \t >>> Error behaviourParameter: sorry, calculating parameter activityTransitionDelta is not currently supported when analysing window(s) of interest \n')


  # check ffpath(s) and genopath(s)

  # need one geno file per ff
  if(length(genopath) != length(ffpath))
    stop('\t \t \t \t >>> Error: the number of ffpaths is not the same as the number of genopaths. Please correct and run again \n')

  # remind user how it works with multiple experiments
  if (length(ffpath)>1) {
    cat('\t \t \t \t >>> Multiple experiments. Will assume that ffpaths and genopaths are in the order in which they are meant to be matched.\n')
  }

  # check the names match
  for (i in 1:length(ffpath)) {

    # get date_box of ffpath i
    ffdtbx <- substr(afterLastSlash(ffpath[i]), 1, 9)

    # get date_box of genopath i
    gndtbx <- substr(afterLastSlash(genopath[i]), 1, 9)

    # check they are the same
    if (!identical(ffdtbx, gndtbx))
      stop('\t \t \t \t >>> Error: frame-by-frame file *', afterLastSlash(ffpath[i]),
           '* and genotype file *', afterLastSlash(genopath[i]), '* do not have the same date/box number.
           \t \t \t Are you sure they are meant to be matched? \n')
  }


  # import frame-by-frame data ----------------------------------------------

  # store them in a list where each slot is one ff
  ffL <- vector(mode='list', length=length(ffpath))

  for (i in 1:length(ffpath)) {

    cat('\n \n \t \t \t \t >>> Reading frame-by-frame data of experiment', substr(afterLastSlash(ffpath[i]), 1, 9), '\n')

    ffL[[i]] <- importRAWs(ffpath[i])
  }


  # split data by day/night or windows of interest (woi) --------------------

  # results will be stored as a list of lists
  # top level = experiment, bottom level = day/night or woi

  # we loop through each experiment and for each we split by day/night or woi, which generates a list
  # (lapply would be better here, but not simple when looking for the light transitions file in the folder)

  dnL <- vector(mode='list', length=length(ffpath))

  # if we are splitting by woi:
  if (!is.na(woi[1])) {
    cat('\n \n \t \t \t \t >>> Splitting frame-by-frame data by windows of interest for experiment',
        substr(afterLastSlash(ffpath[i]), 1, 9), '\n')

    dnL[[i]] <- splitFramesbyWoi(tc=ffL[[i]],
                                 woi=woi)

    # if not, assume we are splitting by day/night:
  } else {
    for (i in 1:length(ffL)) {
      cat('\n \n \t \t \t \t >>> Splitting frame-by-frame data by day/night for experiment',
          substr(afterLastSlash(ffpath[i]), 1, 9), '\n')

      dnL[[i]] <- splitFramesbyDayNight(tc=ffL[[i]],
                                        path=ffpath[i])

    }

    # either way, at the top level each element of the list is an experiment
    # so we will put name as YYMMDD_BX
    # which we can obtain from ffpath
    names(dnL) <- substr(afterLastSlash(ffpath), 1, 9)

  }


  # at this stage we can delete ffL to save space
  rm(ffL)



  # /// to run tests /// ----------------------------------------------------
  # to run quick tests, only calculate day1 on one experiment
  # ! load only one experiment
  # dnL[[1]][c(1, 3, 4, 5)] <- NULL
  # i.e. only keep #2 which is day1


  # calculate parameter -----------------------------------------------------

  # there are three categories of parameters

  # 1-- activity parameters
  # these are parameters that we can calculate directly on the frame-by-frame deltapx values without any transformation
  # the name of the parameter starts with activity

  # 2-- active bout parameters
  # these are parameters which require us to detect individual swimming bouts
  # the name of the parameter starts with activebout

  # 3-- sleep parameters
  # these are parameters which require us to detect individual sleep bouts (aka naps)
  # the name of the parameter starts with sleep

  # there is one overarching function for each category
  # which then calls the _onefish version to calculate that specific parameter

  # which overarching function we call depends on the parameter's category (see above)
  # then lapply will loop through experiments in `dnL` and call the parameter function on one `dn` at a time
  # result will be a list, one slot per experiment
  # each slot is a simple parameter dataframe `pa` where rows = fish, column = day/night


  # category1
  if(substr(parameter, 1, 8) == 'activity') {
    cat('\n \t \t \t \t >>> ACTIVITY parameter \n')
    paL <- lapply(dnL, activityParameter, parameter=parameter, dayduration=dayduration)
  }

  # category2
  if(substr(parameter, 1, 10) == 'activebout') {
    cat('\n \t \t \t \t >>> ACTIVE BOUT parameter \n')
    paL <- lapply(dnL, activeboutParameter, parameter=parameter)
  }

  # category3
  # sleepParameter() need a few more parameters, mainly due to detectNaps()
  # so will need to loop instead of a clean lapply
  if(substr(parameter, 1, 5) == 'sleep') {
    cat('\n \t \t \t \t >>> SLEEP parameter \n')

    paL <- vector(mode='list', length=length(dnL)) # preallocate
    for (i in 1:length(dnL)) {
      paL[[i]] <- sleepParameter(dn=dnL[[i]],
                                 dtbx=names(dnL)[i], # we just need YYMMDD_BX of experiment for some path things
                                 parameter=parameter,
                                 woi=woi,
                                 zthr_min=zthr_min,
                                 inaThr=inaThr)
    }
  }

  # at this stage we can delete dnL to save space
  rm(dnL)
  gc()



  # add metadata columns ----------------------------------------------------
  # parameter, date, box, genotype
  # (some of these columns are repetitive but makes it simpler when combining parameter dataframes)

  for (i in 1:length(ffpath)) {

    # for genotype:
    # check first fish ID in pal to tell whether looking at box1 or box2
    # (only applies when running parallel boxes)
    # note, this remains correct when using plates with less than 96 wells
    # Zebralab starts box2 at well 97 regardless of the plate
    if (paL[[i]]$fish[1] == 'f1') {
      boxnum <- 1
    } else if (paL[[i]]$fish[1] == 'f97') {
      boxnum <- 2
    }

    geno <- importGenotype(genopath[i])
    geno <- fillGenotype(geno=geno, boxnum=boxnum)

    # for date:
    expdate <- substr(afterLastSlash(genopath[i]), 1, 6)

    # for box:
    expbox <- substr(afterLastSlash(genopath[i]), 8, 9)

    # now add these columns
    paL[[i]] <- paL[[i]] %>%
      # add genotype using assignGenotype() function
      add_column(grp=assignGenotype(paL[[i]]$fish, geno), .after='fish') %>%
      # add box #
      add_column(box=expbox, .before='fish') %>%
      # add date
      add_column(date=expdate, .before='box') %>%
      # add parameter name
      add_column(parameter=parameter, .before='date')

  }



  # record in Environment & save to csv -------------------------------------

  # we will record in Environment each parameter dataframe `pa` from `paL`
  # for the name, take parameter name & date_box from ffpath
  # to build something like parameter_YYMMDD_BX

  # using same name, will save to csv in folder where Zebrabox XLS file is
  # in a new folder called bhvparams

  # create bhvparams
  # ! will create wherever the first ffpath is
  dir.create(paste0(beforeLastSlash(ffpath[1]), 'bhvparams/'), showWarnings=FALSE)
  # showWarnings=FALSE so it does not print a warning if directory exists already

  # remember object names we create below
  panms <- c()

  for (i in 1:length(ffpath)) {

    # build object name
    panm <- paste0(parameter, '_', substr(afterLastSlash(ffpath[i]), 1, 9)) # e.g. activeboutLength_210927_13
    # remember object name for use in paramSummary() below
    panms <- c(panms, panm)

    # record it in Environment
    assign(panm, paL[[i]], envir=.GlobalEnv)

    cat('\t \t \t \t >>> Recorded', panm, 'in Environment \n')

    # save parameter dataframe in folder bhvparams
    data.table::fwrite(paL[[i]], file=paste0(beforeLastSlash(ffpath[1]), 'bhvparams/', panm, '.csv'))
    cat('\t \t \t \t >>> Saved', paste0(panm, '.csv'), 'in folder bhvparams \n')

  }


  # run paramSummary() to print summary statistics --------------------------
  paramSummary(pa=panms,
               skipNight0=TRUE,
               poolExps=FALSE,
               poolWins=FALSE)



  # return the first pa -----------------------------------------------------
  # so if run one box, returns the parameter dataframe

  # above was intensive in the case of active bout parameters, so take the garbage out
  gc()

  return(paL[[1]])

}




# multiBehaviourParameters(...) -------------------------------------------

#' Title
#'
#' @param parameters
#' @param ffpath
#' @param genopath
#' @param woi
#' @param zthr_min
#' @param inaThr
#'
#' @return
#' @export
#'
#' @examples
multiBehaviourParameter <- function(parameters,
                                    ffpath,
                                    genopath,
                                    woi=NA,
                                    zthr_min=1,
                                    inaThr=0,
                                    dayduration=14) {


  # check settings ----------------------------------------------------------

  # get list of parameters
  # if user asked for 'all' parameters
  if (parameters[1]=='all') {

    parameters <- allparameters

    # ! if analysing by woi, should skip activityTransitionDelta
    if(!is.na(woi[1])) {
      cat('\t \t \t \t >>> multiBehaviourParameter: analysing by window(s) of interest so skipping activityTransitionDelta \n')
      parameters <- parameters[-which(parameters=='activityTransitionDelta')]
    }

    # if user asked for 'activity' parameters
  } else if (parameters[1]=='activity') {

    parameters <- allparameters[which(substr(allparameters, 1, nchar('activity'))=='activity')]

    # ! if analysing by woi, should skip activityTransitionDelta
    if(!is.na(woi[1])) {
      cat('\t \t \t \t >>> multiBehaviourParameter: analysing by window(s) of interest so skipping activityTransitionDelta \n')
      parameters <- parameters[-which(parameters=='activityTransitionDelta')]
    }

    # if user asked for 'activebout' parameters
  } else if (parameters[1]=='activebout') {

    parameters <- allparameters[which(substr(allparameters, 1, nchar('activebout'))=='activebout')]

    # if gave multiple parameters, check they all exist
  } else if (parameters[1]=='sleep') {

    parameters <- allparameters[which(substr(allparameters, 1, nchar('sleep'))=='sleep')]

  } else if (length(parameters)>0) {

    for (p in 1:length(parameters)) {

      if(! parameters[p] %in% allparameters) {
        print(allparameters)
        stop('\t \t \t \t >>> Error: *', parameters[p], '* is not one of the supported parameters.\
         \t \t \t See available parameters above. \n')
      }

    }

  }

  # if all ok, tell user what we will do
  cat('\n \n \t \t \t \t >>> Will calculate the following parameter(s): ', parameters, '\n \n')


  # run behaviourParameter() ------------------------------------------------

  for (p in 1:length(parameters)) {

    behaviourParameter(parameter=parameters[p],
                       ffpath=ffpath,
                       genopath=genopath,
                       woi=woi,
                       zthr_min=zthr_min,
                       inaThr=inaThr,
                       dayduration=dayduration)

  }

}


