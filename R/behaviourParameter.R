### Frame-by-Frame package ###
### behaviour parameter ###

# main function is behaviourParameter(...)


# function splitFramesbyDayNight(...) -------------------------------------

# path = path to any file in the experiment folder that is YYMMDD_...
# ! assumes vpSorter(...) was run and wrote YYMMDD_BX_lights.csv with information about light transitions

#' Title
#'
#' @param tc
#' @param path
#'
#' @return
#'
#' @examples
#' @export
splitFramesbyDayNight <- function(tc,
                                  path) {

  # guess path to light transitions info and import file
  folderpath <- beforeLastSlash(path)
  filename <- paste0(substr(afterLastSlash(path), 1, 9), '_lights.csv')

  # check that lights.csv exists
  if(!file.exists( paste0(folderpath, filename) ))
    stop('\t \t \t \t >>> Looked for lights transition file ', filename, ' in experiment folder but could not find it.\
    Did you delete this file? Then please re-run vpSorter(...).\
    Did you move this file outside of the experiment folder? It should stay in the same folder as the ...RAWs.csv file.\
    Or perhaps you meant to define a time window using the `woi` setting? \n')

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
  # and therefore that day at the start is incomplete and day at the end is incomplete
  # dn_nms above goes night0, day1, night1, day2, etc.; so easy to exclude night0
  # typically gives day1 / day2 + night1 / night2 for the standard Rihel lab experimental design

  # take the first few according to how many we need
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
#' @export
splitFramesbyWoi <- function(tc,
                             woi) {

  # woi should be an even number of elements as start/end, start/end, etc.
  if (length(woi)%%2 !=0) stop('\t \t \t \t >>> Error splitFramesbyWoi: please give start and stop timestamps of each window of interest.
                               Accordingly, woi should have an even number of timestamps. \n')

  # convert woi timestamps to zth, i.e. number of hours since ZT0
  # ! previous version was assuming ZT0 was 9AM, but this is not always the case
  # we could ask the user for zt0 in case it is different than 9AM, or we can back-calculate it here
  # zhrs column gives number of hours since ZT0 and fullts column gives full timestamp
  # so we simply have to subtract zhrs from fullts to get ZT0

  # lubridate wants durations to be integers
  # so first convert zhrs to milliseconds
  zth1_ms <- round(tc$zhrs[1]*60*60*1000) # looks like milliseconds is small enough that it is always an integer, but round just to be safe
  # then calculate difference between zhrs and fullts
  # to be exactly correct, calculation will give zt0 - 1 frame
  # so we should be adding duration of one frame here
  # I think can assume splitFramesbyWoi is used on full frame-by-frame data?
  # so will calculate framerate here, rather than assume 25 fps
  fps <- averageFramerate(tc$exsecs)
  # duration of 1 frame is 1/fps
  backZT0 <- tc$fullts[1] - lubridate::milliseconds(zth1_ms) + lubridate::milliseconds(1/fps) # 40 ms is duration of one frame at 25 fps

  # tell user so can check just in case
  cat('\t \t \t \t >>> ZT0 back-calculated to', as.character(backZT0), '\t')

  # now, convert woi timestamps given by user to number of hours since ZT0 so we can easily slice the data
  woiz <- as.numeric(unlist(sapply(woi, function(ti) {
    difftime( lubridate::ymd_hms(ti), backZT0, units='hours')
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
#' @export
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

  # potential solution to avoid importing empty columns...
  # but creates issue with rbindlist below
  # unfortunately best at this point to put exceptions where needed
  # lapply(paL, function(pa) {
  #   # names of NA columns:
  #   naCols <- colnames(pa)[apply(is.na(pa), 2, all)]
  #   return( pa[, (naCols) := NULL] )
  # })

  # rbind the list
  # source is recorded through date and box columns
  par <- data.table::rbindlist(paL) # par for parameter rbind-ed

  # pivot to long format
  pal <- par %>%
    pivot_longer(cols=starts_with(c('day', 'night', 'woi')), # same as "minus all metadata columns"
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

  ### if user wants to pool experiments, do it now
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
    # add composite column date + box + grp, i.e. e.g. 220531_14_scr
    add_column(date_box_grp=paste(pal$date, pal$box, pal$grp, sep='_'), .before='param') %>%
    # add composite column day/night + grp, i.e. ignoring which experiment is which
    add_column(daynight_grp=paste(pal$daynight, pal$grp, sep='_'), .before='param') %>%
    # add composite column win + grp, i.e. ignoring which experiment is which
    add_column(win_grp=paste(pal$win, pal$grp, sep='_'), .before='param') %>%
    # add composite column date + box + day/night + grp, e.g. 210927_12_night_scr
    add_column(date_box_daynight_grp=paste(pal$date, pal$box, pal$daynight, pal$grp, sep='_'), .before='param') %>%
    # add composite column date + box + win + grp, e.g. 210927_12_day0_scr
    add_column(date_box_win_grp=paste(pal$date, pal$box, pal$win, pal$grp, sep='_'), .before='param')

  # is clutch column present?
  # (column is present if in situation where multiple clutches tracked in same box)
  # if yes, add composite column date + box + win + clutch + grp
  if('clutch' %in% colnames(pal)) {
    pal <- pal %>%
      # add composite column date + box + win + clutch + grp, e.g. 210927_12_day0_clutch2_scr
      add_column(date_box_clutch_win_grp=paste(pal$date, pal$box, pal$clutch, pal$win, pal$grp, sep='_'), .before='param') %>%
      # add composite column date + box + win + clutch, e.g. 210927_12_day0_clutch2
      add_column(date_box_clutch_win=paste(pal$date, pal$box, pal$clutch, pal$win, sep='_'), .before='param')
  }

  # remove any excluded fish
  nexclu <- length(which(par$grp=='excluded'))

  if(nexclu>0) {
    cat('\t \t \t \t >>> Parameter', unique(pal$parameter), ': removing N =', nexclu, 'excluded wells \n')
    pal <- pal %>%
      filter(grp!='excluded')
  }


  # check grporder
  # and exclude any genotype not mentioned
  # check not given a genotype twice (or more)

  if (!is.na(grporder[1])) { # this is all IF we are given a grporder, otherwise just skip
    if (length(which(duplicated(grporder)))!=0)
      stop('\t \t \t \t >>> Error paramReadPivot: genotype(s) more than once in grporder. Give each genotype once. \n')

    # check we are not given a genotype that is not in parameter
    if(length(which(! grporder %in% unique(pal$grp)!=0)))
      stop('\t \t \t \t >>> Error paramReadPivot: genotype *', grporder[which(! grporder %in% unique(pal$grp))],
           '* is not present in parameter data. \n')

    # if any grp present in data is not given in grporder, assume we need to remove it
    if(length(which(! unique(pal$grp) %in% grporder!=0))) {

      excludegeno <- unique(pal$grp)[which(! unique(pal$grp) %in% grporder!=0)]

      cat('\t \t \t \t >>> Removing all fish of group(s)', excludegeno, '... \n')

      pal <- pal %>%
        filter(! grp %in% excludegeno)

    }
  }


  ### should we skip night0?
  # for standard Rihel lab experiments night0/day1/night1/day2/night2 we often want to skip night0
  if (skipNight0) {
    pal <- pal %>%
      filter(win != 'night0')

    if (nrow(pal)==0) stop('\t \t \t \t >>> No complete day or night left after removing night0! \
                            \t \t \t Did you have only one night? Then you should turn OFF skipNight0, as skipNight0=FALSE. \n')
  }


  # order factors
  # edit 17/08/2023
  # previously, was doing mostly alphabetical order, which e.g. would give experiment in chronological order
  # however, I think better to respect the order given by the user
  # as the user may want to plot exp2 first, exp1 second

  ### date ###
  # convert to factor, respecting order in which users gave pa
  pal$date <- factor(pal$date, levels=unique(pal$date))

  ### box ###
  # convert to factor, respecting order in which users gave pa
  pal$box <- factor(pal$box, levels=unique(pal$box))

  ### date_box ###
  # convert to factor, respecting order in which users gave pa
  pal$date_box <- factor(pal$date_box, levels=unique(pal$date_box))

  ### fish ###
  # convert to factor, no need to worry about order for now
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

  ### clutch ###
  # (if column is present)
  # order alphabetically so clutch1, clutch2, etc.
  if('clutch' %in% colnames(pal)) {
    pal$clutch <- factor(pal$clutch)
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
  # trick is to think about grouping levels "from outside to inside"
  # we first want to separate all day vs night, then within each box1 vs box2, etc.
  # note, below will also be correct when we were given a grporder!
  # indeed, order by grp will follow the levels we set above
  pal$date_box_daynight_grp <-
    factor(pal$date_box_daynight_grp,
           levels= as.vector(unlist(unique(pal[with(pal, order(daynight, date_box, grp)), 'date_box_daynight_grp']))) )
  # about levels=...: we sort the rows by daynight first, box second, grp third
  # then unique of the date_box_daynight_grp column gives us the group in right order

  ### date_box_win ###
  pal$date_box_win <-
    factor(pal$date_box_win,
           levels= as.vector(unlist(unique(pal[with(pal, order(daynight, date_box, win)), 'date_box_win']))) )

  ### date_box_grp ###
  pal$date_box_grp <-
    factor(pal$date_box_grp,
           levels= as.vector(unlist(unique(pal[with(pal, order(daynight, date_box, grp)), 'date_box_grp']))) )

  ### date_box_win_grp ###
  # similar logic, but now win is higher level (see e.g. plots used in PhD thesis)
  # achieves something like:
  # box1_day1_grp1 > box1_day1_grp2 > box1_day2_grp1 > box1_day2_grp2
  # box2_day1_grp1 > box2_day1_grp2 > box2_day2_grp1 > box2_day2_grp2
  # box1_night1_grp1 > box1_night1_grp2 > box1_night2_grp1 > box1_night2_grp2
  # box2_night1_grp1 > box2_night1_grp2 > box2_night2_grp1 > box2_night2_grp2
  pal$date_box_win_grp <-
    factor(pal$date_box_win_grp,
           levels= as.vector(unlist(unique(pal[with(pal, order(daynight, date_box, win, grp)), 'date_box_win_grp']))) )

  # previously...
  # pal$date_box_win_grp <-
  #   factor(pal$date_box_win_grp,
  #          levels= as.vector(unlist(unique(pal[with(pal, order(win, box, grp)), 'date_box_win_grp']))) )

  ### date_box_clutch_win ###
  # (if clutch column present)
  # similar logic
  # achieves something like:
  # box1_clutch1_day1_grp1 > box1_clutch1_day1_grp2
  # box1_clutch2_day*2*_grp1 > box1_clutch2_day*2*_grp2
  # box1_clutch*2*_day1_grp1 > box_clutch*2*_day1_grp2
  # ...
  # then start again for box*2*
  if('clutch' %in% colnames(pal)) {
    pal$date_box_clutch_win <-
      factor(pal$date_box_clutch_win,
             levels= as.vector(unlist(unique(pal[with(pal, order(daynight, date_box, clutch, win)), 'date_box_clutch_win']))) )
  }

  ### date_box_clutch_win_grp ###
  # (if clutch column present)
  # similar logic
  # achieves something like:
  # box1_clutch1_day1_grp1 > box1_clutch1_day1_grp2
  # box1_clutch*2*_day1_grp1 > box1_clutch*2*_day1_grp2
  # box1_clutch1_day*2*_grp1 > box_clutch1_day*2*_grp2
  # ...
  # then start again for box*2*
  if('clutch' %in% colnames(pal)) {
    pal$date_box_clutch_win_grp <-
      factor(pal$date_box_clutch_win_grp,
             levels= as.vector(unlist(unique(pal[with(pal, order(date_box, clutch, win, grp)), 'date_box_clutch_win_grp']))) )
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
                               skipNight0=FALSE,
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

  # case where different clutches in same box
  if(length(ffpath)==1 & length(genopath)>1) {
    cat('\t \t \t \t >>> Given ONE ffpath but MULTIPLE genopaths, will assume different clutches/replicates ran in the same box. \n')
  }

  # if multiple ffpath, need one genopath per ffpath
  if(length(ffpath)>1 & length(ffpath)!=length(genopath))
    stop('\t \t \t \t >>> Error behaviourParameter: the number of ffpaths is not the same as the number of genopaths. Please correct and run again. \n')

  # remind user how it works with multiple experiments
  if (length(ffpath)>1) {
    cat('\t \t \t \t >>> Multiple experiments. Will assume that ffpaths and genopaths are in the order in which they are meant to be matched. \n')
  }

  # check the names match
  for (i in 1:length(ffpath)) {

    # get date_box of ffpath i
    ffdtbx <- substr(afterLastSlash(ffpath[i]), 1, 9)

    # get date_box of genopath i
    gndtbx <- substr(afterLastSlash(genopath[i]), 1, 9)

    # check they are the same
    # (note: this does not check the second or third or ... genotype file in case of multiple clutches in same box)
    if (!identical(ffdtbx, gndtbx))
      stop('\t \t \t \t >>> Error: frame-by-frame file *', afterLastSlash(ffpath[i]),
           '* and genotype file *', afterLastSlash(genopath[i]), '* do not have the same date/box (not the same YYMMDD_BX).
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

  }

  # either way, at the top level each element of the list is an experiment
  # so we will put name as YYMMDD_BX
  # which we can obtain from ffpath
  names(dnL) <- substr(afterLastSlash(ffpath), 1, 9)


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

  ### category1: activity ###
  if(substr(parameter, 1, 8) == 'activity') {

    cat('\n \t \t \t \t >>> ACTIVITY parameter \n')

    # ! EXCEPTION1: activitySunsetStartle
    # parameter activitySunsetStartle is an annoying exception that is best taken care of here
    # exact frame of light transition can be slightly imprecise
    # consequently, it is safer to look for sunset startle in a window starting a bit before the transition and finishing a bit after the transition
    # this is an exception to the framework which consists in calculating parameters by time window/by fish
    # best is to calculate activitySunsetStartle now separately while we still have all frame-by-frame data at hand, then proceed as normal for the other parameters
    if(parameter == 'activitySunsetStartle') {

      cat('\t \t \t \t >>> Calculating ~~~', parameter, '~~~ \n \n')

      paL <- lapply(1:length(dnL), function(i) { # i is experiment index
        activitySunsetStartle(ff=ffL[[i]],
                              dn=dnL[[i]])
      })

    # ! EXCEPTION2: we cannot calculate activityTransitionDelta if there is only one time window (day/night or window of interest)
    } else if(parameter=='activityTransitionDelta' & length(dnL[[1]])==1) { # ! only looking at first experiment given***
      # ***as of 17/03/2023; I am not sure what would happen if user were to give experiments with different number of days/nights
      cat('\t \t \t \t >>> Cannot calculate activityTransitionDelta as there is only one time window. \n')
      return(NULL)
    } else {
      ### normal case ###
      paL <- lapply(dnL, activityParameter, parameter=parameter, dayduration=dayduration)
    }
  }

  ### category2: activebout ###
  if(substr(parameter, 1, 10) == 'activebout') {
    cat('\n \t \t \t \t >>> ACTIVE BOUT parameter \n')
    paL <- lapply(dnL, activeboutParameter, parameter=parameter)
  }

  ### category3: sleep ###
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

  # at this stage we can delete ffL & dnL to save space
  rm(ffL)
  rm(dnL)
  gc()



  # add metadata columns ----------------------------------------------------
  # parameter, date, box, genotype
  # (some of these columns are often just one value repeated but makes it simpler when combining parameter dataframes)

  ## ! exception: procedure is slightly different in case where we have multiple clutches in same box
  ################ # you probably want to skip to part below, this is a fairly rare case
  if (length(ffpath)==1 & length(genopath)>1) {

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

    # for each genotype file, we prepare the grp column as if we were only looking at this genotype file
    grpcols <- lapply(1:length(genopath), function(i) {
      geno <- importGenotype(genopath[i])
      geno <- fillGenotype(geno=geno, boxnum=boxnum)
      return ( assignGenotype(paL[[1]]$fish, geno=geno) )
    })
    # so now we have a list: slot1: grp column as if we only had genotype file 1 // slot2: grp column as if we only had genotype file 2 // etc.
    # name the elements clutch1, clutch2, etc.
    # number will simply reflect the order in which the genopath were given
    names(grpcols) <- sprintf('clutch%i', 1:length(genopath))

    # just to be safe, check each grpcol is the same length
    # i.e. if first genotype is 96 wells, then second should be as well
    if( length(unique(as.numeric(unlist(lapply(grpcols, length))))) > 1 ) { # counts number of unique lengths (should be 1 if all same length)
      stop('\t \t \t \t >>> Error behaviourParameter: looks like genotype files with different plate formats were given \n')
    }

    # now we need to merge the grpcols into one column
    # there should not be any intersection, i.e. there should not be any fish that is mentioned in two genotype files
    # from each grpcol, take all the wells which are not excluded
    notexclu <- lapply(1:length(grpcols), function(i) {
      return( names(grpcols[[i]])[which(grpcols[[i]] != 'excluded')] )
    })
    # now check there is no intersection
    if( length(Reduce(intersect, notexclu)) > 0 )
      stop('\t \t \t \t >>> Error behaviourParameter: the following wells are mentioned in more than one genotype file:',
           sprintf(' %s', Reduce(intersect, notexclu)), '\
           \t \t \t Please check the genotype files/genotypeMaps and re-run. \n')

    # if ok, we can merge in one grpcol
    # make a small dataframe where each column is one grpcol
    grpcoldf <- Reduce(cbind, grpcols)

    # prellocate clutch column and grp column
    clutchcol <- rep(NA, nrow(grpcoldf))
    grpcol <- rep(NA, nrow(grpcoldf))

    for(r in 1:nrow(grpcoldf)) {
      # group values are this row:
      ro <- grpcoldf[r,]

      # if all values are 'excluded', then store 'excluded' in both clutch col and grp col
      if( all(ro=='excluded') ) {
        clutchcol[r] <- 'excluded'
        grpcol[r] <- 'excluded'

      # if not all values are 'excluded',
      # we checked above that there cannot be two (or more) non-excluded values for the same well
      # so can assume only one grp value

      } else {
        # return whichever value is not excluded
        # ! we also need to keep track of where this value is from, i.e. which clutch that larva comes from
        clutchcol[r] <- paste0('clutch', which(ro!='excluded')) # e.g. if value is from column2, then we return clutch2
        grpcol[r] <- ro[which(ro!='excluded')]
      }
    }
    # check no more NA in either grp col or clutch col
    if(sum(is.na(clutchcol))>0) stop('\t \t \t \t >>> Error behaviourParameter: some NA left when preparing the clutch column. Please report the error. \n')
    if(sum(is.na(grpcol))>0) stop('\t \t \t \t >>> Error behaviourParameter: some NA left when preparing the grp column. Please report the error. \n')

    # for date:
    expdate <- substr(afterLastSlash(genopath[i]), 1, 6)

    # for box:
    expbox <- substr(afterLastSlash(genopath[i]), 8, 9)

    # now add these columns
    paL[[i]] <- paL[[i]] %>%
      # add genotype (grp)
      add_column(grp=grpcol, .after='fish') %>%
      # add clutch #
      add_column(clutch=clutchcol, .before='grp') %>%
      # add box #
      add_column(box=expbox, .before='fish') %>%
      # add date
      add_column(date=expdate, .before='box') %>%
      # add parameter name
      add_column(parameter=parameter, .before='date')
  ################
    # end of exception

# standard case:
  } else {

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
               skipNight0=skipNight0,
               poolExps=FALSE,
               poolWins=FALSE)



  # return the first pa -----------------------------------------------------
  # so if run one box, returns the parameter dataframe

  # above was intensive in the case of active bout parameters, so take the garbage out
  gc()

  invisible(paL[[1]])

}





# function multiBehaviourParameter(...) -----------------------------------

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
                                    skipNight0=FALSE,
                                    zthr_min=1,
                                    inaThr=0,
                                    dayduration=14) {


  # check settings ----------------------------------------------------------

  # get list of parameters
  # if user asked for 'all' parameters
  if (parameters[1]=='all') {

    parameters <- allparameters

    # !! if analysing by woi, should skip activityTransitionDelta
    if(!is.na(woi[1])) {
      cat('\t \t \t \t >>> multiBehaviourParameter: analysing by window(s) of interest so skipping activityTransitionDelta \n')
      parameters <- parameters[-which(parameters=='activityTransitionDelta')]
    }

    # if user asked for 'activity' parameters
  } else if (parameters[1]=='activity') {

    parameters <- allparameters[which(substr(allparameters, 1, nchar('activity'))=='activity')]

    # !! if analysing by woi, should skip activityTransitionDelta
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

  #### temporary 16/03/2023
  # turn OFF calculation of compressibility, requires more work
  parameters <- parameters[parameters != 'activityCompressibility']
  ####

  # if all ok, tell user what we will do
  cat('\n \n \t \t \t \t >>> Will calculate the following parameter(s): ', parameters, '\n \n')


  # run behaviourParameter() ------------------------------------------------

  for (p in 1:length(parameters)) {

    behaviourParameter(parameter=parameters[p],
                       ffpath=ffpath,
                       genopath=genopath,
                       woi=woi,
                       skipNight0=skipNight0,
                       zthr_min=zthr_min,
                       inaThr=inaThr,
                       dayduration=dayduration)

  }

  ##### clear sleep detection
  # 28/03/2023
  # if we calculate different sleep parameters, we ran detectNaps() for the first sleep parameter
  # then recorded dnz_YYMMDD_BX in Environment so that we could skip detectNaps() for the subsequent ones
  # this is safe within a run of multiBehaviourParameter, as the user could not have changed detection settings woi, inaThr, zthr_min
  # however, it is risky to keep the previous detection in Environment, because user may have changed the detection settings
  # to address this, we record the previous detection settings as last__woi, last__inaThr, last__zthr_min
  # but there were still some ways to make it fail with specific sequences
  # e.g. detectNaps() on BOX1 with some settings; then BOX2 with new settings; then back to BOX1 with new settings
  # when running again BOX1, last__XXX do not change and dnz_YYMMDD_BX exists, so it skips detection
  # which is wrong
  # a solution would be to record YYMMDD_BX in last__XXX, e.g. last__inaThr_YYMMDD_BX
  # but the gain in speed by keeping pre-recorded dnz_ in Environment is null in great majority cases
  # we just want to use previous detection when analysing different sleep parameters one after the other

  # I think we can keep last__woi, last__inaThr, last__zthr_min for now
  # might be useful if, for example, want to run detectNaps() multiple times with different detection settings

  # remove any object in Environment that starts with dnz, which is pre-recorded nap detection
  remove( list = ls(envir=.GlobalEnv)[startsWith(ls(envir=.GlobalEnv), 'dnz')], envir=.GlobalEnv)

}
