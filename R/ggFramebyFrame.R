###################################################
# ~~~ FramebyFrame package ~~~

# various plotting functions
# except any that involves drawing an activity or sleep trace

# Francois Kroll 2022
# francois@kroll.be
###################################################


# function param2Title(...) -----------------------------------------------

# for each parameter, give the 'official' name to be used as title in plots

#' Title
#'
#' @param param
#'
#' @return
#' @export
#'
#' @examples
param2Title <- function(param) {

  if (param=='activityPercentageTimeActive') {
    return('percentage time active')

  } else if (param== 'activityTotalPx') {
    return('total activity')

  } else if (param=='activitySunsetStartle') {
    return('startle response at sunset')

  } else if(param=='activitySlope') {
    return('slope in activity')

  } else if(param=='activityTransitionDelta') {
    return('delta activity vs previous day or night')

  } else if(param=='activityFractalDim') {
    return('fractal dimension')

  } else if(param=='activityCompressibility') {
    return('compressibility')



  } else if (param=='activeboutLength') {
    return('active bout duration')

  } else if (param=='activeboutMean') {
    return('active bout mean')

  } else if (param=='activeboutSum') {
    return('active bout sum')

  } else if (param=='activeboutStd') {
    return('active bout standard deviation')

  } else if (param=='activeboutMin') {
    return('active bout minimum')

  } else if (param=='activeboutMax') {
    return('active bout maximum')

  } else if (param=='activeboutNum') {
    return('number of active bouts')



  } else if (param=='sleepHours') {
    return('total sleep')

  } else if (param=='sleepNumNaps') {
    return('number of sleep bouts')

  } else if (param=='sleepLatency') {
    return('sleep latency')

  } else if (param=='sleepNapDuration') {
    return('sleep bout duration')
  }


}



# function param2Ytext(...) -----------------------------------------------

# for each parameter, give the text to be used as Y axis in plots

#' Title
#'
#' @param param
#'
#' @return
#'
#' @examples
param2Ytext <- function(param) {

  if (param=='activityPercentageTimeActive') {
    return('% time active')

  } else if (param== 'activityTotalPx') {
    return('total activity (sum of \u0394 px)')

  } else if (param=='activitySunsetStartle') {
    return('startle response at sunset (\u0394 px)')

  } else if(param=='activitySlope') {
    return('slope in activity')

  } else if(param=='activityTransitionDelta') {
    return('delta activity vs previous day or night')

  } else if(param=='activityFractalDim') {
    return('fractal dimension')

  } else if(param=='activityCompressibility') {
    return('compressibility')



  }else if (param=='activeboutLength') {
    return('active bout duration (sec)')

  } else if (param=='activeboutMean') {
    return('active bout mean (\u0394 px)')

  } else if (param=='activeboutSum') {
    return('active bout sum (\u0394 px)')

  } else if (param=='activeboutStd') {
    return('active bout standard deviation (\u0394 px)')

  } else if (param=='activeboutMin') {
    return('active bout minimum (\u0394 px)')

  } else if (param=='activeboutMax') {
    return('active bout maximum (\u0394 px)')

  } else if (param=='activeboutNum') {
    return('number of active bouts')



  } else if (param=='sleepHours') {
    return('total sleep (hr)')

  } else if (param=='sleepNumNaps') {
    return('number of sleep bouts')

  } else if (param=='sleepLatency') {
    return('sleep latency (min)')

  } else if (param=='sleepNapDuration') {
    return('sleep bout duration (min)')
  }


}


# function param2Yunit(...) -----------------------------------------------

# simpler version of param2Ytext,
# for each parameter, give the unit to put on Y axis in plots

#' Title
#'
#' @param param
#'
#' @return
#'
#' @examples
param2Yunit <- function(param) {

  if (param=='activityPercentageTimeActive') {
    return('%')

  } else if (param== 'activityTotalPx') {
    #return('sum of \u0394 px')
    return(bquote('sum of \u0394 px '(x10^6)))

  } else if (param=='activitySunsetStartle') {
    return('\u0394 px')

  } else if(param=='activitySlope') {
    return('slope')

  } else if(param=='activityTransitionDelta') {
    return('\u0394 px')

  } else if(param=='activityFractalDim') {
    return('dimension')

  } else if(param=='activityCompressibility') {
    return('compression ratio')



  } else if (param=='activeboutLength') {
    return('sec')

  } else if (param=='activeboutMean') {
    return('\u0394 px')

  } else if (param=='activeboutSum') {
    return('\u0394 px')

  } else if (param=='activeboutStd') {
    return('\u0394 px')

  } else if (param=='activeboutMin') {
    return('\u0394 px')

  } else if (param=='activeboutMax') {
    return('\u0394 px')

  } else if (param=='activeboutNum') {
    return('#')



  } else if (param=='sleepHours') {
    return('hr')

  } else if (param=='sleepNumNaps') {
    return('#')

  } else if (param=='sleepLatency') {
    return('min')

  } else if (param=='sleepNapDuration') {
    return('min')
  }


}



# function param2shortName ------------------------------------------------

# version of param2Title which gives shorter names
# currently only used in ggFingerprint


#' Title
#'
#' @param param
#'
#' @return
#' @export
#'
#' @examples
param2shortName <- function(param) {

  if (param=='activityPercentageTimeActive') {
    return('% time active')

  } else if (param== 'activityTotalPx') {
    return('total \u0394 px')

  } else if (param=='activitySunsetStartle') {
    return('startle')

  } else if(param=='activitySlope') {
    return('slope')

  } else if(param=='activityTransitionDelta') {
    return('transition delta')

  } else if(param=='activityFractalDim') {
    return('fractal dimension')

  } else if(param=='activityCompressibility') {
    return('compressibility')



  } else if (param=='activeboutLength') {
    return('active bout length')

  } else if (param=='activeboutMean') {
    return('active bout mean')

  } else if (param=='activeboutSum') {
    return('active bout sum')

  } else if (param=='activeboutStd') {
    return('active bout std')

  } else if (param=='activeboutMin') {
    return('active bout min')

  } else if (param=='activeboutMax') {
    return('active bout max')

  } else if (param=='activeboutNum') {
    return('active bout num')



  } else if (param=='sleepHours') {
    return('sleep total')

  } else if (param=='sleepNumNaps') {
    return('sleep bout num')

  } else if (param=='sleepLatency') {
    return('sleep latency')

  } else if (param=='sleepNapDuration') {
    return('sleep bout length')
  }


}


# function ggFramerate(...)
# used for quality check, not for publication
# plots framerate over time
# with option to subsample (recommended), as plotting for every frame takes a long time


# function ggActivityTraceByGroup(...)
# expects binned/smoothed timecourse computed by summaryByGroupFromFrames()
# plots one activity trace by group
# trace is mean activity across the fish of this group
# ! Note, units depend on the bin_nsecs parameter in summaryByGroupFromFrames()
# e.g. bin_nsecs = 5*60, then Y axis unit is sum of deltapx / 5 min
# e.g. bin_nsecs = 10*60, then Y axis unit is sum of deltapx / 10 min
# ribbon is standard error of the mean or standard deviation, as chosen by the user
# xstart and xstop controls where the plot starts/stops in X (hours)
# Note, it may still show a few hours of data before/after these boundaries
# trimstart and trimstop will not show any data before/after
# e.g. trimstart = 24 will not show any data before 24 hr; trimstop = 48 will not show any data after 48 hr


# function ggParameter(...)
# expects parameter summary data computed by behaviourParameter(...) function
# it creates a scatterplot where X = time window / clutch, Y = parameter / each dot = one fish's parameter
# e.g. parameter is activeBoutMax, a dot can be fish5's mean active max during day2




# function ggFramerate(...) -----------------------------------------------

# plots framerate over time from RAWs.csv

# plotting all frames is possible, but takes a few minutes and plot is crowded
# solution: subsample the rows
# subsample = TRUE is yes to subsampling
# subsample = FALSE is no to subsampling (i.e. plot every frame)

# subsample_by = decrease number of frames by how much
# e.g. 10 is take every 10th row, so decrease number of rows by 10x
# Note, parameter will be ignored if subsample = FALSE (just leave anything)

# exportOrNo = TRUE, yes save plot as pdf to drive
# exportOrNo = FALSE, no do not save plot as pdf to drive

# width/height = width/height of pdf plot in mm

# exportname = full path to save plot as pdf

#' Title
#'
#' @param ffpath
#' @param subsample
#' @param subsample_by
#' @param xstart
#' @param xstop
#' @param ymin
#' @param ymax
#' @param sunlines
#' @param dayduration
#' @param xname
#' @param yname
#' @param exportOrNo
#' @param width
#' @param height
#' @param exportPath
#'
#' @return
#' @export
#'
#' @examples
#' @import ggplot2

ggFramerate <- function(ffpath,
                        subsample=TRUE,
                        subsample_by=1000,
                        xstart=0,
                        xstop=0,
                        ymin=0,
                        ymax=30,
                        sunlines=FALSE,
                        dayduration=14,
                        xname='hours since first 9 AM',
                        yname='frames-per-second',
                        exportOrNo=FALSE,
                        width=75,
                        height=55,
                        exportPath) {


  ### check export settings ###

  if (exportOrNo) {
    if(substrEnding(exportPath, 4) != '.pdf') stop('\n \t \t \t \t >>> Error: exportPath does not end with .pdf.')

    # check if output folder exists
    if(!dir.exists(parentFolder(exportPath)))
      stop('\n \t \t \t \t >>> Error exportPath: output folder does not exist. \n')

  }


  ### import frame-by-frame data ###
  ff <- importRAWs(ffpath=ffpath)


  # sunlines & xstop parameters ---------------------------------------------

  if (xstop==0) {xstop=max(ff$zhrs)} # xstop = 0 means plot all of the timecourse

  if (sunlines) {
    # sunsets and sunrises times for 30 days
    suns <- c(rbind(seq(dayduration, 720, 24), seq(24, 720, 24)))
    # typically dayduration = 14
    # so will go 14, 24, 38, 48 etc.

    # pick only the ones within the span of the data we want to plot
    suns <- intersect(suns, xstart:xstop)

  }


  # keep only zhrs and exsecs columns ---------------------------------------

  # create new dataframe
  fps <- as.data.frame(matrix(ncol=4, nrow=nrow(ff)))
  colnames(fps) <- c('frame', 'zhrs', 'exsecs', 'framerate')

  fps$frame <- 1:nrow(fps)
  fps$zhrs <- ff$zhrs
  fps$exsecs <- ff$exsecs

  # calculate framerate over time
  fps$framerate <- c(NA, 1/diff(ff$exsecs)) # first row is NA because nothing to substract by for the first diff

  # remove ff from memory
  rm(ff)


  # subsample, if needed ----------------------------------------------------
  # plotting all frames is possible but takes a few minutes
  # can subsample
  # e.g. take every 20th, i.e. 20x fewer rows

  if (subsample) { # if user wants to subsample

    rows2take <- seq(1, nrow(fps), subsample_by) # e.g. if 10, it will go 1, 11, 21, etc.
    fpss <- fps[rows2take,] # take these rows

  } else { # if user does not want to subsample

    fpss <- fps # then we just copy full fps

  }


  # plot --------------------------------------------------------------------

  fpsplo <- ggplot(fpss, aes(x=zhrs, y=framerate)) +
    geom_hline(yintercept=25, linetype=2, size=1.5, colour='#ebebeb') +
    geom_point(size=0.2) +
    theme_minimal() +
    coord_cartesian(ylim=c(ymin, ymax), xlim=c(xstart, xstop)) +
    scale_x_continuous(breaks=seq(0, max(fps$zhrs), 24)) +
    theme(panel.grid.minor.x=element_blank()) +
    {if(sunlines) geom_vline(xintercept=suns, linetype=2, size=0.2)} +
    xlab(xname) + ylab(yname)


  # export/return plot ------------------------------------------------------

  # export if need to
  if (exportOrNo) {ggsave(filename=exportPath, plot=fpsplo, width=width, height=height, units='mm', useDingbats=FALSE)}

  # in any case, return plot so displays in RStudio
  return(fpsplo)

}



# function ggSleepLatencySurvival(...) ------------------------------------

# expects sleepLatency parameter dataframe(s)

# 11/03/2024: deleted onlyDayorNight setting as sleepLatency is only defined for nights

#' Title
#'
#' @param pa
#' @param grporder
#' @param skipNight0
#' @param poolExp1
#' @param poolExp2
#' @param poolExp3
#' @param onlyWin
#' @param colours
#' @param legendOrNo
#' @param xtextOrNo
#' @param xnameOrNo
#' @param ynameOrNo
#' @param nightBgOrNo
#' @param xmaxh
#' @param detailsOrNo
#' @param exportOrNo
#' @param exportDir
#' @param width
#' @param height
#' @param dayduration
#' @param woi
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr %>%
#' @import ggplot2

ggSleepLatencySurvival <- function(pa,
                                   grporder=NA,
                                   skipNight0=FALSE,
                                   poolExp1=NA,
                                   poolExp2=NA,
                                   poolExp3=NA,
                                   onlyWin=NA,
                                   colours=NA,
                                   legendOrNo=TRUE,
                                   xtextOrNo=TRUE,
                                   xnameOrNo=TRUE,
                                   ynameOrNo=TRUE,
                                   nightBgOrNo=TRUE,
                                   xmaxh=NA,
                                   detailsOrNo=TRUE,
                                   exportOrNo=TRUE,
                                   exportDir,
                                   width=120,
                                   height=70,
                                   dayduration=14,
                                   woi=NA) {

  ### check export settings ###

  if (exportOrNo) {
    # check if output folder exists
    if(!dir.exists(exportDir))
      stop('\n \t \t \t \t >>> Error exportDir: output folder does not exist. \n')
  }

  # check that settings make sense

  # 11/03/2024 deleted check onlyDayorNight

  # cannot skipNight0 AND ask to plot only night0
  if(!is.na(onlyWin)) {
    if (skipNight0 & onlyWin=='night0') stop('\t \t \t \t >>> Error: cannot skipNight0 AND ask to only plot night0. \n')
  }

  # if given colours, need to be same number as grporder
  if (!is.na(grporder[1]) & !is.na(colours[1])) {
    if(length(grporder) != length(colours)) stop('\t \t \t \t Error: please give one colour per group in grporder \n')
  }

  # warn user about woi setting
  if(!is.na(woi)) {
    cat('\t \t \t \t >>> woi setting is given, so will ignore any dayduration value. \n')
    dayduration <- NA
  }

  # use paramReadPivot() to prepare the parameter dataframe(s) we received
  pal <- paramReadPivot(pa,
                        grporder=grporder,
                        skipNight0=skipNight0,
                        poolExp1=poolExp1,
                        poolExp2=poolExp2,
                        poolExp3=poolExp3)

  # check we are only given sleepLatency datapoints
  if(unique(pal$parameter) != 'sleepLatency')
    stop('\t \t \t \t >>> Error: given something else than sleepLatency dataframe(s)')

  # save the current grporder, will use below
  # only useful if user did not give grporder
  # if not, it does not change it
  grporder <- levels(pal$grp)

  # exclude some datapoints if needed, as per the settings

  # 11/03/2024 deleted filtering based on onlyDayorNight

  # if onlyWin is given...
  if (!is.na(onlyWin[1])) {
    pal <- pal %>%
      subset(win==onlyWin)
    # reset the levels of date_box_win here, otherwise will keep the other date_box_win which are not observed in pal anymore
    pal$date_box_win <- factor(pal$date_box_win) # we know there is only date_box_win here so order does not matter
  }

  # ! important *not* to remove NA data points here, they represent larvae which never slept

  # it may be that we were given multiple experiments together

  # additionally, there are usually multiple windows in one `pa`, e.g. night0, day1, etc.
  # do one experiment and window at a time, exporting each time the plot

  # we can use composite column date_box_win to split data into single windows of single experiments
  # i.e. split `pal` into a list where each element is data for one experiment/one window
  palW <- split(pal, pal$date_box_win)

  # now loop through this list
  # and we build the survival plot for one window at a time

  for (ew in 1:length(palW)) { # ew for experiment_window

    # experiment/window we are working on
    winn <- names(palW)[ew] # window name, e.g. 210927_12_day2

    # take these data, will be easier for referring to it below
    paw <- palW[[ew]]

    ### are we analysing by day/night or windows of interest?
    ## if by day/night
    if(!is.na(dayduration)) {
      # are we currently looking at day data or night data?
      dayornight <- unique(paw$daynight)
      # if looking at day data: break
      # as sleepLatency is not defined for days!
      if(dayornight=='day') {
        next # will return to for loop, i.e. skip to next window
      } else if(dayornight=='night') {
        # we will need to tell timestampsToSurvival the last timepoint, i.e. how long the day/night lasted in minutes
        lastt <- (24-dayduration) * 60 # typically 10 hours * 60 minutes
      }

    ## if by windows of interest
    } else if(!is.na(woi)) {

      # woi should be an even number of elements as start/end, start/end, etc.
      if (length(woi)%%2 !=0) stop('\t \t \t \t >>> Error splitFramesbyWoi: please give start and stop timestamps of each window of interest.
                               Accordingly, woi should have an even number of timestamps. \n')

      # calculate duration of each window
      # make a small dataframe: one column with all start timestamps, one column will all stop timestamps
      # each row is a window of interest
      wois <- data.frame(start=woi[seq(1, length(woi), 2)], # start timestamps are at odd indices
                         stop=woi[seq(2, length(woi), 2)]) # stop timestamps are at even indices

      woidur <- apply(wois, 1, function(stasto) {
        difftime( lubridate::dmy_hms(stasto['stop']) , lubridate::dmy_hms(stasto['start']), units='mins')
      })

      # make sure we do not have any negative durations
      # which would be the sign of a stop timestamp occuring before a start timestamp, i.e. not chronological
      if(length(woidur<0)>0) stop('\t \t \t \t >>> Error ggSleepLatencySurvival: can you check the woi setting,
                                  some timestamps are not chronological. \n')

      ###### 30/03/2023 left unfinished
      # just need to give woidur to timestampsToSurvival one by one as lastt setting
      # here is a woi setting to try on
      # woi <- c('16/02/2023 13:00:00', '16/02/2023 16:00:00',
      #          '17/02/2023 10:00:00', '17/02/2023 20:00:00')

    }
    # can now tell user which plot we are doing
    cat('\n \n \t \t \t \t >>> Preparing sleepLatency survival plot for ***', winn, '*** \n')

    # now split this window's `pal` by group
    # creates a list, each slot one 'slice' of the dataframe
    # each slice is called byg, for by group
    # on each slice, run timestampsToSurvival() to get survival timecourse
    swbg <- lapply(split(paw, paw$grp), function(byg) {
      timestampsToSurvival(dts=byg$param, ids=byg$fish, lastt=lastt) # from sleepLatency.R
    })
    # swbg for survival, for one window, split by group (into a list)
    # indeed, lapply returned a list, where names of elements are the group
    # to put back a grp column to each dataframe:
    for (g in 1:length(swbg)) {
      swbg[[g]] <- swbg[[g]] %>%
        add_column(grp=names(swbg)[[g]], .before=1)
    }

    # now fine to put everything in one long dataframe
    sw <- as.data.frame(data.table::rbindlist(swbg)) # sw for survival, one window
    # convert to dataframe for ggplot

    # operations above loses grporder, set it back
    sw$grp <- factor(sw$grp, levels=grporder)

    # do statistics now, if more than 1 group
    if(length(unique(sw$grp))>1) {
      survivalStats(svd=sw,
                    grporder=grporder,
                    lastt=lastt,
                    detailsOrNo=detailsOrNo)
    }

    # add percentage left column
    # and convert timestamp in minutes to hours
    sw <- sw %>%
      add_column(perleft=sw$proleft*100, .after='proleft') %>%
      add_column(dthrs=sw$dt/60)

    # where should the plot ends?
    # if user did not set xmax, make it end at end of window, i.e. lastt calculated above
    # if user set xmax, that is where it ends
    if(is.na(xmaxh)) {
      xmaxh <- lastt/60
    }


    # ready to plot
    ggLatency <- ggplot(sw, aes(x=dthrs, y=perleft, colour=grp)) + # sleep latency survival curve
      geom_step(linewidth=0.7) +
      {if(!is.na(colours[1])) scale_colour_manual(values=colours) } + # if user gave colours, follow them; if not ggplot will do default colours
      theme_minimal() +
      theme(
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=7),
        axis.title.y=element_text(size=9),
        axis.title.x=element_text(size=9),
        legend.title=element_blank()
      ) +

      {if(!legendOrNo) theme(legend.position='none')} +

      {if(!xtextOrNo) theme(axis.text.x=element_blank())} +

      {if(!ynameOrNo) theme(axis.title.y=element_blank())} +
      {if(ynameOrNo) ylab(label='% larvae still awake')} +

      {if(!xnameOrNo) theme(axis.title.x=element_blank())} +
      {if(xnameOrNo) xlab(label='hours since light transition')} +

      {if(nightBgOrNo & dayornight=='night') theme(
        panel.background=element_rect(fill='#d2d2d2', colour=NA)
      )} +

      coord_cartesian(xlim=c(0, xmaxh), ylim=c(0,100)) # putting this last just to make sure we do not finish with a +

    # export plot to pdf
    # build full path
    # directory is given by user as exportDir
    # filename should be YYMMDD_BX_sleepLatencySurv_win.pdf
    if(exportOrNo) {
      filenm <- paste0(unique(paw$date), '_',
                       unique(paw$box), '_',
                       'sleepLatency', '_',
                       unique(paw$win), '.pdf')
      cat('\t \t \t \t >>> Exporting *', filenm, '* in', exportDir, '\n')
      ggsave(filename=paste0(exportDir, '/', filenm), plot=ggLatency, width=width, height=height, units='mm', device=cairo_pdf)
    }

  }

  # can return the last plot so something is displayed in RStudio
  return(ggLatency)
}




# function ggSleepLatencyGrid(...) ----------------------------------------

# mainly a wrapper for ggSleepLatencySurvival()

# make two grids, one day grid and one night grid
# within each grid, each column = one time window, each row = one experiment

#' Title
#'
#' @param pa
#' @param grporder
#' @param skipNight0
#' @param colours
#' @param legendOrNo
#' @param xtextOrNo
#' @param xnameOrNo
#' @param ynameOrNo
#' @param nightBgOrNo
#' @param xmaxNight
#' @param detailsOrNo
#' @param exportDir
#' @param width
#' @param height
#' @param dayduration
#'
#' @return
#' @export
#'
#' @examples
ggSleepLatencyGrid <- function(pa,
                               grporder=NA,
                               skipNight0=FALSE,
                               colours=NA,
                               legendOrNo=TRUE,
                               xtextOrNo=TRUE,
                               xnameOrNo=TRUE,
                               ynameOrNo=TRUE,
                               nightBgOrNo=TRUE,
                               xmaxh=NA,
                               detailsOrNo=TRUE,
                               exportDir,
                               width=150,
                               height=100,
                               dayduration=14) {

  ### check export settings ###
  if(!dir.exists(exportDir))
    stop('\n \t \t \t \t >>> Error exportDir: output folder does not exist. \n')

  # if user did not give xmax, set to full duration
  if(is.na(xmaxh)) {
    xmaxh <- 24 - dayduration
  }

  # make an horizontal grid for each experiment we were given
  # typically: night1 plot / night2 plot

  # outer lapply goes through experiments one by one
  # we get back list latL, one element per experiment, and each element is an horizontal grid, where each plot is one window
  latL <- lapply(1:length(pa), function(p) {

    pe <- paramReadPivot(pa[p],
                         grporder=grporder,
                         skipNight0=skipNight0) # pa for one experiment
    # note, only reason we need to import is to look at the window names
    # otherwise ggSleepLatencySurvival() takes care of the importing etc.

    # windows to plot:
    wnps <- as.character(unique(pe$win)[which(startsWith(unique(pe$win), 'night'))])
    # will give names of night windows, e.g. night1, night2

    # no plot the day windows one by one
    # we get back list expdayL, i.e. day plots for one experiment
    expniL <- lapply(1:length(wnps), function(w) {

      ggSleepLatencySurvival(pa=pa[p], # *** here, goes through one experiment a a time, controlled by outer lapply
                             grporder=grporder,
                             skipNight0=skipNight0,
                             onlyWin=wnps[w], # *** here, goes one window at a time, controlled by inner lapply
                             colours=colours,
                             legendOrNo=legendOrNo,
                             xtextOrNo=xtextOrNo,
                             xnameOrNo=xnameOrNo,
                             ynameOrNo=ynameOrNo,
                             nightBgOrNo=nightBgOrNo,
                             xmaxh=xmaxh,
                             detailsOrNo=detailsOrNo,
                             exportOrNo=FALSE,
                             exportDir=exportDir,
                             width=NA,
                             height=NA,
                             dayduration=dayduration)
    })

    # we put the night plots in a grid
    expdayg <- ggpubr::ggarrange(plotlist=expniL, ncol=length(wnps), nrow=1)

    # and we are done with this experiment
    return(expdayg)
    # outer lapply will now go to next experiment
  })

  # we stack each experiment's horizontal grids in a bigger grid
  latgrid <- ggpubr::ggarrange(plotlist=latL, ncol=1, nrow=length(pa))
  # ncol=1 because we are stacking the grids now, not each plot

  # we are done for the DAY grid
  # build the filename we will export to
  ggsave(paste0(exportDir, 'sleepLatency', '.pdf'), plot=latgrid, width=width, height=height, units='mm')

}


