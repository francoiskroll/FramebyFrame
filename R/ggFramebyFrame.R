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



# function frameratePlot(...) ---------------------------------------------

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

#' Title
#'
#' @param pa
#' @param grporder
#' @param skipNight0
#' @param poolExp1
#' @param poolExp2
#' @param poolExp3
#' @param onlyDayorNight
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
                                   onlyDayorNight=NA,
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

  # check onlyDayorNight
  if (!is.na(onlyDayorNight) & onlyDayorNight!='day' & onlyDayorNight!='night')
    stop('\t \t \t \t >>> Error: onlyDayorNight can only be day, night, or NA. \n')

  # cannot skipNight0 AND ask to plot only night0
  if(!is.na(onlyWin)) {
    if (skipNight0 & onlyWin=='night0') stop('\t \t \t \t >>> Error: cannot skipNight0 AND ask to only plot night0. \n')
  }

  # if only day or night, needs to be consistent with which win to plot
  if (!is.na(onlyWin) & !is.na(onlyDayorNight)) {
    if(substr(onlyDayorNight, 1, 3)!=unique(substr(onlyWin, 1, 3)))
      stop('\t \t \t \t >>> Error: onlyDayorNight and onlyWin settings not compatible,
           probably asking to plot individual night(s) while asking for only day, or vice-versa. \n')
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

  # if onlyDay is given...
  if (!is.na(onlyDayorNight)) {
    pal <- pal %>%
      subset(daynight==onlyDayorNight)
  }

  # if onlyWin is given...
  if (!is.na(onlyWin[1])) {
    pal <- pal %>%
      subset(win==onlyWin)
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

    cat('\n \n \t \t \t \t >>> Preparing sleepLatency survival plot for ***', winn, '*** \n')

    ## are we analysing by day/night or windows of interest?
    # if by day/night
    if(!is.na(dayduration)) {
      # are we currently looking at day data or night data?
      dayornight <- unique(paw$daynight)

      # we will need to tell timestampsToSurvival the last timepoint, i.e. how long the day/night lasted in minutes

      if(dayornight=='day') {
        lastt <- dayduration * 60 # typically 14 hours * 60 minutes
      } else if(dayornight=='night') {
        lastt <- (24-dayduration) * 60 # typically 10 hours * 60 minutes
      }

    # if by windows of interest
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
      ) +

      {if(!legendOrNo) theme(legend.position='none')} +

      {if(!xtextOrNo) theme(axis.text.x=element_blank())} +

      {if(!ynameOrNo) theme(axis.title.y=element_blank())} +
      {if(ynameOrNo) ylab(label='% larvae which \nhave not slept yet')} +

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
#' @param xmaxDay
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



# function ggFingerprint(...) ---------------------------------------------

#' Title
#'
#' @param fgp
#' @param onlyFish
#' @param metric
#' @param grporder
#' @param controlGrp
#' @param removeControl
#' @param onlyExp
#' @param removeParam
#' @param colours
#' @param connectOrNo
#' @param legendOrNo
#' @param ynameOrNo
#' @param ytextOrNo
#' @param xtextOrNo
#' @param xParamNum
#' @param nightBgOrNo
#' @param ymin
#' @param ymax
#' @param exportOrNo
#' @param exportPath
#' @param width
#' @param height
#'
#' @return
#' @export
#'
#' @examples
#' @import ggplot2

ggFingerprint <- function(fgp,
                          onlyFish=NA,
                          metric='mean',
                          grporder=NA,
                          controlGrp,
                          removeControl=FALSE,
                          onlyExp=NA,
                          removeParam=NA,
                          colours=NA,
                          connectOrNo=TRUE,
                          legendOrNo=TRUE,
                          ynameOrNo=TRUE,
                          ytextOrNo=TRUE,
                          xtextOrNo=TRUE,
                          xParamNum=FALSE,
                          nightBgOrNo=TRUE,
                          ymin,
                          ymax,
                          exportOrNo=TRUE,
                          exportPath,
                          width=150,
                          height=100) {


  # check export settings ---------------------------------------------------

  if (exportOrNo) {
    if(substrEnding(exportPath, 4) != '.pdf') stop('\n \t \t \t \t >>> Error: exportPath does not end with .pdf.')

    # check if output folder exists
    if(!dir.exists(parentFolder(exportPath)))
      stop('\n \t \t \t \t >>> Error exportPath: output folder does not exist. \n')

  }


  # import fgp, if needed ---------------------------------------------------

  # if we are given a string, assume it is a path
  # if not, assume we are given fgp object directly

  if(is.character(fgp[1])) {
    fgl <- lapply(fgp, function(fgpath){

      if(!file.exists(fgpath)) stop('\t \t \t \t >>> Could not find file', fgpath, ': please check the path. \n')
      if(!endsWith(fgpath, '.csv')) stop('\t \t \t \t >>> Path does not end with .csv, please check. \n')

      return( read.csv(fgpath) )

    })
    fgp <- data.table::rbindlist(fgl)
  }
  # note, currently no option to give multiple fingerprints as objects
  # or in other words, if you want to give multiple fingerprints, give them as paths


  # check what to plot ------------------------------------------------------

  ### check metric setting
  if (!is.na(metric) & !metric %in% c('mean', 'median'))
    stop('\t \t \t \t Error ggFingerprint: metric setting can be NA or mean or median. \n')

  ### if we are not plotting all groups, remove those not in grporder
  if (!is.na(grporder[1])) { # if given a grporder

    # is there any group found in data that are not found in grporder?
    # below will return name(s) of group(s) found in data but not found in grporder
    grp2remove <- unique(fgp$grp) [! unique(fgp$grp) %in% grporder]

    if(length(grp2remove)>0) { # if any grp to remove
      # remove them
      cat('\t \t \t \t >>> Removing groups ', grp2remove, '\n')
      fgp <- fgp %>%
        subset(grp %in% grporder)
    }

    # now data only has groups found in grporder
    # order groups based on grporder
    fgp$grp <- factor(fgp$grp, levels=grporder)
  }


  ### if we are not plotting controls, remove them
  if (removeControl) {
    fgp <- fgp %>%
      subset(! grp %in% controlGrp)
    # if we removed them already above when looking at grporder this will not change anything
  }

  ### if we are not plotting every experiment, only keep the ones we want
  if (! is.na(onlyExp[1])) {
    fgp <- fgp %>%
      subset(date_box %in% onlyExp)
  }

  ### if we are plotting only one larva, keep that one only
  # (only useful for ggFingerprintGrid so can plot one larva at a time)
  # (important to do this *after* onlyExp so we get the correct fish within that exp)
  # first check that if onlyFish is ON, onlyExp is too
  if(!is.na(onlyFish[1]) & is.na(onlyExp[1]))
    stop('\t \t \t \t >>> Error: as you gave onlyFish, please also give onlyExp \n')

  if (!is.na(onlyFish[1])) {
    fgp <- fgp %>%
      subset(fish %in% onlyFish)
  }

  ### if we are not plotting every parameter, only keep the ones we want
  if (! is.na(removeParam[1])) {
    fgp <- fgp %>%
      subset(! parameter %in% onlyParam)
  }


  # order of parameters -----------------------------------------------------
  # for now, will do in order of allparameters, defined in FramebyFrame.R

  # first level the parameters as we want
  fgp$parameter <- factor(fgp$parameter, levels=allparameters)

  # second level the uparam
  # order(..., parameter) will follow the order we set above
  fgp$uparam <- factor(fgp$uparam,
                       levels=as.vector(unlist(unique(fgp[with(fgp, order(period, parameter)), 'uparam']))))

  # regarding order of fingerprints, it should be based on grporder
  # the groups are already ordered, but we need now to order date_box_grp
  fgp$date_box_grp <- factor(fgp$date_box_grp,
                             levels=as.vector(unlist(unique(fgp[with(fgp, order(grp, date, box)), 'date_box_grp']))))


  # colours -----------------------------------------------------------------

  ### if user did not provide any colours, we use automatic ggplot colours
  if(is.na(colours[1])) {
    colours <- scales::hue_pal()(length(unique(fgp$date_box_grp)))
  }


  # plot --------------------------------------------------------------------

  # set y axis name
  yname <- expression(paste('deviation from controls (', italic(Z), '-score)'))

  # set x text
  # ! need to be very careful not to change the order
  # the trick is to take the levels of uparam (set above), not the actual uparam column
  xticks <- strNthSplit(levels(fgp$uparam), '_', 2)

  # alternative x text is to have parameter number
  # ! should not assume we can just repeat a series of digits twice
  # because sunsetStartle (and maybe other parameters in the future) are not defined during both day and night
  # solution: look up each parameter from xticks (above) in allparameters
  # the position in allparameters becomes its number
  xparamnum <- match(xticks, allparameters)

  # find the middle of the plot to position the night background correctly
  # it should be the last day parameter + 0.5
  xmid <- max(which(startsWith(levels(fgp$uparam), 'day'))) + 0.5

  #####

  # how we start the plot depends a bit if onlyFish or not
  # IF ONLYFISH
  if(!is.na(onlyFish[1])) {

    cat('\t \t \t \t \t >>> plotting fish', onlyFish, '\n')

    # what is the colour we should be using?
    # depends on which group the fish we were given belongs to
    # simply pick a row with that fish and look at the group
    figrp <- as.character(unlist(fgp[which(fgp$fish == onlyFish[1])[1], 'grp']))
    # what is the colour for this group?
    # either the user gave us some colours or not, in which case we set some automatically above
    col2use <- colours[match(figrp, grporder)] # match will give the position of this group in grporder

    # now start the plot
    ggFgp <- ggplot(fgp, aes(x=uparam, y=pazm)) +
      geom_hline(yintercept=0, linetype=1, colour='#a7a7a7', linewidth=0.3) +
      geom_point(colour=col2use, size=0) +
      {if(connectOrNo) geom_line(aes(group=date_box_period_grp), linewidth=0.2, colour=col2use)} +
      {if(nightBgOrNo) annotate(geom='rect', xmin=xmid, xmax=Inf, ymin=-Inf, ymax=Inf, colour=NA, fill='#1d1d1b', alpha=0.2)} + # ***
      theme(panel.grid=element_blank())

    # IF MEAN FINGERPRINT
  } else {

    # do we plot mean ± sd?
    if (metric=='mean') {
      ggFgp <- ggplot(fgp, aes(x=uparam, y=mean, colour=date_box_grp)) +
        {if(nightBgOrNo) annotate(geom='rect', xmin=xmid, xmax=Inf, ymin=-Inf, ymax=Inf, colour=NA, fill='#1d1d1b', alpha=0.2)} + # ***
        geom_hline(yintercept=0, linetype=1, colour='#a7a7a7', linewidth=0.5) +
        geom_pointrange(aes(ymin=mean-sem, ymax=mean+sem), size=0.3) +
        {if(connectOrNo) geom_line(aes(group=date_box_period_grp), linewidth=0.2)} +
        scale_colour_manual(values=colours)

    } else if (metric=='median') {

      # do we plot median ± mad?
      ggFgp <- ggplot(fgp, aes(x=uparam, y=median, colour=date_box_grp)) +
        {if(nightBgOrNo) annotate(geom='rect', xmin=xmid, xmax=Inf, ymin=-Inf, ymax=Inf, colour=NA, fill='#1d1d1b', alpha=0.2)} + # ***
        geom_hline(yintercept=0, linetype=1, colour='#a7a7a7', linewidth=0.5) +
        geom_pointrange(aes(ymin=median-mad, ymax=median+mad), size=0.3) + # note here using MAD
        {if(connectOrNo) geom_line(aes(group=date_box_period_grp), linewidth=0.2)} +
        scale_colour_manual(values=colours)
    }

  }
  # *** annotate() solution for night background is not optimal as it covers the grid
  # but it is a rabbit hole to try to change the panel background of just that plot

  #####

  # polish the plot
  ggFgp <- ggFgp +
    theme_minimal() +
    theme(
      panel.grid.minor.y=element_blank(),
      axis.title.x=element_blank(),
      legend.title=element_blank()) +

    {if(!is.na(onlyFish[1])) theme(plot.margin=unit(c(0, 0, 0, 0), 'lines'))} +
    # to reduce spacing between plots when combining them in grid

    {if(!legendOrNo) theme(legend.position='none')} +

    {if(!ynameOrNo) theme(axis.title.y=element_blank())} +

    {if(ynameOrNo) theme(axis.title.y=element_text(size=9, margin=margin(t=0, r=-1.5, b=0, l=0)))} +

    {if(ynameOrNo) ylab(yname)} +

    {if(!ytextOrNo) theme(axis.text.y=element_blank())} +
    {if(ytextOrNo) theme(axis.text.y=element_text(size=7))} +

    {if(xtextOrNo & !xParamNum) scale_x_discrete(labels=xticks)} +
    {if(xtextOrNo & !xParamNum) theme(axis.text.x=element_text(size=7, angle=45, hjust=1))} +
    {if(!xtextOrNo) theme(axis.text.x=element_blank())} +


    {if(xParamNum) scale_x_discrete(labels=xparamnum)} +
    {if(xParamNum) theme(axis.text.x=element_text(size=6))} +

    coord_cartesian(ylim=c(ymin, ymax))

  ### export the plot
  if(exportOrNo) {
    ggsave(exportPath, ggFgp, width=width, height=height, units='mm')
  }

  ### return plot
  return(ggFgp)

}



# function ggFingerprintGrid(...) -----------------------------------------
# wrapper for ggFingerprint() to make a grid with fingerprints for single larvae

#' Title
#'
#' @param fgp
#' @param grporder
#' @param onlyExp
#' @param removeParam
#' @param colours
#' @param connectOrNo
#' @param legendOrNo
#' @param ynameOrNo
#' @param ytextOrNo
#' @param xtextOrNo
#' @param xParamNum
#' @param nightBgOrNo
#' @param ymin
#' @param ymax
#' @param nrow
#' @param ncol
#' @param exportPath
#' @param width
#' @param height
#'
#' @return
#' @export
#'
#' @examples
ggFingerprintGrid <- function(fgp,
                              grporder=NA,
                              onlyExp=NA,
                              removeParam=NA,
                              colours=NA,
                              connectOrNo=TRUE,
                              legendOrNo=TRUE,
                              ynameOrNo=TRUE,
                              ytextOrNo=TRUE,
                              xtextOrNo=TRUE,
                              xParamNum=FALSE,
                              nightBgOrNo=TRUE,
                              ymin,
                              ymax,
                              nrow,
                              ncol,
                              exportPath,
                              width=150,
                              height=100) {

  ### check export settings ###

  if(substrEnding(exportPath, 4) != '.pdf') stop('\n \t \t \t \t >>> Error: exportPath does not end with .pdf.')

  # check if output folder exists
  if(!dir.exists(parentFolder(exportPath)))
    stop('\n \t \t \t \t >>> Error exportPath: output folder does not exist. \n')


  # if onlyExp is given, keep only that experiment
  if(!is.na(onlyExp[1])) {
    fgp <- fgp %>%
      subset(date_box %in% onlyExp)
  }

  # ! only works for one experiment at a time
  # so stop if multiple experiments
  if(length(unique(fgp$date_box))!=1)
    stop('\t \t \t \t >>> Error: ggFingerprintGrid only works on one experiment at a time.\
         \t \t \t You can give the experiment you want as onlyExp, e.g. onlyExp=220725_14 \n')

  # save experiment as onlyExp
  onlyExp <- unique(fgp$date_box)


  # loop through fish available for that experiment,
  # each time creating the ggFingerprint
  # we get list of plots ggL, where each element is ggFingerprint for one larva
  fis <- mixedsort(as.character(unique(fgp$fish))) # ! mixedsort here, otherwise goes f89, f9, f91

  ggL <- lapply(1:length(fis), function(fi) {
    ggFingerprint(fgp=fgp,
                  onlyFish=fis[fi],
                  grporder=grporder,
                  controlGrp=NA,
                  removeControl=FALSE,
                  onlyExp=onlyExp,
                  removeParam=removeParam,
                  colours=colours,
                  connectOrNo=connectOrNo,
                  legendOrNo=legendOrNo,
                  ynameOrNo=ynameOrNo,
                  ytextOrNo=ytextOrNo,
                  xtextOrNo=xtextOrNo,
                  xParamNum=xParamNum,
                  nightBgOrNo=nightBgOrNo,
                  ymin=ymin,
                  ymax=ymax,
                  exportOrNo=FALSE,
                  exportPath=NA,
                  width=NA,
                  height=NA)
  })


  # place these plots in a grid
  ggG <- ggarrange(plotlist=ggL, nrow=nrow, ncol=ncol)

  # export
  ggsave(exportPath, ggG, width=width, height=height, units='mm')


}





# function ggPairwiseHeat(...) --------------------------------------------

# pwm for pairwise matrix

#' Title
#'
#' @param pwm
#' @param simScore
#' @param grporder
#' @param medianMid
#' @param minCol
#' @param maxCol
#' @param onlyHalf
#' @param scoreSize
#' @param legendOrNo
#' @param labelsOrNo
#' @param width
#' @param height
#' @param exportPath
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#' @import ggplot2

ggPairwiseHeat <- function(fgp,
                           metric='mean',
                           simScore,
                           grporder=NA,
                           removeControl=TRUE,
                           controlGrp,
                           medianMid=FALSE,
                           minCol=NA,
                           maxCol=NA,
                           onlyHalf=NA,
                           scoreSize=NA,
                           legendOrNo=TRUE,
                           labelsOrNo=TRUE,
                           width=250,
                           height=200,
                           exportPath) {

  ### check export settings ###

  if(substrEnding(exportPath, 4) != '.pdf') stop('\n \t \t \t \t >>> Error: exportPath does not end with .pdf.')

  # check if output folder exists
  if(!dir.exists(parentFolder(exportPath)))
    stop('\n \t \t \t \t >>> Error exportPath: output folder does not exist. \n')

  ### check similarity score ###
  if(! simScore %in% c('cosine', 'correlation', 'euclidean'))
    stop('\t \t \t \t >>> Error: does not support this similarity score, similarity scores currently supported are: cosine, correlation, euclidean \n')

  # check that both minCol & maxCol are given or none
  colurs <- c(minCol, maxCol)
  colurs <- colurs[!is.na(colurs)]
  if(length(colurs)==1) stop('\t \t \t \t Error: please give both minCol and maxCol or none \n')


  ### import fgp, if needed ###

  # if we are given a string, assume it is a path
  # if not, assume we are given fgp object directly
  if(is.character(fgp)) {

    if(!file.exists(fgp)) stop('\t \t \t \t >>> Could not find file', fgp, ': please check the path. \n')
    if(!endsWith(fgp, '.csv')) stop('\t \t \t \t >>> Path does not end with .csv, please check. \n')

    fgp <- read.csv(fgp)
  }


  ### calculate similarity scores ###
  # using fingerprintSimilarity(...)
  pwm <- fingerprintSimilarity(fgp=fgp,
                               metric=metric,
                               simScore,
                               grporder=grporder,
                               removeControl=removeControl,
                               controlGrp=controlGrp)

  # fingerprintSimilarity will print the similarity matrix to Console


  ### only upper or lower triangle, if required ###

  if (is.na(onlyHalf)) {
    # do nothing
  } else if (onlyHalf=='upper') {
    pwm <- getLowerTri(pwm, skipCol=1) # **
  } else if (onlyHalf=='lower') {
    pwm <- getUpperTri(pwm, skipCol=1) # **
  } else {
    stop('\t \t \t \t Error: wrong onlyHalf setting, please give \'lower\' or \'upper\' or NA \n')
  }
  # ** counter-intuitive that when upper we keep lower and vice-versa
  # but it works and it is horribly complicated to get these tiles in the right order


  # pivot longer ------------------------------------------------------------

  pwl <- pwm %>%
    pivot_longer(-date_box_grp,
                 names_to='vs_date_box_grp',
                 values_to='score')
  # using score here to stay agnostic whether we mean correlation or cosine similarity or ...

  # order the fingerprints, following the order pwm (which was set based on grporder in fingerprintSimilarity())
  pwl$date_box_grp <- factor(pwl$date_box_grp, levels=rev(pwm$date_box_grp))
  pwl$vs_date_box_grp <- factor(pwl$vs_date_box_grp, levels=rev(pwm$date_box_grp))

  # plot --------------------------------------------------------------------

  # round the scores so we can write them in the plot
  pwl$score <- round(pwl$score, 2)

  # to have a good colour scale for euclidean distances, we need to find the max
  # get max and round it up, maximum colour should correspond to that
  maxcol <- ceiling(max(pwl$score))
  # get median as midpoint
  medcol <- median(pwl$score)

  # did the user give us some colours? If not, set some defaults now
  # if cosine or correlation, -1 is blue and +1 is orange
  # we checked above that both or none were given, so can just check that one is NA
  if(is.na(minCol)) {
    if(simScore %in% c('cosine', 'correlation')) {
      minCol <- '#588a9a'
      maxCol <- '#EE7163'

    } else if(simScore=='euclidean') {
      # bit more complicated, depends whether user wants median as midpoint or just a gradient from 0

      if(medianMid) {
        minCol <- '#588a9a'
        maxCol <- '#EE7163'
      } else {
        # simple gradient from 0
        minCol <- '#ffffff'
        maxCol <- '#b52414' # deeper red, it is #EE7163 - 8 levels
      }
    }
  }

  if (is.na(scoreSize)) {
    scoreSize <- 3
  }

  ggheat <- ggplot(pwl, aes(x=date_box_grp, y=vs_date_box_grp, fill=score)) +
    geom_tile(colour=NA) +
    geom_text(aes(x=date_box_grp, y=vs_date_box_grp, label=score), colour='#595E60', size=scoreSize) +
    theme_minimal() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.x=element_text(angle=90, size=7),
      axis.text.y=element_text(size=7),
      legend.title=element_blank(),
      legend.text=element_text(size=5),
      plot.margin=unit(c(0, 0, 0, 0), 'pt')
    ) +
    {if(!legendOrNo) theme(legend.position='none')} +

    {if(!labelsOrNo) theme(axis.text.x=element_blank(),
                           axis.text.y=element_blank())} +

    guides(fill=guide_colourbar(barwidth=0.5, barheight=5)) +

    {if(simScore %in% c('cosine', 'correlation')) scale_fill_gradient2(low=minCol, high=maxCol,
                                                                       midpoint=0, limit = c(-1,1), na.value=NA) } +
    {if(simScore == 'euclidean' & medianMid) scale_fill_gradient2(low=minCol, high=maxCol,
                                                                  midpoint=medcol, limit = c(0,maxcol), na.value=NA) } +
    {if(simScore == 'euclidean' & !medianMid) scale_fill_gradient2(low=minCol, high=maxCol,
                                                                   limit = c(0,maxcol), na.value=NA) } +
    coord_equal() # makes it sure the tiles are squares



  # export plot -------------------------------------------------------------

  ggsave(exportPath, ggheat, width=width, height=height, units='mm')


  # return plot -------------------------------------------------------------
  # so it displays in RStudio
  return(ggheat)


}
