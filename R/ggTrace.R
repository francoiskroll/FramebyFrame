###################################################
# ~~~ FramebyFrame package ~~~

# ggTrace

# every plotting function that involves drawing an activity or sleep trace

# Francois Kroll 2022
# francois@kroll.be
###################################################




# function ggActivityTraceGrid(...) ---------------------------------------

#' Title
#'
#' @param ffpath
#' @param genopath
#' @param zebpath
#' @param dayduration
#' @param smoothOrNo
#' @param smooth_nsecs
#' @param binOrNo
#' @param bin_nsecs
#' @param onlyWell
#' @param tracecols
#' @param linethick
#' @param ymin
#' @param ymax
#' @param xstart
#' @param xstop
#' @param trimstart
#' @param trimstop
#' @param xmajorOrNo
#' @param ymajorOrNo
#' @param sunlinesOrNo
#' @param nightBgOrNo
#' @param markTimes
#' @param nrow
#' @param ncol
#' @param exportOrNo
#' @param exportPath
#' @param width
#' @param height
#'
#' @return
#' @export
#'
#' @examples

ggActivityTraceGrid <- function (ffpath,
                                 genopath,
                                 zebpath,
                                 dayduration=14,
                                 smoothOrNo=FALSE,
                                 smooth_nsecs=30*60,
                                 binOrNo=TRUE,
                                 bin_nsecs=10*60,
                                 onlyWell=NA,
                                 tracecols=NA,
                                 linethick=0.4,
                                 ymin=0,
                                 ymax,
                                 xstart=0,
                                 xstop=0,
                                 trimstart=0,
                                 trimstop=0,
                                 xmajorOrNo=TRUE,
                                 ymajorOrNo=TRUE,
                                 sunlinesOrNo=FALSE,
                                 nightBgOrNo=FALSE,
                                 markTimes=NA,
                                 nrow,
                                 ncol,
                                 exportOrNo=TRUE,
                                 exportPath,
                                 width=75,
                                 height=55) {

  ### check export settings ###

  if (exportOrNo) {
    if(substrEnding(exportPath, 4) != '.pdf') stop('\n \t \t \t \t >>> Error: exportPath does not end with .pdf.')

    # check if output folder exists
    if(!dir.exists(parentFolder(exportPath)))
      stop('\n \t \t \t \t >>> Error exportPath: output folder does not exist. \n')

  }


  # prepare activity timecourse --------------------------------------------
  # will skip automatically if was calculated already with same settings

  # acttc for activity timecourse
  acttc <- summaryActivityCourse(ffpath=ffpath,
                                 genopath=genopath,
                                 zebpath=zebpath,
                                 dayduration=dayduration,
                                 smoothOrNo=smoothOrNo,
                                 smooth_nsecs=smooth_nsecs,
                                 binOrNo=binOrNo,
                                 bin_nsecs=bin_nsecs)


  # about colours -----------------------------------------------------------

  # all the wells mentioned in the data:
  wids <- colnames(acttc) [ which(grepl("^f+[[:digit:]]", colnames(acttc)))[1] : ncol(acttc) ] # well IDs

  # if the first one is f1, we have box1 data
  # if the first one is f97, we have box2 data
  # this is correct even if plate is not 96-well
  if(wids[1]=='f1') {
    boxnum <- 1
  } else if (wids[1]=='f97') {
    boxnum <- 2
  } else {
    stop('Error ggActivityTraceGrid: first well in this dataset is not f1 or f96, which is not a case we have seen before. \n')
  }

  # now import genotype info
  geno <- fillGenotype(importGenotype(genopath), boxnum=boxnum)
  # fillGenotype will add an excluded column if needed
  # how many groups (not counting excluded) do we have?
  ngrps <- sum(colnames(geno) != 'excluded')

  # if user did not provide any colours, we pick some automatic ggplot colours
  if(is.na(tracecols[1])) {
    tracecols <- scales::hue_pal()(ngrps)
    # if user provided colours, check we have enough of them
  } else {
    if(length(tracecols) != ngrps) stop('\t \t \t \t >>> Error ggActivityTraceGrid: please provide one tracecol for each group in genotype file. \n')
  }

  # if there is some excluded wells, add grey as last tracecol
  if('excluded' %in% colnames(geno)) {
    tracecols <- c(tracecols, '#aeb3b4')
  }

  # as we plot each time N=1, there is no ribbon, so no need to worry about ribbon colours


  # give data to ggTrace(...) -----------------------------------------------
  # one well at a time

  # get the names of the wells
  # grepl gets first column which is fX e.g. f1
  wids <- colnames(acttc) [ which(grepl("^f+[[:digit:]]", colnames(acttc)))[1] : ncol(acttc) ] # well IDs

  # the user may have told us to plot only specific wells
  if(!is.na(onlyWell[1])) {
    wids <- wids[wids %in% onlyWell]
  }

  ggL <- lapply(1:length(wids), function(w) {

    cat('\t \t \t \t \t >>> plotting well', wids[w], '\n')

    ### what is the colour we should be using for that well?
    grpcol <- tracecols[which(geno==substr(wids[w], 2, 99), arr.ind=TRUE)[2]]
    # which() above will return the row & column index where the well is genotype file
    # then we take the column index to pick the colour in tracecols

    ggTrace(tc=acttc,
            genopath,
            dayduration=dayduration,
            ribbon=NA,
            grporder=NA,
            onlyWell = wids[w], # here!
            tracecols=grpcol,
            ribboncols=NA,
            linethick=linethick,
            xname='',
            yname='',
            xtextOrNo=FALSE,
            ytextOrNo=FALSE,
            ymin=ymin,
            ymax=ymax,
            xstart=xstart,
            xstop=xstop,
            trimstart=trimstart,
            trimstop=trimstop,
            xmajorOrNo=xmajorOrNo,
            ymajorOrNo=ymajorOrNo,
            sunlinesOrNo=sunlinesOrNo,
            nightBgOrNo=nightBgOrNo,
            markTimes=markTimes,
            legendOrNo=FALSE,
            gridMode=TRUE,
            exportOrNo=FALSE,
            exportPath=NA,
            width=NA,
            height=NA)

  })


  # arrange in a grid and export --------------------------------------------

  gggrid <- ggpubr::ggarrange(plotlist=ggL, ncol=ncol, nrow=nrow)

  ggplot2::ggsave(exportPath, gggrid, width=width, height=height, units='mm')

  # return plot so it displays in RStudio
  return(gggrid)


}


# ggActivityTraceByGroup(...) ---------------------------------------------

# end-user function for plotting activity trace

#' Title
#'
#' @param ffpath
#' @param genopath
#' @param zebpath
#' @param dayduration
#' @param smoothOrNo
#' @param smooth_nsecs
#' @param binOrNo
#' @param bin_nsecs
#' @param ribbon
#' @param grporder
#' @param onlyWell
#' @param tracecols
#' @param ribboncols
#' @param linethick
#' @param xname
#' @param yname
#' @param xtextOrNo
#' @param ytextOrNo
#' @param ymin
#' @param ymax
#' @param xstart
#' @param xstop
#' @param trimstart
#' @param trimstop
#' @param xmajorOrNo
#' @param ymajorOrNo
#' @param nightBgOrNo
#' @param sunlinesOrNo
#' @param markTimes
#' @param legendOrNo
#' @param exportOrNo
#' @param exportPath
#' @param width
#' @param height
#'
#' @return
#' @export
#'
#' @examples

ggActivityTraceByGroup <- function(ffpath,
                                   genopath,
                                   zebpath,
                                   dayduration=14,
                                   smoothOrNo=FALSE,
                                   smooth_nsecs=30*60,
                                   binOrNo=TRUE,
                                   bin_nsecs=10*60,
                                   ribbon='sem',
                                   grporder=NA,
                                   onlyWell=NA,
                                   tracecols,
                                   ribboncols,
                                   linethick=0.4,
                                   xname,
                                   yname,
                                   xtextOrNo=TRUE,
                                   ytextOrNo=TRUE,
                                   ymin=0,
                                   ymax,
                                   xstart=0,
                                   xstop=0,
                                   trimstart=0,
                                   trimstop=0,
                                   xmajorOrNo=TRUE,
                                   ymajorOrNo=TRUE,
                                   nightBgOrNo=FALSE,
                                   sunlinesOrNo=FALSE,
                                   markTimes=NA,
                                   legendOrNo=TRUE,
                                   exportOrNo=TRUE,
                                   exportPath,
                                   width=75,
                                   height=55) {



  # prepare activity timecourse --------------------------------------------

  # acttc for activity timecourse
  acttc <- summaryActivityCourse(ffpath=ffpath,
                                 genopath=genopath,
                                 zebpath=zebpath,
                                 dayduration=dayduration,
                                 smoothOrNo=smoothOrNo,
                                 smooth_nsecs=smooth_nsecs,
                                 binOrNo=binOrNo,
                                 bin_nsecs=bin_nsecs)




  # give data to ggTrace() --------------------------------------------------
  ggTrace(tc=acttc,
          genopath=genopath,
          dayduration=dayduration,
          ribbon=ribbon,
          grporder=grporder,
          onlyWell=onlyWell,
          tracecols=tracecols,
          ribboncols=ribboncols,
          linethick=linethick,
          xname=xname,
          yname=yname,
          xtextOrNo=xtextOrNo,
          ytextOrNo=ytextOrNo,
          ymin=ymin,
          ymax=ymax,
          xstart=xstart,
          xstop=xstop,
          trimstart=trimstart,
          trimstop=trimstop,
          xmajorOrNo=xmajorOrNo,
          ymajorOrNo=ymajorOrNo,
          nightBgOrNo=nightBgOrNo,
          sunlinesOrNo=sunlinesOrNo,
          markTimes=markTimes,
          legendOrNo=legendOrNo,
          exportOrNo=exportOrNo,
          exportPath=exportPath,
          width=width,
          height=height)
}



# ggSleepTraceByGroup(...) ------------------------------------------------

# end-user function to plot sleep trace by group

#' Title
#'
#' @param ffpath
#' @param genopath
#' @param zebpath
#' @param zthr_min
#' @param inaThr
#' @param epo_min
#' @param dayduration
#' @param smoothOrNo
#' @param smooth_npoints
#' @param ribbon
#' @param grporder
#' @param onlyWell
#' @param tracecols
#' @param ribboncols
#' @param linethick
#' @param xname
#' @param yname
#' @param xtextOrNo
#' @param ytextOrNo
#' @param ymin
#' @param ymax
#' @param xstart
#' @param xstop
#' @param trimstart
#' @param trimstop
#' @param xmajorOrNo
#' @param ymajorOrNo
#' @param nightBgOrNo
#' @param sunlinesOrNo
#' @param markTimes
#' @param legendOrNo
#' @param exportOrNo
#' @param exportPath
#' @param width
#' @param height
#'
#' @return
#' @export
#'
#' @examples
ggSleepTraceByGroup <- function(ffpath,
                                genopath,
                                zebpath,
                                zthr_min=1,
                                inaThr=0,
                                epo_min=10,
                                dayduration=14,
                                smoothOrNo=TRUE,
                                smooth_npoints=5,
                                ribbon='sem',
                                grporder=NA,
                                onlyWell=NA,
                                tracecols,
                                ribboncols,
                                linethick=0.4,
                                xname,
                                yname,
                                xtextOrNo=TRUE,
                                ytextOrNo=TRUE,
                                ymin=0,
                                ymax,
                                xstart=0,
                                xstop=0,
                                trimstart=0,
                                trimstop=0,
                                xmajorOrNo=TRUE,
                                ymajorOrNo=TRUE,
                                nightBgOrNo=FALSE,
                                sunlinesOrNo=FALSE,
                                markTimes=NA,
                                legendOrNo=TRUE,
                                exportOrNo=TRUE,
                                exportPath,
                                width=75,
                                height=55) {



  # prepare sleep timecourse ------------------------------------------------

  zzztc <- summarySleepCourse(ffpath=ffpath,
                              genopath=genopath,
                              zebpath=zebpath,
                              zthr_min=zthr_min,
                              inaThr=inaThr,
                              epo_min=epo_min,
                              dayduration=dayduration,
                              smoothOrNo=smoothOrNo,
                              smooth_npoints=smooth_npoints)



  # give data to ggTrace() --------------------------------------------------

  ggTrace(tc=zzztc,
          genopath=genopath,
          dayduration=dayduration,
          ribbon=ribbon,
          grporder=grporder,
          onlyWell=onlyWell,
          tracecols=tracecols,
          ribboncols=ribboncols,
          linethick=linethick,
          xname=xname,
          yname=yname,
          xtextOrNo=xtextOrNo,
          ytextOrNo=ytextOrNo,
          ymin=ymin,
          ymax=ymax,
          xstart=xstart,
          xstop=xstop,
          trimstart=trimstart,
          trimstop=trimstop,
          xmajorOrNo=xmajorOrNo,
          ymajorOrNo=ymajorOrNo,
          nightBgOrNo=nightBgOrNo,
          sunlinesOrNo=sunlinesOrNo,
          markTimes=markTimes,
          legendOrNo=legendOrNo,
          exportOrNo=exportOrNo,
          exportPath=exportPath,
          width=width,
          height=height)


}




# ggTrace(...) ------------------------------------------------------------

# common function to plot timecourse data
# no defaults because not meant to be called by end user directly

#' Title
#'
#' @param tc
#' @param genopath
#' @param dayduration
#' @param ribbon
#' @param grporder
#' @param onlyWell
#' @param tracecols
#' @param ribboncols
#' @param linethick
#' @param xname
#' @param yname
#' @param xtextOrNo
#' @param ytextOrNo
#' @param ymin
#' @param ymax
#' @param xstart
#' @param xstop
#' @param trimstart
#' @param trimstop
#' @param xmajorOrNo
#' @param ymajorOrNo
#' @param nightBgOrNo
#' @param sunlinesOrNo
#' @param markTimes
#' @param legendOrNo
#' @param gridMode
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
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr summarise_at

ggTrace <- function(tc,
                    genopath,
                    dayduration,
                    ribbon,
                    grporder,
                    onlyWell,
                    tracecols,
                    ribboncols,
                    linethick,
                    xname,
                    yname,
                    xtextOrNo,
                    ytextOrNo,
                    ymin,
                    ymax,
                    xstart,
                    xstop,
                    trimstart,
                    trimstop,
                    xmajorOrNo,
                    ymajorOrNo,
                    nightBgOrNo,
                    sunlinesOrNo,
                    markTimes,
                    legendOrNo,
                    gridMode=FALSE,
                    exportOrNo,
                    exportPath,
                    width,
                    height) {


  # check various settings --------------------------------------------------

  # check exportPath ends with .pdf and that folder exists

  if (exportOrNo) {
    if(substrEnding(exportPath, 4) != '.pdf') stop('\n \t \t \t \t >>> Error: exportPath does not end with .pdf.')

    # check if output folder exists
    if(!dir.exists(parentFolder(exportPath)))
      stop('\n \t \t \t \t >>> Error exportPath: output folder does not exist. \n')

  }

  # if given specific wells, add 'excluded' to grporder
  # here is the logic: if for some reason we are giving specific wells to plot, we genuinely want to plot them,
  # i.e. none should be excluded because not labelled in genotypefile
  # so should not remove excluded, while still allowing a grporder
  # solution: add 'excluded' to the grporder, as last position
  # and will give a default light grey colour
  if (!is.na(onlyWell[1]) & !is.na(grporder[1])) {

    grporder <- c(grporder, 'excluded')

    if (!is.na(tracecols[1])) {tracecols <- c(tracecols, '#aeb3b4')}
    if (!is.na(ribboncols[1])) {ribboncols <- c(ribboncols, '#d6d8d8')}

  }


  # pivot, assign genotypes, summarise by group -----------------------------

  tcl <- pivotLongAssignGenotype(txf=tc,
                                 genopath=genopath) # from genotypeUtilities.R
  # summary timecourse long format

  # if given onlyWell, check now they are actually in the data
  if (!is.na(onlyWell[1])) {
    if(!all(onlyWell %in% tcl$fish))
      stop('\t \t \t \t >>> Error: in onlyWell, given wells that are not present in the data \n')
  }

  # mainly need mean +- SEM within group for each timepoint
  # summary stats for plot with ribbon

  # are we plotting only some well(s)?
  # then take care of this now
  if (!is.na(onlyWell[1])) {
    # about below: bit akward to calculate mean etc. if given a single well, but will not change the data and allows to keep all the rest the same
    sbg <- tcl %>%
      filter(fish %in% onlyWell) %>%
      group_by(grp, fullts, zhrs, exsecs) %>%
      summarise_at(vars(px),
                   list(
                     mean= ~ mean(.),
                     sd= ~ sd(.),
                     sem= ~ sem(.),
                     nfis= ~ length(.)
                   ))
  } else if ( is.na(onlyWell[1]) ) {
    # if plotting grouped by genotype as normal:

    # sbg for summary by group
    sbg <- tcl %>%
      filter(grp != 'excluded') %>%
      group_by(grp, fullts, zhrs, exsecs) %>%
      summarise_at(vars(px),
                   list(
                     mean= ~ mean(.),
                     sd= ~ sd(.),
                     sem= ~ sem(.),
                     nfis= ~ length(.)
                   ))

  }

  # if any group has only one larva, there will not be a SD/SEM ribbon
  # warn the user as this can be confusing
  # note, this is only if the user asked to plot a ribbon, i.e. that ribbon is not NA
  if(!is.na(ribbon) & length(which(sbg$nfis==1)) > 0) {
    cat('\t \t \t \t >>> Note, following group(s) have N=1 so cannot calculate a', toupper(ribbon), 'ribbon:',
        as.character(unique(sbg[which(sbg$nfis==1), 'grp'])), '\n')
  }


  # check some plotting choices ---------------------------------------------

  # should we mark specific times? Then we need to find their Zeitgeber durations
  if (!is.na(markTimes[1])) {
    # Zeitgeber durations are number of hours since 9 AM on day0
    # so for each markTimes, convert to lubridate format and calculate number of hours since 9 AM day 0
    # and that will give where to place the mark on X axis
    markZth <- as.numeric(unlist(sapply(markTimes, function(ti) {
      difftime(lubridate::ymd_hms(ti), lubridate::ymd_hms(paste(lubridate::date(sbg$fullts[1]), '09:00:00')), units='hours')
    })))
  }

  # xstop = 0 means plot all of the timecourse
  if (xstop==0) {xstop=max(sbg$zhrs)}

  # are we plotting every group? depends on what given in grporder
  # additionally, re-order factors of grp column so legend etc. are in the order required
  if ( !is.na(grporder[1]) ) {

    # check that the grps given by the user are actually in the data:
    if(!all(grporder %in% unique(sbg$grp)))
      stop('\n \t \t \t \t >>> Group(s): ', grporder[!grporder %in% unique(sbg$grp)],' are given in grporder but are not present in the data. \
           Do you want to plot every group? You can set grporder=NA. \n')

    if( length(which(! unique(sbg$grp) %in% grporder)) > 0 ) {
      cat('\t \t \t \t >>> Removing larvae from group(s)', unique(sbg$grp) [which(! unique(sbg$grp) %in% grporder)], '\n')

      sbg <- sbg %>%
        filter(grp %in% grporder)
    }

    sbg$grp <- factor(sbg$grp, levels=grporder)

  }

  # should we trim the start of the experiment?
  if (trimstart!=0) { # if trimming beginning of the experiment
    # trimstart = 0 means no trimming
    sbg <- sbg %>%
      filter(zhrs >= trimstart)
  }

  # should we trim the end of the experiment?
  if (trimstop!=0) { # if trimming end of the experiment
    # trimstop = 0 means no trimming
    # (I do not think there is any case where the user would actually want to set trimstop to 0 as that would mean not plotting any data)
    sbg <- sbg %>%
      filter(zhrs <= trimstop)
  }

  # should we plot sunlines? Then we need to find their positions
  if (sunlinesOrNo) {

    # sunsets and sunrises times (in number of hours since day0 sunrise at 9AM) for 10 days
    suns <- c(rbind(seq(dayduration, 720, 24), seq(24, 720, 24)))
    # typically dayduration = 14
    # so will go 14, 24, 38, 48 etc.

    # pick only the ones within the span of the data we want to plot
    suns <- intersect(suns, xstart:xstop)

  }

  # should we add night grey backgounds? Then we need to know how many to add and where
  if(nightBgOrNo) {
    # similar logic as above, first prepare a long sequence of sunrise/sunset times
    suns <- c(rbind(seq(dayduration, 720, 24), seq(24, 720, 24)))
    # assuming experiment starts during the day,
    # the start of each night is at position 1, 3, 5, ...
    # and the stop of each night is at position 2, 4, 6, ...
    # make a small dataframe, each row is one night and two columns: start and stop
    nidf <- data.frame(start=suns[seq(1, length(suns), 2)], stop=suns[seq(2, length(suns), 2)])
    # that's all we need
    # essentially we'll add a bunch of night backgrounds below and rely on coord_cartesian to crop them

  }


  # assuming experiment starts during the day, then first time in suns must be sun

  # when only plotting specific wells, it is likely we do not have all the groups represented
  # and consequently that tracecols/ribboncols are not given in right order
  if (!is.na(grporder[1]) & !is.na(onlyWell[1]) & !is.na(tracecols[1]) & !is.na(ribboncols[1])) {

    # which group is represented in data we kept?
    # the %in% returns TRUE when grp represented in data we kept
    # so can just use it to keep only colours we need
    tracecols <-  tracecols[levels(sbg$grp) %in% sbg$grp]
    ribboncols <- ribboncols[levels(sbg$grp) %in% sbg$grp]

  }

  # if user does not want a ribbon, i.e. ribbon is NA
  # then replace by 'none' so can test below if sem or sd without throwing an error
  # (NA != 'sem' throws an error)
  if(is.na(ribbon)) {ribbon <- 'none'}


  # main plotting function --------------------------------------------------

  # if only one fish, ribbon will throw a Warning

  tracebygrp <-

    sbg %>%

    ggplot(., aes(x=zhrs, y=mean, col=grp)) +

    # add any horizontal lines now so they appear behind the trace
    {if(sunlinesOrNo) geom_vline(xintercept=suns, linetype=2, size=0.2)} +
    {if(!is.na(markTimes[1])) geom_vline(xintercept=markZth, linetype=2, size=0.2)} +

    {if(nightBgOrNo) annotate('rect', xmin=nidf$start, xmax=nidf$stop, ymin=ymin, ymax=Inf, alpha=0.2, fill='#1d1d1b')} +

    # add ribbon SEM
    {if(ribbon=='sem' & !is.na(ribboncols[1])) geom_ribbon(aes(ymin=mean-sem, ymax=mean+sem, fill=grp), colour=NA)} +

    # or ribbon SD
    {if(ribbon=='sd' & !is.na(ribboncols[1])) geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd, fill=grp), colour=NA)} +

    # but if we are not given ribbon colours (previous was if given ribbon colours)
    # then same but adding some transparency to the ribbon
    # add ribbon SEM
    {if(ribbon=='sem' & is.na(ribboncols[1])) geom_ribbon(aes(ymin=mean-sem, ymax=mean+sem, fill=grp), colour=NA, alpha=0.5)} +

    # or ribbon SD
    {if(ribbon=='sd' & is.na(ribboncols[1])) geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd, fill=grp), colour=NA, alpha=0.5)} +

    geom_line(linewidth=linethick) +

    # if we were given trace colours/ribbon colours, add them now
    {if(!is.na(tracecols[1])) scale_colour_manual(values=tracecols)} +
    {if(!is.na(ribboncols[1])) scale_fill_manual(values=ribboncols)} +

    theme_minimal() +
    theme(
      panel.grid.minor.x=element_blank(),
      panel.grid.minor.y=element_blank(),
      axis.title.x=element_text(size=9, margin = margin(t = 2, r = 0, b = 0, l = 0)),
      axis.title.y=element_text(size=9, margin = margin(t = 0, r = 2, b = 0, l = 0)),
      axis.text.x = element_text(size=7, margin = margin(t = 0, r = 0, b = 0, l = 0)),
      axis.text.y = element_text(size=7, margin = margin(t = 0, r = 0, b = 0, l = 0))) +
    scale_x_continuous(breaks=seq(0, max(sbg$zhrs), 24)) +

    {if(!legendOrNo) theme(legend.position='none')} +

    {if(!xtextOrNo) theme(axis.text.x=element_blank())} +
    {if(!ytextOrNo) theme(axis.text.y=element_blank())} +

    {if(!xmajorOrNo) theme(panel.grid.major.x=element_blank())} +
    {if(!ymajorOrNo) theme(panel.grid.major.y=element_blank())} +

    {if(gridMode) theme(plot.margin=unit(c(-0.3, -0.3, -0.3, -0.3), 'lines'))} + # this is important to combine plots in grid

    xlab(xname) + ylab(yname) +
    coord_cartesian(ylim=c(ymin, ymax), xlim=c(xstart, xstop))



  if (!exportOrNo) {return(tracebygrp)} # if do not export the plot to drive, return the ggplot object instead
  # Note; if no export, will stop there due to return() being called, so will not see below

  # export plot
  ggsave(exportPath, width=width, height=height, units='mm', useDingbats=FALSE)

  # and return so can see in RStudio
  return(tracebygrp)

}



# function sleepGrid(...) -------------------------------------------------

# plots single sleep traces

#' Title
#'
#' @param zzz
#' @param genopath
#' @param boxnum
#' @param grp
#' @param howMany
#' @param smoothOrNo
#' @param smooth_npoints
#' @param raiseBottom
#' @param bottomborder
#' @param fill
#' @param xtext
#' @param ytext
#' @param xmajor
#' @param ymajor
#' @param xminor
#' @param yminor
#' @param xstart
#' @param xstop
#' @param ymin
#' @param ymax
#' @param grid_nrow
#' @param grid_ncol
#' @param spacing
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
ggSleepGrid <- function(zzz,
                        genopath,
                        boxnum=1,
                        grp,
                        howMany='all',
                        smoothOrNo=FALSE,
                        smooth_npoints,
                        raiseBottom=FALSE,
                        bottomborder,
                        fill,
                        xtext=FALSE,
                        ytext=FALSE,
                        xmajor=FALSE,
                        ymajor=FALSE,
                        xminor=FALSE,
                        yminor=FALSE,
                        xstart=14, # in Zeitgeber hours
                        xstop=24,
                        ymin=0,
                        ymax=8,
                        grid_nrow=4,
                        grid_ncol=2,
                        spacing=0.01,
                        exportOrNo=TRUE,
                        exportPath,
                        width=159.1,
                        height=246.1) {

  ### check export settings ###

  if (exportOrNo) {
    if(substrEnding(exportPath, 4) != '.pdf') stop('\n \t \t \t \t >>> Error: exportPath does not end with .pdf.')

    # check if output folder exists
    if(!dir.exists(parentFolder(exportPath)))
      stop('\n \t \t \t \t >>> Error exportPath: output folder does not exist. \n')

  }


  # convert to data.frame ---------------------------------------------------
  # at this stage data is small from binning in epoch, so data.table not really needed anymore
  zzz <- as.data.frame(zzz)


  # import genotype ---------------------------------------------------------
  if(substrEnding(genopath, 4) != '.txt')
    stop('\t \t \t \t >>> Error: did you pick the correct file? It is not .TXT \n')

  geno <- importGenotype(genopath)

  # ! if box 2, add 96 to geno
  if(boxnum==2) {
    geno <- geno + 96
  }

  # preallocate list which will store single traces -------------------------

  # how many fish in group we want to plot?
  fis <- as.numeric(unlist(geno[grp]))
  fis <- fis[!is.na(fis)]
  fis <- sprintf('f%i', fis) # add f, so f7, f8, etc.

  if(howMany!='all') {
    fis <- sample(fis, howMany) # if we are not plotting all fish in the group, draw n at random
  }

  zts <- vector(mode='list', length=length(fis)) # zzz traces

  for (f in 1:length(fis)) { # for each fish

    cat('\t \t \t \t \t \t >>> plotting fish', f, 'of', length(fis), '\n')

    # build a small dataframe of just that fish's data
    zf <- zzz[c('zhrs', fis[f])] # zzz for just one fish
    colnames(zf)[2] <- 'data'

    # smooth if needed
    if (smoothOrNo) { # if want to smooth
      zf$data <- data.table::frollmean(zf$data, n=smooth_npoints, hasNA=FALSE)
    }
    # first smooth_npoints datapoints will be NA, leaving like this

    # if no data and no grid, plot is completely absent
    # possible solution: raise a little bit where data is low, so can see a bottom border

    if (raiseBottom) {
      zf[which(zf$data < bottomborder) , 'data'] <- bottomborder # e.g. wherever data is below 0.1, raise it to 0.1
    }

    # plot trace
    zt <- ggplot(zf, aes(x=zhrs, y=data)) +
      geom_area(fill=fill) +
      theme_minimal() +
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.margin = unit(c(spacing, spacing, spacing, spacing), 'lines')
      ) +

      {if(!xtext) theme(axis.text.x=element_blank())} +
      {if(!ytext) theme(axis.text.y=element_blank())} +

      {if(!xmajor) theme(panel.grid.major.x=element_blank())} +
      {if(!ymajor) theme(panel.grid.major.y=element_blank())} +

      {if(!xminor) theme(panel.grid.minor.x=element_blank())} +
      {if(!yminor) theme(panel.grid.minor.y=element_blank())} +

      coord_cartesian(ylim=c(ymin, ymax), xlim=c(xstart, xstop))

    # add plot to the list
    zts[[f]] <- zt

  }


  # turn the list of plots into a grid of plots -----------------------------

  zgri <- ggpubr::ggarrange(plotlist=zts, nrow=grid_nrow, ncol=grid_ncol) # grid of zzz traces


  # save grid pdf -----------------------------------------------------------

  if (!exportOrNo) {return(zgri)} # if do not export the plot to drive, return the ggplot object instead
  # Note; if no export, will stop there due to return() being called, so will not see below


  cat('\t \t \t \t >>> Saving grid as pdf \n')
  # export plot
  ggsave(exportPath, width=width, height=height, units='mm', useDingbats=FALSE)

  # return grid so displays in RStudio
  return(zgri)


}
