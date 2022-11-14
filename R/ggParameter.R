###################################################
# ~~~ FramebyFrame package ~~~

### ggParameter ###

# functions to plot parameters

# Francois Kroll 2022
# francois@kroll.be
###################################################


# function ggParameter(...) -----------------------------------------------

# expects parameter data in long format = pal

# path: does not actually care about path, just needs a path to a file in experiment folder that is YYMMDD_... so can build export file name

# always expects column to be 'param'

#' Title
#'
#' @param pa
#' @param grporder
#' @param skipNight0
#' @param poolExp1
#' @param poolExp2
#' @param poolExp3
#' @param sameClutch1
#' @param sameClutch2
#' @param sameClutch3
#' @param poolDayNight
#' @param onlyExp
#' @param onlyDayorNight
#' @param onlyWin
#' @param colours
#' @param ymin
#' @param ymax
#' @param legendOrNo
#' @param xtextOrNo
#' @param ynameOrNo
#' @param yunitOrNo
#' @param titleOrNo
#' @param blankTitle
#' @param nightBgOrNo
#' @param statsOrNo
#' @param silent
#' @param width
#' @param height
#' @param exportPath
#'
#' @return
#' @export
#'
#' @examples
ggParameter <- function(pa,
                        grporder=NA,
                        skipNight0=FALSE,
                        poolExp1=NA,
                        poolExp2=NA,
                        poolExp3=NA,
                        sameClutch1=NA,
                        sameClutch2=NA,
                        sameClutch3=NA,
                        poolDayNight=FALSE,
                        onlyExp=NA,
                        onlyDayorNight=NA,
                        onlyWin=NA,
                        colours=NA,
                        ymin=NA,
                        ymax=NA,
                        legendOrNo=TRUE,
                        xtextOrNo=TRUE,
                        ynameOrNo=TRUE,
                        yunitOrNo=FALSE,
                        titleOrNo=TRUE,
                        blankTitle=FALSE,
                        nightBgOrNo=TRUE,
                        statsOrNo=FALSE,
                        silent=FALSE,
                        width,
                        height,
                        exportPath) {

  # check that settings make sense

  # check onlyDayorNight
  if (!is.na(onlyDayorNight) & onlyDayorNight!='day' & onlyDayorNight!='night')
    stop('\t \t \t \t >>> Error: onlyDayorNight can only be day, night, or NA. \n')

  # cannot skipNight0 AND ask to plot only night0
  if(!is.na(onlyWin)) {
    if (skipNight0 & onlyWin=='night0') stop('\t \t \t \t >>> Error: cannot skipNight0 AND ask to only plot night0. \n')
  }

  # cannot have onlyWin set if pooling days/nights
  if (!is.na(onlyWin) & poolDayNight) stop('\t \t \t \t >>> Error: cannot plot individual window(s) AND pool days/nights at the same time. \n')

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

  # need to be given both ymin/ymax or none
  # or in other words, sorting c(ymin, ymax) (which removes NA) should be length 0 or length 2
  if (length(sort(c(ymin, ymax))) != 0 & length(sort(c(ymin, ymax))) != 2)
    stop('\t \t \t \t Error: either give both ymin and ymax or none of them \n')

  # check exportPath finishes with .pdf (if it was given)
  if (!missing(exportPath)) {
    if (substrEnding(exportPath, 4) != '.pdf')
      stop('\t \t \t \t >>> Error: exportPath does not finish with .pdf \n')
  }

  # cannot have both yname and yunit
  if(ynameOrNo & yunitOrNo)
    stop('\t \t \t \t Error: do you want Y axis as small sentence or just unit? If small sentence, set ynameOrNo=TRUE;
         if unit, set yunitOrNo=TRUE, but do not set both to TRUE at the same time \n')


  ### deal with statistics, if required ###

  # currently only supports day alone or night alone
  # both day & night becomes more complicated re where to put the *s
  if(statsOrNo & is.na(onlyDayorNight))
    stop('\t \t \t \t >>> Error ggParameter: sorry, currently can only put p-value asterisk when plotting only day or only night, not both.
         When using statsOrNo=TRUE, please use onlyDayorNight=\'day\' or onlyDayorNight=\'night\'.')

  if(statsOrNo) {
    lme <- LMEdaynight(pa=pa,
                       grporder=grporder,
                       poolExp1=poolExp1,
                       poolExp2=poolExp2,
                       poolExp3=poolExp3,
                       sameClutch1=sameClutch1,
                       sameClutch2=sameClutch2,
                       sameClutch3=sameClutch3,
                       skipNight0=skipNight0,
                       silent=silent,
                       detailsOrNo=FALSE)

    # we should have ONE p-value for day and ONE p-value for night, regardless of number of groups
    # check DAY
    if(onlyDayorNight=='day') {
      if(length(unique(lme$day['pval'])) != 1)
        stop('\t \t \t \t >>> Error ggParameter: zero or more than one p-value in Day LME model,
             expecting one regardless of number of groups \n')
    }
    # check NIGHT
    if(onlyDayorNight=='night') {
      if(length(unique(lme$night['pval'])) != 1)
        stop('\t \t \t \t >>> Error ggParameter: zero or more than one p-value in Night LME model,
             expecting one regardless of number of groups \n')
    }
  }
  ###

  # prepare the parameter dataframe(s) we get for plotting
  pal <- paramReadPivot(pa=pa,
                        grporder=grporder,
                        poolExp1=poolExp1,
                        poolExp2=poolExp2,
                        poolExp3=poolExp3,
                        skipNight0=skipNight0)
  # pal for pa in long format

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

  # if onlyExp is given...
  if (!is.na(onlyExp[1])) {
    pal <- pal %>%
      subset(date_box==onlyExp)
  }

  # remove NA values
  # it makes ggplot command easier and avoids Warnings
  if (sum(is.na(pal$param)) > 0) {
    cat('\t \t \t \t >>> Removing', sum(is.na(pal$param)), 'NA datapoints \n')
    pal <- pal[!is.na(pal$param),]
  }


  # ! need to make sure dodge.width is same for dots & mean crosses so they align
  dodgeby <- 0.7

  # ggplot does not want to start with if statement FALSE for some reason
  # so will start creating the plot here, then will add the rest as a second call
  if (poolDayNight) {
    ggParam <- ggplot(pal, aes(x=date_box_daynight_grp, y=param, colour=grp))
  } else if (!poolDayNight) {
    ggParam <- ggplot(pal, aes(x=date_box_win_grp, y=param, colour=grp))
  }

  # note, not easy to add the night background when we are plotting both day and night together
  # will simply turn off for now if both are called
  if (is.na(onlyDayorNight)) { # NA onlyDayorNight means both
    nightBgOrNo <- FALSE
    onlyDayorNight <- 'both' # just anything so not NA and can test below during ggplot call
  }

  # then add the rest
  ggParam <-

    ggParam +

    geom_quasirandom(groupOnX=TRUE, width=0.09, size=0.5, dodge.width=dodgeby) +
    stat_summary(aes(group=grp), fun=mean, geom='point', colour='#595E60', shape=3, size=1.2, stroke=0.8, position=position_dodge(dodgeby)) +
    facet_grid(~date_box_win, scales='free_x', space='free_x') +
    {if(!is.na(colours[1])) scale_colour_manual(values=colours) } + # if user gave colours, follow them; if not ggplot will do default colours
    theme_minimal() +
    theme(
      strip.text.x=element_blank(), # this removes facet titles
      axis.title.x=element_blank(),
      panel.grid.minor.y=element_blank(),
      axis.text.y=element_text(size=7),
      axis.text.x=element_text(angle=45, hjust=1)
    ) +

    {if(!legendOrNo) theme(legend.position='none')} +

    {if(!xtextOrNo) theme(axis.text.x=element_blank())} +

    {if(!ynameOrNo & !yunitOrNo) theme(axis.title.y=element_blank())} +
    {if(ynameOrNo) ylab(label=param2Ytext(unique(pal$parameter)))} +
    {if(yunitOrNo) ylab(label=param2Yunit(unique(pal$parameter)))} +

    {if(titleOrNo & !blankTitle) ggtitle(label=param2Title(unique(pal$parameter)))} +
    {if(blankTitle) ggtitle(label='')} +

    {if(nightBgOrNo & onlyDayorNight=='night') theme(
      panel.background=element_rect(fill='#d2d2d2', colour=NA)
    )} +

    {if(statsOrNo & onlyDayorNight=='day') labs(subtitle=as.character(unique(lme$day['pvalsymbol'])))} + # ***
    {if(statsOrNo & onlyDayorNight=='night') labs(subtitle=as.character(unique(lme$night['pvalsymbol'])))} + # ***

    coord_cartesian(ylim=c(ymin, ymax)) # putting this last just to make sure we do not finish with a +

  # *** we checked above that there was only one p-value in LME model, it is just repeated multiple times if more than 2 groups

  # export plot -------------------------------------------------------------

  # will assume that if exportPath is missing (was not given in call) we do not want to plot
  if(!missing(exportPath)) {
    ggsave(exportPath, ggParam, width=width, height=height, units='mm', device=cairo_pdf)
  }



  # return plot so displays in RStudio
  return(ggParam)

}



# function ggParameterGrid(...) -------------------------------------------

#' Title
#'
#' @param paDir
#' @param grporder
#' @param skipNight0
#' @param poolExp1
#' @param poolExp2
#' @param poolExp3
#' @param sameClutch1
#' @param sameClutch2
#' @param sameClutch3
#' @param poolDayNight
#' @param onlyExp
#' @param onlyDayorNight
#' @param onlyWin
#' @param colours
#' @param ymin
#' @param ymax
#' @param legendOrNo
#' @param ynameOrNo
#' @param yunitOrNo
#' @param xtextOrNo
#' @param titleOrNo
#' @param nightBgOrNo
#' @param statsOrNo
#' @param ncol
#' @param nrow
#' @param width
#' @param height
#' @param exportPath
#'
#' @return
#' @export
#'
#' @examples
ggParameterGrid <- function(paDir,
                            grporder=NA,
                            skipNight0=FALSE,
                            poolExp1=NA,
                            poolExp2=NA,
                            poolExp3=NA,
                            sameClutch1=NA,
                            sameClutch2=NA,
                            sameClutch3=NA,
                            poolDayNight=FALSE,
                            onlyExp=NA,
                            onlyDayorNight=NA,
                            onlyWin=NA,
                            colours=NA,
                            ymin=NA,
                            ymax=NA,
                            legendOrNo=TRUE,
                            ynameOrNo=TRUE,
                            yunitOrNo=FALSE,
                            xtextOrNo=TRUE,
                            titleOrNo=TRUE,
                            nightBgOrNo=TRUE,
                            statsOrNo=FALSE,
                            ncol,
                            nrow,
                            width,
                            height,
                            exportPath) {

  ### import parameter tables in directory(ies) ###

  psL <- importBhvParams(paDir=paDir,
                         poolExp1=poolExp1,
                         poolExp2=poolExp2,
                         poolExp3=poolExp3,
                         grporder=grporder,
                         skipNight0=skipNight0)


  ### plot each parameter ###

  # preallocate list which will store the parameters
  # one plot per parameter

  # gpL for ggParameters List
  gpL <- lapply(psL, function(pa) {

    # with each pa
    # make one DAY plot
    # make one NIGHT plot
    # combine them in a small grid (just side by side)

    # ! exceptions: activitySunsetStartle & sleepLatency are not defined for day, so need to skip in that case
    if (! unique(pa$parameter) %in% c('activitySunsetStartle', 'sleepLatency')) {
      ggday <- ggParameter(pa=pa,
                           grporder=grporder,
                           sameClutch1=sameClutch1,
                           sameClutch2=sameClutch2,
                           sameClutch3=sameClutch3,
                           skipNight0=skipNight0,
                           poolDayNight=poolDayNight,
                           onlyExp=onlyExp,
                           onlyDayorNight='day',
                           onlyWin=onlyWin,
                           colours=colours,
                           ymin=ymin,
                           ymax=ymax,
                           legendOrNo=legendOrNo,
                           xtextOrNo=xtextOrNo,
                           ynameOrNo=ynameOrNo,
                           yunitOrNo=yunitOrNo,
                           titleOrNo=titleOrNo,
                           blankTitle=FALSE,
                           nightBgOrNo=nightBgOrNo,
                           statsOrNo=statsOrNo,
                           silent=TRUE) # note here, we do not print summary LME to Console,
      # otherwise gets printed twice as called again for night
    }

    # ! exceptions: for activitySunsetStartle and sleepLatency, only plots we have is night
    # so if user wants titles, need to add it here
    # and if user wants ynames, need to add it here
    # (for the other parameters, night plot is right next to the day plot so only write Y axis name on the day plot)

    if(unique(pa$parameter) %in% c('activitySunsetStartle', 'sleepLatency') & ynameOrNo) {
      nightyname <- TRUE
    } else {
      nightyname <- FALSE
    }

    if(unique(pa$parameter) %in% c('activitySunsetStartle', 'sleepLatency') & yunitOrNo) {
      nightyunit <- TRUE
    } else {
      nightyunit <- FALSE
    }

    if(unique(pa$parameter) %in% c('activitySunsetStartle', 'sleepLatency') & titleOrNo) {
      nighttitle <- TRUE
      nightblank <- FALSE
    } else {
      nighttitle <- FALSE
      nightblank <- TRUE
    }

    ggnight <- ggParameter(pa=pa,
                           grporder=grporder,
                           sameClutch1=sameClutch1,
                           sameClutch2=sameClutch2,
                           sameClutch3=sameClutch3,
                           skipNight0=skipNight0,
                           poolDayNight=poolDayNight,
                           onlyExp=onlyExp,
                           onlyDayorNight='night',
                           onlyWin=onlyWin,
                           colours=colours,
                           ymin=ymin,
                           ymax=ymax,
                           legendOrNo=legendOrNo,
                           xtextOrNo=xtextOrNo,
                           ynameOrNo=nightyname, # set above, only need to put it if plotting activitySunsetStartle
                           yunitOrNo=nightyunit,
                           titleOrNo=nighttitle,
                           blankTitle=nightblank,
                           nightBgOrNo=nightBgOrNo,
                           statsOrNo=statsOrNo,
                           silent=FALSE) # note here, we do print the LME summary,
    # better to do it for night as all parameters have night (incl. sunset startle)

    # for this parameter, return a small grid: day plot next to night plot
    # ! except if activitySunsetStartle, then we only have the night plot
    if(unique(pa$parameter) %in% c('activitySunsetStartle', 'sleepLatency')) {
      return(ggarrange(plotlist=list(ggnight), ncol=1, nrow=1))
    } else {
      return(ggarrange(plotlist=list(ggday, ggnight), ncol=2, nrow=1))
    }

  })


  # arrange in a grid & export ----------------------------------------------

  # ! check we have enough space
  # ggsave throws an error otherwise
  if((ncol * nrow) < length(gpL)) stop('\t \t \t \t >>> Error: trying to place ', length(gpL), ' parameter plots in a grid ',
                                       ncol, '*', nrow, ' = ', ncol*nrow, ' cells.
                                       Please increase ncol and/or nrow setting so there is enough cells in the grid. \n')

  gpgrid <- ggarrange(plotlist=gpL, ncol=ncol, nrow=nrow,
                      labels=toupper(letters[1:length(gpL)]))

  ggsave(filename=exportPath, plot=gpgrid, width=width, height=height, units='mm', device=cairo_pdf)

}
