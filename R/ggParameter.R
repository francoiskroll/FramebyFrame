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
#' @param fainterExp
#' @param dotSize
#' @param faintMax
#'
#' @return
#' @export
#'
#' @examples
#' @import ggplot2
#' @importFrom dplyr %>%
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
                        fainterExp=FALSE,
                        faintMax=0.5,
                        ymin=NA,
                        ymax=NA,
                        dotSize=0.5,
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

  ### prepare the X axis labels

  # slightly different if clutch is present or not
  # (i.e. if we are in situation where multiple clutches in one box)
  if('clutch' %in% colnames(pal)) {

    # first print the order of the plot for the user to check
    cat('\t \t \t \t >>> Plot', unique(pal$parameter), as.character(unique(pal$daynight)), ': order is', levels(pal$date_box_clutch_win),'\n')
    # now prepare the X axis labels
    # we need a named vector, names = original labels (i.e. date_box_clutch_win in the data) and values = labels we want
    # we will now assume that date_box_clutch_win is exactly YYMMDD_BX_clutchX_dayX or YYMMDD_BX_clutchX_nightX
    # and we will split into dayX/nightX and clutchX and YYMMDD_BX and so that in three lines
    # get dayX/nightX:
    dnx <- strNthSplit(levels(pal$date_box_clutch_win), split='_', nth=4)
    # get clutchX:
    clutchx <- strNthSplit(levels(pal$date_box_clutch_win), split='_', nth=3)
    # get YYMMDD_BX:
    yymmdd_bx <- paste0(strNthSplit(levels(pal$date_box_clutch_win), split='_', nth=1), '_', strNthSplit(levels(pal$date_box_win), split='_', nth=2))
    # now we want each label to be e.g. day1 \n clutchX \n 230214_14
    facetlabs <- sapply(1:length(yymmdd_bx), function(i) {
      paste0(dnx[i], '\n', clutchx[i], '\n', yymmdd_bx[i])
    })
    # now add names
    names(facetlabs) <- levels(pal$date_box_clutch_win)
    # ready to be given in ggplot call

  ### if clutch is not present (standard case)
  } else {

    # first print the order of the plot for the user to check
    cat('\t \t \t \t >>> Plot', unique(pal$parameter), as.character(unique(pal$daynight)), ': order is', levels(pal$date_box_win),'\n')
    # now prepare the X axis labels
    # we need a named vector, names = original labels (i.e. date_box_win in the data) and values = labels we want
    # we will now assume that date_box_win is exactly YYMMDD_BX_dayX or YYMMDD_BX_nightX
    # and we will split into dayX/nightX and YYMMDD_BX so that in two lines
    # get dayX/nightX:
    dnx <- strNthSplit(levels(pal$date_box_win), split='_', nth=3)
    # get YYMMDD_BX:
    yymmdd_bx <- paste0(strNthSplit(levels(pal$date_box_win), split='_', nth=1), '_', strNthSplit(levels(pal$date_box_win), split='_', nth=2))
    # now we want each label to be e.g. day1 \n 230214_14 so exp is below on a new line
    facetlabs <- sapply(1:length(yymmdd_bx), function(i) {
      paste0(dnx[i], '\n', yymmdd_bx[i])
    })
    # now add names
    names(facetlabs) <- levels(pal$date_box_win)
    # ready to be given in ggplot call

  }


  ### prepare the colours
  # if fainterxp is OFF, that means we simply colour by grp, in which case either user gave colours for the groups or we pick the default ggplot ones
  if(!fainterExp & is.na(colours[1])) {
    colours <- scales::hue_pal()(length(unique(pal$grp)))
    # this is "group colours"

  # if fainterExp is ON, that means we colour by date_box_grp, but following (loosely) the colours given by the user for the groups (or the default ggplot ones)
  # in the sense that we respect the colours, but make exp2 a bit fainter, exp3 a lot fainter, etc. to help distinguish them
  # this gets fairly complex, so it is taken care of by fainterExps, which simply return the colours to be used with aes(colour=date_box_grp)
  } else if(fainterExp) {
    colours <- fainterExp(pal=pal,
                          colours=colours,
                          faintMax=faintMax)
    # this is "date_box_grp colours"
    # but we keep the same name so we can give them to scale_colour_manual without adding conditions
  }


  # ! need to make sure dodge.width is same for dots & mean crosses so they align
  dodgeby <- 0.7

  # ggplot does not want to start with if statement FALSE for some reason
  # so will start creating the plot here, then will add the rest as a second call

  # standard case: we do not want to poolDayNight
  if (!poolDayNight & !fainterExp) {
    # then check whether clutch column is present or not
    # if present, we need to also split by clutch
    if('clutch' %in% colnames(pal)) {
      ggParam <- ggplot(pal, aes(x=date_box_clutch_win_grp, y=param, colour=grp))
    } else {
      # THIS IS MOST COMMON CASE
      ggParam <- ggplot(pal, aes(x=date_box_win_grp, y=param, colour=grp))
    }

  # if want to poolDayNight
  } else if (poolDayNight & !fainterExp) {
    if('clutch' %in% colnames(pal)) {
      ggParam <- ggplot(pal, aes(x=date_box_clutch_daynight_grp, y=param, colour=grp))
    } else {
      ggParam <- ggplot(pal, aes(x=date_box_daynight_grp, y=param, colour=grp))
    }

  # if want to make experiments fainter colours, that means we colour by date_box_grp
  } else if (!poolDayNight & fainterExp) {
    ggParam <- ggplot(pal, aes(x=date_box_daynight_grp, y=param, colour=date_box_grp))
  }

  # note 16/03/2023: I should probably delete poolDayNight setting
  # I do not think it is ever a good idea to have two dots (if e.g. day1/day2) per larva
  # readers intuitively assume one dot/one animal
  # also, it is not a good idea not to normalise in either way
  # a similar setting could be avgDayNight, then we average day1 & day2 datapoints together
  # would reduce the width of the plot and achieve one dot per animal

  # note 17/04/2023: I added fainterExp, but did not try with the 'clutch' setting
  # will probably fail

  # note, not easy to add the night background when we are plotting both day and night together
  # will simply turn off for now if both are called
  if (is.na(onlyDayorNight)) { # NA onlyDayorNight means both
    nightBgOrNo <- FALSE
    onlyDayorNight <- 'both' # just anything so not NA and can test below during ggplot call
  }

  # then add the rest
  ggParam <-

    ggParam +

    ggbeeswarm::geom_quasirandom(groupOnX=TRUE, width=0.09, size=dotSize, dodge.width=dodgeby) +
    stat_summary(aes(group=grp), fun=mean, geom='point', colour='#595E60', shape=3, size=1.2, stroke=0.8, position=position_dodge(dodgeby)) +

    # split the plot for clarity
    # if clutch column present, each subplot is one date_box_clutch_win, e.g. 230306_14_clutch2_day1
    {if('clutch' %in% colnames(pal)) facet_grid(~date_box_clutch_win, scales='free_x', space='free_x', switch='both',
                                                labeller=as_labeller(facetlabs))} +
    # but usually each subplot is one date_box_win, e.g. 230306_14_day1
    {if(!'clutch' %in% colnames(pal)) facet_grid(~date_box_win, scales='free_x', space='free_x', switch='both',
                                                labeller=as_labeller(facetlabs))} +

    # usually each subplot = one date_box_win, e.g. 230306_14_day1
    # facet_grid(~date_box_win, scales='free_x', space='free_x', switch='both',
    #            labeller=as_labeller(facetlabs)) +

    scale_colour_manual(values=colours) +
    theme_minimal() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_text(size=9, margin=margin(t=0, r=-1, b=0, l=0)), # if user does not want any y axis title, it gets removed below
      axis.text.x=element_blank(), # we always remove X axis labels, exp_win is given by facet titles and group is given by colour in legend
      panel.grid.minor.y=element_blank(),
      axis.text.y=element_text(size=7, margin=margin(t=0, r=-1, b=0, l=0)),
      legend.title=element_blank(),
      legend.spacing.x=unit(0.0, 'lines'), # brings legend text a little closer to the dots
      legend.box.margin=margin(0,-8,0,-18), # reduces margin around legend to gain space
      legend.text=element_text(size=7),
      strip.text.x=element_text(size=7)
    ) +

    # increase point size in legend
    guides(colour=guide_legend(override.aes=list(size=2),
                               keyheight=unit(0.8, 'lines')), # controls spacing between items in legend
           fill=guide_legend(byrow=TRUE)) +

    # remove legend if user does not want it
    {if(!legendOrNo) theme(legend.position='none')} +

    {if(!xtextOrNo) theme(strip.text.x=element_blank())} + # strip.text.x controls facet titles,
    # which by default are above each subplot, but switch='both' puts them below

    {if(!ynameOrNo & !yunitOrNo) theme(axis.title.y=element_blank())} +

    {if(ynameOrNo) ylab(label=param2Ytext(param=unique(pal$parameter)))} +

    {if(yunitOrNo) ylab(label=param2Yunit(param=unique(pal$parameter)))} +

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
#' @param keysOrNo
#' @param dotSize
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
                            fainterExp=FALSE,
                            faintMax=0.5,
                            legendOrNo=TRUE,
                            dotSize=0.5,
                            ynameOrNo=TRUE,
                            yunitOrNo=FALSE,
                            xtextOrNo=TRUE,
                            titleOrNo=TRUE,
                            nightBgOrNo=TRUE,
                            keysOrNo=FALSE,
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


  ### is there only one night or only one day?
  # take first parameter table as example
  # first column with actual datapoints is:
  firstcol <- which(startsWith(colnames(psL[[1]]), 'day') | startsWith(colnames(psL[[1]]), 'night'))[1]
  # names of the day/night columns:
  dnnames <- colnames(psL[[1]])[firstcol:ncol(psL[[1]])] # e.g. night0, day1

  # if we have only one night, set onlyDayorNight to 'night'
  if(length(dnnames)==1 & startsWith(dnnames[1], 'night')) {
    cat('\t \t \t \t >>> Only one full night: setting onlyDayorNight="night" \n')
    onlyDayorNight <- 'night'

    # if we have only one day, set onlyDayorNight to 'day'
  } else if (length(dnnames)==1 & startsWith(dnnames[1], 'day')) {
    cat('\t \t \t \t >>> Only one full day: setting onlyDayorNight="day" \n')
    onlyDayorNight <- 'day'
  }


  ### plot each parameter ###

  # preallocate list which will store the parameters
  # one plot per parameter

  # gpL for ggParameters List
  gpL <- lapply(psL, function(pa) {

    # with each pa
    # make one DAY plot
    # make one NIGHT plot
    # combine them in a small grid (just side by side)

    #### DAY ####

    # ! exceptions: activitySunsetStartle & sleepLatency are not defined for day, so need to skip in that case
    # we also skip if user asked for onlyDayorNight='night', obviously
    # i.e. we go ahead if onlyDayorNight is NA or 'day'
    # AND if parameter is *not* activitySunsetStartle & sleepLatency
    if ( (is.na(onlyDayorNight) | onlyDayorNight=='day') & ! unique(pa$parameter) %in% c('activitySunsetStartle', 'sleepLatency')) {
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
                           fainterExp=fainterExp,
                           faintMax=faintMax,
                           ymin=NA,
                           ymax=NA,
                           dotSize=dotSize,
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


    #### NIGHT ####

    # in general, when any title is present, we want blankTitle ON for the night plot so the plots align
    if(titleOrNo) {
      blankTitle <- TRUE
    } else {
      blankTitle <- FALSE
    }

    # ! unless user wants titles AND night plot is only we have (e.g for parameter like sunsetStartle),
    # then we should put the title now (i.e. so blankTitle OFF)
    if(titleOrNo & !exists('ggday')) {
      blankTitle <- FALSE
    }

    # ! if we already made a day plot for this parameter,
    # there is no need to repeat the title, Y axis name, etc.
    # so, if day plot exists, turn off a few settings
    if(exists('ggday')) {
      ynameOrNo <- FALSE
      yunitOrNo <- FALSE
      titleOrNo <- FALSE
    }

    # now draw the night plot, except if user explicitly asked for day only
    # i.e. we can go ahead if onlyDayorNight is NA or is =='night'
    if(is.na(onlyDayorNight) | onlyDayorNight=='night') {
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
                             fainterExp=fainterExp,
                             faintMax=faintMax,
                             ymin=NA,
                             ymax=NA,
                             dotSize=dotSize,
                             legendOrNo=legendOrNo,
                             xtextOrNo=xtextOrNo,
                             ynameOrNo=ynameOrNo,
                             yunitOrNo=yunitOrNo,
                             titleOrNo=titleOrNo,
                             blankTitle=blankTitle,
                             nightBgOrNo=nightBgOrNo,
                             statsOrNo=statsOrNo,
                             silent=FALSE) # note here, we do print the LME summary,
      # better to do it for night as all parameters have night (incl. sunset startle)
    }

    cat('\n') # helps make messages in Console clearer

    # for this parameter, return a small grid: day plot next to night plot
    # ! except if onlyDayorNight='night' or activitySunsetStartle or sleepLatency, then we only have the night plot
    if( (!is.na(onlyDayorNight) & onlyDayorNight=='night') | unique(pa$parameter) %in% c('activitySunsetStartle', 'sleepLatency')) {
      return(ggpubr::ggarrange(plotlist=list(ggnight), ncol=1, nrow=1))
    } else if(!is.na(onlyDayorNight) & onlyDayorNight=='day') {

    # ! except if onlyDayorNight='night', then we only have the day plot
      return(ggpubr::ggarrange(plotlist=list(ggday), ncol=1, nrow=1))

    # standard case: we have both the day and the night plot
    } else {
      return(ggpubr::ggarrange(plotlist=list(ggday, ggnight), ncol=2, nrow=1))
    }

  })


  # arrange in a grid & export ----------------------------------------------

  # ! check we have enough space
  # ggsave throws an error otherwise
  if((ncol * nrow) < length(gpL)) stop('\t \t \t \t >>> Error: trying to place ', length(gpL), ' parameter plots in a grid ',
                                       ncol, '*', nrow, ' = ', ncol*nrow, ' cells.
                                       Please increase ncol and/or nrow setting so there is enough cells in the grid. \n')

  # keysOrNo is whether to add A, B, C, ... like a publication figure
  if(keysOrNo) {
    gpgrid <- ggpubr::ggarrange(plotlist=gpL, ncol=ncol, nrow=nrow,
                                labels=toupper(letters[1:length(gpL)]))
  } else {
    gpgrid <- ggpubr::ggarrange(plotlist=gpL, ncol=ncol, nrow=nrow)
  }


  ggplot2::ggsave(filename=exportPath, plot=gpgrid, width=width, height=height, units='mm', device=cairo_pdf)

  cat('\t \t \t \t >>> Parameter grid saved to', exportPath, '\n')

  # return just in case want the object, but do not print plot to RStudio as it takes too long
  invisible(gpgrid)

}



# function fainterExp(...) ------------------------------------------------

# small function to deal with fainter colours in ggParam
# would be a bit long to add to ggParam, which is already complex

# it receives pal and the grp colours
# grp colours can be NA, in which case it will take the default ggplot colours
# and returns the colours to be used with aes(..., colour=date_box_grp)

#' Title
#'
#' @param pal
#' @param colours
#' @param faintMax
#'
#' @return
#' @export
#'
#' @examples
fainterExp <- function(pal,
                       colours=NA,
                       faintMax) {

  # if user did not give colours for the groups, we take the default ggplot ones
  if(is.na(colours[1])) {
    colours <- scales::hue_pal()(length(unique(pal$grp)))
  }

  # in which order are the date_box_grp plotted?
  # (here we essentially want to colour by date_box_grp, so we do not need to worry about win)
  # they will be in the order of the levels:
  dbg_onplot <- levels(pal$date_box_grp)
  # but not all may be plotted
  # for example, user may have given onlyExp in which case not all experiments present in the levels will be present in the plot
  # so keep only those actually in the data
  dbg_onplot <- dbg_onplot[which(dbg_onplot %in% unique(pal$date_box_grp))]
  # this way we have the date_box_grp, in the order they will be plotted

  # what are the groups (genotypes) of those date_box_grp?
  grps_onplot <- strNthSplit(dbg_onplot, '_', 3)

  # what are those groups' indices?
  # e.g. it is grp1; grp2, grp1, grp2, etc.
  # and we use those indices to get the colours which will be used
  dbg_cols <- colours[match(grps_onplot, unique(grps_onplot))]
  # so those are the colours in the order they will appear in the plot
  # they will repeat, for example: HOM red, HET green, WT blue; HOM red, HET green, WT blue; etc.
  # note, unique(grps_onplot) should always give groups in the right order as originally comes from levels(pal$date_box_win_grp)

  # now, which experiments do these colours belong date_box_grp belong to?
  db_onplot <- paste(strNthSplit(dbg_onplot, '_', 1), strNthSplit(dbg_onplot, '_', 2), sep='_')
  # a typical example would something like:
  # exp1_night1_ko, exp1_night1_wt, exp1_night2_ko, exp1_night2_wt; exp2_night1_ko, exp2_night1_wt, exp2_night2_ko, exp2_night2_wt
  # becoming
  # exp1, exp1, exp1, exp1; exp2, exp2, exp2, exp2

  # temporarily split the colours into a list so we have
  # slot1: all the colours for exp1
  # slot2: all the colours for exp2
  # etc.

  # a nice solution:
  # add the experiment date_box as names of the colours
  names(dbg_cols) <- db_onplot
  # and that allows to easily split the colours into a list:
  dbg_cols <- split(dbg_cols, f=names(dbg_cols))

  # we leave the colours in first slot intact
  # we make the colours in second slot a bit fainter
  # we make the colours in third slot a lot fainter
  # etc.

  # we should try to avoid going to completely white,
  # so make the level (i.e. by how much fainter we make each new experiment) proportional to how many experiments we have
  # i.e. if many experiments, we should make each only slightly fainter

  # set 0.6 fainter as maximum for now
  # I think means 60% fainter, 100% being white
  # so each step is:
  faintstep <- faintMax / (length(unique(db_onplot))-1)
  # e.g. if 4 experiments, we leave first as it is / we make second 0.2 fainter / we make third 0.4 fainter / we make fourth 0.6 fainter
  # e.g. if 2 experiments, we leave first as it is / we make second 0.6 fainter

  # to the steps will be
  faintratios <- seq(0, faintMax, faintstep) # e.g. 0, 0.2, 0.4, 0.6 if 4 experiments

  dbg_cols <- unlist(lapply(1:length(dbg_cols), function(e) {
    colorspace::lighten(dbg_cols[[e]], faintratios[e])
    # i.e. if second exp, we make every colour in second slot by the second ratio
  }))

  # these are the new colours we should be using for date_box_grp

  return(dbg_cols)
}
