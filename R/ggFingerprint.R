###################################################
# ~~~ FramebyFrame package: ggFingerprint ~~~

# plotting functions for fingerprints

# Francois Kroll 2022
# francois@kroll.be
###################################################


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
#' @param dotSize
#' @param lineSize
#'
#' @return
#' @export
#'
#' @examples
#' @import ggplot2
#' @import dplyr

ggFingerprint <- function(fgp,
                          lmePath=NA,
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
                          dotSize=0.2,
                          lineSize=0.2,
                          asteriskSize=3,
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
      subset(! parameter %in% removeParam)
  }



  # import LME report, if given ---------------------------------------------

  if(!is.na(lmePath)) {

    ## import
    lme <- read.csv(lmePath)

    ## check that it seems like it matches the experiments we are about to plot

    # exception: if multiple experiments were merged when calculating fingerprints,
    # fingerprint data gives date_box as e.g. mergeExp_1 but LME report (rightly) lists all the experiments
    # so we cannot check in that case
    if('mergeExp' %in% unique(fgp$date)) {
      cat('\t \t \t \t >>> Beware! Cannot check that the correct LME report was given. Please double-check you are giving the LME statistics for exactly the experiments that are plotted. \n')

    } else {
      # extract the experiments analysed in the LMEreport
      lmexps <- as.character(sort(strsplit(unique(lme$exps), ' & ')[[1]]))
      # they should be the same as in the fingerprint data
      fgexps <- as.character(sort(unique(fgp$date_box)))
      if (!identical(lmexps, fgexps)) stop('\t \t \t \t >>> Error ggFingerprint: it seems like the LME report refers to different experiments than the ones we are about to plot. Please check you gave the correct LMEreport. \n')
    }

    ## prepare the p-value asterisks to add to the plot
    # what is the group we are about to plot in the fingerprint?
    # there needs to be only one, otherwise we will have more than one posthoc p-value
    # (in case of multiple groups, we could take the general LME asterisk, can be implemented in the future)

    # check only one group in fingerprint:
    if(length(unique(fgp$grp))>1)
      stop('\t \t \t \t >>> Error ggFingerprint: currently only supports adding LME asterisks if one group is plotted. There can still be one fingerprint per experiment, but it needs to be the same group in each experiment. You can adjust in grporder, e.g. grporder="hom" \n')

    # from the LMEreport, only keeps rows that give this grp as 'beingGroup'
    lme <- subset(lme, beingGroup==unique(fgp$grp))

    # we need to join by uparam
    # create this composite column in lme:
    lme <- lme %>%
      mutate(uparam=paste(daynight, parameter, sep='_'), .before='parameter')

    nrowbefore <- nrow(fgp)
    fgp <- left_join(fgp, lme[,c('uparam', 'LMEslope', 'LMEerror', 'pval', 'pvalsymbol', 'posthocpval', 'posthocpvalsymbol')], by=c('uparam'))
    nrowafter <- nrow(fgp)
    # check we did not create new rows in fingerprint data, I am never fully confident with the joins!
    if(nrowbefore!=nrowafter) stop('\t \t \t \t >>> Error ggFingerprint: something wrong when joining the LME statistics to the fingerprint data. \n')

    # finally, we need to avoid writing multiple times the p-value asterisks
    # so keep only one for each parameter (i.e. all the asterisks for *one* fingerprint), turn the others to NA
    # to do this, we can simply look at the first date_box_grp to be plotted in the data, keep those asterisks and turn the others to NA
    fgp[which(fgp$date_box_grp != unique(fgp$date_box_grp)[1]), 'posthocpvalsymbol'] <- NA

    # finally, also turn to NA any ns
    # I do not think there is any point in cluttering the plots with the "ns"
    fgp[which(fgp$posthocpvalsymbol=='ns'), 'posthocpvalsymbol'] <- NA

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
  # minimum export height to fit the entire line seems to be 48 mm
  # so if bigger, make it two lines
  if(height>48) {
    yname <- 'deviation from controls (z-score)'
  } else {
    yname <- 'deviation from controls\n(z-score)'
  }

  # set x text
  # ! need to be very careful not to change the order
  # the trick is to take the parameter names from the levels of uparam (set above), not the actual uparam column
  xparams <- strNthSplit(levels(fgp$uparam), '_', 2)

  # alternative x text is to have parameter number
  # ! should not assume we can just repeat a series of digits twice
  # because sunsetStartle (and maybe other parameters in the future) are not defined during both day and night
  # solution: look up each parameter from xticks (above) in allparameters
  # the position in allparameters becomes its number

  # ! in this case, allparameters should represent the parameters which are included in the fingerprint
  # e.g. say user only calculated sleep parameters, we should not be naming parameters like 14, 15, 16, ...
  allparamnms <- allparameters[which(allparameters %in% fgp$parameter)]

  # at this stage, unique xticks will represent unique parameters included in the fingerprint
  # so should say the same parameters as allparamnms

  if(!identical(unique(sort(xparams)), sort(allparamnms)))
    stop('\t \t \t \t >>> Error ggFingerprint: something wrong when preparing X axis labels. \n')

  # can prepare the parameter numbers
  # we simply match each unique parameter in allparamnms
  # e.g. activityTotalPx is #2, etc.
  xparamnum <- match(xparams, allparamnms)

  # finally, we can convert the parameter names to better wording with function param2Title
  # so if using names (rather than parameter number), it is more readable
  xparams <- as.character(sapply(xparams, param2shortName))

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
      geom_point(colour=col2use, size=dotSize) +
      {if(connectOrNo) geom_line(aes(group=date_box_period_grp), linewidth=lineSize, colour=col2use)} +
      {if(nightBgOrNo) annotate(geom='rect', xmin=xmid, xmax=Inf, ymin=-Inf, ymax=Inf, colour=NA, fill='#1d1d1b', alpha=0.2)} + # ***
      theme(panel.grid=element_blank())

    # IF MEAN FINGERPRINT
  } else {

    # do we plot mean ± sd?
    if (metric=='mean') {
      ggFgp <- ggplot(fgp, aes(x=uparam, y=mean, colour=date_box_grp)) +
        {if(nightBgOrNo) annotate(geom='rect', xmin=xmid, xmax=Inf, ymin=-Inf, ymax=Inf, colour=NA, fill='#1d1d1b', alpha=0.2)} + # ***
        geom_hline(yintercept=0, linetype=1, colour='#a7a7a7', linewidth=0.5) +
        {if(connectOrNo) geom_line(aes(group=date_box_period_grp), linewidth=lineSize)} +
        geom_pointrange(aes(ymin=mean-sem, ymax=mean+sem), size=dotSize) +
        # add the pvalue asterisks, if user gave a LME report
        {if(!is.na(lmePath)) geom_text(aes(label=posthocpvalsymbol), y=ymin, angle=90, vjust=0.75, hjust=0, size=asteriskSize, colour='#4d4d4d')} +

        scale_colour_manual(values=colours)

    } else if (metric=='median') {

      # do we plot median ± mad?
      ggFgp <- ggplot(fgp, aes(x=uparam, y=median, colour=date_box_grp)) +
        {if(nightBgOrNo) annotate(geom='rect', xmin=xmid, xmax=Inf, ymin=-Inf, ymax=Inf, colour=NA, fill='#1d1d1b', alpha=0.2)} + # ***
        geom_hline(yintercept=0, linetype=1, colour='#a7a7a7', linewidth=0.5) +
        geom_pointrange(aes(ymin=median-mad, ymax=median+mad), size=dotSize) + # note here using MAD
        {if(connectOrNo) geom_line(aes(group=date_box_period_grp), linewidth=lineSize)} +
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

    {if(ynameOrNo) theme(axis.title.y=element_text(size=9, margin=margin(t=0, r=0, b=0, l=0)))} +

    {if(ynameOrNo) ylab(yname)} +

    {if(!ytextOrNo) theme(axis.text.y=element_blank())} +
    {if(ytextOrNo) theme(axis.text.y=element_text(size=7, margin=margin(t=0, r=-1, b=0, l=0)))} +

    {if(xtextOrNo & !xParamNum) scale_x_discrete(labels=xparams)} +
    {if(xtextOrNo & !xParamNum) theme(axis.text.x=element_text(size=7, angle=90, hjust=1, vjust=0.5, margin=margin(t=-2, r=0, b=0, l=0)))} +


    {if(xParamNum) scale_x_discrete(labels=xparamnum)} +
    {if(xParamNum) theme(axis.text.x=element_text(size=6, margin=margin(t=-0.5, r=0, b=0, l=0)))} +

    {if(!xtextOrNo) theme(axis.text.x=element_blank())} +

    coord_cartesian(ylim=c(ymin, ymax))

  ### export the plot
  if(exportOrNo) {
    ggsave(exportPath, ggFgp, width=width, height=height, units='mm', device=cairo_pdf)
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
  fis <- gtools::mixedsort(as.character(unique(fgp$fish))) # ! mixedsort here, otherwise goes f89, f9, f91

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
  ggG <- ggpubr::ggarrange(plotlist=ggL, nrow=nrow, ncol=ncol)

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

  ggsave(exportPath, ggheat, width=width, height=height, units='mm', device=cairo_pdf)


  # return plot -------------------------------------------------------------
  # so it displays in RStudio
  return(ggheat)


}
