###################################################
# ~~~ FramebyFrame package ~~~

# LME statistics

# Francois Kroll 2022
# francois@kroll.be
###################################################


# function LMEdaynight(...) -----------------------------------------------
# expects parameter across day/night for two clutches
# will perform linear mix effects model

# ! assumes column grp and column fish
# parameter is given by param below, needs to be exactly the column name

# Note; mostly from https://bodowinter.com/tutorial/bw_LME_tutorial.pdf
# some also from https://ourcodingclub.github.io/tutorials/mixed-models/

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
#' @param silent
#' @param detailsOrNo
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom tibble add_column
#' @importFrom dplyr mutate
#' @importFrom dplyr filter

LMEdaynight <- function(pa,
                        grporder=NA,
                        skipNight0=FALSE,
                        poolExp1=NA,
                        poolExp2=NA,
                        poolExp3=NA,
                        sameClutch1=NA,
                        sameClutch2=NA,
                        sameClutch3=NA,
                        silent=FALSE,
                        detailsOrNo=TRUE) {

  # prepare parameter dataframe(s) ------------------------------------------

  pal <- paramReadPivot(pa,
                        grporder=grporder,
                        skipNight0=skipNight0,
                        poolExp1=poolExp1,
                        poolExp2=poolExp2,
                        poolExp3=poolExp3)
  # pal for pa long format

  # paramReadPivot() put grp in correct order (set by user or alphabetical)
  # so copy it here (only useful when NA, so can now have alphabetical order)
  grporder <- levels(pal$grp)

  # if only one replicate, give disclaimer
  if(length(unique(pal$date_box))==1) {
    cat('\t \t \t \t >>> Only one experiment given. Do not forget you can give multiple replicates at once to enjoy the full power of LME! \n')
  }

  # tell the parameter we are working on
  if (!silent) {
    cat('\n \n \t \t \t \t ##################################### \n')
    cat('\t \t \t \t Parameter', unique(pal$parameter), '\n')
  }



  # add age column ----------------------------------------------------------
  # for now will say 0, 1, 2, ...
  # simply following the notation of the day or night

  # if informative, we could ask user to tell dpf at day0 to be more intuitive
  pal <- pal %>%
    add_column(dpf=readr::parse_number(as.character(pal$win)), .after='win')


  # add clutch information --------------------------------------------------
  # if same clutch was tracked in two separate experiments
  ### if sameClutch mode *ON* ###
  if (!is.na(sameClutch1[1]) | !is.na(sameClutch2[1]) | !is.na(sameClutch3[1])) {

    # check more than one exp
    if(length(unique(pal$date_box))<2)
      stop('\t \t \t \t >>> Error LMEdaynight: it does not make sense to have fewer than  and give sameClutch setting \n')

    sameClutchMode <- TRUE

    # add clutch column as date_box for now
    pal <- pal %>%
      mutate(clutch=date_box, .before='date')

    # put sameClutch in a list
    scL <- list(sameClutch1, sameClutch2, sameClutch3)

    # loop through the list with the experiments to label as same clutch
    for(sc in 1:length(scL)) {

      if (is.na(scL[[sc]][1])) next

      cat('\t \t \t \t >>> Labelling experiments', scL[[sc]], 'as clutch', sc,'\n')

      # now find all rows which belong to sameClutch experiments and edit clutch column
      pal[which(pal$date_box %in% scL[[sc]]), 'clutch'] <- paste0('clutch', sc)

    }
  } else {
    sameClutchMode <- FALSE
  }


  # skipNight0, if needed ---------------------------------------------------
  # remove all night0 data (if needed)
  if (skipNight0) {
    pal <- pal %>%
      filter(win!='night0')
  }


  # split by day/night ------------------------------------------------------

  if(!silent) {
    cat('\t \t \t \t >>> Separating day and night datapoints \n')
  }

  pday <- pal %>%
    filter(daynight=='day')

  pnight <- pal %>%
    filter(daynight=='night')


  # linear mixed effects model on DAY ---------------------------------------

  if (sum(!is.na(pday$param))>0) {

    lmday <- LMEmodel(pdn=pday,
                      sameClutchMode=sameClutchMode)

    # comparison
    lmdayaov <- anova(lmday$null, lmday$full)

    # details, if required...
    if(detailsOrNo) {

      cat('\n \t \t \t \t >>> DAY \n \n')
      cat('\t \t \t \t >>> full model: \n')
      print(summary(lmday$full))

      cat('\t \t \t \t >>> null model: \n')
      print(summary(lmday$null))

      cat('\t \t \t \t >>> test: \n')
      print(lmdayaov)
      print(summary(lmdayaov))
    }
  }


  # linear mixed effects model on NIGHT -------------------------------------

  if (sum(!is.na(pnight$param))>0) {

    lmnight <- LMEmodel(pdn=pnight,
                        sameClutchMode=sameClutchMode)
    # comparison
    lmnightaov <- anova(lmnight$null, lmnight$full)

    # details, if required...
    if(detailsOrNo) {
      cat('\n \t \t \t \t >>> DAY \n \n')
      cat('\t \t \t \t >>> full model: \n')
      print(summary(lmnight$full))

      cat('\t \t \t \t >>> null model: \n')
      print(summary(lmnight$null))

      cat('\t \t \t \t >>> test: \n')
      print(lmnightaov)
      print(summary(lmnightaov))
    }
  }


  # print summary results for user ------------------------------------------

  # at the same time, prepare a small list with key values to be returned
  # two elements: day and night, and each is a vector:
  # 1-- experiments used (as date_box & date_box & ...)
  # 2-- parameter
  # 3-- results for group
  # 4-- LME slope
  # 5-- LME slope error
  # 6-- pvalue
  # 7-- symbol (i.e. asterisk for significance)
  nms <- c('exps', 'parameter', 'daynight', 'referenceGroup', 'beingGroup', 'LMEslope', 'LMEerror', 'pval', 'pvalsymbol', 'posthocpval', 'posthocpvalsymbol')

  lmeL <- vector(mode='list', length=2) # preallocate a list with two slots
  names(lmeL) <- c('day', 'night') # one for day, one for night

  # for 1--, write as YYMMDD_BX & YYMMDD_BX & ...
  exps <- paste(unique(paste(pal$date, pal$box, sep='_')), collapse=' & ')


  ### DAY ###
  if (sum(!is.na(pday$param))>0) {

    # summary sentences
    if(!silent) {cat('\t \t \t \t >>> DAY summary \n')}
    lmlday <- LMEsummary(lmemodel=lmday$full,
                         anov=lmdayaov,
                         grporder=grporder,
                         silent=silent)
    # LMEsummary will print results in console
    # we also record the key values, which are in a small list prepared by LMEsummary

    # record in the list key values
    # before, add experiments and 'day' or 'night' so we can merge everything in one big table later
    lmlday <- lapply(lmlday, function(l) {
      l2 <- c(exps, unique(pal$parameter), 'day', l)
      names(l2) <- nms # add the names set above
      return(l2)
    })
    # append in one small table for now, which we put in the list lmeL at the day slot
    lmeL$day <- as.data.frame(do.call(rbind, lmlday))
  }

  ### NIGHT ###
  if (sum(!is.na(pnight$param))>0) {

    # summary sentences
    if(!silent) {cat('\t \t \t \t >>> NIGHT summary \n')}
    lmlnight <- LMEsummary(lmemodel=lmnight$full,
                           anov=lmnightaov,
                           grporder=grporder,
                           silent=silent)
    # LMEsummary will print results in console
    # we also record the key values, which are in a small list prepared by LMEsummary

    # record in the list key values
    # before, add experiments and 'day' or 'night' so we can merge everything in one big table later
    lmlnight <- lapply(lmlnight, function(l) {
      l2 <- c(exps, unique(pal$parameter), 'night', l)
      names(l2) <- nms # add the names set above
      return(l2)
    })
    # append in one small table for now, which we put in the list lmeL at the night slot
    lmeL$night <- as.data.frame(do.call(rbind, lmlnight))

  }


  # return key values -------------------------------------------------------

  # from small list lmeL
  # if did not calculate day or night, will just have NULL at that slot
  # can leave it like this
  invisible(lmeL) # invisible here will behave as return except it will not print to Console if not assigning to a variable

}




# function LMEreport(...) -------------------------------------------------
# smaller function to be used within ggParameterGrid()
# writes LME report as csv file from list of parameter tables

# psL is essentially list of parameter tables, read in ggParameterGrid() how created

#' Title
#'
#' @param paDir
#' @param poolExp1
#' @param poolExp2
#' @param poolExp3
#' @param sameClutch1
#' @param sameClutch2
#' @param sameClutch3
#' @param grporder
#' @param skipNight0
#' @param silent
#' @param detailsOrNo
#' @param exportPath
#'
#' @return
#' @export
#'
#' @examples
LMEreport <- function(paDir,
                      poolExp1=NA,
                      poolExp2=NA,
                      poolExp3=NA,
                      sameClutch1=NA,
                      sameClutch2=NA,
                      sameClutch3=NA,
                      grporder=NA,
                      skipNight0=FALSE,
                      silent=FALSE,
                      detailsOrNo=FALSE,
                      exportPath) {

  ### check export is .csv ###
  if(!endsWith(exportPath, '.csv'))
    stop('\t \t \t \t >>> Error LMEreport: exportPath should finish with .csv. \n')

  ### import parameter tables ###
  psL <- importBhvParams(paDir=paDir,
                         poolExp1=poolExp1,
                         poolExp2=poolExp2,
                         poolExp3=poolExp3,
                         grporder=grporder,
                         skipNight0=skipNight0)

  #### run LMEdaynight on each parameter table ###
  lmeall <- lapply(psL, function(ps) {

    lme <- LMEdaynight(pa=ps,
                       sameClutch1=sameClutch1,
                       sameClutch2=sameClutch2,
                       sameClutch3=sameClutch3,
                       grporder=grporder,
                       skipNight0=TRUE,
                       silent=silent,
                       detailsOrNo=detailsOrNo)

    return(as.data.frame(do.call(rbind, lme)))

  })

  # LMEdaynight returns exps and parameter so we can merge it all in one big table
  lmeall <- data.table::rbindlist(lmeall)

  # export
  data.table::fwrite(lmeall, file=exportPath)
  cat('\t \t \t \t >>> Saved', exportPath, ' \n')

}




# function LMEsummary(...) ------------------------------------------------
# small function to prepare summary sentences when running LMEdaynight()

#' Title
#'
#' @param lmemodel
#' @param anov
#' @param grporder
#' @param silent
#'
#' @return
#'
#' @examples
LMEsummary <- function(lmemodel,
                       anov,
                       grporder,
                       silent=FALSE) {

  # get the slope estimates
  ests <- as.data.frame(summary(lmemodel)[10][[1]])
  # easier if we flip it so we have groups are columns
  estt <- t(ests)
  # get the estimates
  est <- estt['Estimate', startsWith(colnames(estt), 'grp') ]
  # get the errors
  std <- estt['Std. Error', startsWith(colnames(estt), 'grp') ]
  # recover the group as columns
  colgrps <- colnames(estt)[startsWith(colnames(estt), 'grp')]
  # currently given as grpwt, grphet, etc. Remove 'grp' so only wt, het, etc.
  colgrps <- gsub('grp', '', colgrps)
  # put the grps as names on vector of estimates & errors
  names(est) <- colgrps
  names(std) <- colgrps
  # check that the groups are the same
  if (! identical(names(est), names(std)))
    stop('\t \t \t \t Error LMEsummary: estimates and std vectors do not give the same groups in same order. \n')
  # which group are we comparing to? It is simply any group that is in grporder but for which we do not have an estimate
  refgrp <- grporder[which(!grporder %in% names(est))] # reference group

  # check we have 1 and only 1
  if (length(refgrp) != 1)
    stop('\t \t \t \t Error LMEsummary: 0 or more than 1 reference groups, which does not make sense. \n')

  # we will also return key values in a small list
  # each element of the list is one small vector with the key values for one treatment group
  # preallocate it:
  lml <- vector(mode='list', length(est)) # i.e. number of elements is number of treatment groups, or total number of groups minus 1

  # get the pval now
  pval <- as.data.frame(anov[8]) # get the p-value table
  pval <- pval$`Pr(>Chisq)`
  pval <- as.numeric(pval[!is.na(pval)])
  # there should only be one pvalue at this stage
  if (length(pval) != 1)
    stop('\t \t \t \t Error LMEsummary: 0 or more than 1 p-values, which is unexpected. \n')

  ### if pval is significant, run posthoc tests ###
  # with package emmeans
  # in practice, easier to always run LMEposthoc
  # and replace by NA if model p-value was not significant
  lmepo <- LMEposthoc(lmemodel=lmemodel)

  # keep only comparisons to reference group
  # e.g. this often means deleting het vs hom comparison
  lmepo <- subset(lmepo, referenceGroup==refgrp)

  # now loop through estimates and print the summary sentence for each comparison
  # also record in small list the key values for that group
  # each iteration is one treatment group
  for (i in 1:length(est)) {

    if (!silent) {
      cat('\t \t \t \t \t >>>', refgrp, 'vs', as.character(names(est)[i]), '=', est[i], '±', std[i], '\n')
    }

    # take correct p-value from posthoc results
    if(!refgrp %in% lmepo$referenceGroup | !names(est)[i] %in% lmepo$beingGroup)
      stop('\t \t \t \t >>> Error LMEsummary: did not find comparison ', refgrp, ' vs ', names(est)[i], ' in posthoc LME results \n')

    popval <- lmepo[which(lmepo$referenceGroup==refgrp & lmepo$beingGroup==names(est)[i]), 'posthocpval']

    # vector of key values:
    kvals <- c(as.character(refgrp),
               as.character(names(est)[i]),
               as.numeric(est[i]),
               as.numeric(std[i]),
               pval,
               pvalAsterisk(pval),
               popval,
               pvalAsterisk(popval))

    lml[[i]] <- kvals

  }

  if (!silent) {
    cat('\t \t \t \t \t >>> Does group significantly affect this behavioural parameter? pval =',
        pval, pvalAsterisk(pval), '\n')

    cat('\t \t \t \t \t >>> Posthoc comparisons: \n')
    print(lmepo)
    cat('\n \n')

  }

  # return the small list with key values
  return(lml)

}



# function LMEposthoc(...) ------------------------------------------------

#' Title
#'
#' @param lmemodel
#'
#' @return
#'
#' @examples
LMEposthoc <- function(lmemodel) {
  lmepo <- summary(emmeans::emmeans(lmemodel, list(pairwise ~ grp), adjust='none'))

  return( data.frame(referenceGroup=strNthSplit(lmepo[[2]][,1], split=' - ', n=1),
                     beingGroup=strNthSplit(lmepo[[2]][,1], split=' - ', n=2),
                     posthocpval=lmepo[[2]][,'p.value']) )

}



# function LMEmodel(...) --------------------------------------------------

# function is essentially to avoid repeating same section of code for both day and night in LMEdaynight()
# pdn is parameter table for just day or night, prepared by LMEdaynight
#' Title
#'
#' @param pdn
#' @param sameClutchMode
#'
#' @return
#'
#' @examples
LMEmodel <- function(pdn,
                     sameClutchMode) {

  # we will return a list with full model / null model

  ### if sameClutch mode *OFF* ###
  if (!sameClutchMode) {

    ### IF ONLY ONE EXP:
    if(length(unique(pdn$date_box))==1) {
      # full model
      lmfull <- lme4::lmer(param ~ grp + (1|fish) + (1|dpf), data=pdn, REML=FALSE)
      # null model
      lmnull <- lme4::lmer(param ~ 1 + (1|fish) + (1|dpf), data=pdn, REML=FALSE)

      return(list(full=lmfull, null=lmnull))

      ### IF MORE THAN ONE EXP:
    } else {
      # full model
      lmfull <- lme4::lmer(param ~ grp + (1|date_box/fish) + (1|dpf), data=pdn, REML=FALSE)
      # null model
      lmnull <- lme4::lmer(param ~ 1 + (1|date_box/fish) + (1|dpf), data=pdn, REML=FALSE)

      return(list(full=lmfull, null=lmnull))
    }

  }

  ### if sameClutch mode *ON* ###
  else if (sameClutchMode) {

    ### IF ONLY ONE CLUTCH
    # note this is the same as "if more than one exp" above, but maybe better to be explicit
    if (length(unique(pdn$clutch))==1) {
      # full model
      lmfull <- lme4::lmer(param ~ grp + (1|date_box/fish) + (1|dpf), data=pdn, REML=FALSE)
      # null model
      lmnull <- lme4::lmer(param ~ 1 + (1|date_box/fish) + (1|dpf), data=pdn, REML=FALSE)

      return(list(full=lmfull, null=lmnull))
    }

    ### IF MORE THAN ONE CLUTCH
    if (length(unique(pdn$clutch))>1) {
      # this is exactly example ~~ leafLength ~ treatment + (1|Bed/Plant/Leaf) + (1|Season) ~~ in tutorial
      # https://ourcodingclub.github.io/tutorials/mixed-models/
      # indeed, fish (leaf) is a subset of date_box (plant) which is a subset of clutch (bed)

      # full model
      lmfull <- lme4::lmer(param ~ grp + (1|clutch/date_box/fish) + (1|dpf), data=pdn, REML=FALSE)
      # null model
      lmnull <- lme4::lmer(param ~ 1 + (1|clutch/date_box/fish) + (1|dpf), data=pdn, REML=FALSE)

      return(list(full=lmfull, null=lmnull))
    }
  }
}

