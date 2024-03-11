###################################################
# ~~~ FramebyFrame package ~~~

# LME statistics

# Francois Kroll 2022
# francois@kroll.be
###################################################


# function LMEdaynight(...) -----------------------------------------------

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
      stop('\t \t \t \t >>> Error LMEdaynight: it does not make sense to have fewer than 2 experiments and give sameClutch setting \n')

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
      # ! date_box may be a factor, in which case it would throw an error to try to modify values
      pal$clutch <- as.character(pal$clutch)
      pal[which(pal$date_box %in% scL[[sc]]), 'clutch'] <- paste0('clutch', sc)

    }
  } else {
    sameClutchMode <- FALSE
  }



  # check for empty win -----------------------------------------------------
  # sometimes an entire window is NA
  # e.g. day for sleepLatency
  # or night0 for transitionDelta
  # we keep them some that all parameter tables have the same number of columns
  # but can become an issue here

  # put back in wide format
  palw <- pal %>%
    pivot_wider(id_cols=fish,
                names_from='date_box_win',
                values_from='param')
  # any date_box_win which is all NA?
  naWin <- names(palw)[which(colSums(is.na(palw))==nrow(palw))]

  # if any, remove them from pal
  if(length(naWin)>0) {
    pal <- pal %>%
      filter(date_box_win!=naWin)
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
                       skipNight0=skipNight0,
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

  ### rest of function assumes we lmemodel is a LME model created by LMEmodel(...)
  # but there is an exception: if there was no random effects
  # which happens when user gives a single experiment with a single clutch with a single day or night
  # then what we calculate a linear regression, not an LME model
  # we will make the output match as best as possible, but the format is slightly different, so take care of this situation first

  # if we calculated a linear regression (not an LME model)...
  if(class(lmemodel)=='lm') {
    estt <- t(summary(lmemodel)$coefficients)

  # standard case: we calculated an LME model
  } else {
    # easier if we flip it (t()) so we have groups are columns
    estt <- t(as.data.frame(summary(lmemodel)[10][[1]]))
  }

  # now the formats match and we can proceed as usual...

  # get the slope estimates
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

  ## slightly different if we calculated a linear regression (not an LME model)...
  if(class(lmemodel)=='lm') {
    pval <- as.numeric(pf(summary(lmemodel)$fstatistic[1],
                          summary(lmemodel)$fstatistic[2],
                          summary(lmemodel)$fstatistic[3],
                          lower.tail = FALSE))
    # this seems to be the only way to extract the p-value of the F statistic, even though it is printed in summary(lmmodel)!
    # note, this p-value is the *same* as the p-value from an ANOVA parameter ~ group
    # can read about this here https://faculty.nps.edu/rbassett/_book/regression-with-categorical-variables.html
    # this can also be shown empirically, e.g. both commands give the same p-value 0.00606:
    # summary(aov(qsec ~ factor(carb), data=mtcars))
    # summary(lm(qsec ~ factor(carb), data=mtcars))

  ## standard case: we calculated an LME model
  } else {
    # easier if we flip it (t()) so we have groups are columns
    pval <- as.data.frame(anov[8]) # get the p-value table
    pval <- pval$`Pr(>Chisq)`
    pval <- as.numeric(pval[!is.na(pval)])
  }

  # there should only be one pvalue at this stage
  if (length(pval) != 1)
    stop('\t \t \t \t Error LMEsummary: 0 or more than 1 p-values, which is unexpected. \n')

  ### run posthoc tests ###
  ## linear regression:
  # somehow LMEposthoc(...) returns exactly the coefficient p-values of the linear regression model already, so will not modify
  # however, keep in mind those p-values are exactly the p-values in the Pr(>|t|) column of summary(LMmodel)
  # note, these p-values are the *same* as p-values from a t-test group vs reference group
  # can read about this here https://faculty.nps.edu/rbassett/_book/regression-with-categorical-variables.html
  # this can also be shown empirically, e.g. assume 'setosa' is reference group, both commands give the same p-value 4.5e-10:
  # summary(lm(Sepal.Width ~ Species, data=iris))
  # pairwise.t.test(iris$Sepal.Width, iris$Species, p.adjust.method='none')

  ## LME model:
  # with package emmeans
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

  # "table of contents" for below:

  # special case: if different clutches in same box (all rest assumes NOT THAT CASE)
  # if sameClutchMode OFF, i.e. can assume one unique clutch per box
    # if only one experiment
      # ! exception if only one day/night: switch to linear regression
    # if multiple replicate experiments
  # if sameClutchMode ON, i.e. same cluch was tracked in two or more boxes
    # if only one clutch (e.g. two boxes ran, same clutch in both)
    # if multiple clutches (e.g. box1 has clutch1, box2 has clutch2, box3 also has clutch2)

  # we will return a list with full model / null model

  ########
  ### special case: if multiple clutches in same box
  # is there a 'clutch' column?; but ! there is also a 'clutch' column when same clutch is tracked in two parallel boxes (in which case sameClutchMode is ON)
  if('clutch' %in% colnames(pdn) & !sameClutchMode) {

    # I do not see how sameClutchMode could be ON and have multiple clutches in same box
    if(sameClutchMode) stop('\t \t \t \t >>> Error LMEmodel: did not consider situation where you would both have the same clutch in multiple boxes AND multiple clutches in the same box. Please report the issue. \n')

    # 16/03/2023: will need to improve here, almost certain a situation where box1 has multiple clutches & box2 has one clutch will fail
    # for now, deal only with case where only one experiment
    # if multiple, throw error
    if(length(unique(pdn$date_box))!=1) stop('\t \t \t \t >>> Error LMEmodel: sorry, did not implement yet a situation where multiple experiments are given and one of them has multiple clutches. Please report the issue. \n')

    # if only one day/night, we cannot have dpf as random effect (as only one level)
    # it is also meaningless to have fish (ID) as random effect as it is does not group the datapoints in any ways
    # (there is only one datapoint per fish)
    if(length(unique(pdn$win))==1) {
      # full model
      cat('\t \t \t \t >>> LME formula:', unique(pdn$parameter), '~ group + (1|clutch) \n')
      lmfull <- lme4::lmer(param ~ grp + (1|clutch), data=pdn, REML=FALSE)
      # null model
      lmnull <- lme4::lmer(param ~ 1 + (1|clutch), data=pdn, REML=FALSE)

      # if more than one day/night
    } else {
      # full model
      cat('\t \t \t \t >>> LME formula:', unique(pdn$parameter), '~ group + (1|clutch/fish) + (1|dpf) \n')
      lmfull <- lme4::lmer(param ~ grp + (1|clutch/fish) + (1|dpf), data=pdn, REML=FALSE)
      # null model
      lmnull <- lme4::lmer(param ~ 1 + (1|clutch/fish) + (1|dpf), data=pdn, REML=FALSE)
    }

    return(list(full=lmfull, null=lmnull))
  }
  ########
  # below: can assume we are not in case where different clutches are in the same box, otherwise we would have returned here

  ### if sameClutch mode *OFF* ###
  if (!sameClutchMode) {

    ### IF ONLY ONE EXP:
    if(length(unique(pdn$date_box))==1) {

      # if only one day/night, we cannot have dpf as random effect (as only one level)
      # it is also meaningless to have fish (ID) as random effect as it is does not group the datapoints in any ways
      # (there is only one datapoint per fish)
      if(length(unique(pdn$win))==1) {
        # full model
        cat('\n \t \t \t \t NOTE: there is only one experiment and only one day or night, so there is no random effects.
            \t \t \t In this case, the statistics are based on a simple LINEAR REGRESSION, *not* a linear mixed-effects model.\
            \t \t \t The main p-value is exactly equal to an ANOVA parameter ~ group. The added benefit of the linear regression step is to give a measure of effect size.\
            \t \t \t The posthoc p-values are t-tests beingGroup vs referenceGroup. \n \n')
        cat('\t \t \t \t >>> LINEAR REGRESSION formula:', unique(pdn$parameter), '~ group \n')

        # full model
        lmfull <- lm(param ~ grp, data=pdn)
        # null model
        lmnull <- lm(param ~ 1, data=pdn)

      # if multiple day/night
      } else {
        # full model
        cat('\t \t \t \t >>> LME formula:', unique(pdn$parameter), '~ group + (1|larva ID) + (1|larva age) \n')
        lmfull <- lme4::lmer(param ~ grp + (1|fish) + (1|dpf), data=pdn, REML=FALSE)
        # null model
        lmnull <- lme4::lmer(param ~ 1 + (1|fish) + (1|dpf), data=pdn, REML=FALSE)
      }

      return(list(full=lmfull, null=lmnull))

    ### IF MORE THAN ONE EXP:
    } else {

      # if only one day/night, we cannot have dpf as random effect (as only one level)
      # it is also meaningless to have fish (ID) as random effect as it is does not group the datapoints in any ways
      # (there is only one datapoint per fish)
      if(length(unique(pdn$win))==1) {
        # full model
        cat('\t \t \t \t >>> LME formula:', unique(pdn$parameter), '~ group + (1|experiment) \n')
        lmfull <- lme4::lmer(param ~ grp + (1|date_box), data=pdn, REML=FALSE)
        # null model
        lmnull <- lme4::lmer(param ~ 1 + (1|date_box), data=pdn, REML=FALSE)

      } else {
        # full model
        cat('\t \t \t \t >>> LME formula:', unique(pdn$parameter), '~ group + (1|experiment/larva ID) + (1|larva age) \n')
        lmfull <- lme4::lmer(param ~ grp + (1|date_box/fish) + (1|dpf), data=pdn, REML=FALSE)
        # null model
        lmnull <- lme4::lmer(param ~ 1 + (1|date_box/fish) + (1|dpf), data=pdn, REML=FALSE)

      }


      return(list(full=lmfull, null=lmnull))
    }

  } # closes if sameClutch mode OFF

############

  ### if sameClutch mode *ON* ###
  else if (sameClutchMode) {

    ### IF ONLY ONE CLUTCH
    # note this is the same as "if more than one exp" above, but better to be explicit
    if (length(unique(pdn$clutch))==1) {

      # if only one day/night, we cannot have dpf as random effect (as only one level)
      # it is also meaningless to have fish (ID) as random effect as it is does not group the datapoints in any ways
      # (there is only one datapoint per fish)
      if(length(unique(pdn$win))==1) {
        # full model
        cat('\t \t \t \t >>> LME formula:', unique(pdn$parameter), '~ group + (1|experiment) \n')
        lmfull <- lme4::lmer(param ~ grp + (1|date_box), data=pdn, REML=FALSE)
        # null model
        lmnull <- lme4::lmer(param ~ 1 + (1|date_box), data=pdn, REML=FALSE)

      } else {
        # full model
        cat('\t \t \t \t >>> LME formula:', unique(pdn$parameter), '~ group + (1|experiment/larva ID) + (1|dpf) \n')
        lmfull <- lme4::lmer(param ~ grp + (1|date_box/fish) + (1|dpf), data=pdn, REML=FALSE)
        # null model
        lmnull <- lme4::lmer(param ~ 1 + (1|date_box/fish) + (1|dpf), data=pdn, REML=FALSE)
      }


      return(list(full=lmfull, null=lmnull))
    }

    ### IF MORE THAN ONE CLUTCH
    if (length(unique(pdn$clutch))>1) {
      # this is exactly example ~~ leafLength ~ treatment + (1|Bed/Plant/Leaf) + (1|Season) ~~ in tutorial
      # https://ourcodingclub.github.io/tutorials/mixed-models/
      # indeed, fish (leaf) is a subset of date_box (plant) which is a subset of clutch (bed)

      # if only one day/night, we cannot have dpf as random effect (as only one level)
      # it is also meaningless to have fish (ID) as random effect as it is does not group the datapoints in any ways
      # (there is only one datapoint per fish)
      if(length(unique(pdn$win))==1) {
        # full model
        cat('\t \t \t \t >>> LME formula:', unique(pdn$parameter), '~ group + (1|clutch/experiment) \n')
        lmfull <- lme4::lmer(param ~ grp + (1|clutch/date_box), data=pdn, REML=FALSE)
        # null model
        lmnull <- lme4::lmer(param ~ 1 + (1|clutch/date_box), data=pdn, REML=FALSE)

      } else {
        # full model
        cat('\t \t \t \t >>> LME formula:', unique(pdn$parameter), '~ group + (1|clutch/experiment/larva ID) + (1|larva age) \n')
        lmfull <- lme4::lmer(param ~ grp + (1|clutch/date_box/fish) + (1|dpf), data=pdn, REML=FALSE)
        # null model
        lmnull <- lme4::lmer(param ~ 1 + (1|clutch/date_box/fish) + (1|dpf), data=pdn, REML=FALSE)

      }

      return(list(full=lmfull, null=lmnull))
    }
  }

}
