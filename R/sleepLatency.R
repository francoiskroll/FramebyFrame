### sleepLatency ###
# parameter sleep latency need various extra functions


# function timestampsToSurvival(...) --------------------------------------

# smaller function to turn a bunch of timestamps into proportion left across time, i.e. survival
# e.g. 1min, 17min, 2min, 8min
# becomes
# 0min = 1.0 left
# 1min = 0.75 left
# 2min = 0.5 left
# 8min = 0.25 left
# 17min = 0.0 left

# ! assumes that every timestamp given signifies one individual 'dies'

# ids = column of fish IDs

# addt0: whether or not (TRUE or FALSE) to add t0 row, i.e. "at t0, 100% of larvae did not sleep yet"
# addtlast: whether or not (TRUE or FALSE) to add tlast row, i.e. "at tlast, X% of larvae did not sleep yet"
# we want this ON when plotting, so that the survival curves cover the whole window
# but it means that there is not one row for each larva
# (we add one t0 and one tlast row for each group, so in each group there will be N+2 rows)
# this makes it confusing to do Kaplan-Meier stats, so leave the option here to turn it OFF

# lastt = last timestamp, i.e. when the experiment stops
# it is only important when still some individuals left at the end of the experiment
# we want to mark last timepoint at which individuals were still there (when individuals became 'censored' in survival stats language)

#' Title
#'
#' @param dts
#' @param ids
#' @param lastt
#' @param removeDuplicates
#'
#' @return
#' @export
#'
#' @examples
timestampsToSurvival <- function(dts,
                                 ids,
                                 lastt,
                                 removeDuplicates) {

  # what is the last proportion left?
  # depends how many 'died'
  # if any did not 'die', assuming it is written NA

  # so total number is
  totn <- length(dts)

  # and number that 'died' is
  dien <- length(dts[!is.na(dts)])

  # so proportion left at the end is
  lastpro <- 1.0 - dien * (1/totn)
  # indeed, we start with 1.0, i.e. 100%
  # and each time one 'dies' proportion drops by 1/total N
  # e.g. there are 4 larvae, 1 dies, that makes 0.75 left

  # now sort the timestamps
  dts2 <- sort(dts, na.last=TRUE)
  # na.last=TRUE important here
  # default sort() removes NAs, but here they represent larvae which have not slept yet
  # now sort the fish IDs in exactly the same order
  ids2 <- ids[order(dts, na.last=TRUE)]

  # preallocate a small dataframe
  # col1: fish ID / col2: time of death** / col2: proportion left
  # ** for sleep latencies, this represents the time of the first nap in minutes (much less dramatic)
  colnms <- c('id', 'dt', 'proleft')
  srv <- as.data.frame(matrix(ncol=length(colnms), nrow=length(dts2))) # srv for survival timecourse
  colnames(srv) <- colnms

  # fill in fish IDs
  srv$id <- ids2
  # fill in timestamps
  srv$dt <- dts2

  # we do not have to go through timestamp one by one,
  # we can simply predict how proportion left decreases as:
  # at first timestamp, proportion left is 1.0 - 1/total N (because at first timestamp, one larva 'died')
  # at last timestamp, proportion left calculated above
  # at every timestamp in-between, proportion drops successively by 1/total N (i.e. at each timestamp, one larva 'dies')
  surv <- seq(1.0 - 1/totn, lastpro, -1/totn)

  # now we have one 'proportion left' per larva which slept at least once
  # add NAs at the end for the larvae which never slept
  # all larvae are in ts2, but only larvae which slept are in surv
  # so number of NA to add is the difference in lengths
  surv <- c(surv, rep(NA, length(dts2) - length(surv)))

  # check that we have one 'proportion left' per timestamp now
  if(length(surv) != length(dts2))
    stop('\t \t \t \t >>> Error: not the same number of proportions and timestamps in timestampsToSurvival() \n')

  # can add it to dataframe
  srv$proleft <- surv

  # if required, add t0: proportion left was 1.0
  srv <- srv %>%
    add_row(id=NA, dt=0, proleft=1.0, .before=1)
  # label with NA ID, so can remove it easily for statistics

  # if required, add last timestamp
  srv <- srv %>%
    add_row(id=NA, dt=lastt, proleft=lastpro, .after=nrow(srv))
  # label with NA ID, so can remove it easily for statistics

  # now can return dataframe srv
  return(srv)

}





# function survivalStats(...) ---------------------------------------------

# it is expecting a fairly specific dataframe svd, for survival dataframe
# will generalise better later if needed

#' Title
#'
#' @param svd
#' @param grporder
#' @param lastt
#' @param detailsOrNo
#'
#' @return
#' @export
#'
#' @examples
survivalStats <- function(svd,
                          grporder,
                          lastt,
                          detailsOrNo) {

  # remove any rows which has ID as NA
  # they are only added for aesthetics, not useful for statistics
  rows2remove <- which(is.na(svd$id))
  if(length(rows2remove)>0) {
    cat('\t \t \t \t >>> Removing', length(rows2remove), 'rows with ID as NA \n')
    svd <- svd[-rows2remove,]
  }

  # add status column to svd
  # 2 means dead, i.e. that ID 'died' at that timepoint
  # here, all the non-NA timestamps, as each timestamp represents a 'time of death'
  svd[which(!is.na(svd$dt)), 'status'] <- 2

  # 1 means censored, i.e. we do not know what happened to that fish (or whatever the ID represents) after that timepoint
  # here, all the NA timestamps at the end, as they represent that ID still 'alive' when the experiment stopped
  svd[which(is.na(svd$dt)), 'status'] <- 1

  # cf. http://www.sthda.com/english/wiki/survival-analysis-basics

  # check all status is either 1 or 2
  if(!all(svd$status %in% c(1, 2)))
    stop('\t \t \t \t Error survivalStats: some status is not 1 or 2. \n')

  # for survival stats model, cannot leave NAs time of death
  # they need to be last timepoint of the experiment, as they represent IDs still 'alive' at the end
  svd[is.na(svd$dt), 'dt'] <- lastt

  # now ready to calculate survival model
  # below is for log-rank test, see http://www.sthda.com/english/wiki/survival-analysis-basics
  # print(survdiff(Surv(dt, status) ~ grp, data=svd))

  # here, will use Cox Proportional-Hazards Model
  cox <- summary(coxph(formula = Surv(dt, status) ~ grp, data = svd))

  if(detailsOrNo) {
    cat('\t \t \t \t #### \n')
    print(cox)
    cat('\t \t \t \t #### \n')
  }

  # cf. here for interpretation of Hazard Ratio
  # https://s4be.cochrane.org/blog/2016/04/05/tutorial-hazard-ratios/

  # get the results in the cox model
  # flip the table so grp as columns
  coxt <- t(cox$coefficients)

  # get the coefficients
  coes <- coxt['coef', startsWith(colnames(coxt), 'grp') ]
  # get the hazard ratios
  hazs <- coxt['exp(coef)', startsWith(colnames(coxt), 'grp') ]
  # get the 95% confidence interval
  errs <- coxt['se(coef)', startsWith(colnames(coxt), 'grp') ]
  # get the pvalues
  pvals <- coxt['Pr(>|z|)', startsWith(colnames(coxt), 'grp') ]

  # currently columns written as grpwt, grphet, etc. Remove 'grp' so only wt, het, etc.
  names(coes) <- gsub('grp', '', names(coes))
  names(hazs) <- gsub('grp', '', names(hazs))
  names(errs) <- gsub('grp', '', names(errs))
  names(pvals) <- gsub('grp', '', names(pvals))

  # check that the groups are the same in all three vectors
  if (!all(sapply(list(names(hazs), names(errs), names(pvals)), FUN=identical, names(coes))))
    stop('\t \t \t \t Error survivalStats: hazard ratios, 95% confidence intervals, and p-values vectors
         do not give the same groups in same order. \n')

  # which group are we comparing to? It is simply any group that is not given here
  refgrp <- grporder[which(!grporder %in% names(hazs))] # reference group
  # check we have 1 and only 1 reference group
  if (length(refgrp) != 1)
    stop('\t \t \t \t Error survivalStats: 0 or more than 1 reference groups, which does not make sense. \n')

  # now loop through hazard ratios and print the summary sentence for each comparison
  cat('\t \t \t \t >>> Cox Proportional-Hazards Model \n')
  cat('\n \t \t \t \t compared to reference group *', refgrp, '*, \n \n')

  for (i in 1:length(hazs)) {
    cat('\t \t \t \t \t group', names(hazs)[i], 'is associated with... \n')
    cat('\t \t \t \t \t Hazard Ratio = ', hazs[i], '\n')
    cat('\t \t \t \t \t Hazard Ratio 95% confidence interval = ', errs[i],'\n')
    cat('\t \t \t \t \t p-value = ', pvals[i], pvalAsterisk(pvals[i]), '\n')

    if(coes[i]<0) {
      cat('\t \t \t \t \t i.e. at any time point, members of group', names(hazs[i]),
          'were', round(100*(1-hazs[i]), 2), '% ±', round(100*errs[i], 2),
          '*less* likely to \'die\' than members of group', refgrp, '; pvalue =', pvals[i], pvalAsterisk(pvals[i]), '\n \n')

    } else if (coes[i]>0) {
      cat('\t \t \t \t \t i.e. at any time point, members of group', names(hazs[i]),
          'were', round(100*(hazs[i]-1), 2), '% ±', round(100*errs[i], 2),
          '% *more* likely to \'die\' than members of group', refgrp, '; pvalue =', pvals[i], pvalAsterisk(pvals[i]), '\n \n')
    }
  }

  # give a disclaimer
  if(length(hazs)>1) {
    cat('\t \t \t \t >>> note, p-values are not corrected for multiple comparisons \n')
  }

  cat('\t \t ############################################################################## \n')
}
