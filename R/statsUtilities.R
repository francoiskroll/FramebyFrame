###################################################
# ~~~ FramebyFrame package ~~~

# STATISTICS UTILITIES

# small functions statistics-related

# Francois Kroll 2022
# francois@kroll.be
###################################################


# function mad2(...) ------------------------------------------------------
# can read here https://stackoverflow.com/questions/31095550/why-do-the-results-of-madx-differ-from-the-expected-results
# median absolute deviation function from R is not computing what is typically meant by the term
# taking definition here https://www.statisticshowto.com/median-absolute-deviation/

# this may be unnecessary/advised again; excellent answer here: https://stats.stackexchange.com/questions/523865/calculating-robust-z-scores-with-median-and-mad
#' Title
#'
#' @param vector
#' @param na.rm
#'
#' @return
#'
#' @examples
mad2 <- function(vector, na.rm=FALSE) {

  if(na.rm) {
    vector <- vector[!is.na(vector)]
  }

  return( median(abs(vector - median(vector))) )

}
# by the way, this is the same as mad(vector, constant=1)
# but here more explicit


# function diff2(...) -----------------------------------------------------
# not statistics
# but I want to 'correct' R's diff function
# e.g. diff(c(FALSE, FALSE, TRUE, TRUE)) gives 0 1 0
# so if look for first TRUE using diff, will give #2
# when actually it is #3
# I think it should return NA 0 1 0, then positions will match original vector
#' Title
#'
#' @param vector
#'
#' @return
#'
#' @examples
#' @export
diff2 <- function(vector) {

  return(c(NA, diff(vector)))

}



# function sem(...) -------------------------------------------------------
# standard error of the mean
#' Title
#'
#' @param x
#'
#' @return
#'
#' @examples
#' @export

sem <- function(x) {
  sd(x)/sqrt(length(x))
}

# standard error of the mean of non-NA values
#' Title
#'
#' @param x
#'
#' @return
#'
#' @examples
sem_narm <- function(x) {
  sd(x, na.rm=TRUE)/sqrt(length(x))
}




# function getUpperTri(...) -----------------------------------------------

# get lower triangle of a pairwise `something` matrix
#' Title
#'
#' @param pwm
#' @param skipCol
#'
#' @return
#'
#' @examples
#' @export
getUpperTri <- function(pwm,
                        skipCol=NA) {

  # then turn lower triangle into NA
  pwm[,(skipCol+1):ncol(pwm)] [lower.tri(pwm[,(skipCol+1):ncol(pwm)])] <- NA

  return(pwm)

}


# function getLowerTri(...) -----------------------------------------------

# get upper triangle of a pairwise `something` matrix
#' Title
#'
#' @param pwm
#' @param skipCol
#'
#' @return
#'
#' @examples
#' @export
getLowerTri <- function(pwm,
                        skipCol=0) {

  # then turn upper triangle into NA
  pwm[,(skipCol+1):ncol(pwm)] [upper.tri(pwm[,(skipCol+1):ncol(pwm)])] <- NA

  return(pwm)

}


# function zScore(...) ----------------------------------------------------

# tiny function to Z-score one datapoint
# given the datapoint, the mean to use, the sd to use
#' Title
#'
#' @param datapoint
#' @param mean
#' @param sd
#'
#' @return
#'
#' @examples
zScore <- function(datapoint,
                   mean,
                   sd) {

  # confirm all input are numeric
  datapoint <- as.numeric(datapoint)
  mean <- as.numeric(mean)
  sd <- as.numeric(sd)

  return(as.numeric((datapoint - mean) / sd))
}



# function pvalAsterisk(...) ----------------------------------------------


#' Title
#'
#' @param num
#'
#' @return
#'
#' @examples
pvalAsterisk <- function(num) {

  # confirm it is a numeric
  num <- as.numeric(num)

  if(is.na(num)) {
    return(NA)
  } else if (num <= 0.001) {
    return('***')
  } else if (num <= 0.01) {
    return('**')
  } else if (num <= 0.05) {
    return('*')
  } else if (num > 0.05) {
    return('ns')
  } else {
    return(NA)
  }

}
