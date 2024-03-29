### Frame-by-Frame package ###
### sleep parameter ###



# function sleepParameter(...) --------------------------------------------

# it expects dn, i.e. frame-by-frame data splitted by day/night in a list
# first, detectNaps(...) converts each element of dn into booleans asleep/not asleep, called ffz for zzz (for asleep or not) frame-by-frame data
# precisely: for each frame, 0 = frame was not part of a nap; 1 = frame was part of a nap

# second, applies a sleep parameter

#' Title
#'
#' @param dn
#' @param parameter
#' @param woi
#' @param zthr_min
#' @param inaThr
#'
#' @return
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom tibble add_column

sleepParameter <- function(dn,
                           dtbx,
                           parameter,
                           woi=NA,
                           zthr_min,
                           inaThr) {

  # what is the parameter function we need to use?

  # need to edit a little bit parameter entered by the user to match the function name
  # below just adds _onefish so calls the right function, e.g. turns sleepHours into sleepHours_onefish
  sleepFunction <- paste0(parameter, '_onefish')
  # so here sleepFunction can become e.g. sleepHours_onefish

  # first convert dn into asleep/not asleep booleans using detectNaps(...)
  # dnz for day/night asleep/not asleep booleans
  dnz <- detectNaps(ffsource=dn,
                    dtbx=dtbx,
                    woi=woi,
                    zthr_min=zthr_min,
                    inaThr=inaThr)

  # (how many time columns?)
  # need this number below so we do not loop through the time columns
  timecols <- which(grepl("^f+[[:digit:]]", colnames(dnz[[1]])))[1] - 1

  # now we calculate `sleep parameter` for each day/night, and each fish
  cat('\n \t \t \t \t >>> Calculating ~~~', parameter, '~~~ \n \n')

  # ** FIRST sapply to loop through time window, typically night0, day1, ...
  pal <- sapply( 1:length(dnz), function(win) {

    cat('\n \t \t \t \t >>>', toupper(names(dnz)[win]), ' \n')

    # sleepFunction is still only a string
    # need to match.fun so points to a real function which can be used below
    sleepFunction <- match.fun(sleepFunction)

    # will convert that time window's data into dataframe temporarily
    dnzw <- as.data.frame(dnz[[win]])

    # we will need to tell 'one fish' sleepFunction the framerate
    # so calculate it here
    fps <- averageFramerate(dnzw$exsecs) # will print fps in Console too

    ### set-up a progress bar
    # how many fish are we about to calculate?
    nfis <- ncol(dnzw)-timecols # number of columns in the data, minus the time columns
    prg <- txtProgressBar(min=0, max=nfis, style=3, char='><> ')

    sapply((timecols+1):ncol(dnzw),
           function(fic) {
             # fic for fish column, will loop e.g. column #4, #5, ... (assuming first three columns are timestamps)

             # now calculate the sleepParameter for this fish
             # we give it the sleepParameter function
             # ffc = frame-by-frame data for that time window/fish, ffc for frame-by-frame column
             # zzc = the asleep/not asleep booleans (called zzz) for that time window/fish, zzc for zzz column

             # cat('\t \t \t \t \t >>> well', colnames(dnzw)[fic], ' \n')
             # which fish are we at?
             setTxtProgressBar(prg, fic-timecols) # update progress bar
             # fic-timecols will give which fish we are at

             return(sleepFunction(zzc=dnzw[,fic],
                                  fps=fps))

           })

  })

  # overarching (** first) sapply gives 'pal' directly as a dataframe columns = day/night rows = fish

  # make sure it is a dataframe
  pal <- as.data.frame(pal)

  # add column names as day/night
  colnames(pal) <- names(dnz)

  # put back fishid as first column
  # take them from column names of dnz
  pal <- pal %>%
    add_column(fish=colnames(dnz[[1]])[(timecols+1):ncol(dnz[[1]])], .before=1)
  row.names(pal) <- NULL

  # exception: sleepLatency is only calculated on nights
  # day sleepLatency is meaningless (we know zebrafish are diurnal) and creates outliers
  # it would have been more intuitive not to calculate it on day data
  # but easier to calculate parameter on every window, then switch day results to NA now
  if(parameter=='sleepLatency') {
    cat('\t \t \t \t >>> sleepLatency is only meaningful for nights, switching day results to NA... \n')

    cols2NA <- which(substr(colnames(pal), 1, 3)=='day')

    if(length(cols2NA)>0) {
      pal[,cols2NA] <- NA
    }
  }

  return(pal)

}
