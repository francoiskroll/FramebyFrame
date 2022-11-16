### Frame-by-Frame package ###
### activity parameter ###


# function activityParameter(...) -----------------------------------------

# it expects dn, i.e. frame-by-frame data splitted by day/night in a list

#' Title
#'
#' @param dn
#' @param parameter
#' @param dayduration
#'
#' @return
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom tibble add_column
activityParameter <- function(dn,
                              parameter,
                              dayduration) {

  # what is the function we need to use?

  # remind unit of activityTotalDeltaPx
  if (parameter=='activityTotalPx') {
    cat('\t \t \t \t >>> activityTotalPx: remember results given in MILLIONS (10^6) of px \n')
  }

  # if activitySlope or activityHeight, shift the name to activityRegression
  # so it calls the right function, and just remember which parameter we meant for after
  if (parameter=='activitySlope' | parameter=='activityTransitionDelta') {
    regparam <- parameter
    parameter <- 'activityRegression'
  }

  # need to edit a little bit parameter entered by the user to match the function name
  # below just adds _onefish so calls the right function, e.g. turns activityTotalPx into activeTotalPx_onefish
  activityFunction <- paste0(parameter, '_onefish')

  # (how many time columns?)
  # need this number below so we do not process the time columns as actual data
  timecols <- which(grepl("^f+[[:digit:]]", colnames(dn[[1]])))[1] - 1

  # then we calculate the activity parameter for each day/night and each fish

  # ** FIRST sapply to loop through time window, typically night0, day1, ...
  pal <- sapply( 1:length(dn), function(win) {

    cat('\t \t \t \t >>> Calculating', parameter, 'for', names(dn)[win], ' \n')

    # note, no parallelisation for activity parameters
    # they are quick to calculate and will likely take more time if parallelised

    # activityFunction is still only a string
    # need to match.fun so points to a real function which can be used below
    activityFunction <- match.fun(activityFunction)

    # will convert that time window's data into dataframe temporarily
    dnw <- as.data.frame(dn[[win]])

    # we will need to tell 'one fish' activityFunction the framerate
    # so calculate it here
    fps <- averageFramerate(dnw$exsecs) # will print fps to Console too
    # in practice, fps is only used by activitySunsetStartle currently

    # sapply to loop through fish
    sapply((timecols+1):ncol(dnw),
           function(fic) {
             # fic for fish column, will loop e.g. column #4, #5, ... (assuming first three columns are timestamps)

             # calculate the activity parameter for this fish
             # using 'one fish' version of the parameter function
             # we give it
             # ffc = frame-by-frame data for that time window/fish, ffc for frame-by-frame column
             cat('\t \t \t \t \t >>> well', colnames(dnw)[fic], ' \n')

             return(activityFunction(ffc=dnw[,fic],
                                     zhc=dnw$zhrs,
                                     bin_nsecs=10*60, # ***
                                     fps=fps))
             # *** parameter applies to regression parameters activitySlope and activityTransitionDelta
             # here, set size of sum bins prior to regression
           })

  })

  # overarching (** first) sapply gives 'pal' directly as a dataframe columns = day/night rows = fish

  # make sure it is a dataframe
  pal <- as.data.frame(pal)

  # add column names as day/night
  colnames(pal) <- names(dn)

  # put back fishid as first column
  # take them from column names of dn
  pal <- pal %>%
    add_column(fish=colnames(dn[[1]])[(timecols+1):ncol(dn[[1]])], .before=1)
  row.names(pal) <- NULL

  # exception: activitySunsetStartle is only calculated on nights
  # it would have been more intuitive not to calculate it on day data
  # but easier to calculate parameter on every window, then switch day results to NA now
  if(parameter=='activitySunsetStartle') {
    cat('\t \t \t \t >>> activitySunsetStartle is only meaningful for nights, switching day results to NA... \n')

    cols2NA <- which(substr(colnames(pal), 1, 3)=='day')

    if(length(cols2NA)>0) {
      pal[,cols2NA] <- NA
    }
  }

  # exception: parameters based on regression need further processing
  # we got a string 'intercept_slope'
  # (bit barbaric but could not find a better way to store the formula)
  if(parameter=='activityRegression') {
    if (regparam=='activitySlope') {

      # split all the strings to get the slopes
      slo <- as.data.frame(apply(pal[2:ncol(pal)], 2, function(col) {
        as.numeric(strNthSplit(stri=col, split='_', nth=2)) }))

      # put back the fish ID to re-create pal
      pal <- slo %>%
        add_column(fish=pal$fish, .before=1)

    } else if (regparam=='activityTransitionDelta') {

      # first calculate 'end intercept' for each window/each fish, i.e. activity at the end of the window

      # split all the strings to get the intercepts
      ine <- as.data.frame(apply(pal[2:ncol(pal)], 2, function(col) {
        as.numeric(strNthSplit(stri=col, split='_', nth=1)) }))

      # any negative intercepts? should switch them to 0
      # see *** below; it is rare for actual intercepts
      ine[which(ine<0, arr.ind=TRUE)] <- 0

      # check
      if (length(which(ine<0) > 0)) stop('\t \t \t \t >>> Some negative intercepts \n')

      # split all the strings to get the slopes
      slo <- as.data.frame(apply(pal[2:ncol(pal)], 2, function(col) {
        as.numeric(strNthSplit(stri=col, split='_', nth=2)) }))

      # calculate the end intercepts
      endi <- sapply(1:ncol(slo), function(w) {
        # w for window, as the above sapply will loop through columns

        sapply(1:length(slo[,w]), function(f) {
          # f for fish, as the above sapply will loop through rows

          # do we have a day or a night?
          if(startsWith(colnames(slo)[w], 'day')) {windur <- dayduration}
          if(startsWith(colnames(slo)[w], 'night')) {windur <- 24-dayduration}

          # calculate end intercept
          ei <- ine[f, w] + slo[f, w] * windur

          # if negative, switch to 0
          if(ei < 0) {ei <- 0}
          # *** not obvious what is the right decision!
          # end intercept is sometimes (rarely) negative because of a rise of activity in early night which biases the regression slope
          # but we will use these end intercepts in difference so I think we have to be sure they are all positive for the operation to make sense
          # it also makes no sense biologically to have negative activity

          return(ei)
        })

      })

      # check
      if (length(which(endi<0) > 0)) stop('\t \t \t \t >>> Some negative end intercepts \n')

      endi <- as.data.frame(endi)
      colnames(endi) <- colnames(slo)

      # now shift end intercepts to next window
      # we want them to represent 'end of previous window'
      # so e.g. day1 should get end intercept of night0
      # like so, they will be correctly aligned to do a simple subtraction with actual intercepts
      # night0 loses its end intercept (gets shifted left to day1) which is by design, transition vs previous does not exist for night0 as no previous window
      # night2 also loses its end intercept (gets shifted left to nothingness) but gets the one from day2, also by design as we do not need to know how night2 finishes
      enp <- cbind(NA, endi[1:(ncol(endi)-1)]) # enp as end of previous window
      colnames(enp) <- colnames(slo)

      # now properly aligned to calculate difference
      # we want 'start activity of new window' - 'end activity of previous window'
      # i.e. ine - enp
      del <- ine - enp # del for deltas

      # put back the fish ID to re-create pal
      pal <- del %>%
        add_column(fish=pal$fish, .before=1)

    }
  }


  # tell we are done
  cat('\t \t \t \t >>> DONE \n \n')
  gc()

  return(pal)

}
