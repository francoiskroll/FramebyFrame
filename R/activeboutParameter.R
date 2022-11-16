### Frame-by-Frame package ###
### active bout parameter ###

# below are two functions

# 1-- main one (last) is activeboutParameter(...)
# it is called when the user asks for a parameter which requires distinguishing single active bouts
# e.g. activeboutMean or activeboutNum
# it loops loops through time windows, and then for each time window through each fish
# to speed up the calculations, looping through the fish is parallelised using parSapply from snow package
# for each fish's data, it calls the activeboutPARAMETER_onefish() function matching the parameter entered by the user
# these functions are stored in activeboutParameter_onefish()

# 2-- a smaller helper function, framesToActiveInactiveTransitions(...)
# it is called by activeboutParameter(...) in order to distinguish single active bouts
# it is given dn, a list of one dataframe per time window, each storing all the frame-by-frame deltapx data for 96 fish
# it converts the list dn into a list dnai which is the same structure but stores active/inactive transitions
# active/inactive transitions are
# 0 (most values) = at this frame, the fish continued moving or continued not moving
# 1 = at this frame, the fish started an active bout, i.e. became active
# -1 = at this frame, the fish stopped an active bout, i.e. became inactive

# function framesToActiveInactiveTransitions(...) -------------------------

# helper functions for single-bout parameters such as activeboutLength
# it expects dn, i.e. frame-by-frame data splitted by day/night in a list
# it converts each day/night's frame-by-frame data into series of 0 or 1 or -1
# 0 = at that frame, fish continued moving/not moving
# 1 = at that frame, fish started moving
# -1 = at that frame, fish stopped moving

#' Title
#'
#' @param dn
#'
#' @return
#'
#' @examples
framesToActiveInactiveTransitions <- function(dn) {

  # how many time columns?
  # i.e. how many columns before first fish data
  # below tests if string looks like f + integer
  # first true will be first data column, minus 1 gives number of time columns
  # looks at first element of dn
  timecols <- which(grepl("^f+[[:digit:]]", colnames(dn[[1]])))[1] - 1

  # copy dn into dnai, i.e. day/night list active/inactive booleans
  dnai <- dn
  # and we will replace slots in dnai

  # invisible(...) below prevents it from printing result of sapply
  # we are using it as a for loop here, to go one time window at a time
  invisible(sapply(1:length(dn), function(win) {

    cat('\t \t \t \t >>> Converting', names(dn)[win], 'data into active/inactive transitions \n')

    # aiw for active/inactive for time window win, e.g. night0
    aiw <- apply(dn[[win]][,(timecols+1):ncol(dn[[win]])], 2, function(col) {diff(col>0)})
    # dn[[l]], element win of list dn, e.g. all of night0 data
    # timecols+1, skip all the timecolumns
    # : ncol(dn[[win]]), until last column of element win of list dn

    # col>0: for every datapoint in column, is it above 0? TRUE if yes (active frame); FALSE if no (inactive frame)
    # diff: difference between successive TRUE/FALSE
    # possibilities are:
    # ! second element minus first element
    # TRUE , TRUE = 1 - 1 = 0, i.e. fish continues moving
    # TRUE , FALSE = 0 - 1 = -1, i.e. fish STOPS moving
    # FALSE , TRUE = 1 - 0 = 1, i.e. fish STARTS moving
    # FALSE , FALSE = 0 - 0 = 0, i.e. fish continues NOT moving

    # diff above will skip first row, so ail will always lose one row
    # solution so we can match rows (frames) between dn and dnai is to add a row of NA on top
    aiw <- rbind(NA, aiw)

    # put back time columns
    aiw <- cbind(dn[[win]][,1:timecols], aiw)

    # convert back to data.table
    aiw <- data.table::data.table(aiw)

    # replace its slot in list dnai
    dnai[[win]] <<- aiw
  }))

  # we now have list dnai, one slot for each time window
  # each element is a dataframe columns = fish, rows = frames
  # datapoints are -1 or 1 or 0, see above what they represent
  return(dnai)

}



# function activeboutParameter(...) ---------------------------------------

# it expects dn, i.e. frame-by-frame data splitted by day/night in a list
# first, it converts dn into active/inactive transitions using framesToActiveInactiveTransitions(...)
# it then calculates the active bout parameter for each fish

# note, activebout parameters return the *mean* parameter for all active bouts
# e.g. activeboutLength: it measures the duration of every active bout and returns the mean of them

#' Title
#'
#' @param dn
#' @param parameter
#'
#' @return
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFromm tibble add_column
activeboutParameter <- function(dn,
                                parameter) {

  # what is the function we need to use?

  # need to edit a little bit parameter entered by the user to match the function name
  # below just adds _onefish so calls the right function, e.g. turns activeboutMean into activeboutMean_onefish
  activeboutFunction <- paste0(parameter, '_onefish')
  # so here activeboutFunction can become activeboutMean_onefish, activeboutMax_onefish, etc.

  # first convert dn into active/inactive transitions using framesToActiveInactiveTransitions(...)
  # dnai for day/night active/inactive transitions
  dnai <- framesToActiveInactiveTransitions(dn)

  # (how many time columns?)
  # need this number below so we do not loop process the time columns
  timecols <- which(grepl("^f+[[:digit:]]", colnames(dn[[1]])))[1] - 1

  # then we calculate mean activebout parameter for each day/night and each fish

  # v1: faster if parallel the windows, not the fish as previously
  # i.e. tell each cluster to work on one window, not each cluster to work on some fish
  # e.g. parallel across windows w/ 5 clusters = 246 sec vs parallel across fish w/ 3 clusters = 482 sec
  # I tried 2 to 7 clusters, minimum was 5 clusters = 246 sec

  # so ideal seems to be to have one cluster per window
  # will set it this way below

  # prepare parallelisation
  cluster <- snow::makeCluster(length(dnai)) # decide number of clusters here
  snow::clusterExport(cluster, c(activeboutFunction, 'transitionsToActiveBoutStartStop', 'averageFramerate'))
  # activeboutFunction from above, e.g. activeboutMean_onefish
  # clusterExport gives to each cluster the functions it will need

  activeboutFunction <- match.fun(activeboutFunction)
  # activeboutFunction is still only a string
  # need to match.fun so points to a real function which can be used below

  cat('\n \t \t \t \t >>> Parallel clusters working each on one time window... \n')
  cat('\t \t \t \t Cannot update on progress on the meanwhile. It should take less than 1 min per window. \n')

  # ** FIRST sapply to loop through time window, typically night0, day1, ...
  # this is parallel sapply, it's like a normal sapply if you ignore first argument 'cluster'
  pal <- snow::parSapply(cluster,
                         1:length(dnai), function(win) {

                           # seems to create a conflict I cannot resolve with data.table annotation
                           # will convert that time window's data into dataframe temporarily
                           dnw <- as.data.frame(dn[[win]])
                           dnaiw <- as.data.frame(dnai[[win]])

                           # we will need to tell 'one fish' activeboutFunction the framerate
                           # so calculate it here
                           fps <- averageFramerate(dnw$exsecs) # will print fps to Console too
                           # in practice, fps is only used for conversion from frames to seconds in activeboutLength

                           # ** SECOND sapply to loop through fish...
                           sapply((timecols+1):ncol(dnaiw),
                                  function(fic) {
                                    # fic for fish column, will loop e.g. column #4, #5, ... (assuming first three columns are timestamps)

                                    # calculate the mean activebout parameter for this fish
                                    # using function activeboutParameter_onefish()
                                    # we give it
                                    # ffc = frame-by-frame data for that time window/fish, ffc for frame-by-frame column
                                    # aic = the active/inactive transitions for that time window/fish, aic for active/inactive column
                                    return(activeboutFunction(ffc=dnw[,fic],
                                                              aic=dnaiw[fic],
                                                              fps=fps))
                                  })

                         })


  # overarching (** first) sapply gives 'pal' directly as a table columns = day/night rows = fish
  # convert to dataframe
  pal <- as.data.frame(pal)

  # add column names as day/night
  colnames(pal) <- names(dnai)

  # put back fishid as first column
  # take them from column names of dnai
  pal <- pal %>%
    add_column(fish=colnames(dnai[[1]])[(timecols+1):ncol(dnai[[1]])], .before=1)

  row.names(pal) <- NULL

  # tell we are done
  cat('\t \t \t \t >>> DONE \n \n')

  return(pal)

}
