### Frame-by-Frame package ###
### miscellaneous ###


# function appendRAWs(...) ------------------------------------------------
# was the experiment interrupted for some reason? e.g. powercut
# function allows to append two or more RAWs.csv into one
# naturally it will not magically recover the missing datapoints

#' Title
#'
#' @param ffpaths
#' @param exportPath
#' @param dayduration
#'
#' @return
#' @export
#'
#' @examples
appendRAWs <- function(ffpaths,
                       exportPath,
                       dayduration=14) {

  if(length(ffpaths)!=2) stop('\t \t \t \t Error appendRAWs: sorry, only supports appending two RAWs.csv for now \n')

  ffs <- lapply(1:length(ffpaths), function(r) {

    importRAWs(ffpath=ffpaths[r])

  })


  # some info about the datapoints we lost
  cat('\n \t \t \t \t >>> Experiment was interrupted from', as.character(max(ffs[[1]]$fullts)),
      'to', as.character(min(ffs[[2]]$fullts)), '\n')
  # i.e. fullts are correct as they give absolute clock times

  # part1 and part2 may not use the same 9AM as reference
  # they would only use the same 9AM as reference if the experiment was interrupted on day0
  # how many days difference between their 9AM references?
  ndays <- lubridate::day(ffs[[2]]$fullts[1]) - lubridate::day(ffs[[1]]$fullts[1])

  # we simply need to add 24 * ndays to the zhrs of part2
  ffs[[2]]$zhrs <- ffs[[2]]$zhrs + 24

  # does it make sense
  cat('\t \t \t \t >>> Zeitgeber durations: missing', round(min(ffs[[2]]$zhrs) - max(ffs[[1]]$zhrs), 2), 'hours =',
      round(60*(min(ffs[[2]]$zhrs) - max(ffs[[1]]$zhrs)), 1), 'minutes of data \n')

  # interruption needs to be a positive duration
  # if negative, something wrong about timestamps or RAWs.csv not given in order in which they should be appended
  if( (min(ffs[[2]]$zhrs) - max(ffs[[1]]$zhrs)) < 0)
    stop('\t \t \t \t >>> Error appendRAWs: finding that the interruption has a negative duration, which does not make sense. Probable causes:\
         \t \t \t \t -- the ffpaths were not given in the chronological order, i.e. earliest RAWs.csv should be given first\
         \t \t \t \t -- when running vpSorter to generate one of the RAWs.csv files, an incorrect Zebralab file (zebpath) was given, e.g. giving the Zebralab file of part1 when running vpSorter for part2 \n')

  # for the exsecs, we need to get an estimate of how long the interruption was
  # which we will get from the fullts
  lagsec <- as.numeric(difftime(ffs[[2]]$fullts[1], ffs[[1]]$fullts[nrow(ffs[[1]])], units='secs'))
  # and now we add to all exsecs of part2: last exsecs of part1 + lagsec
  ffs[[2]]$exsecs <- ffs[[2]]$exsecs + ffs[[1]]$exsecs[nrow(ffs[[1]])] + lagsec

  # does it make sense
  cat('\t \t \t \t >>> Seconds since start of experiment: missing', round(min(ffs[[2]]$exsecs) - max(ffs[[1]]$exsecs), 2), 'seconds =',
      round((min(ffs[[2]]$exsecs) - max(ffs[[1]]$exsecs))/60, 1), 'minutes of data \n')

  # now can append
  ff <- data.table::rbindlist(ffs)

  # now ready to write, as if it was out of Vp_Sorter
  data.table::fwrite(ff, exportPath, row.names=FALSE)


  #### find light transitions ####
  # we need to re-create lights.csv file
  # chunk of code was taken directly from vpSorter when v18

  # sunsets and sunrises times (in number of hours since day0 sunrise at ZT0) for 10 days
  suns <- c(rbind(seq(dayduration, 720, 24), seq(24, 720, 24)))
  # typically dayduration = 14
  # so will go 14, 24, 38, 48 etc.

  # take only the ones we will find in the data
  # round down last timepoint (indeed, if e.g. experiment stops at 71.9 hours, there is no 72 hours sunrise)
  lastz <- floor(max(ff$zhrs))
  # experiment should always start between 0+ and before 14 so should not matter how we round start
  firstz <- floor(min(ff$zhrs))

  suns <- intersect(suns, firstz:lastz) # will give the sunsets/sunrises we expect to encounter in the data

  # find the transition frames
  # using function findLightTransitionFrame(...) from above

  if (length(suns) != 0) { # if no light transition (e.g. experiment lasted just a few hours), should skip this step
    # preallocate small dataframe transition number / sunset or sunrise / frame # / full timestamp / Zeitgeber duration / seconds of experiment
    ltcolnms <- c('transition_num', 'sunset_or_sunrise', 'frame', 'full_timestamp', 'zhrs', 'exsecs') # light transitions column names
    lt <- as.data.frame(matrix(nrow=length(suns), ncol=length(ltcolnms))) # lt for light transitions
    colnames(lt) <- ltcolnms
    lt$full_timestamp <- lubridate::ymd_hms(NA)

    for (s in 1:length(suns)) {

      # fill in transition number
      lt[s, 'transition_num'] <- s

      # fill in sunset or sunrise
      # if index in suns is even (2, 4, ...): sunrise; if index in suns is odd (1, 3, ...): sunset
      if (s %% 2 == 0) {
        lt[s, 'sunset_or_sunrise'] <- 'sunrise'
      } else if (s %% 2 != 0) {
        lt[s, 'sunset_or_sunrise'] <- 'sunset'
      }


      # find frame number of transition
      tfra <- findLightTransitionFrame(ff$zhrs, round(suns[s])) # transition frame

      # fill it in
      lt[s, 'frame'] <- tfra

      # fill in full timestamp
      lt[s, 'full_timestamp'] <- ff[tfra, 'fullts']

      # fill in Zeitgeber duration
      lt[s, 'zhrs'] <- ff[tfra, 'zhrs']

      # fill in number of seconds since start of the experiment
      lt[s, 'exsecs'] <- ff[tfra, 'exsecs']

      # give a quick check to user
      cat('\t \t \t \t >>> Light transition #', s,
          ': ', toupper(lt[s, 'sunset_or_sunrise']), ' at frame ', tfra,
          ', clock = ', as.character(lt[s, 'full_timestamp']),
          ', Zeitgeber duration = ', lt[s, 'zhrs'], '\n',
          sep='')

    }

    # write light transitions
    dtbx <- substr(basename(ffpaths[1]), 1, 9) # get YYMMDD_BX component of ffpath 1
    outpath <- paste0(dirname(ffpaths[1]), whatSlash(ffpaths[1]), dtbx, '_lights.csv')
    data.table::fwrite(lt, file=outpath)
  } # closes "if suns is not length 0"

}



# function expMerger(...) -------------------------------------------------

# this function allows to merge multiple experiments into one
# say you have two 96-well plate experiments, the experiment will merge the files as if they had been generated by a one 192-well experiment
# no normalisation of any kind is performed so use at your own risk

# currently does not support merging two experiments which did not start at exactly the same time
# both experiments have to share the same Zebralab xls file
# could update this in the future
# the solution to merge experiments with different Zebralab xls files is to make them all start when the latest exp started
# i.e. trim some data from the earlier exps

#' Title
#'
#' @param ffpaths
#' @param genopaths
#' @param exportPath
#'
#' @return
#' @export
#'
#' @examples
expMerger <- function(ffpaths,
                      genopaths,
                      exportPath) {

  ### import the RAWs.csv one by one and put them in a list
  ffL <- lapply(1:length(ffpaths), function(r) {

    importRAWs(ffpath=ffpaths[r])

  })


  ###
  identical(ffL[[1]]$zhrs, ffL[[2]]$zhrs)

  # some info about the datapoints we lost
  cat('\n \t \t \t \t >>> Experiment was interrupted from', as.character(max(ffs[[1]]$fullts)),
      'to', as.character(min(ffs[[2]]$fullts)), '\n')
  # i.e. fullts are correct as they give absolute clock times

  # part1 and part2 may not use the same 9AM as reference
  # they would only use the same 9AM as reference if the experiment was interrupted on day0
  # how many days difference between their 9AM references?
  ndays <- lubridate::day(ffs[[2]]$fullts[1]) - lubridate::day(ffs[[1]]$fullts[1])

  # we simply need to add 24 * ndays to the zhrs of part2
  ffs[[2]]$zhrs <- ffs[[2]]$zhrs + 24

  # does it make sense
  cat('\t \t \t \t >>> Zeitgeber durations: missing', round(min(ffs[[2]]$zhrs) - max(ffs[[1]]$zhrs), 2), 'hours =',
      round(60*(min(ffs[[2]]$zhrs) - max(ffs[[1]]$zhrs)), 1), 'minutes of data \n')

  # for the exsecs, we need to get an estimate of how long the interruption was
  # which we will get from the fullts
  lagsec <- as.numeric(difftime(ffs[[2]]$fullts[1], ffs[[1]]$fullts[nrow(ffs[[1]])], units='secs'))
  # and now we add to all exsecs of part2: last exsecs of part1 + lagsec
  ffs[[2]]$exsecs <- ffs[[2]]$exsecs + ffs[[1]]$exsecs[nrow(ffs[[1]])] + lagsec

  # does it make sense
  cat('\t \t \t \t >>> Seconds since start of experiment: missing', round(min(ffs[[2]]$exsecs) - max(ffs[[1]]$exsecs), 2), 'seconds =',
      round((min(ffs[[2]]$exsecs) - max(ffs[[1]]$exsecs))/60, 1), 'minutes of data \n')

  # now can append
  ff <- data.table::rbindlist(ffs)

  # now, remember _RAWs.csv out of Vp_Sorter.R only has one time column, called 'time'
  # and it is exsecs here, so:
  # remove fullts column
  ff[, fullts:=NULL]
  # remove zhrs column (it was not necessary to correct it above, but good as a check)
  ff[, zhrs:=NULL]
  # change name of exsecs to time
  colnames(ff)[which(colnames(ff)=='exsecs')] <- 'time'

  # now ready to write, as if it was out of Vp_Sorter
  data.table::fwrite(ff, exportPath, row.names=FALSE)
}
