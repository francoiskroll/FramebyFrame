### Frame-by-Frame package ###
### import behaviour parameters ###


# function importBhvParams(...) -------------------------------------------
# to import parameter tables from directory(ies) bhvparams
# returns a list, each element is one parameter table

#' Title
#'
#' @param paDir
#' @param poolExp1
#' @param poolExp2
#' @param poolExp3
#' @param grporder
#' @param skipNight0
#'
#' @return
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select
#' @importFrom dplyr mutate

importBhvParams <- function(paDir,
                            poolExp1=NA,
                            poolExp2=NA,
                            poolExp3=NA,
                            grporder=NA,
                            skipNight0=FALSE) {

  # should import all files in paDir
  # but to be safe, will take only .csv files which start with activity or activebout or sleep
  # (as user may have put other stuff in the folder like plots)

  flis <- which(substrEnding(list.files(paDir), 4) == '.csv') # indices of files we will probably want to import from paDir

  # from those, keep only those which start with activity or activebout or sleep
  flis <- flis[which(    substr( list.files(paDir)[flis] , 1, nchar('activity') ) == 'activity' |
                           substr( list.files(paDir)[flis] , 1, nchar('activebout') ) == 'activebout' |
                           substr( list.files(paDir)[flis] , 1, nchar('sleep') ) == 'sleep')]

  # paths we want are therefore
  papaths <- list.files(paDir, full.names=TRUE)[flis]

  # now ready to import all parameter tables
  psl <- paramReadPivot(pa=papaths,
                        poolExp1=poolExp1,
                        poolExp2=poolExp2,
                        poolExp3=poolExp3,
                        grporder=grporder,
                        skipNight0=skipNight0)
  # psl for ParameterS Long format

  # now split it back into one dataframe per parameter
  # will give a list, one element per parameter
  # better doing this than importing each parameter dataframe as one element in a list directly
  # so that we can have multiple times the same parameter in the folder
  # e.g. if pooling together different experiments
  psL <- split(psl, psl$parameter)

  # re-order this list so parameters follow the order of `allparameters` (decided in FramebyFrame.R)
  # we can get the new order by matching `allparameters` with the parameters present in psL
  neword <- match(allparameters, names(psL))
  # any NA means a parameter not present in psL
  neword <- neword[!is.na(neword)]
  # now apply this new order
  psL <- psL[neword]
  # parameters are now in correct order

  # now, each element of psL is one pa in long format
  # however, if we want to give it to ggParameter as it is now, we need wide format (as ggParameter runs paramReadPivot again)
  # we need to reset back to how the parameter table looked in the folder, basically

  psL <- lapply(psL, function(ps) {

    ps %>%
      mutate(pid=paste(parameter, date, box, fish, grp, sep='@'), .before=1) %>%
      pivot_wider(pid,
                  names_from=win,
                  values_from=param) %>%
      mutate(grp=strNthSplit(pid, split='@', nth=5), .before=1) %>%
      mutate(fish=strNthSplit(pid, split='@', nth=4), .before=1) %>%
      mutate(box=strNthSplit(pid, split='@', nth=3), .before=1) %>%
      mutate(date=strNthSplit(pid, split='@', nth=2), .before=1) %>%
      mutate(parameter=strNthSplit(pid, split='@', nth=1), .before=1) %>%
      select(-pid)

  })


  return(psL)

}
