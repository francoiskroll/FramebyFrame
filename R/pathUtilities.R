# small functions to handle path strings


#' Title
#'
#' @param path
#'
#' @return
#'
#' @examples
whatSlash <- function(path) {
  # Am I running on a Windows or a Mac?
  # on Windows, slash in path will be \\
  # on Mac, slash in path with /
  # so simply try:
  if (length(which(strsplit(path, '')[[1]] == '/')) > 0) {
    # then we are on a Mac, and the slash character is:
    return('/')
  } else if (length(which(strsplit(path, '')[[1]] == '\\')) > 0) {
    # then we are on a Windows, and the slash character is:
    return('\\\\')
  }
}



# substrEnding ------------------------------------------------------------
# take n last characters of a string
# e.g. substrEnding('210907_12_RAWs.csv', 3) >>> csv

#' Title
#'
#' @param x
#' @param n
#'
#' @return
#'
#' @examples
substrEnding <- function(x, n){ # x = string (or vector of strings); n = last n characters
  substr(x, nchar(x)-n+1, nchar(x))
}


# afterLastSlash ----------------------------------------------------------
# take everything after last /, usually to get filename and drop the path
# e.g. afterLastSlash(~/Dropbox/phd/210907_PSEN2/210907_12_RAWs.csv) >>> 210907_12_RAWs.csv

#' Title
#'
#' @param stri
#'
#' @return
#'
#' @examples
afterLastSlash <- function(stri) {

  # will assume stri is a path
  # slash character is different if Windows or Mac, so look now which one we have
  # (small variant of whatSlash())
  if (length(which(strsplit(stri, '')[[1]] == '/')) > 0) {
    # then we are on a Mac, and the slash character is:
    slash <- '/'
  } else if (length(which(strsplit(stri, '')[[1]] == '\\')) > 0) {
    # then we are on a Windows, and the slash character is:
    slash <- '\\'
  }

  as.character(sapply(stri, function(st){

    # is there at least one slash in this string?
    # if not, just return the string as it is
    if(length(which(strsplit(st, split='')[[1]]==slash))==0) return(st)

    substr(st,
           which(strsplit(st, '')[[1]] == slash) [length(which(strsplit(st, '')[[1]] == slash))] + 1,
           which(strsplit(st, '')[[1]] == slash) [length(which(strsplit(st, '')[[1]] == slash))] + 999)
  }))
}


# beforeLastSlash ---------------------------------------------------------
# take everything before last /, usually to get path to folder but drop the filename
# e.g. afterLastSlash(~/Dropbox/phd/210907_PSEN2/210907_12_RAWs.csv) >>> ~/Dropbox/phd/210907_PSEN2/

#' Title
#'
#' @param stri
#'
#' @return
#'
#' @examples
beforeLastSlash <- function(stri) {

  # will assume stri is a path
  # slash character is different if Windows or Mac, so look now which one we have
  # (small variant of whatSlash())
  if (length(which(strsplit(stri, '')[[1]] == '/')) > 0) {
    # then we are on a Mac, and the slash character is:
    slash <- '/'
  } else if (length(which(strsplit(stri, '')[[1]] == '\\')) > 0) {
    # then we are on a Windows, and the slash character is:
    slash <- '\\'
  }

  as.character(sapply(stri, function(st){

    # is there at least one slash in this string?
    # if not, just return the string as it is
    if(length(which(strsplit(st, split='')[[1]]==slash))==0) return(st)

    substr(st, 1, which(strsplit(st, '')[[1]] == slash) [length(which(strsplit(st, '')[[1]] == slash))])
  }))
}


# afterLastUnderscore -----------------------------------------------------
# take everything after last _

#' Title
#'
#' @param stri
#'
#' @return
#'
#' @examples
afterLastUnderscore <- function(stri) {

  as.character(sapply(stri, function(st){

    # check there is an underscore
    # if no, return NA
    if(length(which(strsplit(st, '')[[1]] == '_')) == 0) {
      return(NA)
    }

    substr(st,
           which(strsplit(st, '')[[1]] == '_') [length(which(strsplit(st, '')[[1]] == '_'))] + 1,
           which(strsplit(st, '')[[1]] == '_') [length(which(strsplit(st, '')[[1]] == '_'))] + 999)
  }))
}



# beforeLastUnderscore ----------------------------------------------------
# take everything before last _, usually to get YYMMDD_BX
# e.g. 200728_06_deltapxsq.csv >>> 200728_06

#' Title
#'
#' @param stri
#'
#' @return
#'
#' @examples
beforeLastUnderscore <- function(stri) {

  # some really odd behaviour when giving a column sometimes
  # confirm here stri is a simple character vector
  stri <- as.character(unlist(stri))

  as.character(sapply(stri, function(st){

    # check there is an underscore
    # if no, return NA
    if(length(which(strsplit(st, '')[[1]] == '_')) == 0) {
      return(NA)
    }

    substr(st, 1,
           which(strsplit(st, '')[[1]] == '_') [length(which(strsplit(st, '')[[1]] == '_'))] - 1) # -1 is to not take the _ with us
  }))
}



# beforeFirstUnderscore ---------------------------------------------------
# take everything before first _
# can be to split well ID and group it belongs to
# e.g. f189_scr_1107_mesh >>> f189

#' Title
#'
#' @param stri
#'
#' @return
#'
#' @examples
beforeFirstUnderscore <- function(stri) {
  as.character(sapply(stri, function(st){
    substr(st, 1,
           which(strsplit(st, '')[[1]] == '_') [1] - 1) # -1 is to not take the _ with us
  }))
}



# afterFirstUnderscore ----------------------------------------------------
# take everything after first _
# can be to split well ID and group it belongs to
# e.g. f189_scr_1107_mesh >>> scr_1107_mesh

afterFirstUnderscore <- function(stri) {
  as.character(sapply(stri, function(st){
    substr(st,
           which(strsplit(st, '')[[1]] == '_') [1] + 1,
           nchar(st))
  }))
}




# strNthSplit -------------------------------------------------------------
# split string(s) and take the nth element of each
# e.g. strNthSplit on day1_sunny_23C and day2_rainy_20C
# with split = '_' and nth = 2
# gives sunny, rainy

#' Title
#'
#' @param stri
#' @param split
#' @param nth
#'
#' @return
#' @export
#'
#' @examples
strNthSplit <- function(stri,
                        split,
                        nth) {

  # confirm we are given string(s)
  stri <- as.character(unlist(stri))

  as.character(sapply(strsplit(stri, split=split),
                      function(s) {
                        s[nth]
                      }))
}



# startsWith --------------------------------------------------------------

#' Title
#'
#' @param stri
#' @param match
#'
#' @return
#'
#' @examples
startsWith <- function(stri,
                       match) {
  as.logical(sapply(stri,
                    function(s) {
                      substr(s, 1, nchar(match)) == match
                    }))
}



# levelUpPath -------------------------------------------------------------

# e.g. upn = 3 on "/Users/francoiskroll/Dropbox/ZFAD/f0subsample/sorl1_f0sub/iter3/plots/iter3LME.csv"
# will give iter3

# idea is to go 'up' (i.e. to parent folder) in a path and return that component

#' Title
#'
#' @param paths
#' @param upn
#' @param slash
#'
#' @return
#'
#' @examples
levelUpPath <- function(paths,
                        upn,
                        slash='/') {

  cpns <- as.character(sapply( paths, function(pth) {
    rev(unlist(strsplit(pth, slash)))[upn]
  } ))
  # cpns for components
  return(cpns)

}



# parentFolder ------------------------------------------------------------

# similar as levelUpPath but gives the whole path of the parent folder, or the 'grandparent' folder, etc.
# upn is for up n-times
# e.g. upn = 3 on "/Users/francoiskroll/Dropbox/ZFAD/f0subsample/sorl1_f0sub/iter3/plots/iter3LME.csv"
# will give /Users/francoiskroll/Dropbox/ZFAD/f0subsample/sorl1_f0sub/

# by default upn=1, i.e. it just goes up one folder

#' Title
#'
#' @param paths
#' @param upn
#' @param slash
#'
#' @return
#'
#' @examples
parentFolder <- function(paths,
                         upn=1,
                         slash='/') {

  # for each path we are given,
  as.character(sapply(paths, function(pth) {

    # split the path into its components
    cpns <- unlist(strsplit(pth, slash))
    # remove the last n components
    cpns <- cpns[1:(length(cpns)-upn)]
    # paste everything back into a path
    return(paste(cpns, collapse=slash))

  }))

}


# gatherFiles -------------------------------------------------------------

# function to gather files from multiple folders recursively
# eg. folder 1 contains file A and B; folder 2 contains file C and D
# >> gathers files A, B, C, D in one folder

# will copy (not move) files to the output folder for safety


### explanations ###

# gatherFiles(parent, output)

# parent = folder which contains many subfolders, and you want all the files in the subfolders to be gathered in one folder
# output = where you want all the files gathered

# ! not tested when some files have the same names, they will probably overwrite each other


### example usage ###

# path to parent folder
# i.e. folder containing the folders to merge

# par <- '~/Dropbox/phd/october2021_MiSeq/oct2021MiSeq_alldata/PayneMiseqOct21-295623328/'

# output folder
# where will drop all the files
# can exist already or not; will create it if does not exist already
# (will throw a Warning if exists already but can be ignored)

# out <- '~/Dropbox/phd/october2021_MiSeq/data/'

# launch gatherFiles():

# gatherFiles(parent = par, output = out)


#### function ###

#' Title
#'
#' @param parent
#' @param output
#'
#' @return
#'
#' @examples
#' @export
gatherFiles <- function(parent, output) {

  allfi <- list.files(parent,
                      recursive=TRUE,
                      full.names=TRUE) # all paths + files to gather

  dir.create(output, showWarnings=FALSE)

  sapply(1:length(allfi),
         function(f) {
           cat('\t \t \t \t >>> Moving file', f, 'out of', length(allfi), '\n')
           file.copy(from=allfi[f], to=paste0(output, afterLastSlash(allfi[f])))
           # invisible just prevents it from returning TRUE each time it moved one file
         })
}
