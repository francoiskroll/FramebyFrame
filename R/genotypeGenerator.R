###################################################
# ~~~ FramebyFrame package ~~~

# genotypeGenerator

# Francois Kroll 2022
# francois@kroll.be
###################################################

# genotypeGenerator used to be a standalone script
# which went up to v8
# accordingly, this counts as v9

# -------------------------------------------------------------------------

# version history:

# v0
# v1: better formatting of the genotype txt file
# v2: says total number of fish, number of empty wells, etc.
# v3: writes README file
# v4: better way to select the 96 wells; it could sometimes take comments before
# v5: supports 'excluded' wells
# v6: agnostic to plate type, tested on 24-well plate
# v7: tests if running on Mac or Windows, so can choose appropriate slash character for paths
# v8: fixed bug; if a genotype name was in all uppercase e.g. HOM, would delete thinking it is a row name like A, B, C. Now looks if cell is actually A, B, C, ..., Z
# v9: genotypeGenerator as function in FramebyFrame
# >> subsequent changes are tracked in GitHub, not here

#' Title
#'
#' @param plateMap
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom dplyr filter

genotypeGenerator <- function(plateMap) {


  #### import ####
  cells <- tidyxl::xlsx_cells(plateMap)


  #### get rid of comments ####
  # detect row that says 'Comments'
  # i.e. xlsx's row (not tibble's row)
  comrow <- as.numeric(cells[which(cells$character=='Comments'), 'row'])

  if(length(comrow)==0)
    stop('\t \t \t \t >>> Error: could not find the row where comments start. Please keep a row that says `Comments` in first column. \n')

  # get rid of any row after comment row (including row that says Comments)
  cells <- cells %>%
    filter(row < comrow)

  frmts <- cells$style_format #formats
  cnts <- cells$character #contents


  #### clean up ####
  # keep only wells

  # in formats, remove all "Normal" or "Standard" cells
  # actual wells should be genotypeX or empty or excluded
  frmts <- frmts[-which(frmts %in% c('Normal', 'Standard'))]

  # in contents,
  # remove all NA
  cnts <- cnts[!is.na(cnts)]
  # remove all single uppercase letters
  cnts <- cnts[! cnts %in% LETTERS]

  # we can now detect number of wells in the plate

  # check above made sense first
  if( length(frmts) != length(cnts) ) stop('\t \t \t \t >>> Error: something wrong when parsing the .xlsx file.
                                         You probably have a genotype Style applied to an empty cell.
                                         Try changing empty cells around the plate map to Cell Styles > Normal \n')

  # how many wells?
  nwells <- length(cnts)
  cat('\t \t \t \t >>> Detected *', nwells, '* -well plate \n')


  ##### prepare list ####

  # how many genotypes are there
  if ('empty' %in% frmts | 'excluded' %in% frmts) {
    genos <- sort(unique(frmts)[-which(unique(frmts)=='empty' | unique(frmts)=='excluded')])
    geno_names <- unique(cnts[!is.na(cnts)])[-which(unique(cnts[!is.na(cnts)])=='empty' | unique(cnts[!is.na(cnts)])=='excluded')]
  } else {
    genos <- sort(unique(frmts))
    geno_names <- unique(cnts[!is.na(cnts)])
  }

  if (length(genos) != length(geno_names)) {
    cat('\t \t \t \t Genotypes:', genos, '\n')
    cat('\t \t \t \t Genotype names:', geno_names, '\n')
    stop('\t \t \t \t >>> Error: Not the same number of genotypes and genotype names.')
  }

  # matches genotype number to genotype name
  genonames_matched <- c()

  for (G in 1:length(genos)) {
    geno <- which(frmts==genos[G])
    genoname <- unique(cnts[geno])
    if (length(genoname) != 1) stop('\t \t \t \t >>> Error: Check the plate map. Probably same Style used for multiple genotypes.
                                  Look for wells: ', paste(genoname, collapse=', '), '\n') # checkpoint
    genonames_matched <- c(genonames_matched, genoname)
  }

  if (setequal(geno_names, genonames_matched) == FALSE) stop('\t \t \t \t >>> Error: Issue with extracting the genotype names. \n') # checkpoint

  # what is the genotype with the most number of fish
  geno_lgths <- c()
  for (G in 1:length(genos)) {
    fishes <- sort(which(frmts==genos[G]))
    geno_lgths <- c(geno_lgths, length(fishes))
  }
  maxlgth <- max(geno_lgths)


  #### start writing README file ####

  eachgeno <- matrix(nrow=length(genos), ncol=1) # N in each genotype
  for (G in 1:length(genos)){
    eachgeno[G,] <- paste('\t genotype', G, '=', genonames_matched[G], ' || N = ', geno_lgths[G])
  }

  datebox <- substr(strsplit(afterLastSlash(plateMap), split='.xlsx')[[1]], 1, 9)

  readme_header <- rbind (paste('Date_Box:', datebox), sep='',
                          paste('Plate map: ', plateMap, sep=''),
                          '',
                          paste('Total N = ', sum(geno_lgths), ' in ', length(genos), ' groups', sep=''),
                          eachgeno,
                          '',
                          paste('Number of empty wells = ', length(which(cnts=='empty')), sep=''),
                          paste('Number of excluded wells = ', length(which(cnts=='excluded')), sep=''),
                          '')

  write(readme_header, paste0(beforeLastSlash(plateMap), datebox, '_README.txt'), sep='\t') # first write the README header


  #### build genotype lists ####
  Genocols <- vector(mode='list', length=length(genos))

  Genotype <- matrix(nrow=maxlgth, ncol=length(genos))
  fishcheck <- c()
  cat('\n')
  for (G in 1:length(genos)) {
    fishes <- sort(which(frmts==genos[G]))
    message(cat(genos[G], '=', genonames_matched[G], '|| n = ', length(fishes), '|| well ', fishes)) # append to the README file
    fishcheck <- c(fishcheck, fishes)
    length(fishes) <- maxlgth
    Genocols[[G]] <- fishes
    Genotype[,G] <- fishes
  }

  fishcheck <- sort(c(fishcheck, which(frmts=='empty' | frmts=='excluded')))
  if (!identical(fishcheck, 1:nwells)) stop('\t \t \t \t >>> Error: Something wrong: not all wells are taken or a well is in multiple genotypes \n') # checkpoint

  cat('\n')
  cat('Total n =', sum(geno_lgths), '\n')
  # say the number of empty wells / excluded wells
  cat('Number of empty wells =', length(which(cnts=='empty')), '\n')
  cat('Number of excluded wells =', length(which(cnts=='excluded')), '\n')

  # Genotype is now a dataframe of `genotypes` columns. Each element = well ID (integer)
  # missing headers

  # need two header rows: `Genotype1` dummy row and genotype names
  # but R does not allow elements in a same column to have different data types
  # solution: first create the genotype file, then append the fish IDs to it

  header <- rbind(rep('Genotype1', length(genos)), genonames_matched)
  rownames(header) <-  NULL


  #### write genotype file ####

  # first write the header
  write.table(header, paste0(beforeLastSlash(plateMap), datebox, 'genotype.txt'),
              sep='\t', na='', row.names=FALSE, col.names=FALSE, quote=FALSE)

  # then append the fish IDs
  write.table(Genotype, paste0(beforeLastSlash(plateMap), datebox, 'genotype.txt'),
              sep='\t', na='', row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)


  ### until 05/10/2023, genotypeGenerator was also creating omitted.txt file
  # which was list of wells labelled as empty or excluded
  # I think was useless, and makes more files generated
  # deleted section 05/10/2023
}
