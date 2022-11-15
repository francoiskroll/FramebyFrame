# function gglegacyFingerprint(...) ---------------------------------------

# expects lFgp (legacy fingerprint) as dataframe created by legacyFingerprint(...)

#' Title
#'
#' @param lFgp
#' @param onlyGrp
#' @param colours
#' @param legendOrNo
#' @param ynameOrNo
#' @param ytextOrNo
#' @param xtextOrNo
#' @param nightBgOrNo
#' @param ymin
#' @param ymax
#' @param exportOrNo
#' @param exportPath
#' @param width
#' @param height
#'
#' @return
#' @export
#'
#' @examples
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter
#' @importFrom dplyr mutate

gglegacyFingerprint <- function(lFgp,
                                onlyGrp=NA,
                                colours=NA,
                                legendOrNo=TRUE,
                                ynameOrNo=TRUE,
                                ytextOrNo=TRUE,
                                xtextOrNo=TRUE,
                                nightBgOrNo=TRUE,
                                ymin,
                                ymax,
                                exportOrNo=FALSE,
                                exportPath,
                                width=150,
                                height=100) {


  ### pivot longer ###
  fpl <- lFgp %>%
    pivot_longer(-c(uparam, win, parameter),
                 names_to='grp',
                 values_to='zsco')

  ### keep only groups we are plotting ###
  # if not plotting every group:
  if(!is.na(onlyGrp[1])) {
    fpl <- fpl %>%
      filter(grp %in% onlyGrp)
  }


  ### order parameters ###
  # legacy fingerprints are night1, day1, night2, day2 for each parameter
  # but will be nicer to have 'mini-fingerprints', i.e. day1 parameters / day2 parameters / night1 parameters / night2 parameters

  # an easy solution is to get the levels we want from lFgp
  # (as the uparams are unique in lFgp)
  lFgp <- lFgp[order(lFgp$win),]

  # so we want this order for the uparams:
  fpl$uparam <- factor(fpl$uparam, levels=lFgp$uparam)


  ### how to connect in plot ###
  # we want to connect by grp_win:
  fpl <- fpl %>%
    mutate(grp_win=paste(grp, win, sep='_'), .after='parameter')


  ### colours ###
  # if user did not provide any colours, we use automatic ggplot colours
  if(is.na(colours[1])) {
    colours <- hue_pal()(length(unique(fpl$grp)))
  }


  ### set the axes ###
  # set y axis name
  yname <- expression(paste('deviation from controls (', italic(Z), '-score)'))

  # below: find last 'day' parameter
  # we want the grey frame to start just after (+0.5)
  xmid <- max(which(startsWith(levels(fpl$uparam), 'day'))) + 0.5


  ### plot ###
  ggFgp <- ggplot(fpl, aes(x=uparam, y=zsco, colour=grp, group=grp_win)) +
    geom_hline(yintercept=0, linetype=1, colour='#a7a7a7', linewidth=0.5) +
    geom_point() +
    geom_line() +
    {if(nightBgOrNo) annotate(geom='rect', xmin=xmid, xmax=Inf, ymin=-Inf, ymax=Inf, colour=NA, fill='#1d1d1b', alpha=0.2)} +
    scale_colour_manual(values=colours) +
    theme_minimal() +
    theme(
      panel.grid.minor.y=element_blank(),
      axis.title.x=element_blank(),
      legend.title=element_blank()) +

    {if(!legendOrNo) theme(legend.position='none')} +

    {if(!ynameOrNo) theme(axis.title.y=element_blank())} +
    {if(ynameOrNo) theme(axis.title.y=element_text(size=9, margin=margin(t=0, r=-1.5, b=0, l=0)))} +
    {if(ynameOrNo) ylab(yname)} +

    {if(!ytextOrNo) theme(axis.text.y=element_blank())} +
    {if(ytextOrNo) theme(axis.text.y=element_text(size=7))} +

    {if(xtextOrNo) theme(axis.text.x=element_text(size=7, angle=45, hjust=1))} +
    {if(!xtextOrNo) theme(axis.text.x=element_blank())} +

    coord_cartesian(ylim=c(ymin, ymax))


  ### export the plot ###
  if(exportOrNo) {
    ggsave(exportPath, ggFgp, width=width, height=height, units='mm')
  }

  ### return plot ###
  # this way it will display in RStudio
  return(ggFgp)
}
