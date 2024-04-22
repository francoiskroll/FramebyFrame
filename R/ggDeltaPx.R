
### function to plot a small deltapx plot
#' Title
#'
#' @param ffpath
#' @param well
#' @param zhstart
#' @param zhstop
#' @param frstart
#' @param frstop
#' @param colour
#' @param ymax
#' @param width
#' @param height
#' @param xnsecs
#' @param exportPath
#'
#' @return
#' @export
#'
#' @examples
ggDeltaPx <- function(ffpath,
                      well,
                      zhstart=NA,
                      zhstop=NA,
                      frstart=NA,
                      frstop=NA,
                      colour=NA,
                      ymax=10,
                      xnsecs=10,
                      width=100,
                      height=50,
                      exportPath) {
  ## checks
  # user has to give zhstart & zhstop
  # or frstart & frstop
  if( is.na(frstart) & !is.na(frstop) | is.na(frstop) & !is.na(frstart) )
    stop('\t \t \t \t >>> Please give both frstart & frstop, or leave both as NA.\n')

  if( is.na(zhstart) & !is.na(zhstop) | is.na(zhstart) & !is.na(zhstop) )
    stop('\t \t \t \t >>> Please give both zhstart & zhstop, or leave both as NA.\n')

  ## import frame-by-frame data
  ff <- importRAWs(ffpath=ffpath)

  ## keep only data we want to plot
  # if user gave start/stop as zhrs, we need to find the corresponding frame
  # if user gave start/stop as frame indices, we can directly take those rows

  # if user gave zhrs, find matching (closest) frames
  if(!is.na(zhstart) & !is.na(zhstop)) {
    # first find closest frame to zhstart & zhstop
    frstart <- which.min(abs(ff$zhrs-zhstart))
    frstop <- which.min(abs(ff$zhrs-zhstop))
  }

  # then either way, we just keep data we need to plot
  cols2keep <- c('fullts', 'zhrs', 'exsecs', well)
  fft <- ff[frstart:frstop, ..cols2keep]
  colnames(fft)[ncol(fft)] <- 'px'

  # xaxis: have breaks at multiple of fps so we have breaks at round number of seconds
  fps <- round(averageFramerate(ff$exsecs))
  brks <- fft$exsecs[seq(1, nrow(fft), xnsecs*fps)]

  # ready to plot
  ggfft <- ggplot(fft, aes(x=exsecs, y=px)) +
    geom_line(colour=colour, linewidth=0.8) +
    geom_point(shape=21, fill='white', colour=colour, size=1, stroke=0.4) +
    theme_minimal() +
    theme(
      panel.grid.minor.x=element_blank(),
      panel.grid.minor.y=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_text(size=9),
      axis.text.x=element_text(size=7, angle=45, hjust=1),
      axis.text.y=element_text(size=7),
      legend.position='none'
    ) +
    coord_cartesian(ylim=c(0, ymax)) +
    scale_x_continuous(breaks=brks, labels=round(brks)) +
    xlab('seconds') +
    ylab('\u0394 px')

  print(ggfft)

  ggsave(ggfft, filename=exportPath, width=width, height=height, units='mm', device=cairo_pdf)

}
