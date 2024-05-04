#' Default png theme
#' @import ggplot2
#' @export

theme_pngs <- function() {
  theme_bw() %+replace%
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12, hjust = 1, vjust = 0.5),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14, angle = 90),
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 12),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 12, face = "bold"),
          strip.background = element_blank())
}