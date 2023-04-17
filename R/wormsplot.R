#' Automatically Generate a Plot of Historical Figures as They Move From Region to Region
#'
#' `wormsplot()` is a convenience function that automatically dodges worms along the
#' y axis and scales regions accordingly. It comes with default settings for creating
#' an aesthetic historical visualization. Further customization can by applied to
#' the resulting ggplot object.
#'
#' @param data a dataframe or tibble
#' @param x string. The name of the variable to be plotted on the x axis.
#' @param region string. The name of the categorical variable to be plotted on the y axis.
#' @param id string. The name of the grouping variable.
#' @param label.worms If `TRUE` (default) worms will be labeled with their id
#' @param label.args list. additional parameters to be passed to `geom_text()`
#' @param worm.args list. additional parameters to be passed to `stat_worm()`. Specifying `aes()` will overwrite existing aesthetics.
#' @param backplate.args list. additional parameters to be passed to `geom_tile()` for creating
#' colored boxes underneath the plot.
#' @param region.spacing How much padding be added to regions on the y axis?
#' @param worm.color How should the worms be colored? If 'initial' (default), color is according
#' to their region of origin. If 'final', color will be according to their final destination.
#' 'id' will give each worm its own color.
#' @param x.padding Extra space beyond the highest and lowest plotted values on the x axis.
#' @param region.label.width The horizontal width of colored boxes under the region labels.
#' @param region.label.args list. additional parameters to be passed to `geom_tile()` for creating
#' colored boxes underneath region labels.
#' @param x.breaks.by Frequency of breaks on the x axis. Defaults to 10.
#' @import ggplot2
#' @examples
#'   data <- data.frame(
#'     date = c(0, 30,
#'              1, 10, 25, 30,
#'              1, 9, 14, 30),
#'     place = c('Dagobah', 'Dagobah',
#'               'Hoth', 'Dagobah', 'Bespin', 'Bespin',
#'               'Hoth', 'Asteroids', 'Bespin', 'Bespin'),
#'     person = c('Yoda', 'Yoda',
#'                'Luke', 'Luke', 'Luke', 'Luke',
#'                'Princess Leia', 'Princess Leia', 'Princess Leia', 'Princess Leia')
#'   )
#'
#'   wormsplot(data, 'date', 'place', 'person')

#' @import dplyr
#' @import tidyr
#' @importFrom grDevices colorRamp
#' @importFrom RColorBrewer brewer.pal
#' @rdname wormsplot
#' @export
wormsplot <- function(data, x = 'x', region = 'y', id = 'id', label.worms = TRUE,
                      label.args = list(), worm.color = 'initial', worm.args = list(linewidth = 10),
                      backplate.args = list(alpha = .3), region.spacing = 2, x.padding = 1,
                      region.label.width = 4, region.label.args = list(), x.breaks.by = 10){
  # rename variables
  lookup <- c(x = x, y = region, group = id)
  data <-  rename(data, all_of(lookup))

  # transform y axis
  data <- dodge_in_region(data, region.spacing, color = worm.color) %>% arrange(cumwidth)

  regions <- data.frame(x = mean(range(data$x, na.rm = T)),
                        min_x = min(data$x,na.rm = T) - .5*x.padding,
                        labels = factor(unique(data$ylabel), levels = levels(data$ylabel)),
                        breaks = unique(data$cumwidth) - .5*diff(c(0, unique(data$cumwidth))),
                        heights = diff(c(0, unique(data$cumwidth))),
                        width = max(data$x,na.rm = T)-min(data$x,na.rm = T) + x.padding)

  data <- arrange(data, group, x)

  if(label.worms) {
    # save id labels for later
    ids <- data %>%
      pivot_wider(id_cols = c(y, grouplabel, segment), names_from = 'b', values_from = 'x') %>%
      mutate(length = end-start,
             midpoint = start + .5*length) %>%
      group_by(grouplabel) %>% filter(length == max(length)) %>% select(y, midpoint, grouplabel)
  }
  # Add base aesthetics if not specified manually
  if(any(sapply(worm.args, function(x) class(x) == "uneval")) == FALSE){
    worm.args <- append(list(aes(x, y, group = group, color = color)), worm.args)
  }
  # Set default palette
  palette = colorRampPalette(RColorBrewer::brewer.pal(min(8, length(unique(data$y))), "Set1"))(length(unique(data$y)))
  # plot
  p <- ggplot(data) +
    do.call(geom_tile, append(list(aes(x, breaks, height = heights, fill = labels, width = width), data = regions), backplate.args)) +
    do.call(stat_worm, worm.args) +
    do.call(geom_tile, append(list(aes(min_x - .5*region.label.width, breaks, height = heights, fill = labels), width = region.label.width, data = regions), region.label.args)) +
    do.call(geom_text, append(list(aes(min_x - .5*region.label.width, breaks, label = labels), data = regions), label.args)) +
    geom_tile(aes(first(x) - .5*region.label.width, mean(c(max(data$cumwidth), 0)), height = sum(heights), width = first(width) + region.label.width),
              fill = NA, color = "black", linewidth = .5, data = regions) +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_text(margin = margin(r = -15)),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = .5),
          plot.margin = margin(l = 25, r = 0, t = 10, b = 10)) +
    coord_cartesian(ylim = c(0, max(data$cumwidth,na.rm = T)),
                    xlim = c(min(regions$min_x,na.rm = T) - .65*region.label.width, max(data$x,na.rm = T) + x.padding),
                    clip = 'off') +
    scale_fill_manual(breaks = levels(data$color),
                      values = palette) +
    scale_x_continuous(breaks = seq(ceiling(min(data$x,na.rm = T)/x.breaks.by)*x.breaks.by, floor(max(data$x,na.rm = T)/x.breaks.by)*x.breaks.by, by = x.breaks.by),
                       minor_breaks = NULL) +
    labs(x = "", y = "")

  if(label.worms) {
    p <- p + do.call(geom_text, append(list(aes(midpoint, y, label = grouplabel), data = ids), label.args))
  }
  if(worm.color != 'id'){
    p <- p + scale_color_manual(breaks = levels(data$color),
                                values = palette)
  }
  p
}
