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
#' @param shorten_lines How much space should be given for smooth transitions between
#' steps? `0` results in a regular stepped line. Default is 1. Smoothing is capped at
#' half the width of the shortest adjacent step.
#' @param n The number of points drawn for each Bezier curve. In other words: How smooth
#' or jagged are the transitions? Default is 100.
#' @param ... additional parameters to be passed to `geom_worm()` and `geom_text()`
#' @param region.spacing How much padding be added to regions on the y axis?
#' @param color How should the worms be colored? If 'initial' (default), color is according
#' to their region of origin. If 'final', color will be according to their final destination.
#' 'id' will give each worm its own color.
#' @param linewidth The width of the worms.
#' @param x.padding Extra space beyond the highest and lowest plotted values on the x axis.
#' @param region.label.width The horizontal width of colored boxes under the region labels.
#' @param x.breaks.by Frequency of breaks on the x axis. Defaults to 10.
#' @param backplate.alpha Transparency of colored boxes underneath the plot. Defaults to 0.3.
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
#'
#'   data(nobel_physicists)
#'   nobel_physicists %>%
#'     filter(name %in% head(unique(name), 10)) %>%
#'     mutate(country = forcats::fct_drop(country)) %>%
#'     wormsplot('year', 'country', 'name', color = 'initial',
#'     linewidth = 5.5, region.label.width = 18, size = 3.6) +
#'     labs(title = "The Lives of Winners of the Nobel Prize in Physics 1901-1907")

#' @rdname wormsplot
#' @export
wormsplot <- function(data, x = 'x', region = 'y', id = 'id',
                      label.worms = TRUE, shorten_lines = 1, n = 50, ...,
                      region.spacing = 2, color = 'initial', linewidth = 10,
                      x.padding = 1, region.label.width = 4, x.breaks.by = 10,
                      backplate.alpha = .3){
  # rename variables
  lookup <- c(x = x, y = region, group = id)
  data <-  rename(data, all_of(lookup))

  # transform y axis
  data <- dodge_in_region(data, region.spacing, color = color) %>% arrange(cumwidth)

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
      pivot_wider(id_cols = c(y, grouplabel, subgroup), names_from = 'b', values_from = 'x') %>%
      mutate(length = end-start,
             midpoint = start + .5*length) %>%
      group_by(grouplabel) %>% filter(length == max(length)) %>% select(y, midpoint, grouplabel)
  }
  # Set default palette
  palette = colorRampPalette(RColorBrewer::brewer.pal(min(8, length(unique(data$y))), "Set1"))(length(unique(data$y)))
  # plot
  p <- ggplot(data) +
    geom_tile(aes(x, breaks, height = heights, fill = labels, width = width),
              alpha = backplate.alpha, data = regions) +
    geom_worm(aes(x, y, group = group, color = color), linewidth = linewidth, n = n, shorten_lines = shorten_lines, ...) +
    geom_tile(aes(min_x - .5*region.label.width, breaks, height = heights, fill = labels), width = region.label.width,
              data = regions) +
    geom_text(aes(min_x - .5*region.label.width, breaks, label = labels), data = regions, ...) +
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
    p <- p + geom_text(aes(midpoint, y, label = grouplabel), data = ids, ...)
  }
  if(color != 'id'){
    p <- p + scale_color_manual(breaks = levels(data$color),
                                values = palette)
  }
  p
}
