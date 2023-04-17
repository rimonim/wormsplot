#' Worms
#'
#' The worm geom is a stepped line with smooth, aesthetic transitions between steps.
#'
#' @inheritParams ggplot2::geom_path
#' @param shorten_lines How much space should be given for smooth transitions between
#' steps? `0` results in a regular stepped line. Default is 1. Smoothing is capped at
#' half the width of the shortest adjacent step.
#' @param n The number of points drawn for each Bezier curve. In other words: How smooth
#' or jagged are the transitions? Default is 100.
#' @param dodge Worm steps less than this distance apart will be grouped and dodged on
#' the y axis. 0 results in no dodging (default). Dodging is implemented here as part of
#' the stat (rather than as a position function) so that Bezier curves can be drawn afterward.
#' @import ggplot2
#'
#' @details
#' Data must be formatted with one data point for each new step, plus one extra for the
#' end of each worm. Worms are distinguished by the group aesthetic.
#'
#' @section Aesthetics:
#' geom_worm understands the following aesthetics (required aesthetics are in bold):
#'
#' - **x**
#' - **y**
#' - **group**
#' - color
#' - linewidth
#' - linetype
#' - alpha
#' - lineend
#'
#' @examples
#'    # one data point for each new step, plus one extra for the end of the worm
#'    data <- data.frame(
#'       x = c(1, 2, 4, 1, 3, 4, 1, 1.5, 2.5, 4),
#'       y = c(1, 2, 2, 2, 1, 1, 1.5, 1, 1.7, 1.7),
#'       group = rep(c("you", "me", "him"), times = c(3, 3, 4))
#'    )
#'
#'    ggplot(data, aes(x, y, group = group, color = group)) +
#'     geom_worm(linewidth = 10, shorten_lines = .2, dodge = 0.1)

#' @rdname geom_worm
StatWorm <- ggproto("StatWorm", Stat,
                    setup_data = function(data, params){
                      data <- prep_worm(data, params$shorten_lines, groupwise = FALSE)
                      if(params$dodge != 0){
                        dodge_steps(data, params$shorten_lines, params$dodge)
                      }else{
                        data
                      }
                    },
                    compute_group = function(data, scales, dodge = 0, shorten_lines = 1, n = 100) {
                      cols_to_keep <- setdiff(names(data), c("x", "y"))
                      data <- rbind(data[1,], data)
                      newrows <- lapply(2:nrow(data), function(i) {
                        if (data$y[i] != data$y[i-1L]) {
                          rows <- cubic_connector(data$x[i-1L], data$y[i-1L], data$x[i], data$y[i], n)
                        }else{
                          rows <- data[i, c('x', 'y')]
                        }
                        cbind(rows, unclass(data[i, cols_to_keep]))
                      })
                      do.call(rbind, newrows)
                    },
                    required_aes = c('x', 'y', 'group')
)

#' @rdname geom_worm
#' @export
stat_worm <- function(mapping = NULL, data = NULL, geom = "path",
                      position = 'identity',
                      shorten_lines = 1, n = 100, dodge = 0,
                      arrow = NULL, lineend = 'butt', linejoin = "round",
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = StatWorm, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(shorten_lines = shorten_lines, n = n, dodge = dodge,
                  arrow = arrow, lineend = lineend, linejoin = linejoin,
                  na.rm = na.rm, ...)
  )
}

#' @rdname geom_worm
#' @export
geom_worm <- function(mapping = NULL, data = NULL, stat = 'worm',
                      position = 'identity',
                      shorten_lines = 1, n = 100, dodge = 0,
                      arrow = NULL, lineend = 'butt', linejoin = "round",
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPath,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(shorten_lines = shorten_lines, n = n, dodge = dodge,
                  arrow = arrow, lineend = lineend, linejoin = linejoin,
                  na.rm = na.rm, ...)
  )
}
