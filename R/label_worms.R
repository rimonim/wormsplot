#' Labeling Worms
#'
#' It can be difficult to properly place labels for individual worms, especially with
#' dodging along the y axis. `label_worms()` transforms data in the appropriate manner.
#' Make sure all parameters match the worms being plotted!
#'
#' @param data A data frame in standard worm format (see `geom_worm`).
#' @param x string. The name of the variable to be plotted on the x axis.
#' @param y string. The name of the variable to be plotted on the y axis.
#' @param id string. The name of the grouping variable, to be formatted as a label.
#' @param shorten_lines How much space should be given for smooth transitions between
#' steps? `0` results in a regular stepped line. Default is 1. Smoothing is capped at
#' half the width of the shortest adjacent step.
#' @param dodge Worm steps less than this distance apart will be grouped and dodged on
#' the y axis. 0 results in no dodging (default). Dodging is implemented here as part of
#' the stat (rather than as a position function) so that Bezier curves can be drawn afterward.
#' @import ggplot2
#'
#' @details
#' Labels will be placed in the center of the widest section of each worm.
#'
#' @section Value:
#' A data frame with columns `x`, `y`, `group`, and `width` (i.e. the width of the chosen segment)
#'
#' @examples
#'    # one data point for each new step, plus one extra for the end of the worm
#'    data <- data.frame(
#'       x = c(1, 2, 4, 1, 3, 4, 1, 1.5, 2.5, 4),
#'       y = c(1, 2, 2, 2, 1, 1, 1.5, 1, 1.7, 1.7),
#'       group = rep(c("you", "me", "him"), times = c(3, 3, 4))
#'    )
#'
#'    worm_labels <- label_worms(data, label = 'group', shorten_lines = .2, dodge = 0.1)
#'
#'    ggplot(data, aes(x, y)) +
#'     geom_worm(aes(group = group, color = group), linewidth = 10, shorten_lines = .2, dodge = 0.1) +
#'     geom_text(aes(label = group), data = worm_labels)

#' @rdname label_worms
#' @export
label_worms <- function(data, x = "x", y = "y", id = "group",
                        shorten_lines = 1, dodge = 0L){
  # rename x and y
  data <- rename(data, x = eval(x), y = eval(y), group = eval(id))

  # prep worm
  data <- prep_worm(data, shorten_lines, groupwise = FALSE)

  # dodge if needed
  if (dodge != 0L) {
    data <- dodge_steps(data, shorten_lines, dodge)
  }else{
    data <- arrange(mutate(arrange(data, group),
                           segment = rep(1:n(), each = 2, length.out = n()),
                           b = rep(c('start', 'end'), length.out = n())),
                    group, segment, x)
  }
  data %>%
    pivot_wider(id_cols = c("y", "group", "segment"), names_from = 'b', values_from = 'x') %>%
    mutate(width = end-start,
           x = start + .5*width) %>%
    group_by(group) %>% filter(width == max(width)) %>% ungroup() %>%
    select(x, y, group, width)
}




