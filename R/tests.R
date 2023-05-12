testdf <- data.frame(
  x = c(1, 2, 4, 1, 3, 4, 1, 1.5, 2.5, 4),
  y = c(1, 2, 2, 2, 1, 1, 1.5, 1, 1.7, 1.7),
  group = rep(c("you", "me", "him"), times = c(3, 3, 4))
)

generate_worms <- function(seed = 1,
                           n_worms = 10, n_ys = 50, min = 0, max = 10){
  set.seed(seed)
  ys <- runif(n_ys, min, max)
  do.call(rbind, lapply(1:n_worms, function(group){
    rows = sample(2:4, 1)
    data = data.frame(group = group,
                      x = c(sample(ys, 1), rep(NA, rows-1L)),
                      y = c(sample(ys, 1), rep(NA, rows-1L)))
    x_current = data$x[1]
    for (i in 2:(rows-1L)) {
      data$y[i] <- sample(setdiff(ys, data$y[i-1L]), 1)
      x_current <- data$x[i] <- x_current + runif(1, 0, 10)
    }
    data$y[rows] <- data$y[rows-1L]
    data$x[rows] <- data$x[rows-1L] + runif(1, 0, 10)
    data
  }))
}


test1 <- function() {
  testdf %>%
    ggplot(aes(x, y, group = group, color = group)) +
    geom_worm(linewidth = 10, shorten_lines = .2, dodge = .1)
}

test2 <- function() {
  testdf %>%
    prep_worm(shorten_lines = .2, groupwise = FALSE) %>%
    dodge_steps(dodge = .6)
}
