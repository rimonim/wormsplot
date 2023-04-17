testdf <- data.frame(
  x = c(1, 2, 4, 1, 3, 4, 1, 1.5, 2.5, 4),
  y = c(1, 2, 2, 2, 1, 1, 1.5, 1, 1.7, 1.7),
  group = rep(c("you", "me", "him"), times = c(3, 3, 4))
)

test1 <- function(testdf) {
  testdf %>%
    ggplot(aes(x, y, group = group, color = group)) +
    geom_worm(linewidth = 10, shorten_lines = .2, dodge = 0.5)
}

test2 <- function(testdf) {
  testdf %>%
    prep_worm(shorten_lines = .2, groupwise = FALSE) %>%
    dodge_steps(dodge = .6)
}
