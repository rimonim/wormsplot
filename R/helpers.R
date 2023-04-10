# Helper Functions -------------------------------------------------------------

logistic <- function(x){1 / (1 + exp(-x))}

lerp <- function(a, b, t) {
  a + (b - a)*t
}

cubic_connector <- function(x1, y1, x4, y4, n = 100) {
  x2 <- x3 <- lerp(x1, x4, .5)
  y2 <- y1
  y3 <- y4

  outx <- NULL
  outy <- NULL
  for (t in seq(0, 1, length.out = n)) {
    p0x <- lerp(x1, x2, t)
    p0y <- lerp(y1, y2, t)
    p1x <- lerp(x2, x3, t)
    p1y <- lerp(y2, y3, t)
    p2x <- lerp(x3, x4, t)
    p2y <- lerp(y3, y4, t)

    q0x <- lerp(p0x, p1x, t)
    q0y <- lerp(p0y, p1y, t)
    q1x <- lerp(p1x, p2x, t)
    q1y <- lerp(p1y, p2y, t)

    outx <- append(outx, lerp(q0x, q1x, t))
    outy <- append(outy, lerp(q0y, q1y, t))
  }
  data.frame(x = outx, y = outy)
}

prep_worm <- function(data, shorten_lines = 1, groupwise = TRUE) {
  if(nrow(data)<2){rlang::abort("data must have at least two points")}
  cols_to_keep <- setdiff(names(data), c("x", "y"))
  if(groupwise == TRUE) {
    data <- rbind(data[1,], data)
    newrows <- lapply(2:nrow(data), function(i) {
      if (data$y[i] != data$y[i-1L]) {
        shortening <- min(abs(data$x[i] - data$x[i-1L])/2, abs(data$x[i+1L] - data$x[i])/2,
                          shorten_lines*logistic(abs(data$y[i] - data$y[i-1L])))

        rows <- data.frame(x = c(data$x[i] - shortening, data$x[i] + shortening),
                           y = c(data$y[i-1L], data$y[i]))
        cbind(rows, unclass(data[(i-1L):i, cols_to_keep]))
      }else{
        rows <- data[i, c('x', 'y')]
        cbind(rows, unclass(data[i, cols_to_keep]))
      }
    })
  }else{
    newrows <- NULL
    for (g in unique(data$group)) {
      d <- filter(data, group == g)
      d <- rbind(d[1,], d)
      newrows <- append(newrows, lapply(2:nrow(d), function(i) {
        if (d$y[i] != d$y[i-1L]) {
          shortening <- min(abs(d$x[i] - d$x[i-1L])/2, abs(d$x[i+1L] - d$x[i])/2,
                            shorten_lines*logistic(abs(d$y[i] - d$y[i-1L])))

          rows <- data.frame(x = c(d$x[i] - shortening, d$x[i] + shortening),
                             y = c(d$y[i-1L], d$y[i]))
          cbind(rows, unclass(d[(i-1L):i, cols_to_keep]))
        }else{
          rows <- d[i, c('x', 'y')]
          cbind(rows, unclass(d[i, cols_to_keep]))
        }
      }))
    }
  }
  do.call(rbind, newrows)
}

dodge_in_region <- function(data, region.spacing = 1, color = 'initial') {
  if (region.spacing < 1) {
    rlang::abort("`region.spacing` must be at least 1")
  }
  data <- data %>% mutate(grouplabel = group,
                          group = as.integer(as.factor(group)),
                          ylabel = factor(y),
                          y = as.integer(ylabel)) %>% group_by(group)
  # add color
  if (color == 'id'){
    data <- data %>% mutate(color = grouplabel) %>% ungroup()
  }else if (color == 'initial'){
    data <- data %>% mutate(color = factor(first(ylabel), levels = levels(ylabel))) %>% ungroup()
  }else{
    data <- data %>% mutate(color = factor(last(ylabel), levels = levels(ylabel))) %>% ungroup()
  }

  # count overlaps
  data <- data %>%
    prep_worm(groupwise = FALSE) %>%
    arrange(group) %>% mutate(subgroup = rep(1:n(), each = 2, length.out = n()),
                              b = rep(c('start', 'end'), length.out = n())) %>%
    arrange(y, x)
  overlaps <- 0L
  data$overlaps <- NA
  for (i in 1:nrow(data)) {
    if(data$b[i] == 'start'){
      overlaps <- overlaps + 1L
      data$overlaps[i] <- max(1L, overlaps)
    }else{
      data$overlaps[i] <- max(1L, overlaps)
      overlaps <- overlaps - 1L
    }
  }
  # max overlaps for each group becomes its width
  data <- data %>% group_by(y) %>% mutate(width = max(overlaps))
  data <- data %>%
    left_join(data %>%
                summarise(width = max(width)) %>%
                mutate(cumwidth = cumsum(width) + 1:nrow(.)*region.spacing),
              by = c('y', 'width')) %>%
    mutate(suby = rep(as.integer(as.factor(rle(group)$values)), times = rle(group)$lengths))
  data <- data %>% mutate(y = cumwidth - suby) %>%
    arrange(group, x)
  data
}
