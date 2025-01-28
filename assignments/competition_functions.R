# Functions used for plotting and scoring Assignment 1 results

plotass1 <- function(data, F, L, U, Actual = NULL, xlab = NULL) {
  Fquo <- rlang::as_label(enquo(F))
  p <- ass1 %>%
    arrange({{ F }}) %>%
    mutate(
      Name = factor(Name, ordered = TRUE, levels = data$Name[order(data[[Fquo]])])
    ) %>%
    filter(!is.na({{ F }})) %>%
    ggplot(aes(y = Name)) +
    geom_linerange(aes(xmin = {{ L }}, xmax = {{ U }})) +
    geom_point(aes(x = {{ F }}), col = "blue", size = .5) +
    scale_x_continuous(sec.axis = dup_axis())
  if (!is.null(Actual)) {
    p <- p +
      geom_vline(xintercept = Actual, col = "red")
    xlab <- paste0(xlab, ". Actual = ", Actual)
  }
  if (!is.null(xlab)) {
    p <- p + xlab(xlab)
  }
  xlim <- data %>%
    summarise(
      lower = quantile(c({{F}},{{L}}), 0.01, na.rm=TRUE),
      upper = quantile(c({{F}},{{U}}), 0.99, na.rm=TRUE)
    ) %>%
    as.numeric()
  p <- p + coord_cartesian(xlim=xlim)

  return(p)
}


### Function to save eps or pdf figures

savefig <- function(filename, height = 10, width = (1 + sqrt(5)) / 2 * height,
                    type = c("pdf", "eps", "jpg", "png"), pointsize = 10, family = "Helvetica",
                    sublines = 0, toplines = 0, leftlines = 0, res = 300, ...) {
  type <- match.arg(type)
  filename <- paste(filename, ".", type, sep = "")
  if (type == "eps") {
    postscript(
      file = filename, horizontal = FALSE,
      width = width / 2.54, height = height / 2.54, pointsize = pointsize,
      family = family, onefile = TRUE, print.it = FALSE
    )
  }
  else if (type == "pdf") {
    pdf(
      file = filename, width = width / 2.54, height = height / 2.54, pointsize = pointsize,
      family = family, onefile = TRUE
    )
  }
  else if (type == "jpg") {
    ragg::agg_jpeg(filename = filename, width = width, height = height, res = res, quality = 100, units = "cm") # , pointsize=pointsize*50)
  }
  else if (type == "png") {
    ragg::agg_png(filename = filename, width = width, height = height, res = res, units = "cm") # , pointsize=pointsize*50)
  }
  else {
    stop("Unknown file type")
  }
  par(mgp = c(2.2, 0.45, 0), tcl = -0.4, mar = c(
    3.2 + sublines + 0.25 * (sublines > 0),
    3.5 + leftlines, 1 + toplines, 1
  ) + 0.1)
  par(pch = 1)
  invisible()
}

savepng <- function(...) {
  savefig(..., type = "png")
}

# Scoring function
score <- function(question, actual, forecast, lower, upper) {
  if(is.null(actual)) {
    return(NULL)
  } else {
    # Correct reversal of upper and lower bounds
    switch <- lower > upper
    switch[is.na(switch)] <- FALSE
    tmp <- upper
    upper[switch] <- lower[switch]
    lower[switch] <- tmp[switch]
    # Point forecast score
    rank1 <- rank(abs(actual - forecast))
    # Interval forecast score
    interval_score <- (upper - lower) +
      10 * pmax(0, lower - actual) + 10 * pmax(0, actual - upper)
    rank2 <- rank(interval_score)
    # Return results
    out <- as_tibble(cbind(point = rank1, interval = rank2))
    colnames(out) <- paste0("Q", question, "_", colnames(out))
    return(out)
  }
}
