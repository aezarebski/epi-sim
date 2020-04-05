#' Produce a figure showing the lineages-through-time plot for the
#' reconstructed tree.
#'
#' This will fail without output if either the input CSV file does not exist or
#' the output PNG file does exist.
#'
#' Usage
#' -----
#'
#'   $ Rscript vis/ltt.R observed-events.csv output-figure.png
#'
library(ggplot2)

#' Return the number of lineages that where removed in each catastrophe.
#'
#' @param people1 character vector describing the primary people in the event.
#'
size_of_catast <- function(people) {
  sapply(strsplit(people, split = ":"), length)
}

#' Return a data frame describing the LTT.
#'
#' @param events character vector describing the events in the reconstructed
#'   tree.
#' @param times numeric vector describing the times of the events in the
#'   reconstructed tree.
#' @param people1 character vector describing the primary people in the event.
#'
phylo_ltt <- function(events, times, people1) {
  deltas <- rep(0, length(events))
  inc_mask <- grepl(pattern = "infection", x = events)
  deltas[inc_mask] <- 1
  rem_mask <- grepl(pattern = "sample", x = events)
  deltas[rem_mask] <- -1
  cat_mask <- grepl(pattern = "catastrophe", x = events)
  deltas[cat_mask] <- -size_of_catast(people1[cat_mask])
  data.frame(time = c(0, times), ltt = cumsum(c(1, deltas)), event = c("origin", events))
}

#' Return a figure showing the LTT plot.
#'
#' @param phylo_ltt_df data.frame like the one produced by \code{phylo_ltt}
#'
phylo_ltt_plot <- function(phylo_ltt_df) {
  ggplot(phylo_ltt_df, aes(x = time, y = ltt)) +
    geom_step(
      size = 1,
      colour = "grey3"
    ) +
    geom_point(
      mapping = aes(colour = event),
      size = 5
    ) +
    labs(
      x = "Time",
      y = "LTT",
      title = "Lineages-Through-Time Plot",
      colour = "Event Type"
    ) +
    theme_bw()
}


main <- function(observed_events_csv, figure_png) {
  stopifnot(file.exists(observed_events_csv))
  stopifnot(!file.exists(figure_png))


  x <- read.csv(observed_events_csv,
    header = FALSE,
    stringsAsFactors = FALSE
  )
  names(x) <- c(
    "event",
    "time",
    "first_person",
    "second_person"
  )
  x <- subset(x,
              subset = event != "occurrence" & event != "disaster"
              )

  fig <- phylo_ltt_plot(phylo_ltt(x$event, x$time, x$first_person))

  ggsave(figure_png,
    plot = fig,
    width = 14.8,
    height = 10.5,
    units = "cm"
  )
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  main(args[1], args[2])
}
