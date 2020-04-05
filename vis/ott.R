#' Produce a figure showing the observations through time plot which consists of
#' the occurrent and disaster events.
#'
#' This will fail without output if either the input CSV file does not exist or
#' the output PNG file does exist.
#'
#' Usage
#' -----
#'
#'   $ Rscript vis/ott.R demo-output-observed-events.csv demo-output-ott.png
#'
library(ggplot2)

#' Return the number of lineages that where removed in a scheduled event.
#'
#' @param people1 character vector describing the primary people in the event.
#'
size_of_scheduled_event <- function(people) {
  sapply(strsplit(people, split = ":"), length)
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
              subset = event == "occurrence" | event == "disaster"
              )
  occ_df<- subset(x, subset = event == "occurrence", select = c(time,first_person))
  dist_df <- subset(x, subset = event == "disaster", select = c(time,first_person))
  dist_df$size <- size_of_scheduled_event(dist_df$first_person)
  event_times <- data.frame(time = c(occ_df$time, rep(dist_df$time, dist_df$size)))
  fig <- ggplot(event_times, aes(x = time)) + geom_histogram()

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
