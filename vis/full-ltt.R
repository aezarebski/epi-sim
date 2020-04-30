#' Produce a figure showing the whole simulation as prevalence through time.
#'
#' This will fail without output if either the input CSV file does not exist or
#' the output PNG file does exist.
#'
#' Usage
#' -----
#'
#'   $ Rscript vis/full-ltt.R all-events.csv output-figure.png
#'
library(ggplot2)
library(dplyr)
library(purrr)



plotting_df <- function(all_events_df) {

    removal_events <- all_events_df %>%
        filter(type != "infection") %>%
        select(time, type) %>%
        transpose
    insertion_events <- all_events_df %>%
        filter(type == "infection") %>%
        select(time, type) %>%
        transpose

    all_events <- c(removal_events, insertion_events)
    event_order <- all_events %>% map(~ .x$time) %>% unlist %>% sort.int(index.return = TRUE) %>% `$`("ix")
    sorted_events <- all_events[event_order]

    event_times <- sorted_events %>% map(~ .x$time) %>% unlist

    num_events <- length(event_times)
    prevalence <- numeric(num_events)
    curr_prev <- 1
    for(ix in 1:num_events) {
        curr_event <- sorted_events[[ix]]$type
        if (curr_event == "infection") {
            curr_prev <- curr_prev + 1
        } else if (curr_event == "removal") {
            curr_prev <- curr_prev - 1
        } else {
            stop(sprintf("Cannot recognise event: %s", curr_event))
        }
        prevalence[ix] <- curr_prev
    }

    return(data.frame(time = event_times,
                      count = prevalence))
}




main <- function(events_log, figure_png) {
    stopifnot(file.exists(events_log))
    stopifnot(!file.exists(figure_png))


    all_events_df <- read.csv(events_log, header = FALSE, stringsAsFactors = FALSE)
    names(all_events_df) <- c("type", "time", "primary", "secondary")

    plot_df <- plotting_df(all_events_df)


    fig <- ggplot(data = plot_df, mapping = aes(x = time, y = count)) +
        geom_line() +
        labs(x = "Time", y = "Prevalence")

    ggsave(figure_png,
           plot = fig,
           width = 14.8,
           height = 10.5,
           units = "cm"
           )

}



if (!interactive()) {
    ## Rscript vis/full-ltt.R demo-output-all-events.csv demo-figure.png
    args <- commandArgs(trailingOnly = TRUE)
    main(args[1], args[2])
}
