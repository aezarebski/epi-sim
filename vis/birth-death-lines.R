#' Produce a figure showing the whole simulation.
#'
#' This will fail without output if either the input CSV file does not exist or
#' the output PNG file does exist.
#'
#' Usage
#' -----
#'
#'   $ Rscript vis/birth-death-lines.R all-events.csv output-figure.png
#'
library(ggplot2)
library(dplyr)
library(purrr)


#' Predicate for whether an event had a particular id as either the primary or
#' secondary person.
#'
#' @param event the event
#' @param person_type character specifying primary or secondary
#' @param id integer describing the id
#'
event_about <- function(event, person_type, id) {
    if (person_type == "primary") {
        id_string <- event$primary
        ids <- event$primary %>%
            as.character %>%
            strsplit(":") %>%
            map(as.integer) %>%
            unlist()
        id %in% ids
    } else if (person_type == "secondary") {
        id_string <- event$secondary
        if (is.na(id_string)) {
            ids <- c()
        } else {
            ids <- id_string %>%
                as.character
        }
    } else {
        stop("Error because event_about did not recognise person_type: ",
             person_type)
    }
  id %in% ids
}

was_removed <- function(removals, id) {
  some(removals, ~ event_about(.x, "primary", id))
}


insertion_time <- function(insertions, id) {
  if (id > 1) {
    (keep(insertions, ~ event_about(.x, "secondary", id)))[[1]]$time
  } else {
    0
  }
}

removal_time_and_type <- function(removals, id) {
  if (was_removed(removals, id)) {
    tmp <- (keep(removals, ~ event_about(.x, "primary", id)))[[1]]
    list(time = tmp$time, type = tmp$type)
  } else {
    list(time = Inf, type = "remaining")
  }
}

plotting_df <- function(all_events_df) {
  removal_events <- all_events_df %>%
    filter(type != "infection") %>%
    select(primary, time, type) %>%
    transpose()
  insertion_events <- all_events_df %>%
    filter(type == "infection") %>%
    select(secondary, time) %>%
    transpose()

  identities <- insertion_events %>%
    map(~ `$`(.x, "secondary")) %>%
    unlist()
  identities <- c(1, identities)


  bd_line <- function(insertions, removals, id) {
    i_time <- insertion_time(insertions, id)
    r_time_and_type <- removal_time_and_type(removals, id)
    list(id = id, birth_time = i_time, death_time = r_time_and_type$time, type = r_time_and_type$type)
  }

  future::plan(future::multiprocess)
  plot_dfs <- furrr::future_map(identities, ~ bd_line(insertion_events, removal_events, .x))
  return(bind_rows(plot_dfs))
}




main <- function(events_log, figure_png) {
  stopifnot(file.exists(events_log))
  stopifnot(!file.exists(figure_png))


  all_events_df <- read.csv(events_log, header = FALSE, stringsAsFactors = FALSE)
  names(all_events_df) <- c("type", "time", "primary", "secondary")

  plot_df <- plotting_df(all_events_df)

  scheduled_events <- data.frame(time = c(3, 3.5, 4.0), type = c("catastrophe", "disaster", "catastrophe"))

  cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  fig <- ggplot(data = plot_df, mapping = aes(x = id, ymin = birth_time, ymax = death_time, colour = type)) +
    geom_linerange() +
    geom_hline(data = scheduled_events, mapping = aes(yintercept = time, colour = type), linetype = "dashed") +
    labs(x = "Infection number", y = "Time", colour = "Removal\nType") +
    scale_color_manual(values = cb_palette) +
    coord_flip()

  ggsave(figure_png,
    plot = fig,
    width = 14.8,
    height = 10.5,
    units = "cm"
  )
}



if (!interactive()) {
  ## Rscript vis/birth-death-lines.R demo-output-all-events.csv demo-figure.png
  args <- commandArgs(trailingOnly = TRUE)
  main(args[1], args[2])
}
