library(ggplot2)
library(dplyr)


x <- read.csv("demo-output.csv", header = FALSE, stringsAsFactors = FALSE)
names(x) <- c("event", "time", "first_person", "second_person")

ltt <- function(events) {
    deltas <- rep(0, length(events))
    inc_mask <- grepl(pattern = "infection", x = events)
    deltas[inc_mask] <- 1
    dec_mask <- grepl(pattern = "(removal|sample)", x = events)
    deltas[dec_mask] <- -1
    cumsum(c(1,rev(deltas)))
}

plot_df <- data.frame(time = c(0, rev(x$time)), ltt = ltt(x$event), event = c(NA, rev(x$event)))


ggplot(plot_df, aes(x = time, y = ltt)) +
    geom_step() +
    geom_point(data = filter(plot_df, event == "sample"), colour = "red") +
    scale_y_log10() +
    labs(x = "(Forward) Time", y = "Lineages Through Time\n(logarithmic-scale)")

ggsave("demo-output.png", width = 10.5, height = 7.4, units = "cm")
