library(ggplot)
library(plotly)
library(dplyr)
source("app_wrangling.R")

scatter_plot_dates <- function(data, col="category", list_=list()) {
  # changes between no selection and selection
  if (length(list_) == 0) {
    set_data <- data
    set_color <- "grey"
  }else {
    set_data <- call_boardgame_radio(data, col, list_)
    set_color <- group
  }
  
  # scatter plot
  scatter_plot <- data %>%
    ggplot() +
    aes(x = year_published,
        y = average_rating,
        fill = set_color) +
    geom_point() +
    labs(x = "",
         y = "Average Rating")
  scatter_plot <- ggplotly(scatter_plot, "text") %>% style(hoverinfo = "name")
  return(data)
}
