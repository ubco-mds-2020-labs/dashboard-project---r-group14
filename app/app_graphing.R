library(ggplot2)
library(plotly)
library(ggpubr)
source("./app/app_wrangling.R")

scatter_plot_dates <- function(data, col = "category", list_ = c()) {
  # mean line data
  mean_data <- mean_line(data)

  # changes between no selection and selection
  if (length(list_) == 0) {
    set_data <- data %>%
      remove_columns()
    set_color <- "#cccccc"
  } else {
    set_data <- call_boardgame_radio(data, col, list_) %>%
      unnest(group) %>%
      remove_columns()
    set_color <- "group"
  }


  # scatter plot
  scatter_plot <- set_data %>%
    ggplot() +
    scale_fill_brewer(palette = "Set2") +
    geom_point(aes(
      x = year_published,
      y = average_rating,
      # fill = eval(parse(text = set_color)),
      color = eval(parse(text = set_color))
    ),
    alpha = 0.4
    ) +
    geom_line(
      data = mean_data,
      aes(
        x = year_published,
        y = total_avg_rating
      ), color = "#d3d3d3"
    ) +
    labs(
      x = "",
      y = "Average Rating",
      # fill = "",
      color = ""
    ) +
    ggtitle("Game Popularity Based on Published Year") +
    theme_minimal()

  scatter_plot <- ggplotly(scatter_plot, tooltip = c("y")) %>% layout(height = 375)

  return(scatter_plot)
}


count_plot_dates <- function(data, col = "category", list_ = c()) {
  if (length(list_) == 0) {
    set_data <- data %>%
      remove_columns()
    set_color <- "#cccccc"
  } else {
    set_data <- call_boardgame_radio(data, col, list_) %>%
      unnest(group) %>%
      remove_columns()
    set_color <- "group"
  }


  # count plot
  count_plot <- set_data %>%
    ggplot() +
    aes(
      x = year_published,
      fill = eval(parse(text = set_color))
    ) +
    geom_bar() +
    labs(
      x = "",
      y = "# of Games Published",
      fill = "",
      title = "Game Count based on Published Year"
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, NA)
    ) +
    theme_minimal()

  count_plot <- ggplotly(count_plot, tooltip = c("y")) %>% layout(height = 375)
  return(count_plot)
}


rank_plot_dates <- function(data, col = "category", year_in = 1990, year_out = 2010, color_) {
  setdata <- call_boardgame_top(data, col, year_in, year_out)
  rank_plot <- setdata %>%
    ggplot() +
    aes(
      x = average,
      y = eval(parse(text = col))
    ) +
    labs(
      x = "Average Rating",
      y = col
    ) +
    coord_cartesian(xlim = c(5, 10)) +
    geom_col(fill = color_) +
    theme_minimal()

  rank_plot <- ggplotly(rank_plot, tooltip = c("average"))
  return(rank_plot)
}


rank_plot_facet <- function(data, year_in, year_out) {
  cat_plot <- rank_plot_dates(data, "category", year_in, year_out, color_ = "#ff7f0e")
  mech_plot <- rank_plot_dates(data, "mechanic", year_in, year_out, color_ = "#17becf")
  pub_plot <- rank_plot_dates(data, "publisher", year_in, year_out, color_ = "#e377c2")
  total_plot <- subplot(cat_plot, mech_plot, pub_plot, nrows = 1, margin = 0.1) %>% layout(height = 250)
  return(total_plot)
}


top_n_plot <- function(data, cat = c(), mech = c(), pub = c(), n = 10) {
  set_data <- call_boardgame_filter(data, cat, mech, pub, n)
  top_plot <- set_data %>%
    ggplot() +
    scale_fill_brewer(palette = "Set3") +
    aes(
      x = name,
      y = average_rating,
      fill = name
    ) +
    geom_col() +
    labs(
      x = "",
      y = "Average Rating",
      fill = "Boardgame Name"
    ) +
    ggtitle("Top 10 Games Based on User Selection") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_line(colour = "#d3d3d3")
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 10)
    )

  return(ggplotly(top_plot))
}
