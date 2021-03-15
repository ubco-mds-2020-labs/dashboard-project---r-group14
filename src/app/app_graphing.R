library(ggplot2)
library(plotly)
library(ggpubr)
source("./src/app/app_wrangling.R")

scatter_plot_dates <- function(data, col="category", list_=list()) {
  # changes between no selection and selection
  if (length(list_) == 0) {
    print("jlakdjflkasjdfkljsklfjlk")
    set_data <- data
    set_color <- "grey"
  }else {
    set_data <- call_boardgame_radio(data, col, list_)
    set_color <- set_data$group
  }
  
  # scatter plot
  scatter_plot <- set_data %>%
    ggplot() +
    aes(x = year_published,
        y = average_rating,
        fill = set_color) +
    geom_point()
    # labs(x = "",
    #      y = "Average Rating") +
    # ggtitle("Game Popularity Based on Published year") +
    # geom_line(aes(
    #   x=year_published,
    #   y=mean(average_rating),
    #   fill="dark grey"
    # ))


  # scatter_plot <- ggplotly(scatter_plot, "text") %>% style(hoverinfo = "name")

  return(scatter_plot)
}

count_plot_dates <- function(data, col='category', list_=list()) {
  if (length(list_) == 0) {
    set_data <- data
    set_color <- "green"
  }else {
    set_data <- call_boardgame_radio(data, col, list_)
    set_color <- set_data$group
  }
  # count plot
  count_plot <- set_data %>%
    ggplot() +
    aes(x = year_published,
        fill = set_color) +
    geom_bar() +
    labs(x = "",
         y = "# of Games Published") +
    ggtitle("GameCount based on Published Year")
  
  return(ggplotly(count_plot))
  
}

rank_plot_dates <- function(data, col="category", year_in=1990, year_out=2010, color_="#ff7f0e") {
  setdata <- call_boardgame_top(data, col, year_in, year_out)
  
  rank_plot <- setdata %>%
    ggplot() +
    aes(x=average,
        y=eval(parse(text=col))) +
    geom_col(fill = colour_) 
  
  return(ggplot(rank_plot))
}

rank_plot_facet <- function(data, year_in, year_out) {
  
  cat_plot <- rank_plot_dates(data, "category", year_in, year_out, color_="#ff7f0e" )
  mech_plot <- rank_plot_dates(data, "mechanic", year_in, year_out, color_="#17becf" )
  pub_plot <- rank_plot_dates(data, "publisher", year_in, year_out, color_="#e377c2" )
  
  return(subplot(cat_plot, mech_plot, pub_plot, nrows=1))
}

top_n_plot <- function(data, cat=list(), mech=list(), pub=list(), n=10) {
  set_data <- call_boardgame_filter(data, cat, mech, pub, n)
  
  top_plot <- set_data %>%
    ggplot() +
    aes(x=name,
        y=average_rating,
        fill=name) +
    geom_col() +
    labs(x = "",
         y="Average Rating",
         fill="Name") +
    ggtitle("Top 10 Games Based on User Selection") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank() ) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,10))
  
  return(ggplotly(top_plot))
}