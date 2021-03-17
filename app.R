# load packages
# Installs dash bootstrap
library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(dashTable)
source("./app/app_wrangling.R")
source("./app/app_graphing.R")

# * WRANGLING - load board game data here
boardgame_data <- call_boardgame_data()

# * WRANGLING - get dictionary for dropdowns here
col_key_list <- c("category", "mechanic", "publisher")
col_dict <- vector(mode = "list", length = 3)
names(col_dict) <- col_key_list

# * WRANGLING - sets up column names for second tab
col_ids <- c(
  "name",
  "min_players",
  "max_players",
  "min_playtime",
  "max_playtime",
  "year_published",
  "category",
  "mechanic",
  "designer",
  "publisher",
  "average_rating",
  "users_rated"
)
col_names <- c(
  "Name",
  "Min Players",
  "Max Players",
  "Min Playtime",
  "Max Playtime",
  "Year Published",
  "Category",
  "Mechanic",
  "Designer",
  "Publisher",
  "Average game rating",
  "Users Rated"
)
name_id_join <- function(x, y) {
  return(list(name = x, id = y))
}
col_named_list <- map2(.x = col_names, .y = col_ids, .f = name_id_join)

# * WRANGLING - sets up years for the slider
years <- seq(1950, 2015, 5) %>% map(toString)
names(years) <- years

# creates a dictionary of group (eg mechanic) to all the elements in that group
for (idx in 1:3) {
  col_dict[[idx]] <- subset_data(boardgame_data, col_key_list[idx])[[1]]
}

# title for entire dashboard
title <- function() {
  return(htmlDiv(list(htmlH1("Board Game Trends Dashboard"))))
}

# description card tab 1
description_card <- function() {
  return(htmlDiv(
    id = "description-card",
    list(
      htmlH5("Welcome to our Board Games Dashboard"),
      htmlDiv(
        id = "intro",
        list("Explore board game trends over time based on category, mechanics \
                    and publisher selection below. Also visualize the top categories,\
                    mechanics and publishers by year using our interactive features.")
      )
    )
  ))
}

# control card for tab 1
generate_control_card <- function() {
  return(htmlDiv(
    id = "control-card",
    list(
      htmlLabel("Select what you want to view:"),
      htmlBr(),
      htmlBr(),
      dccRadioItems(
        id = "radio-selection",
        options = list(
          list(label = "Categories", value = "category"),
          list(label = "Mechanics", value = "mechanic"),
          list(label = "Publishers", value = "publisher")
        ),
        value = "mechanic", labelStyle = list("display" = "block")
      ),
      htmlBr(),
      htmlLabel("Select elements to view:"),
      htmlBr(),
      htmlBr(),
      dccDropdown(
        id = "radio-dependent",
        options = list(),
        multi = TRUE,
        value = list(),
        # labelStyle=list("display"="block")
      )
    )
  ))
}



generate_control_card_tab2 <- function() {
  return(htmlDiv(
    id = "control-card2",
    list(
      htmlP("Please select any combination of categories, mechanics, publishers."),
      htmlBr(),
      htmlP("Please select categories:"),
      dccDropdown(
        id = "category-widget",
        options = (
          col_dict[["category"]] %>% map(function(x) {
            return(list(label = x, value = x))
          })
        ),
        multi = TRUE,
        value = list(),
      ),
      htmlBr(),
      htmlP("Please select mechanics:"),
      dccDropdown(
        id = "mechanic-widget",
        options = (
          col_dict[["mechanic"]] %>% map(function(x) {
            return(list(label = x, value = x))
          })
        ),
        multi = TRUE,
        value = list(),
      ),
      htmlBr(), htmlP("Please select publishers:"),
      dccDropdown(
        id = "publisher-widget",
        options = (
          col_dict[["publisher"]] %>% map(function(x) {
            return(list(label = x, value = x))
          })
        ),
        multi = TRUE,
        value = list(),
      ),
      htmlBr(),
      htmlBr()
    )
  ))
}


# lower description for tab 1
lower_description <- function() {
  return(htmlDiv(
    list(
      htmlH4("Top 5 Categories, Mechanics and Publishers by Rating"),
      htmlP("Two sets of bar charts with year range sliders are provided \
to allow comparison for two different periods.", ),
      htmlBr(),
      htmlP("Drag the year sliders below to select your year ranges and \
compare the top 5 categories, mechanics and publishers \
between time periods.")
    )
  ))
}

# data set description for tab 1
data_set_description <- function() {
  return(htmlDiv(
    list(
      htmlH4("Description of Dataset"),
      htmlP(" This dataset comes from the Board Game Geek website and \
includes boardgames with descriptions, general game \
details, publisher, and user ratings for 10,000 boardgames\
published between 1950 and 2021.")
    )
  ))
}


# card 1 containing the description and control card for tab 1
first_card <- dbcCard(
  dbcCardBody(
    htmlDiv(
      id = "left-column",
      className = "four columns", list(
        description_card(), htmlBr(),
        generate_control_card(),
        htmlBr(),
        htmlBr(),
        htmlBr(),
        data_set_description(),
        htmlBr(),
        htmlBr()
      )
    )
  )
)

second_card <- dbcCard(
  dbcCardBody(htmlDiv(
    list(
      htmlH4("Board Game Ratings and Counts from 1950 to 2016"),
      htmlP(
        "Select either categories, mechanics or publishers.\
      Then select different elements to view on the\
      following two figures."
      ),
      dccGraph(id = "scatter"),
      dccGraph(id = "counts")
    )
  ))
)


# card 3 containing the lower description and collapsible data set description for tab 1
third_card <- dbcCard(
  dbcCardBody(
    dbcCol(id = "bottom left row", className = "four columns", list(lower_description()))
  )
)

# card 4 containing the top slider and first faceted bar chart
fourth_card <- dbcCard(
  dbcCardBody(list(htmlDiv(list(
    htmlDiv(id = "output-container-range-slider"),
    dccRangeSlider(
      id = "non-linear-range-slider", min = 1950, max = 2016, step = 1,
      marks = years, value = list(1990, 2010)
    ), dccGraph(id = "facet_1")
  ))))
)


# card 5 containing the top slider and second faceted bar chart
fifth_card <- dbcCard(dbcCardBody(list(
  htmlDiv(
    list(
      htmlDiv(id = "output-container-range-slider2"),
      dccRangeSlider(
        id = "non-linear-range-slider2", min = 1950, max = 2016, step = 1,
        marks = years, value = list(1990, 2010)
      )
    )
  ),
  dccGraph(id = "facet_2")
)))

# card 6 containing the control card for tab 2
sixth_card <- dbcCard(dbcCardBody(list(generate_control_card_tab2())))

seventh_card <- dbcCard(
  dbcCardBody(
    list(dccGraph(id = "top_n_games"))
  )
)

# card 8 containing the data table for the top n games for tab 2
eight_card <- dbcCard(
  dbcCardBody(
    list(
      htmlH5("Top 10 Games Facts Table:"),
      dashDataTable(
        id = "top_n_games_datatable",
        columns = col_named_list,
        style_table = list("overflowY" = "scroll", "overflowX" = "scroll"),
        sort_action = "native",

        style_data = list(
          whiteSpace = "normal"
        ), style_cell = list(maxWidth = "260px")
      )
    )
  )
)


# tab styling features for layout
tabs_styles <- list("height" = "44px")
tab_style <- list(
  "borderBottom" = "1px solid #d6d6d6",
  "padding" = "6px",
  "fontWeight" = "bold",
  "display" = "All"
)

tab_selected_style <- list(
  "borderTop" = "1px solid #d6d6d6",
  "borderBottom" = "1px solid #d6d6d6",
  "backgroundColor" = "#119DFF",
  "color" = "white",
  "padding" = "6px"
)

#  set up app stylesheet and server
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

# layout made up of cards

app$layout(htmlDiv(list(
  dbcContainer
  # add title
  (htmlDiv(
      id = "title_top", className = "title on top",
      list(title())
    )
  ),
  # make tabs
  htmlDiv(dccTabs(id = "tabs_in_app", value = "tab-1", list(
    # tab 1
    dccTab(
      label = "Game Dynamics Over Time", value = "tab-1",
      children = list(dbcRow(list(
        dbcCol(first_card, width = 3),
        dbcCol(second_card, width = 9)
      )), dbcRow(list(
        dbcCol(third_card, width = 3),
        dbcCol(list(fourth_card, fifth_card), width = 9)
      ))),
      style = tab_style, selected_style = tab_selected_style
    ),
    # tab 2
    dccTab(
      label = "Top Games", value = "tab-2",
      children = list(dbcRow(list(
        dbcCol(sixth_card, width = 3),
        dbcCol(list(seventh_card, eight_card), width = 9)
      ))),
      style = tab_style,
      selected_style = tab_selected_style
    )
  )))
)))


# app callbacks

# radio button selection options to populate drop down
# this will return something like [{"label": c, "value": c} for c in col_dict[col]]
app$callback(
  list(output("radio-dependent", "options")),
  list(input("radio-selection", "value")),
  function(chosen_selection) {
    outlist <- list(lapply(col_dict[[chosen_selection]], function(x) {
      return(list(label = x, value = x))
    }))
    return(outlist)
  }
)


# scatter plot tab 1
app$callback(
  output = list(id = "scatter", property = "figure"),
  list(
    input("radio-selection", "value"),
    input("radio-dependent", "value")
  ),
  function(column, list_) {
    p <- scatter_plot_dates(boardgame_data, column, unlist(list_))
    return(p)
  }
)

# stacked histogram of counts annual published counts
app$callback(
  output = list(id = "counts", property = "figure"),
  list(
    input("radio-selection", "value"),
    input("radio-dependent", "value")
  ),
  function(column, list_) {
    p2 <- count_plot_dates(boardgame_data, column, unlist(list_))
    return(p2)
  }
)


# 1st facet chart

app$callback(
  output = list(id = "facet_1", property = "figure"),
  params = list(input(id = "non-linear-range-slider", property = "value")),
  function(value) {
    val1 <- as.numeric(value[1])
    val2 <- as.numeric(value[2])
    p3 <- rank_plot_facet(boardgame_data, val1, val2)
    return(p3)
  }
)


# 2nd facet chart

app$callback(
  output = list(id = "facet_2", property = "figure"),
  params = list(input(id = "non-linear-range-slider2", property = "value")),
  function(value) {
    val1 <- as.numeric(value[1])
    val2 <- as.numeric(value[2])
    p4 <- rank_plot_facet(boardgame_data, val1, val2)
    return(p4)
  }
)



# 1st year range slider output tab 1

app$callback(
  list(output("output-container-range-slider", "children")),
  list(input("non-linear-range-slider", "value")),
  function(value) {
    value1 <- value[1]
    value2 <- value[2]
    string <- paste("Years Selected ", value1, "to ", value2)
    return(list(string))
  }
)

# 2nd year range slider output tab 1

app$callback(
  list(output("output-container-range-slider2", "children")),
  list(input("non-linear-range-slider2", "value")),
  function(value) {
    value1 <- value[1]
    value2 <- value[2]
    string <- paste("Years Selected ", value1, "to ", value2)
    return(list(string))
  }
)


# bar char tab 2
app$callback(
  output = list(id = "top_n_games", property = "figure"),
  list(
    input("category-widget", "value"),
    input("mechanic-widget", "value"),
    input("publisher-widget", "value")
  ),
  function(c, m, p, n = 10) {
    p <- top_n_plot(boardgame_data, c, m, p, n = 10)
    return(p)
  }
)

# data table tab 2
app$callback(
  output = list(
    list(id = "top_n_games_datatable", property = "data"),
    list(id = "top_n_games_datatable", property = "columns")
  ),
  list(
    input("category-widget", "value"),
    input("mechanic-widget", "value"),
    input("publisher-widget", "value")
  ),
  function(c, m, p, n = 10) {
    t <- call_boardgame_filter(boardgame_data, c, m, p, n = 10)
    t <- t[c(
      "name", "min_players",
      "max_players", "min_playtime",
      "max_playtime", "year_published",
      "category", "mechanic",
      "designer", "publisher",
      "average_rating", "users_rated"
    )]
    columns <- col_named_list
    return(list(t, columns))
  }
)


app$run_server(host = "0.0.0.0")
