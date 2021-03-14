# load packages
# Installs dash bootstrap
library(devtools)
library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
source("./src/app/app_wrangling.R")
source("./src/app/app_graphing.R")
library(dashTable)

options(warn = 1, keep.source = TRUE, error = quote({
  # Debugging in R
  #   http://www.stats.uwo.ca/faculty/murdoch/software/debuggingR/index.shtml
  #
  # Post-mortem debugging
  #   http://www.stats.uwo.ca/faculty/murdoch/software/debuggingR/pmd.shtml
  #
  # Relation functions:
  #   dump.frames
  #   recover
  # >>limitedLabels  (formatting of the dump with source/line numbers)
  #   sys.frame (and associated)
  #   traceback
  #   geterrmessage
  #
  # Output based on the debugger function definition.
  # TODO: setup option for dumping to a file (?)
  # Set `to.file` argument to write this to a file for post-mortem debugging    
  dump.frames()  # writes to last.dump
  n <- length(last.dump)
  if (n > 0) {
    calls <- names(last.dump)
    cat("Environment:\n", file = stderr())
    cat(paste0("  ", seq_len(n), ": ", calls), sep = "\n", file = stderr())
    cat("\n", file = stderr())
  }
  if (!interactive()) q()
}))


# * WRANGLING - load board game data here
boardgame_data <- call_boardgame_data()

# * WRANGLING - get dictionary for dropdowns here
col_key_list <- c("category", "mechanic", "publisher")
col_dict <- vector(mode = "list", length = 3)
names(col_dict) <- col_key_list

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
id="description-card",
list(
htmlH5("Welcome to our Board Games Dashboard"),
htmlDiv(
id="intro",
list("Explore board game trends over time based on category, mechanics \
                    and publisher selection below. Also visualize the top categories,\
                    mechanics and publishers by year using our interactive features.")))))}

# control card for tab 1
generate_control_card<- function() {
return (htmlDiv(
id="control-card",
list(
htmlLabel("Select what you want to view:"),
htmlBr(),
htmlBr(),
dccRadioItems(
  id="radio-selection",
  options=list(
    list(label = "Categories", value = "category"),
    list(label = "Mechanics", value = "mechanic"),
    list(label = "Publishers", value = "publisher")
  ),
  value="mechanic"
),
htmlBr(),
htmlLabel("Select elements to view:"),
htmlBr(),
htmlBr(),
dccDropdown(
  id = "radio-dependent",
  options=list(),
  multi=TRUE,
  value=NULL,
  # labelStyle=list("display"="block")
)
)))}



generate_control_card_tab2 <- function() {
return(htmlDiv(
id="control-card2",
list(
htmlP( "Please select any combination of categories, mechanics, publishers."),
htmlBr(),
htmlP("Please select categories:"),
dccDropdown(
  id = "category-widget",
  options=(
    col_dict[["category"]] %>% map(function(x) return(list(label=x, value=x)))
  ),
  multi=TRUE,
  value=NULL,
),
htmlBr(),
htmlP("Please select mechanics:"),
dccDropdown(
  id = "mechanic-widget",
  options=(
    col_dict[["mechanic"]] %>% map(function(x) return(list(label=x, value=x)))
  ),
  multi=TRUE,
  value=NULL,
),
htmlBr(),htmlP("Please select publishers:"),
dccDropdown(
  id = "publisher-widget",
  options=(
    col_dict[["publisher"]] %>% map(function(x) return(list(label=x, value=x)))
  ),
  multi=TRUE,
  value=NULL,
),
htmlBr(),
htmlBr())))}


# lower description for tab 1
lower_description<- function() {
return (htmlDiv(
list(htmlH4("Top 5 Categories, Mechanics and Publishers by Rating"),
htmlP("Two sets of bar charts with year range sliders are provided \
to allow comparison for two different periods.",),
htmlBr(),
htmlP("Drag the year sliders below to select your year ranges and \
compare the top 5 categories, mechanics and publishers \
between time periods."))))}

# data set description for tab 1
data_set_description <- function() {
return (htmlDiv(
list(htmlH4("Description of Dataset"),
htmlP(" This dataset comes from the Board Game Geek website and \
includes boardgames with descriptions, general game \
details, publisher, and user ratings for 10,000 boardgames\
published between 1950 and 2021."))))}


# card 1 containing the description and control card for tab 1
first_card = dbcCard(
dbcCardBody(
htmlDiv(id="left-column",
className="four columns",list(description_card(),htmlBr(),generate_control_card()))))

second_card = dbcCard(
dbcCardBody(htmlDiv(list(htmlH4("Board Game Ratings and Counts from 1950 to 2016"),htmlP(
"Select either categories, mechanics or publishers.\
Then select different elements to view on the\
following two figures.")))))
          # scatter plot tab 1 goes here
         # histogram tab 1 goes here)))

# card 3 containing the lower description and collapsible data set description for tab 1
third_card = dbcCard(
dbcCardBody(
dbcCol(id="bottom left row",className="four columns",list(lower_description()))))

# card 4 containing the top slider and first faceted bar chart
fourth_card = dbcCard(
dbcCardBody(list(htmlDiv(list(htmlDiv(id='output-container-range-slider'),
dccRangeSlider(id='non-linear-range-slider',min = 1950,max = 2016,step=1,
marks = list(
"1950"= "1950",
"1955"= "1955",
"1960"= "1960",
"1965"= "1965",
"1970"= "1970",
"1975"= "1975",
"1980"= "1980",
"1985"="1985",
"1990"= "1990",
"1995"="1995",
"2000"="2000",
"2005"="2005",
"2010" ="2010",
"2015"= "2015"),value = list(1990,2010))))
              # plot goes here 
)))
    
  

# card 5 containing the top slider and second faceted bar chart
fifth_card = dbcCard(dbcCardBody(list(htmlDiv(
list(htmlDiv(id="output-container-range-slider2"),
dccRangeSlider(
id="non-linear-range-slider2",min = 1950,max = 2016,step=1,
marks = list(
"1950"= "1950",
"1955"= "1955",
"1960"= "1960",
"1965"= "1965",
"1970"= "1970",
"1975"= "1975",
"1980"= "1980",
"1985"="1985",
"1990"= "1990",
"1995"="1995",
"2000"="2000",
"2005"="2005",
"2010" ="2010",
"2015"= "2015"),value = list(1990,2010))))
    # plot goes here 
)))

# card 6 containing the control card for tab 2
sixth_card = dbcCard(dbcCardBody(list(generate_control_card_tab2())))

seventh_card = dbcCard(dbcCardBody(
  list(
    htmlDiv("Plot for top 10 games goes here"),
    dashDataTable(
      id="top_n_games_datatable",
      data=NULL,
      columns =lapply(colnames(data), 
                      function(colName){
                        list(
                          id = colName,
                          name = colName
                        )
                      }),      # top n games chart goes here
)
)))

# card 8 containing the data table for the top n games for tab 2
eight_card = dbcCard(dbcCardBody(list(htmlH5("Top 10 Games Facts Table:")
      # data table for top 10 games goes here, comes from wrangling
)))

# card 9 for data set description tab 1
ninth_card = dbcCard(
dbcCardBody(htmlDiv(dbcButton("Click here to view dataset description",id="collapse-button",className="mb-3",color="primary",),
dbcCollapse(dbcCard(dbcCardBody(data_set_description())), id="collapse"))))

# tab styling features for layout
tabs_styles = list("height"= "44px")
tab_style = list(
"borderBottom"="1px solid #d6d6d6",
"padding"= "6px",
"fontWeight"= "bold",
"display"="All")

tab_selected_style = list(
"borderTop"= "1px solid #d6d6d6",
"borderBottom"= "1px solid #d6d6d6",
"backgroundColor"= "#119DFF",
"color"= "white",
"padding"= "6px")

#  set up app stylesheet and server
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

# layout made up of cards
app$layout(htmlDiv(list(dbcContainer(htmlDiv(id="title_top",className="title on top", list(title()))), htmlDiv(dccTabs(id="tabs_in_app", value="tab-1"
,list(dccTab(label="Game Dynamics Over Time", value='tab-1',children=list(dbcRow(list(dbcCol(first_card, width=3), dbcCol(second_card, width=9))), dbcRow(list(dbcCol(third_card, width=3),dbcCol(list(fourth_card,fifth_card), width=9))), dbcRow(dbcCol(ninth_card, width=3))), style=tab_style,
selected_style=tab_selected_style),dccTab(label="Top Games", value="tab-2",children=list(dbcRow(list(dbcCol(sixth_card, width=3), dbcCol(list(seventh_card,eight_card), width=9)))),style=tab_style,
selected_style=tab_selected_style)))))))
  
  
# app callbacks

# radio button selection options to populate drop down
# this will return something like [{"label": c, "value": c} for c in col_dict[col]]
app$callback(
  list(output("radio-dependent", "options")),
  list(input("radio-selection", "value")),
  function(chosen_selection){

    outlist <- list(lapply(col_dict[[chosen_selection]], function(x) return(list(label=x, value=x))))
  
    return(outlist)
  }
)


# scatter plot tab 1



# stacked histogram of counts annual published counts


# 1st facet chart



# 2nd facet chart



# 1st year range slider output tab 1

app$callback(list(output("output-container-range-slider", "children")),
list(input("non-linear-range-slider", "value")),
function(value) {
value1 <- value[1]
value2 <- value[2]
string=paste("Years Selected ", value1,"to ", value2)
return(list(string))})

# 2nd year range slider output tab 1

app$callback(
list(output("output-container-range-slider2", "children")),
list(input("non-linear-range-slider2", "value")),
function(value) {
value1 <- value[1]
value2 <- value[2]
string=paste("Years Selected ", value1,"to ", value2)
return(list(string))})


# collapsible data set description
# ** Tried to get this to work... can't seem to figure out the R equivalent


#app$callback(
  #list(output('collapse', 'is_open')),
  #list(input('collapse-button', 'n_clicks')),
  #list(state('collapse', 'is_open')),
  #function(n_clicks, is_open) {
    #if (n_clicks){
      #return(!(is_open))
    #}
    #else{return(is_open)}
  #})

# top n games table
app$callback(
  list(output("top_n_games_datatable", "data")),
  list(input("category-widget", "value"),
       input("mechanic-widget", "value"),
       input("publisher-widget", "value")),
  function(c, m, p, n=10) {
    print(c)
    print(m)
    print(p)
    # print(data.frame(boardgame_data %>% select("game_id", "name") %>% head(n)))
    # data.frame(boardgame_data %>% select("game_id", "name") %>% head(n))
    
    shit <- data.frame(boardgame_data[1:15], c("game_id", "name"))
    columns <- lapply(colnames(shit), function(colName){list(id = colName,name = colName)})
    return(shit)
  }
)



app$run_server(debug = T) 

