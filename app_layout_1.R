# load packages
# Installs dash bootstrap
library(devtools)
library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
# library for dictionaries
library(hash)



# * WRANGLING - load board game data 



# *WRANGLING - get dictionary for dropdowns

# fake chart for layout 
# Load ggplot2
p <- ggplot(msleep) +
  aes(x = bodywt,
      y = sleep_total) +
  geom_point() +
  scale_x_log10()



# hash table for tab 1 sliders
h <- hash() 
  h[['1950']] <- 1950
  h[['1955']] <- 1955
  h[['1960']] <- 1960
  h[['1965']] <- 1965
  h[['1970']] <- 1970
  h[['1975']] <- 1975
  h[['1980']] <- 1980
  h[['1985']] <- 1985
  h[['1995']] <- 1995
  h[['2000']] <- 2000
  h[['2005']] <- 2005
  h[['2010']] <- 2010
  h[['2015']] <- 2015
  

# title
title<-function(){
 
return (htmlDiv(list(htmlH1("Board Game Trends Dashboard"))))
  
}

# description card tab 1
description_card<- function(){
  
return (htmlDiv(
  id="description-card",
  list(
    htmlH5("Welcome to our Board Games Dashboard"),
    htmlDiv(
      id="intro",
      list("Explore board game trends over time based on category, mechanics \
                    and publisher selection below. Also visualize the top categories,\
                    mechanics and publishers by year using our interactive features."
    ))
  )
))

}

# control card for tab 1
generate_control_card<- function(){
 
return (htmlDiv(
  id="control-card",
  list(
    htmlLabel("Select what you want to view:"),
    htmlBr(),
    htmlBr(),
    #add radio button
    htmlBr(),
    htmlLabel("Select elements to view:"),
    htmlBr(),
    htmlBr()
   # add radio button dependent drop down,
  
)))
}

# lower description for tab 1
lower_description<- function(){
  
return (htmlDiv(
  list(
    htmlH4("Top 5 Categories, Mechanics and Publishers by Rating"),
    htmlP(
      "Two sets of bar charts with year range sliders are provided \
                    to allow comparison for two different periods.",
    ),
    htmlBr(),
    htmlP(
      "Drag the year sliders below to select your year ranges and \
                    compare the top 5 categories, mechanics and publishers \
                    between time periods."
    )
  
)))
}

# data set description for tab 1
data_set_description <- function(){
 
return (htmlDiv(
 list(
    htmlH4("Description of Dataset"),
    htmlP(
      " This dataset comes from the Board Game Geek website and \
                    includes boardgames with descriptions, general game \
                    details, publisher, and user ratings for 10,000 boardgames\
                    published between 1950 and 2021."
    )
  
)))}


# card 1 containing the description and control card for tab 1
first_card = dbcCard(
  dbcCardBody(
    
      htmlDiv(
        id="left-column",
        className="four columns",
        list(
          description_card(),
          htmlBr(),
          generate_control_card())
          
        )
      )
    
  )

second_card = dbcCard(
  dbcCardBody(
  
      htmlDiv(
        list(
          htmlH4("Board Game Ratings and Counts from 1950 to 2016"),
          htmlP(
            "Select either categories, mechanics or publishers.\
                             Then select different elements to view on the\
                                following two figures."
          ))
          # scatter plot tab 1 goes here
         # histogram tab 1 goes here
        
  )
))

# card 3 containing the lower description and collapsable data set description for tab 1
third_card = dbcCard(
  dbcCardBody(
    
      dbcCol(
        id="bottom left row",
        className="four columns",
        list(lower_description()),
      )
    
  )
)

fourth_card = dbcCard(
  dbcCardBody(
    
      dbcRow(
        
          htmlDiv(
            list(
              htmlDiv(
                id="output-container-range-slider",
                
              ),
              htmlBr(),
              htmlBr(),
              dccRangeSlider(
                id="non-linear-range-slider",
                min=1950,
                max=2016,
                step=1,
                value=list(1990, 2010),
                marks=h,
              ),
              htmlBr(),
              # 1st facet chart for bottom tab 1 goes here
              
              htmlBr(),
              htmlBr()
            
          )
        
      )
    
  )
))

fifth_card = dbcCard(
  dbcCardBody(
    
    dbcRow(
      
      htmlDiv(
        list(
          htmlDiv(
            id="output-container-range-slider2",
            
          ),
          htmlBr(),
          htmlBr(),
          dccRangeSlider(
            id="non-linear-range-slider2",
            min=1950,
            max=2016,
            step=1,
            value=list(1990, 2010),
            marks=h,
          ),
          htmlBr(),
          # 2nd facet chart for bottom tab 1 goes here
          htmlBr(),
          htmlBr()
          
        )
        
      )
      
    )
  ))

# card 9 for data set description tab 1
ninth_card = dbcCard(
  dbcCardBody(
    
      htmlDiv(
        
          dbcButton(
            "Click here to view dataset description",
            id="collapse-button",
            className="mb-3",
            color="primary",
          ),
          dbcCollapse(
            dbcCard(dbcCardBody(data_set_description())), id="collapse"
          )
        
      )
    
  )
)

#  set up app stylesheet and server
app = Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

# layout made up of cards
app$layout(htmlDiv(list(
  
    dbcContainer(
      
        
                htmlDiv(
                  id="title_top",
                  className="title on top",
                  list(title())
               
         
    )), htmlDiv(dccTabs(id="tabs_in_app", value='tabs',list(
  
  dccTab(label='Game Dynamics Over Time', children=list(htmlBr(),htmlBr(),htmlBr(),dbcRow(list(dbcCol(first_card, width=3), dbcCol(second_card, width=9))), dbcRow(list(dbcCol(third_card, width=3), dbcCol(fourth_card, width=9))), dbcRow(ninth_card))),
  dccTab(label='Top Games')))))))
  
  
# app callbacks


app$run_server(debug = T) 