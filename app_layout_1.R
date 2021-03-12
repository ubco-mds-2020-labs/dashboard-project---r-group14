# load packages
# Installs dash bootstrap
library(devtools)
library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
install_github('facultyai/dash-bootstrap-components@r-release')
library(dashBootstrapComponents)


# * WRANGLING - load board game data 



# *WRANGLING - get dictionary for dropdowns



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



app = Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

app$layout(htmlDiv(list(
  
    dbcContainer(
      
        
                htmlDiv(
                  id="title_top",
                  className="title on top",
                  list(title())
               
         
    )), htmlDiv(dccTabs(id="tabs_in_app", value='tabs',list(
  
  dccTab(label='Game Dynamics Over Time', value='tab-1', list(first_card)),
  dccTab(label='Top Games', value='tab-2')))))))
  
    

app$run_server(debug = T) 