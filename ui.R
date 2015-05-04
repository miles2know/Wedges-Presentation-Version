
# libraries ------------------------------------------------------------------------------------------------------------

# 1) highlight all of the libraries below
# 2) press cntrl + enter
# 3) click the green Run App button to the right

library(shiny)
library(ggvis)
library(shinythemes)
library(rCharts)

#----------------------------------------------------------------------------------------------------------------------

shinyUI(navbarPage("NEG/ECP Scenario Analysis",theme = shinytheme("cerulean"),
     tabPanel("New England CO2e Emissions",
      fluidRow(
       column(3,
        wellPanel(
         uiOutput("c1"),
         br()
     )
    ),
       column(9,
        wellPanel(h4("Total New England CO2e Emissions",align="center"),style="width: 75%;",
         ggvisOutput("plot1")
     )
    )
   )
  ),
    tabPanel("Wedges Analysis",
      fluidRow(
       column(3,h4(tags$b("% Change in (BAU) 2030 Energy Consumption Relative to 2015"),align="center"),
        wellPanel(
         uiOutput("c2")
     ),
        wellPanel(
         uiOutput("c4")
     ),
        wellPanel(
         uiOutput("c6")
     ),
        wellPanel(
        uiOutput("c3")
     ),
        wellPanel(
        uiOutput("c5")
     ),
        p(tags$b("*The sliders represent % reduction in energy consumption in 2030 relative to 2015."))
   ),
       column(6,
        wellPanel(h4(tags$b("Greenhouse Gases by Sector",align="center"),align = "center"),style="width: 100%; height: 75%;",
         plotOutput("gridPlot")
     )
    ),
       column(3,
        wellPanel(h4("2030 Percent Reduction in GHG",align="center"),style="width: 100%;height: 479px;",
         DT::dataTableOutput("pctTable")
     )
    )
   )
  )
 ) 
) 
