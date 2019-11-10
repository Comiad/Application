#Cole Adams
#Pitcher Scrim 2018-2019 Shiny App
#2/12/19

library(shiny)
library(shinydashboard)
library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(knitr)
library(xtable)
library(DT)
library(yaml)

# Sets up Tabs UI

dashboardPage(skin="red",
              
  #Webpage Title
  dashboardHeader(title="Pitching Overview"),
              
  #Defines Sidebar Tabs
  dashboardSidebar(sidebarMenu(
  menuItem("Overview", tabName = "overview", icon = icon("balance-scale")),
  menuItem('Pitchers', tabName = "pitchers", icon = icon("user-circle")),
  menuItem('Plots', tabName = "plots", icon = icon("square"))
  )),
              
  #Sets Up Main Tab Def
  dashboardBody(
    tabItems(
  #Opening Tab
      tabItem(tabName = "overview",
        fluidRow(
        #Latex Functionality
        withMathJax(),
        #Set's 
        column(6,
        #Description of App
        h1("App Purpose"),
        #Description Main Body
        box(background="red",width=12,
          h4("This application shows in-depth statistics for pitchers."),
          h4("This application includes both statistics, as well as a pitch chart and spin rate development chart.")
        )),
                         
        column(6,
        #How To Purpose
        h1("Instructions"),
        #How To Main Body
        box(background="red",width=12,
          h4("Choose the pitcher you want to analyze"),
          h4("Select the pitch (or all pitches) you want to inspect"),
          h4("Click the pitch type on the pitch chart to hide it in the chart.")
          ))
        
        )
  ),
  
  #Main App Layout    
  tabItem(tabName = "pitchers",
      fluidRow(
      #Left Side of the App (3 of 12 width)
        column(3,br(),
      #Player Input
      selectizeInput("iPit",label=h3("Pitcher Name"),choices=sort(c('Agnos, Jake', 'Burleson, Alec', 'Kuchmaner, Jake', 'Lanier, Sam', 'Smith, Tyler', 'Voliva, Evan', 'Williams, Gavin', 'Team'))),
      #Splits Input
      selectizeInput("splits",label=h4("Versus"),choices=sort(c('All','Left', 'Right'))),
      #Stat Input
      selectInput('pitchType', label = h3('Pitch Type'), choices = sort(c('All', 'Fastball', 'Changeup', 'Curveball'))),
      tableOutput("mytable")
      
      
      
      
    ),
      #Right side of app (9 of 12 width)
        column(9, fluidRow(
      #Show applicable plots
        plotlyOutput("pitcherPlot"),
        plotlyOutput("spinPlot"),
        plotlyOutput('veloPlot')
          ))
        )),
    
    tabItem(tabName = "plots",
            sidebarLayout(
              #Left Side of the App (3 of 12 width)
              sidebarPanel(
                     #Player Input
                     selectizeInput("zPit",label=h3("Pitcher Name"),choices=sort(c('Agnos, Jake', 'Burleson, Alec', 'Kuchmaner, Jake', 'Lanier, Sam', 'Smith, Tyler', 'Voliva, Evan', 'Williams, Gavin'))),
                     #Player Input
                     selectizeInput("zsplits",label=h4("Versus"),choices=sort(c('All','Left', 'Right'))),
                     
                     selectInput('zpitchType', label = h3('Pitch Type'), choices = sort(c('All', 'Fastball', 'Changeup', 'Curveball')))

              ),
              mainPanel(
                plotlyOutput('zPlots'),
                plotlyOutput('zPlots2')
              )


            )
  )
)))
