#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readxl)
library(magrittr)

ui <- dashboardPage(
  dashboardHeader(title = "Partial AUC"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Partial AUC", tabName = "partial_AUC", icon = icon("line-chart")),
      menuItem("Example", tabName = "example", icon = icon("eye"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # signup tab
      tabItem(tabName = "partial_AUC",
              fluidRow(
                column(6,
                       box(width = NULL, 
                           collapsible = TRUE,
                           solidHeader = TRUE,
                           title = "Data Input",
                           status = "primary",
                           helpText("please enter the information of the dataset"),
                           numericInput("num", "Num of time points", value = 10, min = 1, max = 30),
                           fileInput("file", "Data mfile", accept = "xlsx"),
                           actionButton("goButton", "Go!", icon = icon("thumbs-up"))
                       ),  
                       tags$hr(),
                       box(width = NULL,
                           tableOutput("data_table"),
                           collapsible = TRUE,
                           title = "pAUC",
                           status = "primary",
                           solidHeader = TRUE)),
                column(6,
                       box( width = NULL,
                            plotOutput("plot1", height="250px"),
                            collapsible = TRUE,
                            title = "Plot",
                            status = "primary",
                            solidHeader = TRUE),
                       tags$hr(),
                       box(width = NULL,
                           plotOutput("plot2", height="250px"),
                           collapsible = TRUE,
                           title = "Plot",
                           status = "primary",
                           solidHeader = TRUE),
                       tags$hr(),
                       box(width = NULL,
                           plotOutput("plot3", height="250px"),
                           collapsible = TRUE,
                           title = "Plot",
                           status = "primary",
                           solidHeader = TRUE)
                )
              ),
              tags$hr()
      ),
      # login tab
      tabItem(tabName = "example")
    )
  )
)