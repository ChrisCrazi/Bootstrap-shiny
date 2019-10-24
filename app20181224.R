# forÑ­»·¸³Öµ°¸Àý
# means <- c(1, 50, 20)
# out <- vector("list", length(means))
# for (i in seq_along(means)) {
#   out[[i]] <- rnorm(10, means[[i]])
# }


# getting a reproducible example
# library(reprex)
# Ctrl + c
# reprex()

# packages check
options("repos" = c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))

# if (!require(devtools))
#   install.packages("devtools")
# if (!require(shiny))
#   devtools::install_github("rstudio/shiny")
if (!require(shiny))
  install.packages("shiny")
if (!require(shinydashboard))
  install.packages("shinydashboard")
if (!require(ggplot2))
  install.packages("ggplot2")
if (!require(dplyr))
  install.packages("dplyr")
if (!require(readxl))
  install.packages("readxl")
if (!require(magrittr))
  install.packages("magrittr")
if (!require(jsonlite))
  install.packages("jsonlite")

## app.R ##
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

server <- function(input, output) {
  currentdata  <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    df <- read_xlsx(inFile$datapath)
    
    # names(df) <- c("x", "y")
    # table <- df %>% 
    #   mutate(z = round(((y + lag(y)) / (x - lag(x))), 3))
    # table[1, 3] <- 0
    # table <- table %>%
    #   mutate(pAUC = round(cumsum(z), 3)) %>% 
    #   mutate(percentage = as.integer(pAUC / max(pAUC) * 100))
    # table
    
    names(df) <- c("sub", "sequence", "period", "formulation", "time", "concentration")
    df1 <- filter(df, formulation == "T")
    df2 <- filter(df, formulation == "R")
    
    df1 <- group_by(df1, time)
    df1_mean <- summarise(df1, concentration = mean(concentration))
    df1 <- ungroup(df1)
    df1 <- mutate(select(filter(df1, sub == df1[[1]][[1]]), sub, sequence, period, formulation, time), concentration = df1_mean$concentration)
    
    df1_1 <- df1 %>% 
      mutate(z = ((concentration + lag(concentration)) / (time - lag(time))))
    df1_1[1, 7] <- 0
    df1_1 <- df1_1 %>%
      mutate(pAUC = round(cumsum(z), 3)) %>% 
      mutate(percentage = as.integer(pAUC / max(pAUC) * 100))
    
    df2 <- group_by(df2, time)
    df2_mean <- summarise(df2, concentration = mean(concentration))
    df2 <- ungroup(df2)
    df2 <- mutate(select(filter(df2, sub == df1[[1]][[1]]), sub, sequence, period, formulation, time), concentration = df2_mean$concentration)
    
    df2_2 <- df2 %>% 
      mutate(z = ((concentration + lag(concentration)) / (time - lag(time))))
    df2_2[1, 7] <- 0
    df2_2 <- df2_2 %>%
      mutate(pAUC = round(cumsum(z), 3)) %>% 
      mutate(percentage = as.integer(pAUC / max(pAUC) * 100))
    table <- rbind(df1_1, df2_2)
    table <- select(table, -z, -sub, -sequence, -period)
    table
  })
  
  output$data_table <- renderTable({
    input$goButton
    # currentdata()
    isolate(currentdata())
  }, 
  options = list(
    pageLength = 10
    # initComplete = I("function(settings, json) {alert('Done.');}")
  ))

  
  output$plot1 <- renderPlot({
    if (input$goButton == 0)
      return()
    
    # ggplot(isolate(currentdata())) +
    #   geom_col(aes(as.factor(x), percentage, fill = as.factor(x))) +
    #   xlab("time") +
    #   ylab("partial AUC %")
    
    currentdata() %>%
      isolate() %>%
      ggplot() +
      geom_col(aes(as.factor(time), percentage, fill = as.factor(time))) +
      xlab("time") +
      ylab("partial AUC %") +
      facet_wrap(~formulation)
  })
  
  output$plot2 <- renderPlot({
    if (input$goButton == 0)
      return()
    
    # ggplot(isolate(currentdata())) +
    #   geom_line(aes(x, percentage)) +
    #   xlab("time") +
    #   ylab("partial AUC %")
    
    currentdata() %>%
      isolate() %>%
      ggplot() +
      geom_line(aes(time, percentage, color = formulation), size = 1) +
      geom_point(aes(time, percentage, color = formulation), size = 2)
  })
  
  output$plot3 <- renderPlot({
    if (input$goButton == 0)
      return()
    
    # isolate(currentdata()) %>% 
    #   filter(percentage < 99) %>%
    #   ggplot() +
    #   geom_line(aes(x, percentage)) +
    #   xlab("time") +
    #   ylab("partial AUC %")
    currentdata() %>%
      isolate() %>%
      filter(percentage < 99) %>%
      ggplot() +
      geom_line(aes(time, percentage, color = formulation), size = 1) +
      geom_point(aes(time, percentage, color = formulation), size = 2)
  })
}

shinyApp(ui, server)