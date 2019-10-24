#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
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