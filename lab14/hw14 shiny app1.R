library(tidyverse)
library(shiny)
library(shinydashboard)
library("naniar")
library("janitor")
# load data
UC_admit <- read_csv("data/UC_admit.csv") 

UC_admit <- UC_admit %>%
  mutate(`Perc FR`=as.numeric(sub("%", "", `Perc FR`))) %>% 
  clean_names() # clean the data

ui <- dashboardPage(
  dashboardHeader(title = "UC Campus Admissions by Ethnicity 2010-2019"),
  dashboardSidebar(disable=T),
  dashboardBody(
    fluidRow(
      box(title = "Plot Options", width = 3,
          radioButtons("x", "Select Year", choices = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"), 
                       selected = "2010"),
          selectInput("y", "Select Campus", choices = c("Davis", "Irvine", "Berkeley", "Irvine", "Los_Angeles", "Merced", "Riverside", "San_Diego", "Santa_Barbara", "Santa_Cruz"),
                      selected = "Davis"),
          selectInput("z", "Select Admit Category", choices = c("Applicants", "Admits", "Enrollees"),
                      selected = "Applicants")
      ), # close the first box
      box(title = "UC Admissions", width = 8,
          plotOutput("plot", width = "600px", height = "500px")
      ) # close the second box
    ) # close the row
  ) # close the dashboard body
) # close the ui

server <- function(input, output, session) { 
  
  output$plot <- renderPlot({
    UC_admit %>% 
      filter(academic_yr==input$x & campus==input$y & category==input$z) %>% 
      ggplot(aes(x=reorder(ethnicity, filtered_count_fr), y=filtered_count_fr)) + 
      geom_col(color="black", fill="pink", alpha=0.75) +
      theme_light(base_size = 18) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1))+
      labs(x = "Ethnicity", y = "Number")
  })
  
  session$onSessionEnded(stopApp)
  
}

shinyApp(ui, server) 