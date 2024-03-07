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
  dashboardHeader(title = "UC Admissions"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
    fluidRow(
      
      box(title = "Plot Options", width = 3, 
          selectInput("x", "Select Campus", 
                      choices = unique(UC_admit$campus), 
                      selected = "Davis"), 
          selectInput("y", "Select Category", 
                      choices = unique(UC_admit$category), 
                      selected = "Applicants"), 
          selectInput("z", "Select Admit Ethnicity", 
                      choices = unique(UC_admit$ethnicity), 
                      selected = "International")
      ), #close the first box
      
      box(title = "Yearly Trends", width = 8,
          plotOutput("plot", width = "600px", height = "500px")
      ) #close the second box
    ) #close the row
    
  ) #close the dashboard body
) #close the ui

server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  output$plot <- renderPlot({
    
    UC_admit %>% mutate(academic_yr=as.factor(academic_yr)) %>% 
      filter(ethnicity != "All" & perc_fr != "NA") %>%  # filter out
      filter(campus == input$x) %>% 
      filter(category == input$y) %>% 
      filter(ethnicity == input$z) %>%
      ggplot(aes_string(x="academic_yr", y="perc_fr")) +
      geom_point() +
      geom_path(group="keep") +
      labs(x="Academic Year",
           y="Percentage of Applicants, Admits, or Enrollees") +
      theme(axis.title.x = element_text(size=8),
            axis.title.y = element_text(size=8))
    
  })
  
}

shinyApp(ui, server)