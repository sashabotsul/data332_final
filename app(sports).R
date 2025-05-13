library(shiny)
library(ggplot2)
library(DT)
library(readxl)
library(RCurl)
library(bslib)
library(dplyr)
library(tidyr)
library(hms)
library(lubridate)
library(viridis)
library(leaflet)
library(leaflet.extras)
rm(list=ls())

data_url <- getURL()
dataset <- read.csv(text = data_url)

column_names<-colnames(dataset)


ui <- fluidPage(
  theme = shinythemes::shinytheme('superhero'),

  navset_card_underline(
    header = h1('Sports Salaries'),
    
    nav_panel('Our Project',
              h2('Our Project'),
              wellPanel(
              tags$p('For this project, we have chosen to analyze different sports
              salaries, comparing them within sports and between other sports.', style = "font-size: 18px;")),
              h3('Our Research'),
              wellPanel(
                tags$p('We chose to look at different sport salaries, starting in 1985.
                   The sports we are looking at are baseball and basketball.
                   The reason we chose to look at these salaries is because these are 2 
                   of the more popular sports in the United States. We thought it would
                   be interesting to see how different sport salaries compare and if there
                   would be a trend in the data when adjusting the growth in salaries to 
                   inflation. We are interested to see if a more popular sport among the 2
                   might have a higher salary, which we believe will be basketball.', 
                       style = "font-size: 18px;")),
              h3('Scope of Project'),
              wellPanel(
                tags$p('The scope of our project is as follows:', style = "font-size: 18px;"),
                tags$p('Compare salaries of sports (baseball and basketball) to each other and
                   within each sport, as well as the salaries adjusted for inflation.', style = "font-size: 18px;")),
              h3('Requirements of Project'),
              wellPanel(
                tags$p("The requirements we have set for our project include:", style = "font-size: 18px;"),
                tags$ul(
                  tags$li("Clean the data of any unnecessary columns", style = "font-size: 18px;"),
                  tags$li("Adjust salaries for inflation", style = "font-size: 18px;"),
                  tags$li("Compare salary growth to inflation growth", style = "font-size: 18px;"),
                  tags$li("Compare sport salary trend lines", style = "font-size: 18px;"),
                  tags$li("Compare individual salaries within a sport", style = "font-size: 18px;")
                )
              ),
           
              h3('Ideas and Original Plans'),
              wellPanel(
                tags$p('We ran into several conflicts with our plans during our project.
                   Originally, we wanted to include more sports, including football,
                   hockey, and soccer. When we started, it was clear hockey and soccer
                   were not available. After looking for football in many different places,
                   it was too hard to come across. While we could have scraped websites with
                   data that could have been used, it would have violated the terms of service.
                   Additionally, we were hoping to compare our salary information to the
                   viewership trends of the sports. Once again, this data was not readily 
                   available for all of the sports or for more than just world championships.',
                   style = "font-size: 18px;"))
              ),
  
  
  nav_panel('Salary Charts',
            h2('Salary Charts'),
            
            h4('name of chart'),
            #plotOutput(''),
            wellPanel(h5('description of chart')),
            
            h4('name of chart'),
            #plotOutput(''),
            wellPanel(h5('description of chart'))
            
            ),
  
  nav_panel('Chart',
            h2('Chart'),
            
            h4('name of chart'),
            #plotOutput(''),
            wellPanel(h5('description of chart'))
            
            ),
  nav_panel('Salary growth between teams', 
            h2('Salary growth between teams'),
            fluidRow(
    column(3, 
           selectInput('salary_metric', 'Choose Salary Metric:',
                       choices = c('Average Salary', 'Total Salary'),
                       selected = 'Average Salary'),
           #selectInput('sport_choice', 'Choose a Sport')
                       #choices = unique(dataset$Sport),
                       #selected = unique(dataset$Sport[1])
    ),
    
    column(9,
           #plotOutput('salary_by_year'),
           wellPanel(h5(''))
           )
  )
  )
  )
  
  
)

server<- function(input, output) {
  
  #Chart1
  #output$chart1<- renderPlot({})
  
  #Chart2
  #output$chart2<- renderPlot({})
  
  #Chart3
  #output$chart3<- renderPlot({})
  
  #Chart4
  #in notes
  
  
}

shinyApp(ui=ui, server=server)
