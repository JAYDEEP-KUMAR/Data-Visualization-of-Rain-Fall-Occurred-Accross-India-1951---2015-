# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(sqldf)

recommendation <- read.csv('C:\\Users\\jayde\\Desktop\\r\\rainfall in india 1901-2015.CSV',stringsAsFactors = F,header=T)

#head(recommendation)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Rainfall Dashboard")  




#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    
  )
)


frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)




frow2 <- fluidRow(
box(
    title = "Relation Between Year and Rainfall Record"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("a", height = "300px")
  ), 

box(
  title = "Relation Between Year and Rainfall Record"
  ,status = "primary"
  ,solidHeader = TRUE 
  ,collapsible = TRUE 
  ,plotOutput("d", height = "300px")
) ,



box(
  title = "Maximum And Minimum Rainfall occured"
  ,status = "primary"
  ,solidHeader = TRUE 
  ,collapsible = TRUE 
  ,plotOutput("e", height = "300px")
) ,
box(
  title = "Relation Between Year and Rainfall Record"
  ,status = "primary"
  ,solidHeader = TRUE 
  ,collapsible = TRUE 
  ,plotOutput("b", height = "300px")
) 
, 

box(
  title = "Area Covered Yet"
  ,status = "primary"
  ,solidHeader = TRUE 
  ,collapsible = TRUE 
  ,plotOutput("f", height = "300px")
) 


, 

box(
  title = "Rate of Rain fall in ANDAMAN & NICOBAR ISLANDS"
  ,status = "primary"
  ,solidHeader = TRUE 
  ,collapsible = TRUE 
  ,plotOutput("c", height = "300px")
) 
)



# combine the two fluid rows to make the body
body <- dashboardBody(frow1,frow2)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Rainfall Dashboard', header, sidebar, body, skin='yellow')

# create the server functions for the dashboard  
server <- function(input, output) { 

  
  library("sqldf", lib.loc="~/R/win-library/3.5")
  
 yearn= sqldf("select count(DISTINCT YEAR) from recommendation")
 
 yearm= sqldf("select max(ANNUAL) from recommendation")
 yeark= sqldf("select YEAR from recommendation where ANNUAL=(select max(ANNUAL) from recommendation)")
 yeark1= sqldf("select SUBDIVISION from recommendation where ANNUAL=(select max(ANNUAL) from recommendation)")
 
 yearl= sqldf("select min(ANNUAL) from recommendation")
 yearp= sqldf("select YEAR from recommendation where ANNUAL=(select min(ANNUAL) from recommendation)")
 yearp1= sqldf("select SUBDIVISION from recommendation where ANNUAL=(select min(ANNUAL) from recommendation)")
 
  

  output$value1 <- renderValueBox({
    valueBox(
      formatC(yearn, format="d", big.mark=',')
      ,paste('Total numbers of Years is ',yearn)
      ,icon = icon("calendar")
      ,color = "purple")
    
    
  })
  
  output$value2 <- renderValueBox({
    valueBox(
      formatC(yearm, format="d", big.mark=',')
      ,paste('Highest Rainfall in ',yeark1,' in ',yeark)
      ,icon = icon("calendar")
      ,color = "green")
    
    
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      formatC(yearl, format="d", big.mark=',')
      ,paste('Lowest Rainfall in ', yearp1,' in ',yearp)
      ,icon = icon("calendar")
      ,color = "red")
    
    
  })
  
  
  #creating the plotOutput content
  

  output$a <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=YEAR, y=ANNUAL)) + 
      geom_point() +  ggtitle("Relation Between Year and Rainfall Record") + xlab('Years') + ylab('Rainfall')
  })
  
  output$d <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=YEAR, y=ANNUAL)) + 
      geom_smooth() +  ggtitle("Relation Between Year and Rainfall Record") + xlab('Years') + ylab('Rainfall')
  })
  
  mindy=sqldf("select SUBDIVISION,YEAR,min(ANNUAL) from data group by YEAR") 
  colnames(mindy)[which(names(mindy) == "min(ANNUAL)")] <- "ANNUAL" 
  mindy
  maxdy=sqldf("select SUBDIVISION,YEAR,max(ANNUAL) from data group by YEAR") 
  colnames(maxdy)[which(names(maxdy) == "max(ANNUAL)")] <- "ANNUAL" 
  maxdy
  output$e <- renderPlot({
    ggplot() + geom_line(data=maxdy, aes(x=YEAR, y = ANNUAL), color= "green") +  
      geom_line(data=mindy, aes(x=YEAR,y=ANNUAL), color = "red") + labs(title = "Comparision between maximum and minimum rainfall occur per year") + xlab('Years') + ylab('Rainfall')
  })
  output$b <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=YEAR, y=ANNUAL)) + 
      geom_line() +  ggtitle("Relation Between Year and Rainfall Record") + xlab('Years') + ylab('Rainfall')
  })
  
  
  output$f <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=YEAR, y=ANNUAL)) + 
      geom_area() +  ggtitle("Area Covered Yet ") + xlab('Years') + ylab('Rainfall')
  })
  
  
  and= sqldf("select * from recommendation where SUBDIVISION = 'ANDAMAN & NICOBAR ISLANDS'")
 
 
  output$c <- renderPlot({
    ggplot(data = and, 
           aes(x=YEAR, y=ANNUAL)) + 
      geom_line() +  ggtitle("For ANDAMAN & NICOBAR ISLANDS ") + xlab('Years') + ylab('Rainfall')
  })
  
  
}


shinyApp(ui, server)