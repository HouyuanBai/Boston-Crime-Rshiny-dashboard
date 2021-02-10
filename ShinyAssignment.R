#load the revelant libraries
library(shiny)  #for web applications
library(shinydashboard)
library(DataExplorer)
library(scales)
library(dplyr)
library(ggplot2)

#get the working directory
getwd()

# read the data from the directory
crimes <- read.csv("crimesN.csv",stringsAsFactors = FALSE, header = TRUE)


############################################################################################################################
# The customizations for this dashboard are : 3 value boxes, two ggplots, dashboard icon on the left #
############################################################################################################################

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Crimes Report", titleWidth=350)  

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
    title = "Total Records by Year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("totalpaidprovspec", height = "400px")
  )
  
  ,
  box(
    title = " Records in Hours by Year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("totalpaidmonth", height = "400px")
  ) 
)


# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='green')


# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation to derive the values on the boxes on top
  total.records <- sum(crimes$Numbers)
  
  set.seed(42)
  
  crimes0 <- data.frame(when = as.POSIXct('2020-01-01 00:00:00', tz="UTC") + runif(100000, min = 0, max = 365*24*3600))
  head(crimes0)
  range(crimes0$when)
  
  crimes0 <- within(crimes0, {
    month = format(when, format = "%b")
    hour  = format(when, format = "%H")
  })
  head(crimes0)
  sort(table(crimes0$month), decreasing = TRUE)
  sort(table(crimes0$hour), decreasing = TRUE)
  
  crimes_rx <- reactive({
    # ...
    crimes0
  })
  crimes_freq <- reactive({
    dat <- req(crimes_rx())
    list(
      month = sort(table(dat$month), decreasing = TRUE),
      hour = sort(table(dat$hour), decreasing = TRUE)
    )
  })
  
  total.records.hour <- 20
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total.records, format="d", big.mark=',')
      ,'Total Records in 2016,2017 and 2018'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")
  })
  
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(total.records.hour, format="d", big.mark=',')
      ,'The hour including most records'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "navy")
  })
  
  output$value3<- renderValueBox({
    req(crimes_freq())
    valueBox(
      names(crimes_freq()$month)[1]
      ,'The month including most records'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "navy")
  })
  #creating the plotOutput content
  crimessum<- crimes %>% 
    group_by(YEAR) %>%
    summarize(Numbers = sum(Numbers))
  
  options(scipen=999)
  
  output$totalpaidprovspec <- renderPlot({
    ggplot(data = crimessum)+ 
      geom_bar(mapping = aes(x=reorder(YEAR,-Numbers),
                             y=Numbers),stat = "identity",show.legend = FALSE,
               fill = c("light blue","light grey","light grey"))+
      
      ylab(" ") + 
      xlab(" ")+
      ggtitle(" ")
  })
  
  library(timeDate)
  
  crimessum2<-crimes %>%
    group_by(YEAR,HOUR) %>%
    summarise(Numbers = sum(Numbers))
  
  
  output$totalpaidmonth <- renderPlot({
    ggplot(data = crimessum2)+ 
      geom_line(mapping = aes(x=HOUR, y = Numbers,col = factor(YEAR), group=YEAR))+
      xlab("HOUR")+
      ylab("Total records")+
      ggtitle(" ")
  })
  
  
}

shinyApp(ui, server)