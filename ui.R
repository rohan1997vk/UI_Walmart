library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)
setwd("C:/Users/rohan/OneDrive/Desktop/Revenue Datasets/Walmart")
stores <- read.csv("stores.csv")
features <- read.csv("features.csv")
train <- read.csv("train.csv")
test <- read.csv("test.csv")
dataset <- merge(stores,features)
train_features1 <- left_join(train,features, by = c("Store", "Date"))
train_features_type <- left_join(train_features1,stores, by = c("Store"))
train_features2 <- left_join(train_features_type,features ,by = c("Store", "Date"))

ui <- fluidPage(
  titlePanel("Walmart Visualization"),
  sidebarLayout(      
    sidebarPanel(
      selectInput("sales", "Sales V/S :",choices=colnames(train_features2))
      ),
    mainPanel(plotOutput("WalmartPlots"))
  )
)

server <- function(input, output) 
  
{output$WalmartPlots <- renderPlot({

    
  ggplot(train_features2, aes(x=as.numeric(train_features2[,input$sales]),y= (train_features2$Weekly_Sales/100))) +
    geom_histogram(fill = "lightskyblue", stat = "identity" ) + facet_wrap(Type~.) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 6),
          axis.title = element_text(size = 8.5))+
     xlab(input$sales) + ylab("Weekly_Sales")

})}            
shinyApp(ui,server)


