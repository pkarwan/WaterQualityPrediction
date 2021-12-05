# Project III, ST 558
#Author: Pramodini Karwande

#install.packages(c("shiny", "shinydashboard", "tidyverse", "dplyr", "ggplot2", "stringr", "caret", "DT", "plotly", "tree", "rattle", "randomForest" ))

library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(caret)
library(DT)
library(tree)
library(rattle)
library(randomForest)
library(plotly)


waterPotabilityFullData<- as_data_frame(read.csv("./data/water_potability.csv"))
#desc(waterPotabilityFullData)
#str(waterPotabilityFullData) #tibble [3,276 x 10] (S3: tbl_df/tbl/data.frame)

#remove NA
waterPotabilityFullData <- waterPotabilityFullData %>% na.omit(waterPotabilityFullData)
#str(waterPotabilityFullData) #tibble [2,011 x 10] (S3: tbl_df/tbl/data.frame)

water_type<- waterPotabilityFullData['ph']
water_type <- round(water_type, digits = 1)
waterPotabilityFullData <- waterPotabilityFullData %>% 
  mutate(water_type = case_when(
    (water_type>8) ~ "Alkaline",
    (water_type<=8 & water_type>7.5) ~ "Seawater",
    (water_type==7.5) ~ "Tap",
    (water_type< 7.5 & water_type>=6.5) ~ "Bottled",
    (water_type<6.5 & water_type>=5.5) ~ "Distilled",
    (water_type<5.5 ~ "Acidic")
  ))

#str(waterPotabilityFullData)

Hard_level <- waterPotabilityFullData['Hardness']
Hard_level <- round(Hard_level, digits = 1)
waterPotabilityFullData <- waterPotabilityFullData %>% 
  mutate(Hard_level = case_when(
    (Hard_level>=0 & Hard_level<120) ~ "Medium hard",
    (Hard_level>=120 & Hard_level<180) ~ "Hard",
    (Hard_level>180) ~ "Very Hard"
  ))

name<- names(waterPotabilityFullData)
#name
#[1] "ph"              "Hardness"        "Solids"          "Chloramines"     "Sulfate"        
#[6] "Conductivity"    "Organic_carbon"  "Trihalomethanes" "Turbidity"       "Potability"

waterPotabilityFullData <-  waterPotabilityFullData %>% select(everything()) %>%mutate(across(c("Potability","water_type","Hard_level"), factor))
#waterPotabilityFullData

# Define server 
shinyServer(function(input, output, session) {
  
  # Reactive value for data Table
  datasetInput<- reactive({
    
    if(length(input$dtsetvar) != 0){
      dtFull<- waterPotabilityFullData %>% dplyr::select(!!!input$dtsetvar)
      return(dtFull) 
    }
    
    else
      return(waterPotabilityFullData)
    
  })
  
  #Table of selected dataset
  
  output$dtset<- renderDataTable({
    datasetInput()
  }, filter = "top")
  
  # Downloadable csv of selected dataset 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("WaterQualityData", ".csv", sep = "")
    },
    content = function(file) {
      #setClass(data.frame(datasetInput()))
      # class(datasetInput())<- class(as.tibble(datasetInput()))
      write.csv(as.data.frame(datasetInput()), file, row.names = FALSE)
    }
  )
  
  #******************************************************#
  #*Data Exploration 
  # Output numerical summary
  output$summary <- renderPrint({
    if(input$vName == "ph" || input$vName == "Hardness" || input$vName == "solids" || 
       input$vName == "Chloramines" || input$vName == "Sulfate" || 
       input$vName == "Conductivity" || input$vName == "Organic_carbon" || 
       input$vName == "Trihalomethanes" || input$vName == "Turbidity") 
      summary(waterPotabilityFullData[,input$vName])
    
    
    else if(input$vName == "Hard_level" || input$vName == "water_type") 
      table(waterPotabilityFullData[,input$vName])
    
    else if(input$vName == "Potability")
      summary(waterPotabilityFullData[,input$vName])
  })
  
  #Plot Helper function
  histplotHelper <- function() {
    ggplot(waterPotabilityFullData, aes(input$vName)) + geom_bar(aes(color = waterPotabilityFullData$potibility)) + ggtitle(input$vName)
  }
  
  output$histPlot <- renderPlot({
    if(input$vName == "ph" || input$vName == "Hardness" || input$vName == "solids" || 
           input$vName == "Chloramines" || input$vName == "Sulfate" || 
           input$vName == "Conductivity" || input$vName == "Organic_carbon" || 
           input$vName == "Trihalomethanes" || input$vName == "Turbidity") {
      histplotHelper()
    }
    
    ggplotly(ggplot(waterPotabilityFullData, aes(water_type, ph)) +
               geom_boxplot(aes('potabiliy')) +
               ggtitle("water_type vs ph "))
    
    s <- ggplot(waterPotabilityFullData, aes(ph, fill = 'potibility'))
    s + geom_bar(position = "dodge")
    s + geom_bar(position = "fill")
    
  })
  
  #Download plots
  output$histPlotDownload <- downloadHandler(
    filename =  function() {
      paste0(input$vName,".png")
    },
    content = function(file) {
      png(file) # open the png device
      print(histplotHelper())
      dev.off()  # turn the device off
      
    }
  )
  
  #***************************************************************************
  
})