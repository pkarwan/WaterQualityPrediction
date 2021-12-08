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


Hard_level <- waterPotabilityFullData['Hardness']
Hard_level <- round(Hard_level, digits = 1)
waterPotabilityFullData <- waterPotabilityFullData %>% 
  mutate(Hard_level = case_when(
    (Hard_level>=0 & Hard_level<120) ~ "Medium hard",
    (Hard_level>=120 & Hard_level<180) ~ "Hard",
    (Hard_level>180) ~ "Very Hard"
  ))

name<- names(waterPotabilityFullData)
name
#[1] "ph"              "Hardness"        "Solids"          "Chloramines"     "Sulfate"        
#[6] "Conductivity"    "Organic_carbon"  "Trihalomethanes" "Turbidity"       "Potability"
#[11] "water_type"      "Hard_level" 

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
    if(input$vName == "ph" || input$vName == "Hardness" || input$vName == "Solids" || 
       input$vName == "Chloramines" || input$vName == "Sulfate" || 
       input$vName == "Conductivity" || input$vName == "Organic_carbon" || 
       input$vName == "Trihalomethanes" || input$vName == "Turbidity") 
      summary(waterPotabilityFullData[,input$vName])
    
    
    else if(input$vName == "Hard_level" || input$vName == "water_type")
      table(waterPotabilityFullData[,input$vName])

    else if(input$vName == "Potability") 
      summary(waterPotabilityFullData[,input$vName])

  })
  
  
  # Categorical summaries
  output$tbl1 <- renderTable({
    table(waterPotabilityFullData$Hard_level,waterPotabilityFullData$water_type)
  })
  
  output$tbl2 <- renderTable({
    table(waterPotabilityFullData$Potability,waterPotabilityFullData$water_type)
  })
  
  #Plot Helper function
  histplotHelper <- function() {
    if(input$vName == "ph" || input$vName == "Hardness" || input$vName == "Solids" || 
       input$vName == "Chloramines" || input$vName == "Sulfate" || 
       input$vName == "Conductivity" || input$vName == "Organic_carbon" || 
       input$vName == "Trihalomethanes" || input$vName == "Turbidity") {
    ggplot(waterPotabilityFullData, aes_string(x=input$vName,
                                        fill = "water_type")) + 
      geom_histogram(position="dodge") +
      ggtitle(input$vName)
    }
    else {
      ggplot(waterPotabilityFullData, aes_string(x=input$vName, fill=input$vName)) +
        geom_bar()+
        ggtitle(input$vName)
    }
  } 
 
  output$histPlot <- renderPlot({
    histplotHelper()
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
  
  # For ConditionalPanel
  output$scatterPlot <- renderPlot({
    scatterplotHelper()
  })
  
  #Plot Helper function
  scatterplotHelper <- function() {
    ggplot(waterPotabilityFullData, aes(x=`Hardness`, y=`ph`, colour=`Potability`)) +
      geom_point(size=2, shape=23) +
      ggtitle(paste0("Hardness vs PH"))
  }

  output$hist <- renderPlot({
    plothistHelper()
  })
  
  plothistHelper <- function() {
    ggplot(waterPotabilityFullData, aes(x=`Organic_carbon`, colour=`Potability`)) +
      geom_histogram() +
      ggtitle(paste0("Organic Carbon Graph"))
  }

  output$box <- renderPlot({
    boxplotHelper()
  })
  
  boxplotHelper <- function() {
    ggplot(waterPotabilityFullData, aes(x=`Sulfate`,y=`Chloramines`, colour=`Potability`)) +
      geom_boxplot() +
      ggtitle(paste0("Sulfate vs Chloramines"))
  }
  
  #Download plots
  output$condPlotDownload <- downloadHandler(
    filename =  function() {
      paste0(input$varPlot,".png")
    },
    content = function(file) {
      png(file) # open the png device
      
      if(input$varPlot == "scatter") {
        print(scatterplotHelper())
      }
      
      if(input$varPlot == "hist") {
        print(plothistHelper())
      }
      
      if(input$varPlot == "box") {
        print(boxplotHelper())
      }

      dev.off()  # turn the device off
      
    }
  )

  
  #Update slider
  #observe({updateSliderInput(session, "bins", max = input$maxBins)})
  
  #***************************************************************************
  
  
  # Model Page
  ## Model Fitting
  
  output$runMdlOutput <- renderPrint({
    userInp <- as.data.frame(t(userInput()))
    colnames(userInp) <- input$colsForModel
    
    trainModels()
  })
    
    
  trainModels <- eventReactive(input$mdlRunButton, {
    df <- waterPotabilityFullData %>% select(-water_type,-Hard_level)
    set.seed(123)
    
    #Create train and test datasets
    train <- sample(1:nrow(df), nrow(df)*(input$dataSlider/100))
    quality_train <- df[train,]
    quality_test <- df[-train,]
    
  })
  
})
