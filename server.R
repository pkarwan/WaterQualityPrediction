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
name<- names(waterPotabilityFullData)
#name
#[1] "ph"              "Hardness"        "Solids"          "Chloramines"     "Sulfate"        
#[6] "Conductivity"    "Organic_carbon"  "Trihalomethanes" "Turbidity"       "Potability"

# Define server 
shinyServer(function(input, output, session) {
})