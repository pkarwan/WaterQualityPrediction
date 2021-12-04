# Project III, ST 558
#Author: Pramodini Karwande

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


waterPotabilityFullData<- read.csv("./data/water_potability.csv")
name<- names(waterPotabilityFullData)

# Define server 
shinyServer(function(input, output, session) {
})