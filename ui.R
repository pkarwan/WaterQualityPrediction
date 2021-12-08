#install.packages(c("shiny", "shinydashboard", "tidyverse", "dplyr", "ggplot2", "stringr", "caret", "DT", "plotly", "tree", "rattle", "randomForest" ))

library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(caret)
library(DT)
library(plotly)
library(shinycssloaders)

#Load data
waterPotabilityFullData<- as_tibble(read.csv("./data/water_potability.csv"))
waterPotabilityFullData <- waterPotabilityFullData %>% na.omit(waterPotabilityFullData)
#waterPotabilityFullData <- scale(waterPotabilityFullData)
#head(waterPotabilityFullData)

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
    (Hard_level>=0 & Hard_level<17.1) ~ "Soft",
    (Hard_level>=17.1 & Hard_level<120) ~ "Medium hard",
    (Hard_level>=120 & Hard_level<180) ~ "Hard",
    (Hard_level>180) ~ "Very Hard"
  ))

#str(waterPotabilityFullData)

name<- names(waterPotabilityFullData)

waterPotabilityFullData <-  waterPotabilityFullData %>% select(everything()) %>%mutate(across(c("Potability","water_type","Hard_level"), factor))
str(waterPotabilityFullData)

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Water Quality Prediction"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("clipboard")),
        menuItem("Data", tabName = "data", icon = icon("database")),
        menuItem("Data Exploration", tabName = "dataexp", icon = icon("search",lib = "glyphicon")),
        menuItem("Modeling", tabName = "modeling", icon = icon("signal",lib = "glyphicon"))               
      )),
    
    #Data Exploration page
    dashboardBody(
      tabItems(
        tabItem(tabName = "dataexp",
                fluidRow( h2("3. Data Exploration"),br(), br(),
                          h4("  As we will be focusing on the the prediction of water quality, the data exploration
                                      will concentrate on potability column from the data table. The data table for the highest user ratings 
                                      shown for the demonstration purpose only, which can be dowloaded from the 
                                      `Data` tab. Therefore download option is not provided here. 
                                      "),
                          br(), br(),
                          
                      box(selectInput("vName", "Select the Variable", 
                                  selected = "ph",
                                  choices = colnames(waterPotabilityFullData))
                ),
                
                box(
                  h4("Summary"),
                  verbatimTextOutput("summary"),
                  br())),
                
                fluidRow( tags$div( withSpinner(plotOutput("histPlot", height = 200),  type = getOption("spinner.type", default = 6),
                                        color = getOption("spinner.color", default = "#FFA500")), 
                            downloadButton(outputId = "histPlotDownload", label = "Download Plot")),
                  
                
                ) , 
                br(),br(),
                
                fluidRow(
                # For conditional output
                tags$div(
                box(selectInput("varPlot", "Select the Type of plot",
                                selected = "Scatter Plot",
                                choices = c("Scatter Plot"="scatter", "Box Plot"="box", "Histogram"="hist")
                )),
                # Only show this panel if the plot type is a scatter
                conditionalPanel("input.varPlot == 'scatter'",
                                 plotOutput("scatterPlot", height = 200),
                                 
                ),
                # Only show this panel if the plot type is a hist
                conditionalPanel("input.varPlot == 'hist'",
                                 plotOutput("hist", height = 200)
                ),
                # Only show this panel if the plot type is a box
                conditionalPanel("input.varPlot == 'box'",
                                 plotOutput("box", height = 200)
                ),
                br(),br(),br(),br(),br(),br(),
                downloadButton(outputId = "condPlotDownload", label = paste0("Download Plot"))
                
                )),
                
                # Summary for Categorical Data
                fluidRow(
                  h4("Please find below details for the Water type, Hard level and Potability"),
                  br(),
                  div(em("Water Type ",": ",  "Alkaline, ", "Acidic, ", "Bottled, ", "Distilled, ","Seawater, ", "Tap ")),
                  div(em("Hard level" ,": ",  "Hard, ", "Medium Hard, ", "Very Hard, ")),
                  div(em("Potability" ,": ",  "0 = Not Safe to drink, ", "1 = Safe to drink")),
                  
                  box(tableOutput("tbl1")),
                  box(tableOutput("tbl2"))
                )
                
                ),
        
        #Modeling Page
        tabItem(tabName = "modeling",
                fluidPage(
                  fluidRow(
                    tabsetPanel(
                      tabPanel("Modeling Info",
                               br(),tags$div("We will be using below three models to predict the quality of the water if its safe to consume for humans."),br(),br(),
                               tags$div(fluidRow(strong("K-Nearest Neighbours Model")),
                                        tags$div("K Nearest Neighbor algorithm(KNN) is a Supervised Learning algorithm. It is getting used for (mostly) classification and regression problems.
                                                 KNN suggests it considers **CLOSEST** K Nearest Neighbors (Data points) to predict the class or continuous value for the new Datapoint.
                                                 It often use Euclidean distance between predictors to determine closest data points and predict the class with highest probability"),
                                        br(), 
                                        fluidRow(tags$i("Advantages : ")),
                                        tags$ul("It can be used for both regression and classification"),
                                        tags$ul("It is easy to implement"),        
                                        tags$ul("It simple and effective in nature"),
                                        tags$ul("The algorithm is highly unbiased in nature and makes no prior assumption of the underlying data."),
                                        tags$ul("KNN is a Lazy Learner (Instance based learning).There is no training period for it. It stores the training dataset and learns from it only at the time of making real time predictions. 
                                                This makes the KNN algorithm much faster than other algorithms that require training."),
                                        br(),       
                                        fluidRow(tags$i("Drawbacks :")),
                                        tags$ul("In large datasets, the cost of calculating the distance between the new point and each existing points is huge which degrades the performance of the algorithm."),
                                        tags$ul("The KNN algorithm doesn't work well with high dimensional data because with large number of dimensions, it becomes difficult for the algorithm to calculate the distance in each dimension."),
                                        tags$ul("We need to do feature scaling before applying KNN algorithm to any dataset."),
                                        tags$ul("KNN is sensitive to noise in the dataset. Need to manually impute missing values and remove outliers."),
                                        ),br(),br(),
                               
                               # Boosted Model
                               tags$div(fluidRow(strong("Boosted Decision Tree Model")),
                                        tags$div("Boosting is one of the ensemble technique to create a collection of predictors.Approach to use boosting algorithm - Trees grows sequentially. Each sequence tree is grown on a modified version of original data.
                                                 Prediction updated as trees grown. This can be applied by initializing prediction as zero. Then find the residuals.Fit the tree with d splits, treating residuals as response. 
                                                 Then update the predictions and update residuals for new predictions and repeat number of times."),
                                        withMathJax(),
                                        
                                        tags$div(
                                        h3("Formula to update predictions of Boosted Decision Tree Model"),
                                        span("y-hat(x) = y-hat (x)+ λy^b(x)"),br(),
                                        span("λ")," is a shrinkage parameter,",br(),
                                        span("y^b(x)")," Treating the residuals as the response (which they are for the first fit),",br(),
                                        span("y-hat(x)")," predictions"
                                        ),br(),
                                        
                                        fluidRow(tags$i("Advantages : ")),
                                        tags$ul("Easy to tune"),
                                        tags$ul("The features can be a mix of categorical and continuous data"),
                                        tags$ul("Training on the residuals gives very good accuracy"),br(),
                                        fluidRow(tags$i("Drawbacks :")),
                                        tags$ul("Sensitive to overfitting"),
                                        tags$ul("slow training of trees"),
                                        tags$ul(),
                                        tags$ul(),
                                        ),br(),br(), 
                               
                               # Random Forest Model
                               tags$div(fluidRow(strong("Random Forest Model")),
                                        tags$div("A random forest is a supervised machine learning algorithm that is constructed from decision tree algorithms and also uses Ensemble Learning technique. It is used to solve regression and classification problems.
                                                 The random forest algorithm establishes the outcome based on the predictions of the decision trees. 
                                                 It predicts by taking the average or mean of the output from various trees. Increasing the number of trees, reduces overfitting of data and increases the accuracy of the outcome."),br(),
                                        fluidRow(tags$i("Advantages : ")),
                                        tags$ul("Random Forest works well with both categorical and continuous values"),
                                        tags$ul("It reduces overfitting in decision trees and helps to improve the accuracy"),
                                        tags$ul("It automates missing values present in the data"),
                                        tags$ul("Normalising of data is not required as it uses a rule-based approach."),
                                        tags$ul("It is flexible to both classification and regression problems"),br(),
                                        fluidRow(tags$i("Drawbacks :")),
                                        tags$ul("Random Forest Model requires much computational power as well as resources as it builds numerous trees to combine their outputs. "),
                                        tags$ul("It also requires much time for training as it combines a lot of decision trees to determine the class."),
                                        tags$ul("Due to the ensemble of decision trees, it also suffers interpretability and fails to determine the significance of each variable"),
                                        )
                      ),
                      tabPanel("Model Fitting",
                               
                               ),
                      tabPanel("Prediction")
                    )
                  )
                )
                
          
        ),
        
        #Dataset page
        
        tabItem(tabName = "data",
                fluidRow( h2("2. Dataset"), br(), br(),
                          h4("The intention of this page is to allow the user navigate throught the whole
                                    data set and play around to choose the attributes as per the requirement of
                                    user. Here you can also subset the data set and downoload as '.csv' file."), br(),
                          
                ),
                tags$div( 
                  br(),
                  h4("Select options below to subset the data"),
                  br(),
                  varSelectInput("dtsetvar", "Select variables to subset", waterPotabilityFullData, multiple = T)
                ),
                br(),
                br(),
                tags$div(downloadButton("downloadData", "Download"),br(),
                         dataTableOutput("dtset") 
                         ),br(),
                ),
        
        
        
        # About page            
        tabItem(tabName = "about",
                h2("About"),
                img(src = "https://images.squarespace-cdn.com/content/v1/583ca2f2d482e9bbbef7dad9/1485814003621-5AKV9QXZDBZTH4S1CE6L/iStock-1538821421900.jpg", width = "70%"),
                h3("Purpose of the App"),
                h4("Our goal is to predict the water quality. To achieve this goal, we will be to fit the models and measure the statistics, how accurately can it be predicted the water quality. We will be using  the Random Forest model, logistic Regression model and
                         Linear Regression model and choose the best one to predict the water quality."),
                br(),
                br(),   
                
                h3("Navigate Through the App: This app contains 4 different components"),
                h3("1. About"), 
                h4("This page describes purpose of the app, purpose of each tab (page) of the app and information about the data"),
                
                h3("2. Data"),
                h4("In this page user can scroll through the data, subset it and save the data as a '.csv' file."),
                
                h3("3. Data Exploration"),
                h4("In this page displays numerical and graphical summaries. User can download plot, change type of plot and type of summary reported, change the variable and filter the rows to change the data in the plots/summaries"),
                
                
                h3("4. Modeling"),
                h4("This page will display three supervised learning models - multiple linear regression or generalized linear regression model, regression or classification tree, and a random forest model.This page contains three tabs :"),
                em(h4(strong("I. Modeling Info tab :"))),
                h4("This tab displays three modeling approaches, the benefits of each, and the drawbacks of each."),
                
                em(h4(strong("II. Model Fitting tab :"))),
                h4("User can split the data and choose the proportion of data used in each training and testing data set. User can choose model settings for each model. The models get compared on the test set and appropriate fit statistics reported."),
                em(h4(strong("III. Prediction tab :"))),
                h4("User can choose one of the models for prediction. User can select the values of the predictors and obtain a prediction for the response."),
                br(),
                br(),
                
                
                
                h2("Data Set Information"),
                h4("This data set is downloaded from ", 
                   a(href= "https://www.kaggle.com/adityakadiwal/water-potability", "kaggle"),
                   "Access to safe drinking-water is essential to health, a basic human right and a component of effective policy for health protection. This is important as a health and development issue at a national, regional and local level. In some regions, it has been shown that investments in water supply and sanitation can yield a net economic benefit, since the reductions in adverse health effects and health care costs outweigh the costs of undertaking the interventions.
						   This dataset describes if water is safe to consume as a drinking water based on water-potability."),
               br(),
               
               h3("Attribute Information"),
               h4("The water_potability.csv file contains water quality metrics for 3276 different water bodies."),
               br(),
        
               h4("1 - pH value", "- PH is an important parameter in evaluating the acid–base balance of water. It is also the indicator of acidic or alkaline condition of water status. WHO has recommended maximum permissible limit of pH from 6.5 to 8.5. The current investigation ranges were 6.52–6.83 which are in the range of WHO standards.", br(), br(), 
                  "2 - Hardness", "- Hardness is mainly caused by calcium and magnesium salts. These salts are dissolved from geologic deposits through which water travels. The length of time water is in contact with hardness producing material helps determine how much hardness there is in raw water. Hardness was originally defined as the capacity of water to precipitate soap caused by Calcium and Magnesium.", br(),br(), 
                  "3 - Solids (Total dissolved solids - TDS)", "- Water has the ability to dissolve a wide range of inorganic and some organic minerals or salts such as potassium, calcium, sodium, bicarbonates, chlorides, magnesium, sulfates etc. These minerals produced un-wanted taste and diluted color in appearance of water. This is the important parameter for the use of water. The water with high TDS value indicates that water is highly mineralized. Desirable limit for TDS is 500 mg/l and maximum limit is 1000 mg/l which prescribed for drinking purpose.", br(), br(),
                  "4 - Chloramines", "- Chlorine and chloramine are the major disinfectants used in public water systems. Chloramines are most commonly formed when ammonia is added to chlorine to treat drinking water. Chlorine levels up to 4 milligrams per liter (mg/L or 4 parts per million (ppm)) are considered safe in drinking water.", br(), br(),
                  "5 - Sulfate","- Sulfates are naturally occurring substances that are found in minerals, soil, and rocks. They are present in ambient air, groundwater, plants, and food. The principal commercial use of sulfate is in the chemical industry. Sulfate concentration in seawater is about 2,700 milligrams per liter (mg/L). It ranges from 3 to 30 mg/L in most freshwater supplies, although much higher concentrations (1000 mg/L) are found in some geographic locations.", br(),br(), 
                  "6 - Conductivity","- Pure water is not a good conductor of electric current rather’s a good insulator. Increase in ions concentration enhances the electrical conductivity of water. Generally, the amount of dissolved solids in water determines the electrical conductivity. Electrical conductivity (EC) actually measures the ionic process of a solution that enables it to transmit current. According to WHO standards, EC value should not exceeded 400 μS/cm.", br(), br(),
                  "7 - Organic_carbon", "- Total Organic Carbon (TOC) in source waters comes from decaying natural organic matter (NOM) as well as synthetic sources. TOC is a measure of the total amount of carbon in organic compounds in pure water. According to US EPA < 2 mg/L as TOC in treated / drinking water, and < 4 mg/Lit in source water which is use for treatment.", br(),br(),
                  "8 - Trihalomethanes", "- THMs are chemicals which may be found in water treated with chlorine. The concentration of THMs in drinking water varies according to the level of organic material in the water, the amount of chlorine required to treat the water, and the temperature of the water that is being treated. THM levels up to 80 ppm is considered safe in drinking water.", br(), br(),
                  "9 - Turbidity", "- The turbidity of water depends on the quantity of solid matter present in the suspended state. It is a measure of light emitting properties of water and the test is used to indicate the quality of waste discharge with respect to colloidal matter. The mean turbidity value obtained for Wondo Genet Campus (0.98 NTU) is lower than the WHO recommended value of 5.00 NTU.", br(), br(),
                  "10 - Potability", "- Indicates if water is safe for human consumption where 1 means Potable and 0 means Not potable."),                                                         
                  br(), br(), br(),
        
        br(),
                )
        

    )
  )
)
)
