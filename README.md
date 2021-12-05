# Water Quality Prediction

## Is this water safe to drink??
The most fundamental human needs for water are for drinking, cooking and personal hygiene. Quality of the water plays very important role in these human needs. The quality of the water in nature also affects the condition of ecosystems that all living organisms depend on. At the same time, humans use water bodies as convenient to dispose of domestic, industrial and agricultural waste which sometime risks health of living organism to satisfy their day to day water needs. So it is very important to verify the water quality if this is safe to use for human, animals, trees,... Through this study, I'm proposing a machine learning models to predict quality of the water and helps user to verify if its safe to consume. Using this shiny app interface, user will will be able to browse data, perform analysis based on features available, view the graphical analysis and perform water quality prediction.

##List of Packages
####Note: please find the list of below packages needed to run this Shiny App

- `shiny`         : Shiny package makes it easy to build interactive web apps straight from R.
- `shinydashboard`: This package provides a theme on top of 'Shiny', making it easy to create attractive                      dashboards.
- `tidyverse`     : The tidyverse is an collection of R packages designed for data science.
- `dplyr`         : Dplyr provides the most common data manipulation functionalities.
- `ggplot2`       : This package is dedicated to data visualization
- `stringr`       : Stringr package provide a cohesive set of functions designed to make working with                         strings as easy as possible.
- `plotly`        : Plotly create interactive web graphics from 'ggplot2' graphs 
- `caret`         : The caret package contains functions to streamline the model training process for                         complex regression and classification problems.
- `shinycssloaders` : shinycssloaders used to automatically show loader animations while a Shiny output is                      (re)calculating. 
- `DT`            : DT package provides R interface to DataTables.


## Codes for installing packages
- `install.packages("shiny", "shinydashboard", "DT", "dplyr", "ggplot2","caret", "plotly", "tidyverse", "stringr",  "shinycssloaders")`

shiny::runGitHub("WaterQualityPrediction", "pkarwan", ref = "main")
