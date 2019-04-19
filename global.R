library(tidyverse)
library(dplyr)
library(shiny)
library(leaflet)
library(rpart)
library(ggplot2)
library(usmap)
library(plotly)

source("helperFunctions.R")
source("mapCodes.R")
source("predict.R")

loadData()
initData()
loadFitWithEMp(h1b)
loadFitNoEMp(h1b)





library(shinydashboard)
library(flexdashboard)
