library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(readxl)
library(gtools)
library(shinyWidgets)
library(gridExtra)

# Import Data
GolfData <- read_csv("totalnew.csv")

# gain <- read_csv("gained_putting.csv")
# GolfData <- GolfData %>% 
#   filter(., Round == "Competitive" | Round == "Qualifying" | Round == "Tournament") %>% 
#   merge(., gain[, c("PuttLength", "Exp_Putts")], by = "PuttLength") %>% 
#   mutate(., strokesgained = Exp_Putts - Putts) %>% 
#   mutate(., FIR = recode(FIR, "Miss" = "Missed")) 
# GolfData = GolfData[c(2, 12, 3:11, 13, 14, 1)]


button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"