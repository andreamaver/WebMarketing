rm(list=ls())
#### OPTIONS ####

options(scipen=999)
set.seed(123456)

#### LIBRARIES ####
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(lubridate)
library(RQuantLib)
## add other libraries
library(rfm)
library(UBL)
library(caTools)
library(ROCR)
library(rpart)
library(pROC)
library(caret)
library(arules)
library(arulesViz)

#### DIRECTORIES ####
working_dir = "C:\\Andrea\\Università\\Data Science\\2 Web marketing\\def"
data_dir = "C:\\Andrea\\Università\\Data Science\\2 Web marketing\\Project\\DMktg_DSLab_Data_1"

setwd(working_dir)

#### EXECUTION FULL PIPELINE ####
# Uncomment to execute the entire pipeline of scripts
PIPELINE_scripts <- c(
  'B01_ingestion.R'
  , 'C01_preparation_df1.R'
  , 'C02_preparation_df2.R'
  , 'C03_preparation_df3.R'
  , 'C04_preparation_df4.R'
  , 'C05_preparation_df5.R'
  , 'C06_preparation_df6.R'
  , 'C07_preparation_df7.R'
  ## add other scripts
  , 'D01_finding_interval.R'
  , 'D02_rfm.R'
  , 'D03_churn_models.R'
  , 'D04_basket_analysis.R'
  )

for(i in PIPELINE_scripts){
  source(i, echo = TRUE)
}