# ***************************************************************
# Erick Gordón
# erick.gordon@rowsums.com
# https://www.linkedin.com/in/erickgordonb/
# R User Group Panamá
# ***************************************************************
# Date: 14-sep-2019
# ***************************************************************
library(tidyverse) 
library(DataExplorer)
library(janitor)
# ***************************************************************
# EDA - Exploratory data analysis

# Linear regression example
climate_tbl <- readr::read_csv("./Data/climate_change.csv")
climate_tbl <- climate_tbl %>%
  clean_names()

climate_tbl %>%
  glimpse()

table(climate_tbl$year)
summary(climate_tbl)

climate_tbl %>% 
	gather(key, value,2:11) %>% 
	summary_col_by_group(key, col = value)
