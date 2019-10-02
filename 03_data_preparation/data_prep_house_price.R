cat("\014")
# ****************************************************
# Created by: Erick Gordon - erick.gordon@rowsums.com
# ****************************************************
# Load libraries ----
library(tidyverse)
library(janitor)      # basic manipulation 
library(DataExplorer) # general data explorer
library(recipes)
library(h2o) 
# ****************************************************
PATH_IN <- "./00_data/in/"
PATH_OUT <- "./00_data/out/"
# ****************************************************
# Load data ----
raw_train_tbl <- readr::read_csv(paste0(PATH_IN, "train.csv"))
raw_test_tbl <- readr::read_csv(paste0(PATH_IN, "test.csv"))

raw_train_tbl <- raw_train_tbl %>% 
	clean_names()

raw_test_tbl <- raw_test_tbl %>% 
	clean_names()

raw_train_tbl %>% 
	glimpse()

# ****************************************************

raw_train_tbl %>% 
	plot_missing()




