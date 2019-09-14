# ***************************************************************
# Erick Gordón 
# erick.gordon@rowsums.com
# https://www.linkedin.com/in/erickgordonb/
# ***************************************************************
# libraries
library(tidyverse)
library(janitor)
library(DataExplorer)
# ***************************************************************
# functions ----

summary_col_by_group <- function(data, ..., col, alpha = 0.05) {
	grouping_vars <- quos(...)
	col_var <- enquo(col) 
	
	ret <- data %>% 
		group_by(!!! grouping_vars) %>% 
		summarize(Count=n(), Total=sum(!! col_var, na.rm = TRUE), 
			      Min  = min(!! col_var, na.rm = TRUE), 
			      Max  = max(!! col_var, na.rm = TRUE), 
			      SD   = sd( !! col_var, na.rm = TRUE), 
			      Q1   = quantile(!! col_var, probs = c(0.25), na.rm = TRUE),
            Mean = mean(!! col_var, na.rm = TRUE), Median=median(!! col_var, na.rm = TRUE), 
			      #Skewed = skewness(!! col_var, na.rm = TRUE), 
			      Q3   = quantile(!! col_var, probs = c(0.75), na.rm = TRUE),
			      IQR  = IQR(!! col_var, na.rm = TRUE),
			      SE   = Mean/sqrt(Count), 
            t    = qt(p=(1 - (alpha/2)), df=(Count - 1)),
            LS   = Mean + (t  * SE), 
			      LI = Mean - (t  * SE)) %>% 
		ungroup() %>% 
		as_tibble() 
	
	return(ret) 
}

