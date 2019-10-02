cat('\014')
# libraries
# Created by: Erick Gordón B.
# install R base libraries 
install.packages("devtools")
install.packages("fs")				    # file system
install.packages("tidyverse")     # data manipulation
install.packages("DataExplorer")  # EDA 
install.packages("janitor")				# cleaning
install.packages("readr")         # read files
install.packages("purrr")          # Programación funcioinal 
devtools::install_github("goodekat/ggResidpanel")

write.csv(summary_tbl, "./00_data/out/file.csv")