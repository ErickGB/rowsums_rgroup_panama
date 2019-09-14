# ***************************************************************
# Erick Gordón 
# erick.gordon@rowsums.com
# https://www.linkedin.com/in/erickgordonb/
# ***************************************************************


library(fs)
make_project_dir <- function() {
    dir_names <- c(
        "00_data", 
        "00_scripts", 
        "01_business_understanding",
        "02_data_understanding",
        "03_data_preparation",
        "04_modeling",
        "05_evaluation",
        "06_deployment")
    dir_create(dir_names)
    dir_ls()
}
make_project_dir()
