# Libraries ----
library(rio);
library(DataExplorer);
# Data Import ----
DataFile <- "veteran.xlsx"
DataScript <- "R_step_by_step.R"
Dat <- if(file.exists(DataFile)){
  import(DataFile)
} else{
  # source(DataScript)
  system(paste("R -f", DataScript), wait = TRUE)
  import(DataFile)
}
create_report(Dat)
