
## list of installed packages
installed_packages=installed.packages()

## list of packages to use
list.of.packages <- c(

  ##  data manage
  "dplyr", "tidyverse",
  
  # combinations
  "gtools",
  
  ## to parallel
  "foreach",
  "parallel",
  "doParallel",
  
  ## for knn
  "dbscan",
  
  ## for rollmeanr
  "zoo",
  
  #color blind palette
  "grafify",
  
  ## graphs
  "ggplot2",
  
  ## for tables
  "flextable",
  
  # for plots arranging
  "gridExtra",
  "cowplot",
  
  # for ordinal numbers
  "english"
  )


## list of packages to use that are not installed yet
new.packages <- list.of.packages[!(list.of.packages %in% installed_packages)]

## installing the packages if needed
if(length(new.packages)) install.packages(new.packages)


## loading  the packages 
for (package in list.of.packages) {
  library(package, character.only=T)
}
