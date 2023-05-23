
## list of installed packages
installed_packages=installed.packages()

## list of packages to use
list.of.packages <- c(
  
  ## to obtain data sets
  "palmerpenguins", # penguns
  "mlbench", # breastcancer
  "HDclassif", # wine
  "pdp", #pima
  "rsvd", # digits
  
  ##  to read and treat data
  "readr",
  "dplyr"
)

## list of packages to use that are not installed yet
new.packages <- list.of.packages[!(list.of.packages %in% installed_packages)]

## installing the packages if needed
if(length(new.packages)) install.packages(new.packages)


## loading  the packages 
for (package in list.of.packages) {
  library(package, character.only=T)
}

