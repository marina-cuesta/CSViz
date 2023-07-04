#######################################################################
#######################################################################
######   SCRIPT TO OBTAIN THE CSViz RESULTS FOR THE DATA SETS    ######
#######################################################################
#######################################################################

## path of the project
path=getwd()

## loading necessary scripts
 # to load packages
source(file=paste0(path,"/src/packages_loading.R"))
source(file=paste0(path,"/src/packages_data_loading.R"))
 # to load the functions
source(file=paste0(path,"/src/functions.R"))

## path to save the results
path_save=paste0(path,"/results/")


###################################
######   READING DATA SET    ######
###################################

###### SELECT ONE of the following datasets and uncomment the corresponding line
data_name="hepatitis"
# data_name="wine"
# data_name="parkinsons"
# data_name="sonar"
# data_name="chatfield_4"
# data_name="fri_c0_250_50"
# data_name="bodyfat"
# data_name="spectfheart"
# data_name="penguins"
# data_name="ionosphere"
# data_name="musk1"
# data_name = "uniformly distributed"
# data_name = "emotions_amazed.surprised"
# data_name = "emotions_relaxing.calm"
# data_name = "emotions_quiet.still"
# data_name = "emotions_sad.lonely"
# data_name = "emotions_angry.aggresive"
# data_name = "breastcancer" # unique_values_factor=8
# data_name = "pima"
# data_name = "analcatdata_authorship"
# data_name = "spirals"
# data_name = "tiger"
# data_name = "elephant"
# data_name = "pc4"
# data_name = "MI_class9"
# data_name  ="steel-plates-fault"
# data_name = "madelon" #   unique_values_factor=2
# data_name = "kc1"
# data_name = "scene_beach"
# data_name = "scene_sunset"
# data_name = "scene_fallfoliage"
# data_name = "scene_field"
# data_name = "yeast_class1"
# data_name = "yeast_class2"
# data_name = "yeast_class5"
# data_name = "spambase"
# data_name = "waveform-5000"
# data_name = "Satellite"
# data_name = "sylvine"
# data_name = "clean2"
# data_name = "thyroid"
# data_name = "magic" #reduced dataset
# data_name = "mv" #reduced dataset
# data_name = "jannis" #reduced dataset
# data_name = "albert" #reduced dataset
# data_name = "asteroid" #reduced dataset
# data_name = "MiniBooNE" #reduced dataset
# data_name = "image_testing" #reduced dataset
# data_name = "creditcard" #reduced dataset
# data_name = "Higgs" #reduced dataset


## reading the corresponding dataset
source(file=paste0(path,"/src/data_reading.R"))

## overview of the dataset
dim(data)
str(data)



##############################################
######   PREPARING DATA FOR FUNCTION    ######
##############################################

## selecting variables with few NA values
 # the maximum percentage of NA values is 15%
threshold_na_values=15
 # computing which variables meet the condition of having less Na values than the threshols
cond_variables=(colMeans(is.na(data))*100)<threshold_na_values
# selecting those variables
data=data[,cond_variables]
dim(data)

# Dividing data into dataX and dataY
dataX = data %>%
  dplyr::select(-class)
dataY = data %>%
  dplyr::mutate(class=factor(class)) %>% 
  pull(class)



############################################
######   COMPUTING CSViz SUBSPACES    ######
############################################

## Parameters of the function
k=0.05
unique_values_factor=10
trunc_data_elimination=T
scale_data=TRUE

## function to compute the subspaces
results_CSViz_subspaces=CSViz_subspaces_computation(dataX, dataY, k=k, 
                                                    unique_values_factor=unique_values_factor,
                                                    trunc_data_elimination=trunc_data_elimination,
                                                    scale_data=scale_data)


#######################################################
######   VISUALIZATION OF THE CSViz SUBSPACES    ######
#######################################################

## Parameters of the visual function
min_data=0.05
max_kdn=1

## Detailed information table of the CSViz subspaces
results_CSViz_table_info=CSViz_table_info(results_CSViz_subspaces,min_data,max_kdn)
results_CSViz_table_info
 # number of subspaces meeting min_data and max_kdn conditions
n_subspaces_conditions=dim(results_CSViz_table_info$body$dataset)[1]
 # saving the table as png in the results folder
table_name=paste0(path_save,data_name,"_CSViz_table_info.png")
flextable::save_as_image(results_CSViz_table_info, path = table_name)


## Plots and information tables of the CSViz subspaces
results_CSViz_plots_info=CSViz_plots_info(results_CSViz_subspaces,min_data,max_kdn)
 # saving the plot in the results folder
filename=paste0(data_name,"_CSViz_plots_info.png")
ggplot2::ggsave( filename,
                 plot = results_CSViz_plots_info,
                 device = png(),
                 path = path_save,
                 width = 20,
                 height =n_subspaces_conditions*10,
                 units = "cm",
                 dpi = 300,  limitsize = TRUE,  bg = NULL)


## Storytelling plots of the CSViz subspaces
results_CSViz_storytelling=CSViz_storytelling(results_CSViz_subspaces,min_data,max_kdn)
 # saving the plot
filename=paste0(data_name,"_CSViz_plots_storytelling.png")
ggplot2::ggsave( filename,plot = results_CSViz_storytelling,
        device = png(),
        path = path_save,
        width = 20,
        height = n_subspaces_conditions*10,
        units = "cm",
        dpi = 300,  limitsize = TRUE,  bg = NULL)


