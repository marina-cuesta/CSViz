# CSViz: Class Separability Visualization for High-Dimensional Datasets

This repository is the official implementation of the CSViz method. 

## Abstract
Data visualization is an essential task during the lifecycle of any Data Science project, particularly during the Exploratory Data Analysis for a correct preparation and understanding of the data. In classification problems, data visualization is useful for revealing the existence of class separability patterns within the dataset. This information is very valuable and it can be later used during the process of building a Machine Learning model. High-Dimensional Data arises as one of the biggest challenges in Data Science.  They require special treatment since traditional visualization techniques, such as the scatterplot matrix, have limitations when dealing with them due to space restrictions. Other visualization methods involve dimensionality reduction techniques, which can lead to important information losses and lack of interpretability. In this paper, the CSViz method is introduced as a new Visual Analytics approach to address the challenge of visualizing High-Dimensional Data using subspaces. The proposed method enables a general overview of the class separability offering a series of 2-Dimensional subspaces visualizations containing exclusive subsets of the original points that encompass the most valuable and significant separable patterns.  The proposal is tested over 50 datasets of different characteristics providing promising results. In all cases, more than 90\% of the data observations are shown with three or less plots. Hence, it significantly eases the Exploratory Data Analysis phase by reducing the number of plots to be inspected in a scatterplot matrix and thus, the amount of time invested in it.


## Requirements

The CSViz method has been implemented using the R software version 4.2.1.


## Folders

* src: This folder contains the code to reproduce the results and to apply the CSViz method to any new dataset if desired.

* data: This folder contains the datasets used to illustrate and evaluate the effectiveness of  the method. 

* results: This folder contains the CSViz results for the 50 datasets.



## Reproducing the results

In the src folder, open CSViz_main.R. Choose the dataset you want to get the CSViz results from, uncomment the corresponding line ```data_name="data_name"``` and run everything. 


The parameters of each function are specified in the functions.R script at the beginning of each function implementation. These parameters can be modified in the CSViz_main.R script by changing the following lines:

```
############################################
######   COMPUTING CSViz SUBSPACES    ######
############################################

## Parameters of the function
k=0.05
unique_values_factor=10
trunc_data_elimination=T
scale_data=TRUE
```

```
##################################################
######   VISUALIZING THE CSViz SUBSPACES    ######
##################################################

## Parameters of the visual function
min_data=0.05
max_kdn=1
```



## Applying the CSViz method to new data

To apply the CSViz method to new data, place the data file in the data folder. Add the lines to read them in the data_reading.R script following the structure. Add the line ```data_name="new_data_name"``` in the CSViz_main.R file and run the code. 



