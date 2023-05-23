######################################
######################################
######   SCRIPT TO READ DATA    ######
######################################
######################################

## path of the project
path=getwd()


###################################
######   READING DATA SET    ######
###################################

## hepatitis dataset
if(data_name=="hepatitis"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data)
  # renaming target variable to "class"
  names(data)[length(data)]="class"
}


## wine dataset
if(data_name=="wine"){
  data(wine)
  data=wine
  # renaming target variable to "class"
  names(data)[1]="class"
}


## parkinsons dataset
if(data_name=="parkinsons"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## sonar dataset
if(data_name=="sonar"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data)
  # renaming target variable to "class"
  names(data)[length(data)]="class"
}


## chatfield_4 data set
if(data_name=="chatfield_4"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## fri_c0_250_50  data set
if(data_name=="fri_c0_250_50"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## bodyfat data set
if(data_name=="bodyfat"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## spectfheart dataset
if(data_name=="spectfheart"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## penguins dataset
if(data_name=="penguins"){
  data(penguins)
  data= as.data.frame(penguins)
  # renaming target variable to "class"
  names(data)[1]="class"
  # deleting unnecessary variables
  data=data %>% dplyr::select(-c(island,sex,year))
}


## ionosphere dataset
if(data_name=="ionosphere"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## musk1 dataset
if(data_name=="musk1"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## uniformly distributed data set
if(data_name=="uniformly distributed"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data) %>%
    dplyr::mutate(class=factor(class))
}


## emotions_amazed.surprised data set
if(data_name=="emotions_amazed.surprised"){
  data_name_aux="emotions"
  path_data=paste0(path,"/data/",data_name_aux,".csv")
  data <- read.csv(path_data)
  # dataX variables
  dataX_variables=1:72
  # selecting one class variable: one in between 73 and 78
  dataY_variable=73
  data_name=paste0("emotions_",names(data)[dataY_variable])
  # selecting variables
  data=data[c(dataX_variables,dataY_variable)]
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## emotions_relaxing.calm data set
if(data_name=="emotions_relaxing.calm"){
  data_name_aux="emotions"
  path_data=paste0(path,"/data/",data_name_aux,".csv")
  data <- read.csv(path_data)
  # dataX variables
  dataX_variables=1:72
  # selecting one class variable: one in between 73 and 78
  dataY_variable=75
  data_name=paste0("emotions_",names(data)[dataY_variable])
  # selecting variables
  data=data[c(dataX_variables,dataY_variable)]
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## emotions_quiet.still data set
if(data_name=="emotions_quiet.still"){
  data_name_aux="emotions"
  path_data=paste0(path,"/data/",data_name_aux,".csv")
  data <- read.csv(path_data)
  # dataX variables
  dataX_variables=1:72
  # selecting one class variable: one in between 73 and 78
  dataY_variable=76
  data_name=paste0("emotions_",names(data)[dataY_variable])
  # selecting variables
  data=data[c(dataX_variables,dataY_variable)]
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## emotions_sad.lonely data set
if(data_name=="emotions_sad.lonely"){
  data_name_aux="emotions"
  path_data=paste0(path,"/data/",data_name_aux,".csv")
  data <- read.csv(path_data)
  # dataX variables
  dataX_variables=1:72
  # selecting one class variable: one in between 73 and 78
  dataY_variable=77
  data_name=paste0("emotions_",names(data)[dataY_variable])
  # selecting variables
  data=data[c(dataX_variables,dataY_variable)]
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## emotions_angry.aggresive data set
if(data_name=="emotions_angry.aggresive"){
  data_name_aux="emotions"
  path_data=paste0(path,"/data/",data_name_aux,".csv")
  data <- read.csv(path_data)
  # dataX variables
  dataX_variables=1:72
  # selecting one class variable: one in between 73 and 78
  dataY_variable=78
  data_name=paste0("emotions_",names(data)[dataY_variable])
  # selecting variables
  data=data[c(dataX_variables,dataY_variable)]
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}



## Breastcancer data set
if(data_name=="breastcancer"){
  data(BreastCancer)
  data=BreastCancer
  # renaming target variable to "class"
  names(data)[length(data)]="class"
  # deleting unnecessary variables
  data=data %>% dplyr::select(-c(Id))
  # converting X variables to numeric
  data = data %>%
    dplyr::mutate(across(!class, as.numeric))
}


## Pima data set
if(data_name=="pima"){
  data(pima)
  data = pima
  # renaming target variable to "class"
  names(data)[length(data)]="class"
}


## analcatdata_authorship data set
if(data_name=="analcatdata_authorship"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}



## Generated data set: spirals
if(data_name=="spirals"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data) %>%
    dplyr::mutate(class=factor(class))
  # selection of variables to consider
  var=c(1,2)
  data = data %>% dplyr::select(all_of(var),class)
}



## tiger data set
if(data_name=="tiger"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
  # converting X variables to numeric
  data = data %>%
    dplyr::mutate(across(!class, as.numeric))
  # deleting unnecessary variables
  data=data %>% dplyr::select(-c(ID.Bag))
}



## elephant data set
if(data_name=="elephant"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
  # converting X variables to numeric
  data = data %>%
    dplyr::mutate(across(!class, as.numeric))
  # deleting unnecessary variables
  data=data %>% dplyr::select(-c(ID.Bag))
}


## pc4  data set
if(data_name=="pc4"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## MI_class9  data set
if(data_name=="MI_class9"){
  data_name_aux="MI"
  path_data=paste0(path,"/data/",data_name_aux,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # changind class variables names
  names(data)[113:124]=paste0("class",1:length(113:124))
  # dataX variables
  dataX_variables=2:112
  # selecting one class variable: one in between 113 and 124
  dataY_variable=121
  data_name=paste0("MI_",names(data)[dataY_variable])
  # selecting variables
  data=data[c(dataX_variables,dataY_variable)]
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
  
}



## steel-plates-fault  data set
if(data_name=="steel-plates-fault"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}



## madelon data set
if(data_name=="madelon"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
  # converting X variables to numeric
  data = data %>%
    dplyr::mutate(across(!class, as.numeric))
}


## kc1  data set
if(data_name=="kc1"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}




## scene_beach  data set
if(data_name=="scene_beach"){
  data_name_aux="scene"
  path_data=paste0(path,"/data/",data_name_aux,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # dataX variables
  dataX_variables=1:294
  # selecting one class variable: one in between 295 and 300
  dataY_variable=295
  names(data)[dataY_variable]
  data_name=paste0("scene_",names(data)[dataY_variable])
  # selecting variables
  data=data[c(dataX_variables,dataY_variable)]
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}



## scene_sunset  data set
if(data_name=="scene_sunset"){
  data_name_aux="scene"
  path_data=paste0(path,"/data/",data_name_aux,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # dataX variables
  dataX_variables=1:294
  # selecting one class variable: one in between 295 and 300
  dataY_variable=296
  names(data)[dataY_variable]
  data_name=paste0("scene_",names(data)[dataY_variable])
  # selecting variables
  data=data[c(dataX_variables,dataY_variable)]
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## scene_fallfoliage  data set
if(data_name=="scene_fallfoliage"){
  data_name_aux="scene"
  path_data=paste0(path,"/data/",data_name_aux,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # dataX variables
  dataX_variables=1:294
  # selecting one class variable: one in between 295 and 300
  dataY_variable=297
  names(data)[dataY_variable]
  data_name=paste0("scene_",names(data)[dataY_variable])
  # selecting variables
  data=data[c(dataX_variables,dataY_variable)]
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## scene_field  data set
if(data_name=="scene_field"){
  data_name_aux="scene"
  path_data=paste0(path,"/data/",data_name_aux,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # dataX variables
  dataX_variables=1:294
  # selecting one class variable: one in between 295 and 300
  dataY_variable=298
  names(data)[dataY_variable]
  data_name=paste0("scene_",names(data)[dataY_variable])
  # selecting variables
  data=data[c(dataX_variables,dataY_variable)]
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}




## yeast_class1  data set
if(data_name=="yeast_class1"){
  data_name_aux="yeast"
  path_data=paste0(path,"/data/",data_name_aux,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # dataX variables
  dataX_variables=1:103
  # selecting one class variable: one in between 104 and 117
  dataY_variable=104
  data_name=paste0("yeast_",names(data)[dataY_variable])
  # selecting variables
  data=data[c(dataX_variables,dataY_variable)]
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}




## yeast_class2  data set
if(data_name=="yeast_class2"){
  data_name_aux="yeast"
  path_data=paste0(path,"/data/",data_name_aux,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # dataX variables
  dataX_variables=1:103
  # selecting one class variable: one in between 104 and 117
  dataY_variable=105
  data_name=paste0("yeast_",names(data)[dataY_variable])
  # selecting variables
  data=data[c(dataX_variables,dataY_variable)]
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## yeast_class5  data set
if(data_name=="yeast_class5"){
  data_name_aux="yeast"
  path_data=paste0(path,"/data/",data_name_aux,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # dataX variables
  dataX_variables=1:103
  # selecting one class variable: one in between 104 and 117
  dataY_variable=108
  data_name=paste0("yeast_",names(data)[dataY_variable])
  # selecting variables
  data=data[c(dataX_variables,dataY_variable)]
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## spambase  data set
if(data_name=="spambase"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## waveform-5000  data set
if(data_name=="waveform-5000"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## Satellite  data set
if(data_name=="Satellite"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
}


## sylvine  data set
if(data_name=="Satellite"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # renaming target variable to "class"
  names(data)[1]="class"
}


## clean2 data set
if(data_name=="clean2"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # renaming target variable to "class"
  names(data)[length(names(data))]="class"
  # deleting unnecessary variables
  data=data %>% dplyr::select(-c(X,X0))
  # converting X variables to numeric
  data = data %>%
    dplyr::mutate(across(!class, as.numeric))
}



## thyroid dataset
if(data_name=="thyroid"){
  path_data=paste0(path,"/data/",data_name,".csv")
  data <- read.csv(path_data,stringsAsFactors = T)
  # renaming target variable to "class"
  names(data)[length(data)]="class"
}


## magic reduced dataset
if(data_name=="magic"){
  path_data=paste0(path,"/data/",data_name,"_reduced.csv")
  data <- read.csv(path_data,stringsAsFactors = T)
}


## mv reduced dataset
if(data_name=="mv"){
  path_data=paste0(path,"/data/",data_name,"_reduced.csv")
  data <- read.csv(path_data,stringsAsFactors = T)
}

## jannis reduced dataset
if(data_name=="jannis"){
  path_data=paste0(path,"/data/",data_name,"_reduced.csv")
  data <- read.csv(path_data,stringsAsFactors = T)
}


## albert reduced dataset
if(data_name=="albert"){
  path_data=paste0(path,"/data/",data_name,"_reduced.csv")
  data <- read.csv(path_data,stringsAsFactors = T)
}


## asteroid reduced dataset
if(data_name=="asteroid"){
  path_data=paste0(path,"/data/",data_name,"_reduced.csv")
  data <- read.csv(path_data,stringsAsFactors = T)
}


## MiniBooNE reduced dataset
if(data_name=="MiniBooNE"){
  path_data=paste0(path,"/data/",data_name,"_reduced.csv")
  data <- read.csv(path_data,stringsAsFactors = T)
}


## image_testing reduced dataset
if(data_name=="image_testing"){
  path_data=paste0(path,"/data/",data_name,"_reduced.csv")
  data <- read.csv(path_data,stringsAsFactors = T)
}


## creditcard reduced dataset
if(data_name=="creditcard"){
  path_data=paste0(path,"/data/",data_name,"_reduced.csv")
  data <- read.csv(path_data,stringsAsFactors = T)
}

## Higgs reduced dataset
if(data_name=="Higgs"){
  path_data=paste0(path,"/data/",data_name,"_reduced.csv")
  data <- read.csv(path_data,stringsAsFactors = T)
}