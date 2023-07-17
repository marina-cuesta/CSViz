#############################################
#############################################
######    Delete variables with NAs    ######
#############################################
#############################################
## Function to delete variables with a higher proportion of NA values than  
 # threshold_na_values
## PARAMETERS: 
# - data is a dataframe
# - threshold_na_values is the maximum proportion of NA values allowed; must be  in [0,1]

variables_deletion_NAs <- function (data,threshold_na_values){
  
  ## Error if threshold_na_values is not between 0 and 1
  if (threshold_na_values<0 | threshold_na_values>1){
    stop('threshold_na_values must be between 0 and 1')
    return(NULL)
  }
  # computing which variables meet the condition of having less Na values than the threshold
  cond_variables=(colMeans(is.na(data)))<threshold_na_values
  # selecting those variables
  data=data[,cond_variables]
  return(data)
}

##########################################
##########################################
######    kDN complexity measure    ######
##########################################
##########################################

## KDN computation at instance, class and data set level. 
## PARAMETERS: 
# - dataX is the dataframe with the feature variables
# - dataY is the vector with the target variable. It must be a factor
# - k is the percentage of data that is going to be considered as neighbors,must be  in (0,1]
# - scale_data = {T,F} indicates if the data must be scaled before computing kdn

kdn <- function(dataX, dataY , k,scale_data=F){
  
  #######################################
  #### Checking initial requirements ####
  #######################################
  
  ## Error if the number of data in dataX is not the same as in dataY
  nx=dim(dataX)[1]
  ny=length(dataY)
  # If nx != ny, ERROR
  if (nx != ny){
    stop('dataX and dataY must have same number of data')
    return(NULL)
  }
  
  ## error if dataY is not a factor
  if(!(is.factor(dataY))){
    stop('dataY must be a dactor')
    return(NULL)
  }
  
  
  ###################################################
  #### If there is only one data point, kdn is 0 ####
  ###################################################
  if(nx==1){
    
    kdn_instance_level=0
    kdn_class_level=numeric(length(levels(dataY)))
    kdn_dataset_level=0
    
  ####################################################################    
  #### If there is more than one data point, kdn must be computed ####
  #################################################################### 
  } else{
    
    ## scaling data if indicated
    if (scale_data){
      dataX=as.data.frame(scale(dataX))
    }
    
    ## classes of dataY
    classes=levels(dataY)
    
    ## number of available data in each class
    n_data_classes=table(dataY)
    

    #####################
    ## KDN computation ##
    #####################
    
    ## number of data to be considered as neighbours in each class
    k_neighbours_classes=ceiling(n_data_classes*k)
    
    ## maximum number of k_neighbours_classes
    k_max=max(k_neighbours_classes)
    
    ## computing the knn model to find k_max neighbours for each data item
    knn_model_aux=dbscan::kNN(dataX,k=k_max)
    knn_model=knn_model_aux$id
    
    ## computing the class of each of the neighbors of each data item
    knn_model=as.vector(knn_model)
    knn_model=matrix(dataY[knn_model],ncol=k_max)
    
    ## initialize vector for knn instance and class level
    kdn_instance_level=numeric(0)
    kdn_class_level=numeric(0)
    
    ## computing the kdn at instance level and clas level by batches depending on the class
    for (class in classes){
      
      ## number of neighbours for this class 
      k_neighbours=k_neighbours_classes[class]
      
      ## data points belonging to this class
      mask_class=dataY==class
      
      ## knn results for this class
      knn_model_class=as.data.frame(knn_model[mask_class,1:k_neighbours])
      
      ## Identifying for each row, the disagreeing neighbors
      disagreeing_neighbors=knn_model_class!=class
      
      ## computing for each row, the proportion of disagreeing neighbors
      kdn_instances=apply(disagreeing_neighbors,1, sum)/k_neighbours
      
      ## adding these values to kdn_instance_level
      kdn_instance_level[mask_class]=kdn_instances
      
      ## computing kdn class level
      kdn_class_level=c(kdn_class_level,mean(kdn_instances))
    }
    
    # names of kdn_instance_level vector
    names(kdn_class_level)=classes
    
    ## computing kdn dataset level
    kdn_dataset_level=mean(kdn_instance_level)
  }
  
  
  #######################
  ## Preparing results ##
  #######################
  
  results_kdn=list()
  results_kdn$instance_level=kdn_instance_level
  results_kdn$class_level=kdn_class_level
  results_kdn$dataset_level=kdn_dataset_level
  
  return(results_kdn)
  
  
}



################################################
################################################
######   Detecting increasing tendency    ######
################################################
################################################

## Function to detect if a series has a an increasing tendency and, if so, the index where it starts
## PARAMETERS:
# - series is the vector with data series to examine

detect_increasing_tendency=function(series){
  ## length of series
  n_data=length(series)
  
  ## compute the smooth of the series
  n_smooth=3
  series_smooth=zoo::rollmeanr(series,n_smooth,fill=NA)
  
  ## compute steps
  series_steps=lead(series_smooth)-series_smooth
  # compute the smooth of the steps
  series_steps_smooth=zoo::rollmeanr(series_steps,n_smooth,fill=NA)
  
  ## identifying positive steps
  ix_positive_steps=which(series_steps_smooth>0)
  
  ## if there is not any positive steps, then series does not have an increasing tendecy and ix_return is Inf 
  if(length(ix_positive_steps)==0){
    ix_return=Inf
  
  ## if there are positive steps, we compute is the tendency is significantly increasing and the ix where it starts
  } else{
    ix_return=Inf
    ## if tehre is not any positive steps, then series does not have an increasing tendecy and ix_return is Inf #threshold for assuming an increasing tendency
    tendency_threshold=0.7
    ## checking if the tendency is significantly increasing 
    for (ix in ix_positive_steps){
      ## number of positive steps from ix position until the end
      n_positive_steps=sum(series_steps_smooth[ix:n_data]>0,na.rm=TRUE)
      ## number of data from ix position until the end
      n_data_ix=length(na.omit(series_steps_smooth[ix:n_data]))
      ## if number of positive steps is great or equal than tendency_threshold*number of remaining data,
      # we assume that there is an increasing tendency
      if (n_positive_steps>=round(n_data_ix*tendency_threshold,0)){
        ix_return=ix
        break
      }
    }
  }
  return(ix_return)
}



############################################################################
############################################################################
######   Extracting a separable subset of points of a 2D  subspace    ######
############################################################################
############################################################################

## Function to extract a separable subset of points out of 2D subspace which can be a 
# data set restricted to two features variables. The subset is obtained deleting data points.
## PARAMETERS:
# - dataX_2D is the 2D data frame with the features variables
# - dataY is the vector with the label variable. It must be a factor
# - k is the percentage of data to be considered in the kdn computation; must be in (0,1])
# - trunc_data_elimination = {T, F} to indicate if data elimination must be truncated so that a class is not fully deleted

extract_separable_subset_2D <- function(dataX_2D, dataY, k, 
                                  trunc_data_elimination){ 
  
  #######################################
  #### Checking initial requirements ####
  #######################################
  
  ## Error if dataX_2D and dataY don't have same number of data
  if (dim(dataX_2D)[1]!=length(dataY)) {
    stop("dataX_2D and dataY must have same number of data")
  }
  
  ## Error if there is no data
  if (dim(dataX_2D)[1]==0) {
    stop("dataX_2D and dataY are empty")
  }
  
  ## Error if dataY is not a factor
  if(!(is.factor(dataY))){
    stop('dataY must be a dactor')
    return(NULL)
  }
  
  
  #################################
  ##### Data charactheristics ##### 
  #################################
  
  ## number of points
  n=dim(dataX_2D)[1]
  
  ## classes in data set
  classes=levels(dataY)
  
  ## number of classes
  n_classes=length(classes)
  
  
  ###########################################
  ##### Deleting data points iteration  #####
  ###########################################
  
  ## vector to track and store KDN data set level values 
  vector_kdn_dataset=numeric(0)
  
  ## matrix to track and store KDN class level values 
  matrix_kdn_class=NULL
  
  ## vector to track and store the index of the deleted data points 
  ix_deleted=numeric(0)
  
  ## Computing KDN with the current dataset
  results_kdn=kdn(dataX=dataX_2D,dataY=dataY,k=k,scale_data=F)
  kdn_dataset=results_kdn$dataset_level
  
  ## Filling vector_kdn_dataset with the current value
  vector_kdn_dataset[1]=kdn_dataset
  ## Filling matrix_kdn_class with the current values
  matrix_kdn_class=rbind(matrix_kdn_class,results_kdn$class_level)
  
  ## indexes of the original data before manipulating them
  ix_data_points=1:n
  
  ## compute stoping condition: no data or kdn=0
  stop_condition=length(dataY)==0|kdn_dataset==0
  
  iter=1
  while(!stop_condition){
    ###########################################################
    ##### Deleting the data point with maximum KDN value  #####
    ###########################################################
    
    ## KDN instance level value of the current dataset
    kdn_instance_level_values=results_kdn$instance_level
    
    ## Finding the index of the data point with maximum KDN instance level
    max_kdn_instance_level=sort(kdn_instance_level_values,decreasing = T)[1]
    ix_max_kdn_instance_level=order(kdn_instance_level_values,decreasing = T)[1]
    
    ## computing the threshold to decide whether or not to delete the data point
    mean_kdn=mean(kdn_instance_level_values)
    sd_kdn=sd(kdn_instance_level_values)
    kdn_deleting_threshold=mean_kdn+2*sd_kdn
    
    #### If max(kdn) is smaller than kdn_deleting_threshold, break ####
    if(max_kdn_instance_level < kdn_deleting_threshold){
      break
    }
    
    #### If max(kdn) is bigger than kdn_deleting_threshold, the data point is deleted ####
    else{
      ## Deleting from dataX_2D the data point with that index
      dataX_2D=dataX_2D[-ix_max_kdn_instance_level,]
      
      ## Deleting from dataY the data point with that index
      dataY=dataY[-ix_max_kdn_instance_level]
      
      ## Filling the ix_deleted vector with the deleted index on this iteration 
      ix_deleted=c(ix_deleted,ix_data_points[ix_max_kdn_instance_level])
      
      ## Deleting from ix_data_points the deleted ix data points to
      #  update the remaining data points in the subspace
      ix_data_points=ix_data_points[-ix_max_kdn_instance_level]
      
      ## KDN of the current dataset after the deleting of the data point
      results_kdn=kdn(dataX=dataX_2D,dataY=dataY,k=k,scale_data=F)
      kdn_dataset=results_kdn$dataset_level
      
      ## Filling vector_kdn_dataset with the current value
      vector_kdn_dataset[iter+1]=results_kdn$dataset_level
      
      ## Filling matrix_kdn_class with the current values
      matrix_kdn_class=rbind(matrix_kdn_class,results_kdn$class_level)
      
    }
    
    ## re computing the stopping condition. no data or kdn=0
    stop_condition=(length(dataY)==0)|(kdn_dataset==0)
    iter=iter+1
  }
    
  
  ##################################################
  ##### Data deleting truncation if indicated  #####
  ##################################################
  
  ## If trunc_data_elimination parameter is TRUE, we truncate the number of data points deleted
  # when the KDN of one class starts increasing. This avoids deleting a whole class
  
  #### If trunc_data_elimination parameter is TRUE ####
  if (trunc_data_elimination){
    
    #### If there is at least one data point deleted, the truncation is performed if needed ####
    if(length(ix_deleted)>=1){
      # Detecting for each class when their KDN starts increasing
      ix_truncate_candidates=numeric(n_classes)
      for(class in 1:n_classes){
        kdn_class=matrix_kdn_class[,class]
        ix_class=detect_increasing_tendency(kdn_class)
        ix_truncate_candidates[class]=ix_class
      }
      
      ## The data ix to truncate results is the minimum of the candidates
      ix_truncate=min(ix_truncate_candidates)
      ## If the minimum is Inf (increasing never happen), then no truncation is done
      if(ix_truncate==Inf){
        ix_truncate=length(ix_deleted)
      }
      
      ## we truncate the ix_deleted by the detected ix
      ix_deleted=ix_deleted[1:ix_truncate]
      
      ## we update the ix_data_points also
      ix_data_points=(1:n)[-ix_deleted]
      
      ## Getting the kdn of subspace
      kdn_subspace=vector_kdn_dataset[ix_truncate+1]
      
      
      
      #### If there is not any data point deleted, everything remains as it is ####
    } else if (length(ix_deleted)==0){
      ix_truncate=length(ix_deleted)
      kdn_subspace=vector_kdn_dataset[ix_truncate+1]
    }
    
  
  #### If trunc_data_elimination parameter is FALSE, everything remains as it is ####
  } else{
    ix_truncate=length(ix_deleted)
    kdn_subspace=vector_kdn_dataset[ix_truncate+1]
  }
    
    
    
  ########################################
  ##### Preparing results to return  ##### 
  ########################################
  
  results_separable_subspace=list()
  # ix data points that belong to the subspace
  results_separable_subspace$ix_subspace=ix_data_points
  # ix data points that don't belong to the subspace
  results_separable_subspace$ix_not_subspace=ix_deleted
  # kdn of the resulting subspace
  results_separable_subspace$kdn=kdn_subspace

    return(results_separable_subspace)
}



##############################################
##############################################
######   CSViz subspaces computation    ######
##############################################
##############################################

## Function to extract all separable subspaces in a data set.
## PARAMETERS:
# - dataX is the data frame with the features variables
# - dataY is the vector with the label variable. It must be a factor
# - k is the percentage of data to be considered in the kdn computation, must be in (0,1]
# - unique_values_factor is an integer indicating the minimum required number of different values in a variable
#   for it to be considered as continous. If it has less than unique_values_factor different values, it will be considered
#   as a factor.
# - trunc_data_elimination = {T, F} to indicate if data elimination must be truncated so that a class is not fully deleted
# - scale_data = {T, F} if data must be scaled in the method 

CSViz_subspaces_computation<- function(dataX, dataY, k, 
                                       trunc_data_elimination=TRUE,
                                       unique_values_factor,
                                       threshold_na_values=0.15,
                                       scale_data=TRUE){
  
  #######################################
  #### Checking initial requirements ####
  #######################################
  
  ## Error if parameter k is not in (0,1)
  if (k<=0 | k> 1) {
    stop("k must be between 0 and 1")
  }
  
  ## Error if parameters trunc_data_elimination or scale_data are not True or False
  if ((!(trunc_data_elimination%in%c(T,F)))|((!(scale_data%in%c(T,F))))) {
    stop("trunc_data_elimination and scale_data must be True or False")
  }
  
    ## Error if dataX_2D and dataY don't have same number of data
  if (dim(dataX)[1]!=length(dataY)) {
    stop("dataX and dataY must have same number of data")
  }
  
  ## Error if there is no data
  if (dim(dataX)[1]==0) {
    stop("dataX and dataY are empty")
  }
  
  ## Error if all the variables are categorical (less than unique_values_factor different values)
  count_unique <- rapply(dataX,  function(x) length(unique(x)))
  if(sum(count_unique>unique_values_factor)==0){
    stop("all variables in dataX have less than unique_values_factor categories")
  }

  ## Error if dataY is not a factor
  if(!(is.factor(dataY))){
    stop('dataY must be a dactor')
    return(NULL)
  }
  
  ## Error if dataY has only one value
  if(length(levels(dataY))<=1){
    stop('dataY must have at least two different values')
    return(NULL)
  }
  
  ## Paralleling is needed. Compute the number of cores to use
  n.cores <- parallel::detectCores() - 1
  
  
  #############################
  #### Data preprocessing  ####
  #############################
  
  ## Deleting variables with a higher proportion of NAs than threshold_na_values
  dataX=variables_deletion_NAs(dataX,threshold_na_values)
  
  ## Deleting variables with unique value
  dataX = dataX %>% dplyr::select(where(~n_distinct(.) > 1))

  ## Deleting na values
  data=cbind(dataX,dataY)
  data = na.omit(data)
  dataX = data %>% dplyr::select(-dataY)
  dataY = data %>%  pull(dataY)
  
  ## Deleting variables with unique value again after the na values deletion
  dataX = dataX %>%  dplyr::select(where(~n_distinct(.) > 1))

  ## Error if dataX has no data after the preprocessing
  if (dim(dataX)[1]==0) {
    stop("All rows in (dataX,dataY) have at least one NA value")
  }
  
  ## Converting variables to factor if they have less than unique_values_factor different values
  dataX=dataX %>%  dplyr::mutate(across(where(~n_distinct(.) <= unique_values_factor),factor))
  
  ## Error if all the variables are categorical (less than unique_values_factor different values)
  count_unique <- rapply(dataX,  function(x) length(unique(x)))
  if(sum(count_unique>unique_values_factor)==0){
    stop("all variables in dataX have less than unique_values_factor categories")
  }
  
  ########################################################
  #### Preprocessing factor variables if there is any ####
  ########################################################
  
  ## checking if there is factor variable
  var_factors=sapply(dataX, function(x) is.factor(x))
  
  ## Extend data frame with model matrix if needed
  if (any(var_factors)){
    
    ## change the names of the factors for an adequate naming in the model matrix
    colnames(dataX)[var_factors]=paste0(colnames(dataX)[var_factors],"_")
    
    ## names of variables that are factor
    names_factor=names(dataX)[var_factors]
    
    ## model matrix
    dataX=model.matrix(~.,data=dataX)
    dataX=as.data.frame(dataX)[,-1]
  }
  
  ## save data to return them 
  results = list()
  results$dataX=dataX
  
  ## checking if there is factor variable
  var_factors=sapply(results$dataX, function(x) length(unique(x))<=unique_values_factor)
  
  ##############################################################
  ##### Scaling data if indicated by "scale_data" argument ##### 
  ##############################################################
  
  ## scaling numeric and binarized factor variables to balance the importance
  if (scale_data){
    dataX=as.data.frame(scale(dataX))
  }


  
  #################################
  ##### Data charactheristics ##### 
  #################################
  
  ## number of points
  n=dim(dataX)[1]
  
  # number of variables 
  p=dim(dataX)[2]
  
  ## dataY as factor
  dataY=factor(dataY) 
  
  ## classes in data set
  classes=levels(dataY)
  

  #####################################
  ######   auxiliar variables    ######
  #####################################
  
  ## Orginal dataX and dataY
  dataY_aux=dataY
  dataX_aux=dataX
  
  ## matrix to track the pair of variables used in each iteration
  used_pair_variables=matrix(nrow = 0, ncol = 2)
  
  ## vector to track the remaining data obs (at first they are the same as initial)
  remaining_data=1:n
  
  ## list to track the obtained subspaces 
  obtained_subspaces=list()
  
  ##############################################################
  ######   iteration to obtain the separable subspaces    ######
  ##############################################################

  ## storing all the possible pair of variables in a matrix
  pair_variables=gtools::combinations(p, 2)
  
  ## selecting the valid pair of variables: those where at least one variable is continous
  valid_pair_variables=matrix(!var_factors[as.vector(pair_variables)],ncol=2)
  pair_variables=matrix(pair_variables[apply(valid_pair_variables, 1, any),],ncol=2)
  n_pair_variables_total=dim(pair_variables)[1]

  ## computing the stopping condition: all pairs of variables have been used or no data
  stop_condition=(dim(used_pair_variables)[1]==n_pair_variables_total)|(length(dataY_aux)==0)
  
  ## iterating
  iter=1
  while(!stop_condition){
    print(paste("iteration number", iter))
    
    ## selecting the pair of variables that have not been selected yet
    pair_variables=suppressMessages(as.matrix(dplyr::anti_join(data.frame(pair_variables),data.frame(used_pair_variables))))
    n_pair_variables=dim(pair_variables)[1]
    
    
    ## parallel loop to obtain the kdn value for each pair of variable
    # we define the cluster and register it so it can be used by %dopar%
    my.cluster <- parallel::makeCluster(
      n.cores,
      type = "PSOCK"
    )
    
    # register it to be used by %dopar%
    doParallel::registerDoParallel(cl = my.cluster)
    kdn_values <-
      foreach::foreach(pair=1:n_pair_variables, .combine='c' ,.export=c("kdn"),.packages = "dbscan")  %dopar% {
        dataX_aux_2D=dataX_aux[,pair_variables[pair,]]
        results_kdn=kdn(dataX_aux_2D,dataY_aux,k,scale_data=F)
        results_kdn$dataset_level
      }
    parallel::stopCluster(cl = my.cluster)
    
    
    ## Choosing the pair of variables for which its subspace has minimum kdn 
    min_kdn_value=which.min(kdn_values) 
    pair_variables_selected=pair_variables[min_kdn_value,]
    
    ## Computing separable subset of points out of the 2D subspaces
    dataX_aux_2D=dataX_aux[,pair_variables_selected]
    obtained_subspace=extract_separable_subset_2D(dataX_2D=dataX_aux_2D, dataY=dataY_aux, 
                                            k=k,trunc_data_elimination)

    ## Filling the variable to track the obtained subspaces
    ix_not_subspace=obtained_subspace$ix_not_subspace
    ix_subspace=obtained_subspace$ix_subspace
    
    obtained_subspaces[[iter]]=list()
    obtained_subspaces[[iter]]$variables=pair_variables_selected
    obtained_subspaces[[iter]]$data_subspace=remaining_data[ix_subspace]
    obtained_subspaces[[iter]]$data_not_subspace=remaining_data[ix_not_subspace]
    obtained_subspaces[[iter]]$kdn=obtained_subspace$kdn
    

    ## updating data
    remaining_data=remaining_data[ix_not_subspace]
    dataY_aux=dataY_aux[ix_not_subspace]
    dataX_aux=dataX_aux[ix_not_subspace,]
    
    ## We update the used pair of variables in this iteration
    used_pair_variables=rbind(used_pair_variables,pair_variables_selected)
    
    ## recomputing stoping condition
    stop_condition=(dim(used_pair_variables)[1]==n_pair_variables_total)|(length(dataY_aux)==0)
    
    ## increasing iteration
    iter=iter+1
  }
  
  ## preparing variable to return
  results$dataY=dataY
  results$k=k
  results$obtained_subspaces=obtained_subspaces
  
  return(results)
}




##############################################
##############################################
######    Getting legend from a plot    ######
##############################################
##############################################

## Function to obtain the class legend for a data
## PARAMETERS:
# - dataX is the data frame with the feature variables
# - dataY is the vector with the target variable. It must be a factor
get_legend_data <- function(dataX,dataY) {
  ## classes in data set
  classes=levels(dataY)
  ## number of classes
  n_classes=length(unique(dataY))
  
  ## colors palette of the classes for the plots
  palette_colors="fishy" # from "grafify" library
  ## point shapes  of the classes
  point_shapes= c(16, 17, 15, 18,3,4,8,25,6)[1:n_classes]
  
  ## simple plot to obtain the legend
  dataX$class=dataY
  plot <-  ggplot2::ggplot() + ggplot2::geom_point(data=dataX,
                                                   aes(x=dataX[,1],y=dataX[,2],
                                                       colour=class,shape=class))+
    # adding color
    grafify::scale_colour_grafify(palette = palette_colors)+
    # adding point shapes
    ggplot2::scale_shape_manual(values = point_shapes)+
    ##adding theme
    ggplot2::theme_bw()+
    # adding legend
    ggplot2::theme(legend.position = "bottom")
  
  ## extracting the legend
  legend=cowplot::get_legend(plot)
  return(legend)
}


######################################################
######################################################
######  PLAIN INFORMATION TABLE OF SUBSPACES    ######
######################################################
######################################################

## Function to obtain a plain table with the detailed information about the the obtained subspaces 
## PARAMETERS:
# - CSViz_computed_subspaces is the return of the CSViz_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized; must be in [0,1]
# - max_kdn: maximum kDN allowed for a subspace to be displayed; must be in [0,1]

CSViz_table <- function(CSViz_computed_subspaces, min_data,max_kdn){
  
  #######################################
  #### Checking initial requirements ####
  #######################################
  
  ## Error if parameters min_data or max_kdn are not in [0,1]
  if ((min_data<0 | min_data> 1)|(max_kdn<0 | max_kdn> 1)) {
    stop("min_data and max_kdn must be between 0 and 1")
  }
  
  
  ###########################
  #### gathering results ####
  ###########################
  
  ## Gathering obtained subspaces
  obtained_subspaces=CSViz_computed_subspaces$obtained_subspaces
  
  ## Number of obtained subspaces
  n_subspaces=length(obtained_subspaces)
  
  ## Gathering original dataX and dataY
  dataX=CSViz_computed_subspaces$dataX
  dataY=CSViz_computed_subspaces$dataY
  
  
  ################################
  ##### Data characteristics ##### 
  ################################
  
  ## number of points
  n=dim(dataX)[1]
  
  ## dataY as factor
  dataY=factor(dataY) 
  
  ## classes in data set
  classes=levels(dataY)
  
  ## number of classes
  n_classes=length(unique(dataY))
  
  ## number of points in each class
  n_data_classes=table(factor(dataY, classes))
  
  
  #########################################################
  ##### Creating data frame to store the information  ##### 
  #########################################################
  
  n_col_df=5+n_classes*2+1
  table_information=data.frame(matrix(nrow=0,ncol = n_col_df))
  
  ## names of the dataframe to store the information
  names(table_information)=c("subspace","var1", "var2", "% acccum data total", paste("% acccum data class",classes),
                             "% data total" , paste("% data class",classes), "kdn")
  
  ##########################################################################
  ##### Obtaining information for each subspace and filling the table  ##### 
  ##########################################################################
  
  ## auxiliar variables to compute the information
  sum_data=0
  sum_data_classes=numeric(n_classes)
  
  ## Looping over each subspace
  for (subspace in 1:n_subspaces){
    
    ## subspace of this iteration
    subspace_iter=obtained_subspaces[[subspace]]
    
    ## % of data showed in this iteration
    n_data_subspace_iter=length(subspace_iter$data_subspace)
    perc_data_subspace_iter= n_data_subspace_iter/n
    
    ## KDN value of the subspace in this iteration
    kdn_dataset_level=round(subspace_iter$kdn,2)
    
    ## if the subspace has a lower percentage of data than min_data or higher kdn than max_kdn, the method does not plot it
    if ((perc_data_subspace_iter<min_data)|(kdn_dataset_level>max_kdn)){
      next
    }
    
    ## format of  % of data showed in this iteration
    perc_data_subspace_iter= round(100*n_data_subspace_iter/n,2)
    
    ## Variables showed in this iteration
    variables_iter=subspace_iter$variables
    
    ## cumulative % of data showed in this iteration
    sum_data=sum_data+n_data_subspace_iter
    cum_perc_data_subspace_iter=round(100*sum_data/n,2)
    
    ## % of data of each class showed in this iteration
    dataY_subspace=dataY[subspace_iter$data_subspace]
    n_data_classes_subspace_iter=table(factor(dataY_subspace, classes))
    perc_data_classes_subspace_iter= round(100*n_data_classes_subspace_iter/n_data_classes,2)
    
    ## cumulative % of data of each class showed in this iteration
    sum_data_classes=sum_data_classes+n_data_classes_subspace_iter
    cum_perc_data_classes_subspace_iter= round(100*sum_data_classes/n_data_classes,2)
    
    ## Filling the data frame
    table_information[subspace,]=c(subspace,names(dataX)[variables_iter[1]],
                                   names(dataX)[variables_iter[2]],
                                   cum_perc_data_subspace_iter,cum_perc_data_classes_subspace_iter,
                                   perc_data_subspace_iter,perc_data_classes_subspace_iter,
                                   kdn_dataset_level)
  }
  
  ####################################################################
  ##### message if there is not any subspace meeting conditions  ##### 
  ####################################################################
  
  if(dim(table_information)[1]==0){
    cat("There is not any subspace meeting conditions: total data proportion >= min_data and kdn data set <= max_kdn.")
    return(NULL)
  }
  
  return(table_information)
}


############################################################
############################################################
######   FORMATTED INFORMATION TABLE OF SUBSPACES     ######
############################################################
############################################################

## Function to display a formatted table with the detailed information about the the obtained subspaces 
## PARAMETERS:
# - CSViz_computed_subspaces is the return of the CSViz_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized; must be in [0,1]
# - max_kdn: maximum kDN allowed for a subspace to be displayed; must be in [0,1]

CSViz_display_table <- function(CSViz_computed_subspaces, min_data,max_kdn){
  
  ############################################
  ##### Obtaining Data Y characteristics ##### 
  ############################################
  
  ## Gathering original dataX and dataY
  dataY=CSViz_computed_subspaces$dataY
  
  ## dataY as factor
  dataY=factor(dataY) 
  
  ## classes in data set
  classes=levels(dataY)
  
  ## number of classes
  n_classes=length(unique(dataY))
  
  
  #############################################################
  ##### Obtaining plain information table to be formatted ##### 
  #############################################################
  
  ## Obtaining plain table to be formatted
  table_information = CSViz_table(CSViz_computed_subspaces, min_data,max_kdn)
  
  
  ############################
  ##### Formatting table ##### 
  ############################
  
  ## new colnames of the dataframe to store the information
  names(table_information)=c("subspace","var1", "var2", "total", classes,
                             "total \r",paste(classes,c("\r")), "kdn")
  
  ## Format of the table to export
  table_information_format <- flextable::flextable(table_information) %>% 
    bg(bg="white",part="all") %>% 
    flextable::theme_box() %>% 
    flextable::add_header_row(colwidths = c(1,1,1,1,n_classes,1,n_classes, 1), 
                              values = c("","","", "total","class","total","class","")) %>% 
    flextable::add_header_row(colwidths = c(1,2,n_classes+1,n_classes+1,1), 
                              values = c("subspace","variables","%n accum.","%n", "kDN")) %>% 
    flextable::merge_at(i = 1:3 ,j = 1, part = "header")%>%
    flextable::merge_at(i = 1:3 ,j = 2:3, part = "header")%>%
    flextable::merge_at(i = 2:3 ,j = 4, part = "header") %>% 
    flextable::merge_at(i = 2:3 ,j = (4+n_classes+1), part = "header") %>% 
    flextable::merge_at(i = 1:3 ,j = (4+n_classes+1+n_classes+1), part = "header")%>%
    flextable::fontsize(size = 10,part='all') %>% 
    flextable::autofit(add_w=-2,add_h=-2) %>% 
    flextable::vline(i=1:3,j = 1, border = officer::fp_border(color = "black", style = "solid"), part = "header")
  table_information_format <- flextable::align(table_information_format, align = "center", part = "all")
  
  return(table_information_format)
}

#############################################################
#############################################################
######   PLOT OF THE AVAILABLE FOR A FIXED SUBSPACE    ######
#############################################################
#############################################################

## Function to plot the available data for a subspace
## PARAMETERS:
# - CSViz_computed_subspaces is the return of the CSViz_subspaces_computation function
# - subspace is an integer selecting one of the obtained subspaces

CSViz_available_data_subspace_plot <- function(CSViz_computed_subspaces, subspace){
  
  ###########################
  #### gathering results ####
  ###########################
  
  ## Gathering obtained subspace
  obtained_subspace=CSViz_computed_subspaces$obtained_subspaces[[subspace]]
  
  ## Gathering original dataX and dataY
  dataX=CSViz_computed_subspaces$dataX
  dataY=CSViz_computed_subspaces$dataY
  
  
  #################################
  ##### Data charactheristics ##### 
  #################################
  
  ## dataY as factor
  dataY=factor(dataY) 
  
  ## number of classes
  n_classes=length(unique(dataY))
  
  
  ###########################################################
  ##### plot aesthetic: Colors palette and point shapes ##### 
  ###########################################################
  
  ## colors palette of the classes for the plots
  palette_colors="fishy" # from "grafify" library
  
  ## point shapes  of the classes
  point_shapes= c(16, 17, 15, 18,3,4,8,25,6)[1:n_classes]
  
  
  #################################################################
  ##### obtaining plot of the available data for the subspace ##### 
  #################################################################
  
  ## obtained results for the subspace in the corresponding iteration
  ix_subspace=obtained_subspace$data_subspace
  ix_not_subspace=obtained_subspace$data_not_subspace
  variables=obtained_subspace$variables
  
  ## Data for plotting
  dataX_plot=dataX[c(ix_subspace,ix_not_subspace),variables]
  dataY_plot=dataY[c(ix_subspace,ix_not_subspace)]
  
  ## number of unique values of the X columns to plot
  count_unique_values <- rapply(dataX_plot,  function(x) length(unique(x)))
  
  ## Adding variable class to data for plotting
  dataX_plot$class=dataY_plot
  
  ## Obtaining x and y limits for the plots
  extra_limit=0
  x_lim_sup=max(dataX_plot[,1])+extra_limit*abs(max(dataX_plot[,1]))
  y_lim_sup=max(dataX_plot[,2])+extra_limit*abs(max(dataX_plot[,2]))
  
  x_lim_inf=min(dataX_plot[,1])-extra_limit*abs(min(dataX_plot[,1]))
  y_lim_inf=min(dataX_plot[,2])-extra_limit*abs(min(dataX_plot[,2]))
  
  fixed_xlim=c(x_lim_inf,x_lim_sup)
  fixed_ylim=c(y_lim_inf,y_lim_sup)
  
  ## if one variable is binary (less than two unique variables), reorder 
  # dataX_plot so that the binary variable is on first position (and 
  # then in the x axis in the plot)
  if(any(count_unique_values <=2)){
    # identifying the variable to plot in x axis and in y axis
    var_x=which(count_unique_values<=2)
    var_y=c(1,2)[!(c(1,2) %in% var_x)]
    dataX_plot=dataX_plot[,c(var_x,var_y,3)]
  }
  
  ## initialize the plot for the corresponding subspace
  plot_data = ggplot2::ggplot(data=dataX_plot,
                              aes(colour=class,shape = class))  
  
  ## scatter plot if both variables are numeric (more than two unique variables)
  if(all(count_unique_values>2)){
    plot_data = plot_data+ggplot2::geom_point(aes(x=dataX_plot[,1],y=dataX_plot[,2],),size = 2.2)
    
    ## setting xlim and ylim if they are specified in the parameters of the function
    if(!is.null(fixed_xlim)&!(is.null(fixed_ylim))){
      plot_data=  plot_data +
        # setting x and y axes limits
        ggplot2::coord_cartesian(xlim=fixed_xlim, ylim=fixed_ylim)
    }
    
    ## geom_jitter if one variable is not numeric (less than two unique variables)
  } else if(any(count_unique_values <=2)){
    # plotting geom jitter
    set.seed(1234)
    plot_data = plot_data+ ggplot2::geom_jitter(aes(x=factor(dataX_plot[,1]),y=dataX_plot[,2]),size = 2.2)
    
    ## setting ylim if it is are specified in the parameters of the function
    if(!is.null(fixed_xlim)&!(is.null(fixed_ylim))){
      plot_data=  plot_data +
        # setting  y axes limits
        ggplot2::coord_cartesian( ylim=fixed_ylim)
    }
  }
  
  ## adding color
  plot_data= plot_data +
    grafify::scale_colour_grafify(palette = palette_colors)+
    ## adding point shapes
    ggplot2::scale_shape_manual(values = point_shapes)+
    
    ## plot theme
    ggplot2::theme_bw() +
    
    ## tittle
    ggplot2::ggtitle(paste(c("available data for the"),english::ordinal(subspace),"subspace")) +    
    ## axes names
    ggplot2::xlab(colnames(dataX_plot)[1]) +
    ggplot2::ylab(colnames(dataX_plot)[2]) +
    
    ## theme of title and axes
    ggplot2::theme(plot.title = element_text(hjust = 0.5),
                   legend.position = "bottom")  
  
  return(plot_data)
}


###########################################
###########################################
######   PLOT OF A FIXED SUBSPACE    ######
###########################################
###########################################

## Function to plot the available data for a subspace
## PARAMETERS:
# - CSViz_computed_subspaces is the return of the CSViz_subspaces_computation function
# - subspace is an integer selecting one of the obtained subspaces
# - fixed_xlim is a vector of length 2 containing the minimum and maximum limits for the x axis
# - fixed_ylim is a vector of length 2 containing the minimum and maximum limits for the y axis

CSViz_subspace_plot <- function(CSViz_computed_subspaces, subspace, fixed_xlim=NULL, fixed_ylim=NULL){
  
  ###########################
  #### gathering results ####
  ###########################
  
  ## Gathering obtained subspace
  obtained_subspace=CSViz_computed_subspaces$obtained_subspaces[[subspace]]
  
  ## Gathering original dataX and dataY
  dataX=CSViz_computed_subspaces$dataX
  dataY=CSViz_computed_subspaces$dataY
  
  
  #################################
  ##### Data charactheristics ##### 
  #################################
  
  ## dataY as factor
  dataY=factor(dataY) 
  
  ## number of classes
  n_classes=length(unique(dataY))
  
  
  ###########################################################
  ##### plot aesthetic: Colors palette and point shapes ##### 
  ###########################################################
  
  ## colors palette of the classes for the plots
  palette_colors="fishy" # from "grafify" library
  
  ## point shapes  of the classes
  point_shapes= c(16, 17, 15, 18,3,4,8,25,6)[1:n_classes]
  
  
  ##########################################
  ##### obtaining plot of the subspace ##### 
  ##########################################
  
  ## obtained results for the subspace in the corresponding iteration
  ix_subspace=obtained_subspace$data_subspace
  ix_not_subspace=obtained_subspace$data_not_subspace
  variables=obtained_subspace$variables
  
  ## Data for plotting
  dataX_plot=dataX[ix_subspace,variables]
  dataY_plot=dataY[ix_subspace]
  
  ## number of unique values of the X columns to plot
  count_unique_values <- rapply(dataX_plot,  function(x) length(unique(x)))
  
  ## Adding variable class to data for plotting
  dataX_plot$class=dataY_plot
  
  ## if one variable is binary (less than two unique variables), reorder 
  # dataX_plot so that the binary variable is on first position (and 
  # then in the x axis in the plot)
  if(any(count_unique_values <=2)){
    # identifying the variable to plot in x axis and in y axis
    var_x=which(count_unique_values<=2)
    var_y=c(1,2)[!(c(1,2) %in% var_x)]
    dataX_plot=dataX_plot[,c(var_x,var_y,3)]
  }
  
  ## initialize the plot for the corresponding subspace
  plot_subspace = ggplot2::ggplot(data=dataX_plot,
                                  aes(colour=class,shape = class))  
  
  ## scatter plot if both variables are numeric (more than two unique variables)
  if(all(count_unique_values>2)){
    plot_subspace = plot_subspace+ggplot2::geom_point(aes(x=dataX_plot[,1],y=dataX_plot[,2],),size = 2.2)
    
    ## setting xlim and ylim if they are specified in the parameters of the function
    if(!is.null(fixed_xlim)&!(is.null(fixed_ylim))){
      plot_subspace=  plot_subspace +
        # setting x and y axes limits
        ggplot2::coord_cartesian(xlim=fixed_xlim, ylim=fixed_ylim)
    }
    
    ## geom_jitter if one variable is not numeric (less than two unique variables)
  } else if(any(count_unique_values <=2)){
    # plotting geom jitter
    set.seed(1234)
    plot_subspace = plot_subspace+ ggplot2::geom_jitter(aes(x=factor(dataX_plot[,1]),y=dataX_plot[,2]),size = 2.2)
    
    ## setting ylim if it is are specified in the parameters of the function
    if(!is.null(fixed_xlim)&!(is.null(fixed_ylim))){
      plot_subspace=  plot_subspace +
        # setting  y axes limits
        ggplot2::coord_cartesian( ylim=fixed_ylim)
    }
  }
  
  ## adding color
  plot_subspace= plot_subspace +
    grafify::scale_colour_grafify(palette = palette_colors)+
    ## adding point shapes
    ggplot2::scale_shape_manual(values = point_shapes)+
    
    ## plot theme
    ggplot2::theme_bw() +
    
    ## tittle
    ggplot2::ggtitle(paste(english::ordinal(subspace),"subspace")) +
    
    ## axes names
    ggplot2::xlab(colnames(dataX_plot)[1]) +
    ggplot2::ylab(colnames(dataX_plot)[2]) +
    
    ## theme of title and axes
    ggplot2::theme(plot.title = element_text(hjust = 0.5),
                   legend.position = "bottom")  
  
  return(plot_subspace)
}



######################################################
######################################################
######   LIST OF PLOTS AND INFORMATION TABLE    ######
######################################################
######################################################

## Function to obtain a list containing the plot of each of the obtained subspace and the information table
## PARAMETERS:
# - CSViz_computed_subspaces is the return of the CSViz_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized; must be in [0,1]
# - max_kdn: maximum kDN allowed for a subspace to be displayed; must be in [0,1]

CSViz_plots_table <- function(CSViz_computed_subspaces, min_data, max_kdn){
  
  ## the initial check requirements is performed in CSViz_table function
  
  ####################################################
  ##### obtaining information table of subspaces ##### 
  ####################################################
  
  information_table=CSViz_display_table(CSViz_computed_subspaces, min_data, max_kdn)
  
  ## Number of obtained subspaces meeting min_data and max_kdn condiions
  n_subspaces=dim(information_table$body$dataset)[1]
  
  ############################
  ##### storing results  ##### 
  ############################
  
  ## list to store the table and plots 
  table_plots_list=list()
  
  ## storing the table at the end of the list
  table_plots_list[[n_subspaces+1]]=information_table
  
  
  #############################################
  ##### Obtaining plots of the subspaces  ##### 
  #############################################
  
  table_plots_list[1:n_subspaces] = lapply(1:n_subspaces, CSViz_subspace_plot, CSViz_computed_subspaces =CSViz_computed_subspaces )
  
  
  return(table_plots_list)
}


#################################################################
#################################################################
######   DISPLAY PLOT OF SUBSPACES AND TABLE INFORMATION   ######
#################################################################
#################################################################

## Function to display the plot of the subspaces and their information table in a single plot
## PARAMETERS:
# - CSViz_computed_subspaces is the return of the CSViz_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized; must be in [0,1]
# - max_kdn: maximum kDN allowed for a subspace to be displayed; must be in [0,1]

CSViz_display_subspaces <- function(CSViz_computed_subspaces, min_data, max_kdn){
  
  ## the initial check requirements is performed in CSViz_table function
  
  ##########################################################
  #### gathering number of classes in the original data ####
  ##########################################################
  
  ## Gathering original dataY
  dataY=CSViz_computed_subspaces$dataY
  
  ## Gathering original dataY
  dataX=CSViz_computed_subspaces$dataX
  
  ## classes in data set
  classes=levels(dataY)
  
  ## number of classes
  n_classes=length(unique(dataY))
  
  
  ##################################################
  #### obtaining information table of subspaces ####
  ##################################################
  
  table=CSViz_table(CSViz_computed_subspaces, min_data, max_kdn)
  
  ## getting the information needed for the display
  table=table[,(4+n_classes+1):dim(table)[2]]
  names(table)=c("total", classes,"kDN")
  
  ## Number of obtained subspaces
  n_subspaces=dim(table)[1]
  
  
  ###################################################
  ##### Obtaining the grid of plots and tables  ##### 
  ###################################################
  
  ## list to store the plots and tables
  list_grid=vector("list", n_subspaces)
  
  ## Looping over the subspaces to obtain each plot and information table
  for (subspace in 1:(n_subspaces)){
    
    ## subspace plot of the subspace
    plot_subspace=CSViz_subspace_plot(CSViz_computed_subspaces, subspace)+
      # deleting the legend
      ggplot2::theme( legend.position = "none")  
    
    ## converting to  grob
    set.seed(1234)
    plot_subspace=ggplotGrob(plot_subspace)
    
    ## information table of this subspace
    table_subspace=table[subspace,]
    
    ## formating the table
    table_subspace_format <- flextable::flextable(table_subspace) %>% 
      flextable::add_header_row(colwidths = c(1,n_classes, 1), values = c("total","class", "")) %>% 
      flextable::add_header_row(colwidths = c(n_classes+1, 1), values = c("%n accum.", "kDN")) %>% 
      flextable::align(i = 1, part = "header", align = "center") %>% 
      flextable::merge_at(i = 1:3 ,j = n_classes+2, part = "header")%>%
      flextable::merge_at(i = 2:3 ,j = 1, part = "header") %>% 
      flextable::fontsize(size = 10,part='all') %>% 
      flextable::autofit(add_w=-2,add_h=-2) %>%
      flextable::theme_box()
    table_subspace_format <- flextable::align(table_subspace_format, align = "center", part = "all")
    
    ## converting the table into a ggplot
    table_subspace_format <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::annotation_custom(grid::rasterGrob(as_raster(table_subspace_format)))

    
    ## grid of the plot and table of this subspace
    list_grid[[subspace]]= cowplot::plot_grid(plot_subspace,table_subspace_format, 
                                              nrow = 1, align = "v",
                                              scale = c(1, 0.7),rel_widths=c(2,2))
  }
  
  
  #####################################
  ##### grid of plots and tables  ##### 
  #####################################
  
  ## obtaining legend for the plot
  legend=get_legend_data(dataX,dataY)

  ## grid of plots and tables
  display_subspaces=cowplot::plot_grid(plotlist=list_grid, nrow=length(list_grid))
  ## adding legend
  display_subspaces=cowplot::plot_grid(display_subspaces,legend, nrow = 2, rel_heights = c(10, 0.4))+
    theme(panel.background = element_rect(fill = 'white', color = NA))

  return(display_subspaces)
}



######################################################
######################################################
######  DISPLAY STORYTELLING OF THE SUBSPACES   ######
######################################################
######################################################

## Function to display the storytelling of the subspaces: plot of the available data for the 
 # subspace and plot of the subspace in a single plot
## PARAMETERS:
# - CSViz_computed_subspaces is the return of the CSViz_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized; must be in [0,1]
# - max_kdn: maximum kDN allowed for a subspace to be displayed; must be in [0,1]

CSViz_display_storytelling <- function(CSViz_computed_subspaces, min_data, max_kdn){
  
  ## the initial check requirements is performed in CSViz_table function
  
  ##################################################
  #### obtaining information table of subspaces ####
  ##################################################
  
  table=CSViz_table(CSViz_computed_subspaces, min_data, max_kdn)
  
  ## Number of obtained subspaces
  n_subspaces=dim(table)[1]
  
  ###################################################
  ##### Obtaining the grid of plots and tables  ##### 
  ###################################################
  
  ## list to store the plots and tables
  list_grid=vector("list", n_subspaces)
  
  ## Looping over the subspaces to obtain each plot and information table
  for (subspace in 1:(n_subspaces)){
    
    ## plot of the available data for the subspace
    plot_data=CSViz_available_data_subspace_plot(CSViz_computed_subspaces,subspace)+
      # deleting the legend
      ggplot2::theme( legend.position = "none")  
    
    ## getting xlim and ylim of the plot
    ylim=layer_scales(plot_data)$y$get_limits()
    xlim=layer_scales(plot_data)$x$get_limits()
    
    ## converting to  grob
    set.seed(1234)
    plot_data=ggplotGrob(plot_data)
    
    ## subspace plot of the subspace
    plot_subspace=CSViz_subspace_plot(CSViz_computed_subspaces, subspace,
                                      fixed_xlim = xlim, fixed_ylim = ylim )+
      # deleting the legend
      ggplot2::theme( legend.position = "none")  
    
    ## converting to  grob
    set.seed(1234)
    plot_subspace=ggplotGrob(plot_subspace)
    
    ## grid of the plot and table of this subspace
    list_grid[[subspace]]= cowplot::plot_grid(plot_data,plot_subspace, 
                                              nrow = 1, align = "v")
  }
  
  ##########################
  ##### grid of plots  ##### 
  ##########################
  
  ## obtaining legend for the plot
  legend=get_legend_data(dataX,dataY)
  
  ## grid of plots and tables
  display_storytelling=cowplot::plot_grid(plotlist=list_grid, nrow=length(list_grid))
  
  ## adding legend
  display_storytelling=cowplot::plot_grid(display_storytelling,legend, nrow = 2, rel_heights = c(10, 0.4))+
    theme(panel.background = element_rect(fill = 'white', color = NA))
  
  return(display_storytelling)
}
