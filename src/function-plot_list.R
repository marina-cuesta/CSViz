

#########################################################################
#########################################################################
######   VISUALIZATION OF SUBSPACES: GRAPHS + INFORMATION TABLE    ######
#########################################################################
#########################################################################

## Function to visualize the obtained subspaces with a plot and a information table for each one
## PARAMETERS:
# - results_CSViz_subspaces is the return of the CSViz_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized
# - max_kdn: maximum kDN allowed for a subspace to be displayed


CSViz_visualization <- function(results_CSViz_subspaces, min_data, max_kdn){
  
  #######################################
  #### Checking initial requirements ####
  #######################################
  
  ## Error if parameters min_data or max_kdn are not in [0,1]
  if ((min_data<0 | min_data> 1)|(max_kdn<0 | max_kdn> 1)) {
    stop("min_data and max_kdn must be between 0 and 1")
  }
  
  
  ###########################
  #### GATHERING RESULTS ####
  ###########################
  
  ## Gathering obtained subspaces
  obtained_subspaces=results_CSViz_subspaces$obtained_subspaces
  
  ## Gathering original dataX and dataY
  dataX=results_CSViz_subspaces$dataX
  dataY=results_CSViz_subspaces$dataY
  
  
  #################################
  ##### Data charactheristics ##### 
  #################################
  
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
  
  
  ############################################################
  ##### graph aesthetic: Colors palette and point shapes ##### 
  ############################################################
  
  ## colors palette of the classes for the plots
  palette_colors="fishy" # from "grafify" library
  
  ## point shapes  of the classes
  point_shapes= c(16, 17, 15, 18,3,4,8,25,6)[1:n_classes]
  
  
  ###################################################
  ##### Obtaining plots and information tables  ##### 
  ###################################################
  
  ## Number of obtained subspaces
  n_subspaces=length(obtained_subspaces)
  
  ## list to store the plots and tables
  list_grid=list()
  
  ## Looping over the subspaces to obtain each plot and information table
  for (subspace in 1:(n_subspaces)){
    
    ## subspace of this iteration
    subspace_iter=obtained_subspaces[[subspace]]
    
    ## obtained results for the subspace in the corresponding iteration
    ix_subspace_iter=obtained_subspaces[[subspace]]$data_subspace
    ix_not_subspace_iter=obtained_subspaces[[subspace]]$data_not_subspace
    variables_iter=obtained_subspaces[[subspace]]$variables
    kdn_dataset_level=obtained_subspaces[[subspace]]$kdn
    
    ## percentage of data in this subspace
    n_data_subspace_iter=length(ix_subspace_iter)
    perc_data_subspace_iter=n_data_subspace_iter/n
    
    ## if the subspace has a lower percentage of data than min_data or higher kdn than max_kdn, the method does not plot it
    if ((perc_data_subspace_iter<min_data)|(kdn_dataset_level>max_kdn)){
      next
    }
    
    #################################
    ##### PLOT OF THE SUBSPACE  ##### 
    #################################
    
    ## Data for plotting
    dataX_plot=dataX[ix_subspace_iter,variables_iter]
    dataY_plot=dataY[ix_subspace_iter]
    
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
                                    aes(x=dataX_plot[,1],y=dataX_plot[,2],
                                        colour=class,shape = class))  
    
    ## scatter plot if both variables are numeric (more than two unique variables)
    if(all(count_unique_values>2)){
      plot_subspace = plot_subspace+ggplot2::geom_point(size = 2.2)
      
      ## geom_jitter if one variable is not numeric (less than two unique variables)
    } else if(any(count_unique_values <=2)){
      # plotting geom jitter
      set.seed(1234)
      plot_subspace = plot_subspace+ ggplot2::geom_jitter(aes(x=factor(dataX_plot[,1]),y=dataX_plot[,2]),size = 2.2) 
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
                     legend.position = "none")   
    
    

    
    
    
    ##############################################
    ##### GRID PLOT + TABLE OF THIS SUBSPACE ##### 
    ##############################################
    
    list_grid[[subspace]]= cowplot::plot_grid(plot_subspace,df_information_format, 
                                              nrow = 1, align = "v",
                                              scale = c(1, 0.7),rel_widths=c(2,2))
  }
  
  ################################################################
  ##### MESSAGE IF THERE IS NOT SUBSPACE MEETING CONDITIONS  ##### 
  ################################################################
  
  if(length(list_grid)==0){
    cat("There is not any subspace meeting conditions: total data proportion >= min_data and kdn data set <= max_kdn.")
    return(NULL)
  }
  
  
  
  ##############################
  ##### GRID PLOT + TABLE  ##### 
  ##############################
  
  ## obtaining legend for the plot
  legend=get_legend_data(dataX,dataY)
  
  
  ## grid of plots and tables
  plot_subspaces_info1=gridExtra::grid.arrange(grobs =list_grid, nrow=length(list_grid))
  plot_subspaces_info=gridExtra::grid.arrange(plot_subspaces_info1, legend, nrow = 2, heights = c(10, 0.4))
  
  return(plot_subspaces_info)
}
