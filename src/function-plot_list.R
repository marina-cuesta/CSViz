#############################################################################
#############################################################################
######   VISUALIZATION OF SUBSPACES: INFORMATION TABLE OF SUBSPACES    ######
#############################################################################
#############################################################################

## Function to obtain a plain table with the detailed information about the the obtaines subspaces 
## PARAMETERS:
# - results_CSViz_subspaces is the return of the CSViz_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized
# - max_kdn: maximum kDN allowed for a subspace to be displayed

CSViz_table <- function(results_CSViz_subspaces, min_data,max_kdn){
  
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
  obtained_subspaces=results_CSViz_subspaces$obtained_subspaces
  
  ## Number of obtained subspaces
  n_subspaces=length(obtained_subspaces)
  
  ## Gathering original dataX and dataY
  dataX=results_CSViz_subspaces$dataX
  dataY=results_CSViz_subspaces$dataY
  
  
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
  df_information=data.frame(matrix(nrow=0,ncol = n_col_df))
  
  ## names of the dataframe to store the information
  names(df_information)=c("subspace","var1", "var2", "% acccum data total", paste("% acccum data class",classes),
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
    df_information[subspace,]=c(subspace,names(dataX)[variables_iter[1]],
                                names(dataX)[variables_iter[2]],
                                cum_perc_data_subspace_iter,cum_perc_data_classes_subspace_iter,
                                perc_data_subspace_iter,perc_data_classes_subspace_iter,
                                kdn_dataset_level)
  }
  
  ####################################################################
  ##### message if there is not any subspace meeting conditions  ##### 
  ####################################################################
  
  if(dim(df_information)[1]==0){
    cat("There is not any subspace meeting conditions: total data proportion >= min_data and kdn data set <= max_kdn.")
    return(NULL)
  }
  
  return(df_information)
}


#############################################################################
#############################################################################
######   VISUALIZATION OF SUBSPACES: INFORMATION TABLE OF SUBSPACES    ######
#############################################################################
#############################################################################

## Function to display a formatted table with the detailed information about the the obtaines subspaces 
## PARAMETERS:
# - results_CSViz_subspaces is the return of the CSViz_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized
# - max_kdn: maximum kDN allowed for a subspace to be displayed

CSViz_display_table <- function(results_CSViz_subspaces, min_data,max_kdn){
  
  ############################################
  ##### Obtaining Data Y characteristics ##### 
  ############################################
  
  ## Gathering original dataX and dataY
  dataY=results_CSViz_subspaces$dataY

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
  df_information = CSViz_table(results_CSViz_subspaces, min_data,max_kdn)
  
  ####################################################################
  ##### message if there is not any subspace meeting conditions  ##### 
  ####################################################################
  
  ## Number of obtained subspaces meeting min_data and max_kdn condiions
  n_subspaces=dim(df_information)[1]
  
  if(n_subspaces==0){
    cat("There is not any subspace meeting conditions: total data proportion >= min_data and kdn data set <= max_kdn.")
    return(NULL)
  }
  
  ############################
  ##### Formatting table ##### 
  ############################
  
  ## new colnames of the dataframe to store the information
  names(df_information)=c("subspace","var1", "var2", "total", classes,
                          "total \r",paste(classes,c("\r")), "kdn")
  
  ## Format of the table to export
  df_information_format <- flextable::flextable(df_information) %>% 
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
  df_information_format <- flextable::align(df_information_format, align = "center", part = "all")
  
    return(df_information_format)
}


#######################################################################
#######################################################################
######   VISUALIZATION OF SUBSPACES: PLOT OF A FIXED SUBSPACE    ######
#######################################################################
#######################################################################

## Function to visualize the obtained subspaces with a plot and a information table for each one
## PARAMETERS:
# - results_CSViz_subspaces is the return of the CSViz_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized
# - max_kdn: maximum kDN allowed for a subspace to be displayed

CSViz_subspace_plot <- function(results_CSViz_subspaces, subspace, fixed_xlim=NULL, fixed_ylim=NULL){
  
  ###########################
  #### gathering results ####
  ###########################
  
  ## Gathering obtained subspace
  obtained_subspace=results_CSViz_subspaces$obtained_subspaces[[subspace]]
  
  ## Gathering original dataX and dataY
  dataX=results_CSViz_subspaces$dataX
  dataY=results_CSViz_subspaces$dataY
  
  
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



#########################################################################################
#########################################################################################
######   VISUALIZATION OF SUBSPACES: PLOT OF THE AVAILABLE FOR A FIXED SUBSPACE    ######
#########################################################################################
#########################################################################################

## Function to visualize the obtained subspaces with a plot and a information table for each one
## PARAMETERS:
# - results_CSViz_subspaces is the return of the CSViz_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized
# - max_kdn: maximum kDN allowed for a subspace to be displayed

CSViz_available_data_subspace_plot <- function(results_CSViz_subspaces, subspace){
  
  ###########################
  #### gathering results ####
  ###########################
  
  ## Gathering obtained subspace
  obtained_subspace=results_CSViz_subspaces$obtained_subspaces[[subspace]]
  
  ## Gathering original dataX and dataY
  dataX=results_CSViz_subspaces$dataX
  dataY=results_CSViz_subspaces$dataY
  
  
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


#################################################################################
#################################################################################
######   VISUALIZATION OF SUBSPACES: LIST OF PLOTS + INFORMATION TABLE    ######
#################################################################################
#################################################################################

## Function to visualize the obtained subspaces with a plot and a information table for each one
## PARAMETERS:
# - results_CSViz_subspaces is the return of the CSViz_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized
# - max_kdn: maximum kDN allowed for a subspace to be displayed


CSViz_plots_table <- function(results_CSViz_subspaces, min_data, max_kdn){
  
  #######################################
  #### Checking initial requirements ####
  #######################################
  
  ## Error if parameters min_data or max_kdn are not in [0,1]
  if ((min_data<0 | min_data> 1)|(max_kdn<0 | max_kdn> 1)) {
    stop("min_data and max_kdn must be between 0 and 1")
  }
  
  
  ####################################################
  ##### obtaining information table of subspaces ##### 
  ####################################################
  
  information_table=CSViz_display_table(results_CSViz_subspaces, min_data, max_kdn)
  
  ## Number of obtained subspaces meeting min_data and max_kdn condiions
  n_subspaces=dim(information_table$body$dataset)[1]
  
  
  ####################################################################
  ##### message if there is not any subspace meeting conditions  ##### 
  ####################################################################
  
  if(n_subspaces==0){
    cat("There is not any subspace meeting conditions: total data proportion >= min_data and kdn data set <= max_kdn.")
    return(NULL)
  }
  
  
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
  
  table_plots_list[1:n_subspaces] = lapply(1:n_subspaces, CSViz_subspace_plot, results_CSViz_subspaces =results_CSViz_subspaces )
  

  return(table_plots_list) ## strutucture(table_plots_list, class="csviz)
}

resultados=CSViz_plots_table(results_CSViz_subspaces, min_data, max_kdn)
resultados[[1]]
resultados[[2]]
resultados[[3]]

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

CSViz_display_subspaces <- function(results_CSViz_subspaces, min_data, max_kdn){
  
  #######################################
  #### Checking initial requirements ####
  #######################################
  
  ## Error if parameters min_data or max_kdn are not in [0,1]
  if ((min_data<0 | min_data> 1)|(max_kdn<0 | max_kdn> 1)) {
    stop("min_data and max_kdn must be between 0 and 1")
  }
  
  
  ##########################################################
  #### gathering number of classes in the original data ####
  ##########################################################
  
  ## Gathering original dataY
  dataY=results_CSViz_subspaces$dataY
  
  ## Gathering original dataY
  dataX=results_CSViz_subspaces$dataX
  
  ## classes in data set
  classes=levels(dataY)
  
  ## number of classes
  n_classes=length(unique(dataY))
  
  
  ##################################################
  #### obtaining information table of subspaces ####
  ##################################################
  
  table=CSViz_table(results_CSViz_subspaces, min_data, max_kdn)
    
  ## getting the information needed for the display
  table=table[,(4+n_classes+1):dim(table)[2]]
  names(table)=c("total", classes,"kDN")
  
  
  ###################################################
  ##### Obtaining the grid of plots and tables  ##### 
  ###################################################
  
  ## Number of obtained subspaces
  n_subspaces=dim(table)[1]
  
  ## list to store the plots and tables
  list_grid=vector("list", n_subspaces)
  
  ## Looping over the subspaces to obtain each plot and information table
  for (subspace in 1:(n_subspaces)){
    
    ## subspace plot of the subspace
    plot_subspace=CSViz_subspace_plot(results_CSViz_subspaces, subspace)+
      # deleting the legend
      ggplot2::theme( legend.position = "none")  
    
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
    set.seed(1234)
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
  display_subspaces=cowplot::plot_grid(display_subspaces,legend, nrow = 2, rel_heights = c(10, 0.4))
  return(display_subspaces)
}



###################################################################
###################################################################
######   VISUALIZATION OF SUBSPACES: DISPLAY STORYTELLING    ######
###################################################################
###################################################################

## Function to visualize the obtained subspaces with a plot and a information table for each one
## PARAMETERS:
# - results_CSViz_subspaces is the return of the CSViz_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized
# - max_kdn: maximum kDN allowed for a subspace to be displayed

CSViz_display_storytelling <- function(results_CSViz_subspaces, min_data, max_kdn){

    #######################################
    #### Checking initial requirements ####
    #######################################
    
    ## Error if parameters min_data or max_kdn are not in [0,1]
    if ((min_data<0 | min_data> 1)|(max_kdn<0 | max_kdn> 1)) {
      stop("min_data and max_kdn must be between 0 and 1")
    }
    
  
    ##################################################
    #### obtaining information table of subspaces ####
    ##################################################
    
    table=CSViz_table(results_CSViz_subspaces, min_data, max_kdn)
    
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
      plot_data=CSViz_available_data_subspace_plot(results_CSViz_subspaces,subspace)+
        # deleting the legend
        ggplot2::theme( legend.position = "none")  
      
      ## getting xlim and ylim of the plot
      ylim=layer_scales(plot_data)$y$get_limits()
      xlim=layer_scales(plot_data)$x$get_limits()
      
      ## converting to  grob
      set.seed(1234)
      plot_data=ggplotGrob(plot_data)
      

      ## subspace plot of the subspace
      plot_subspace=CSViz_subspace_plot(results_CSViz_subspaces, subspace,
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
    display_storytelling=cowplot::plot_grid(display_storytelling,legend, nrow = 2, rel_heights = c(10, 0.4))
    return(display_subspaces)
  }
  