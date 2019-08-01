# changelog



# 20180115: add a mew function to plot the maxquant summary, check the the example for the required data format
# 20180110: revised version of matrix_missingvalue_filtering,  a new function name and 
# 20180109: revised/ a function to tidy proteingroups
# 20180109: revised/add a function to filter proteingroups by reverse, and concaminant
# 20180105: add a function from sthda to add grid on the scattter3d
# 20180104: add a function for changing color transparency
# 20171219: add a withCosoleRedirect function to direct the error messge to the browser
# 20171218: add a abundance support to compare to list: compare_two_vectorlist_with_value
# 20171216: specify package name to recode
# 20171212: add three functions, remove_allNA_rows, remove_allNA_columns, remove_1st_column


# special plot for maxquant result QC, to plot selected plot using data.frame with the follwing columns
# 1st column as sample names,
# 2nd column as values to plot
# columns after are meta information, usually the grouping information
# see example for the format
# this function has a newly desgined struction, to flexibly plot as required, instead of plotting all
# this is achieved by using a list structure to store the plot as required



MQ_QC_plot<- function(data.frame, 
                      plot_type = c("scatter", "bar", "density", "histogram", "freqpoly", "box", "violin") ,
                      group = NULL, 
                      cutoff = 20, 
                      maintitle = "", 
                      xlabel = "",
                      vertical =  FALSE,
                      ...
                ){
  
  
  # in case some column names are not valid names (containign special symbol, like space, % etc)
  names(data.frame)[1] <- "names"
  names(data.frame)[2] <- "value"
  
  data.frame$plot_order_x <- 1: nrow(data.frame) # this column is going to be used as x axis
  
  # of there is no grouping information, give all rows the same group
  if(is.null(group)){
    group <- "All"
    data.frame$All = "All"
    
  }
  
  
  if(length(plot_type) == 0){
    stop
  }else{
    plot_out <- list()
  }
  
  if("scatter" %in% plot_type){
    scatter_plot <- ggplot(data.frame) + 
      # geom_rect(data=data.frame(xmin=-Inf, xmax=Inf, ymin= -Inf, ymax=cutoff), 
      #           aes(xmin=xmin, xmax=xmax, ymin=ymin,ymax=ymax), 
      #           fill="red", alpha=0.1) +
      # geom_rect(data=data.frame(xmin=-Inf, xmax=Inf, ymin= cutoff, ymax=Inf), 
      #           aes(xmin=xmin, xmax=xmax, ymin=ymin,ymax=ymax), 
      #           fill="lightblue", alpha=0.5) +
      annotate("rect", xmin=-Inf, xmax= Inf, ymin=0, ymax=cutoff, alpha=0.1, fill="red") +
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=cutoff, ymax=Inf, alpha=0.5, fill="lightblue") +
      geom_point(aes_string(x = "plot_order_x", y = "value", colour = group)) +
      geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
      scale_x_continuous(breaks = 1:nrow(data.frame),labels = data.frame[,1]) +
      labs(title = maintitle, x = "", y = xlabel) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    
      if(vertical){
        scatter_plot <- scatter_plot + coord_flip()
      }
    
    plot_out <- c(plot_out, list("scatter_plot" = scatter_plot))
  }
  if("bar" %in% plot_type){
    bar_plot <- ggplot(data.frame) +
      
      annotate("rect", xmin=-Inf, xmax= Inf, ymin=0, ymax=cutoff, alpha=0.1, fill="red") +
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=cutoff, ymax=Inf, alpha=0.5, fill="lightblue") +
      geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
      geom_col(aes_string(x = "plot_order_x", y = "value", fill = group))+
      scale_x_continuous(breaks = 1:nrow(data.frame),labels = data.frame[,1]) +
      
      labs(title = maintitle, x = "",y = xlabel) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    
    if(vertical){
      bar_plot <- bar_plot + coord_flip()
    }
    
    plot_out <- c(plot_out, list("bar_plot" = bar_plot))
    
  }
  if("freqpoly" %in% plot_type){
    # distritibution
    freqpoly_plot <- ggplot(data.frame) +
      annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
      annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
      geom_freqpoly(aes_string("value",colour = group) )+
      geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)+
      labs(title = maintitle,  x = "",y = xlabel) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    if(vertical){
      freqpoly_plot <- freqpoly_plot + coord_flip()
    }
    
    plot_out <- c(plot_out, list("freqpoly_plot" = freqpoly_plot))
  }
  
  if("histogram" %in% plot_type){
    histogram_plot <- ggplot(data.frame) +
      annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
      annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
      geom_histogram(aes_string("value", colour = group, fill = group),position = "identity",alpha = 0.5)+
      geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)+
      labs(title = maintitle,  x = "",y = xlabel) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    if(vertical){
      histogram_plot <- histogram_plot + coord_flip()
    }
    plot_out <- c(plot_out, list("histogram_plot" = histogram_plot))
  }
  
  
  
  if("density" %in% plot_type){
    density_plot <-ggplot(data.frame) +
      annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
      annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
      geom_density(aes_string("value", colour = group, fill = group),position = "identity",alpha = 0.5) +
      geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)+
      labs(title = maintitle,  x = "",y = xlabel) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    if(vertical){
      density_plot <- density_plot + coord_flip()
    }
    plot_out <- c(plot_out, list("density_plot" = density_plot))
  }
  
  
  if("violin" %in% plot_type){
    violin_plot <- ggplot(data.frame) +
      geom_violin(aes_string(x =group,  y = "value", colour = group, fill = group))+
      geom_jitter(aes_string(x =group,  y = "value",colour = group, fill = group),shape=21)  +
      geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
      labs(title = maintitle,  x = "",y = xlabel) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    if(vertical){
      violin_plot <- violin_plot + coord_flip()
    }
    plot_out <- c(plot_out, list("violin_plot" = violin_plot))
    
  }
  
  
  if("box" %in% plot_type){
    box_plot <- ggplot(data.frame) +
      geom_boxplot(aes_string(x =group,  y = "value", colour = group, fill = group))+
      geom_jitter(aes_string(x =group,  y = "value",colour = group, fill = group),shape=21)  +
      geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
      labs(title = maintitle,  x = "",y = xlabel) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    if(vertical){
      box_plot <- box_plot + coord_flip()
    }
    plot_out <- c(plot_out, list("box_plot" = box_plot))
    
  }
  
  
  return(plot_out) 
  
  # Example
  # my_test <- data.frame("samplename" = paste0("sample_", 1:20),
  #                       "msms_id" = c(abs(rnorm(10))*10+20, abs(rnorm(10))*10+30),
  #                       "treat_group" = c(paste0("group_", rep("A",10)), paste0("group_", rep("B",10)))
  # )
  # 
  # tt <-  MQ_QC_plot(my_test, plot_type = c("scatter","bar","density", "histogram", "freqpoly", "box", "violin"), cutoff = 35, group = "treat_group", maintitle = "MSMS ID Rate", xlabel = "MS/MS ID %")
  # now tt has all the required plot
  
  
}







# this function organize the readin data.frame of  summary.txt  
# then split it into three sections, one for rawfile summary, one for experiemnt summary, and one for total summary
# return a list




organize_summary.txt <- function(df_summary.txt){
  
  # take out the last line to organize
  last_line <- t(df_summary.txt[nrow(df_summary.txt),])
  last_line[last_line[,1] == ""] <- NA
  last_line[last_line[,1] == "0"] <- NA
  last_line <- last_line[-1,, drop = FALSE]
  last_line <- last_line[which(!is.na(last_line[,1])),, drop =  FALSE]
  colnames(last_line) <- ("values")
  
  # take out rows about raw files summary
  summary <- df_summary.txt[-nrow(df_summary.txt),] # remove the last line
  df_rawfiles <- remove_allNA_columns(summary[which(summary[,2] != ""),])
  
  # take out rows abotu experiment summary
  df_experiment <- summary[which(summary[,2] == ""),]
  df_experiment[df_experiment== ""] <- NA
  df_experiment <- remove_allNA_columns(df_experiment)
  
  
  return(list("summary_all" = last_line,
              "summary_rawfiles" = df_rawfiles,
              "summary_experiment" = df_experiment
              
  ))
  
}








# matrix_process is a wrapper of several function to do missvalue filtering, missing value imputation
# log transoformation, and scale
# default argument is do scaling only on column
# Value: a list, use $data_matrix_processed to get the processed value 

marix_process2 <- function(data_matrix, 
                           missingvalue_filtering = TRUE,
                           zero_as_missing = TRUE,
                           Q = 0.75, 
                           Imputation = TRUE, 
                           Imputation_alpha = 0.9, # do not change unless you know it
                           log10_transform = FALSE,
                           log2_transform = FALSE,
                           scale_columnwise = TRUE, 
                           scale_rowwise = FALSE)
{
  # 
  result_list <- list() # this is a new way to record the temprary result
  
  if(missingvalue_filtering){
    if(zero_as_missing){
      data_matrix[data_matrix == 0]<-NaN # replace the 0 with NaN
    }
    
    # missing value filtering
    filter_result <- matrix_missingvalue_filtering(data_matrix, Q)
    
    # take over the result
    data_matrix <- filter_result$data_qualified 
    #my_list  <- 
    result_list <- c(result_list, list( data_qualified_for_missing_value = data_matrix,
                                        data_not_filtered_out_for_missing_value = filter_result$data_not.qualified,
                                        number_of_rows_qualified_by_missing_value = filter_result$number.qualified,
                                        number_of_rows_filtered_out_by_missing_value = filter_result$number.not.qualified
    ))
    
  }
  
  if(log10_transform){ # log transform
    data_matrix <- log10(data_matrix)
    result_list <- c(result_list, list(data_matrix_log10_tranformed = data_matrix))
  }
  
  if(log2_transform){ # log transform
    data_matrix <- log2(data_matrix)
    result_list <- c(result_list, list(data_matrix_log2_tranformed = data_matrix))
  }
  
  
  
  if(Imputation){  # missing value imputation
    data_matrix <- rrcovNA::impSeqRob(data_matrix, alpha = Imputation_alpha)$x
    result_list <- c(result_list, list(data_matrix_imputed = data_matrix))
  }
  
  if (scale_columnwise){ # do column scaling, keep in mind that the scale function in R is scaling by column
    data_matrix <- scale(data_matrix)
    result_list <- c(result_list, list(data_matrix_scale_columnwise = data_matrix))
  }
  
  if (scale_rowwise){ # scaling of each protein
    data_matrix <- t(scale(t(data_matrix)))
    result_list <- c(result_list, list(data_matrix_scale_rowwise = data_matrix))
  }
  
  #my_list  <- 
  result_list <- c(result_list, list(data_matrix_processed = data_matrix))
  
  return(result_list)
}



#________________________________________________________________________________________

#     matrix_missingvalue_filtering
#________________________________________________________________________________________

# ___Description___: 
# filter a matrix out clumn with more than NA/infinte preset(inf values could be generated from log transformation or dividing conversion), 
# threshold is the Q value of the valid values, rows with more valid values than threshold will be kept as qualified
# only do row-wise filtering, transpose first if do column-wise filtering

#__Usage__:
# matrix_missingvalue_filtering(data, Q =3)
# matrix_missingvalue_filtering(data, Q = 0.75)

# ___Arguments___:
# data: data matrix with missing values, NA/inf
# threshold: how many (percentage) non-missingvalues are required to be in the matrix
#             can be two types, one is the nnumber of the missing values,the other one is the so called Q value, which is the percentage(0 <= Q <= 1) to the number of columns
#             Q value == 1, require no missing values, 
#             Q value == 0, no filtering,
#             Q value will be converted to the number of missing value (1 < number < ncol(data))
#             will report an error if not setup in this range
#             in this function, celing is used to convert the percentage, if not expected, try floor/round etc

# ___Values___:
# a list
# qualified data matrix, not.qulified data.matrix, number of rows of quailfied/ not qulified. 
# 


matrix_missingvalue_filtering <- function(data.matrix, Q = 1){ #  value can only be from 0~1 or 1~ number of columns
  
  data.matrix[is.infinite(as.matrix(data.matrix))]<-NA # the is.infinite function does not work on data.frame, 
  # in case there are infinte values there
  
  if(Q < 0|Q > ncol(data.matrix) ){
    print ("Q value can only be from 0~1 or 1~ number of columns")
  }else{
    
    if(Q > 0 && Q <= 1){ # concet the q value to the real missing value number
      threshold <- ceiling(ncol(data.matrix)*Q)
    }else if(Q >1 && Q <=  ncol(data.matrix)) {
      threshold <- Q
    }
    
    data_qualified <- data.matrix[which(apply(data.matrix,1,function(x)(sum(is.na(x))) <= (ncol(data.matrix)-threshold))),]
    data_not.qualified <- data.matrix[which(apply(data.matrix,1,function(x)(sum(is.na(x))) > (ncol(data.matrix)-threshold))),]
    return(list(data_qualified = data_qualified, 
                data_not.qualified = data_not.qualified,
                number.qualified = nrow(data_qualified), 
                number.not.qualified = nrow(data_not.qualified)
    ))
    
  }
}





# this is a revised version for tidy proteinsGroups
# proteinGroups has to be readin with check.names = FALSE, otherwise not working
tidy_proteingroups_2 <- function(proteinGroups = NULL, 
                                 experimentDesgin = NULL,
                                 remove_rows_marked_with = c("Only identified by site", "Reverse","Potential contaminant"),
                                 keep_cols_starts_with = "LFQ intensity ", # the columns to be selected starting with, use the maximum lengh
                                 filter_all_zero = TRUE){
  suppressMessages(install.packages.auto(dplyr)) # select command
  
  
  protein.ids_split <- strsplit(as.vector(proteinGroups$"Protein IDs"), ";") # this is a list of list of split names
  protein_primary_ids <- unlist(lapply(protein.ids_split, function(x) x[1])) # only keep the first one
  
  # rownames to be the protein ID
  proteinGroups_annoatation <- select(proteinGroups, c("Protein IDs", "Protein names","Gene names","Fasta headers"))
  rownames(proteinGroups_annoatation) <- proteinGroups$id
  proteinGroups_annoatation$first_major_protein <- protein_primary_ids
  
  # rename the rownames of the matrix to have the same id
  rownames(proteinGroups) <- proteinGroups$id 
  
  
  # removing rows marked with 
  if (length(remove_rows_marked_with) >=1 ){
    PG_row_filtering_plus <- PG_filter_rows_by_plus(proteinGroups, columnames = remove_rows_marked_with)
    proteinGroups_filtered <- PG_row_filtering_plus$proteinGroups_filtered
    proteinGroups_filtered_out_as_plus <- PG_row_filtering_plus$number_of_rows_filtered_out
  } else{
    proteinGroups_filtered_out_as_plus <- NULL
  }
  
  # keeping columns starts with
  if (length(keep_cols_starts_with) >= 1 ){
    proteinGroups_filtered <- select(proteinGroups_filtered, starts_with(keep_cols_starts_with)) # select command in dplyr package
  }
  
  # shorten column names, by removing the "starts with"
  colnames(proteinGroups_filtered)<-gsub(keep_cols_starts_with, "", colnames(proteinGroups_filtered))
  
  #remove rows with all 0, this is only for data value matrix
  if (filter_all_zero){
    index_all_zero <- apply(proteinGroups_filtered,1,function(x)all(x == 0))
    proteinGroups_filtered <- proteinGroups_filtered[-which(index_all_zero),]
    proteinGroups_filtered_out_all_zero <- length(which(index_all_zero))
    print(paste0("ProteinGroups with all zeros: ", proteinGroups_filtered_out_all_zero))
  }else{
    proteinGroups_filtered_out_all_zero <- NULL
  }
  
  
  # subsect the annotation file
  proteinGroups_annoatation <- proteinGroups_annoatation[match(rownames(proteinGroups_filtered),rownames(proteinGroups_annoatation)),]
  
  
  # further select columns in Experiment desgin, if not all used in Experiment desgin(if experiment desgin provided)
  if(class(experimentDesgin) ==  "data.frame"){
    proteinGroups_filtered<- proteinGroups_filtered[,which(colnames(proteinGroups_filtered) %in% experimentDesgin[,1])]
    experimentDesgin<-experimentDesgin[match(colnames(proteinGroups_filtered),as.character(experimentDesgin[,1])),] # just in case the order is not the same
    # check if the experiment is aligned properly
    alignment_check<-cbind(as.character(experimentDesgin[,1]),colnames(proteinGroups_filtered))
    
    
    
    return(list(data_matrix = proteinGroups_filtered,
                proteinGroups_annoatation = proteinGroups_annoatation,
                proteinGroups_filtered_out_as_plus =  proteinGroups_filtered_out_as_plus,
                proteinGroups_filtered_out_all_zero = proteinGroups_filtered_out_all_zero,
                groups = experimentDesgin,
                alignment_check = alignment_check
    )) 
    
  }else{
    #write.table(proteinGroups_filtered,"Out_ProteinGroups_tidied_up.txt",sep="\t",row.names = TRUE,col.names = NA)  
    return(list(proteinGroups_filtered = proteinGroups_filtered,
                proteinGroups_annoatation = proteinGroups_annoatation,
                proteinGroups_filtered_out_as_plus =  proteinGroups_filtered_out_as_plus,
                proteinGroups_filtered_out_all_zero = proteinGroups_filtered_out_all_zero
    )) 
  }
  
}







# this function is a revised version filtering out the rows marked by "+", mostly with three columns c("Only.identified.by.site", "Reverse","Potential.contaminant"))


PG_filter_rows_by_plus<-function(proteinGroups, columnames = c("Only.identified.by.site", "Reverse","Potential.contaminant")){
  
  suppressMessages(install.packages.auto(dplyr))
  suppressMessages(install.packages.auto(lazyeval)) # this is very important for passing column names as parameters to the function
  # notice that the rownames are silently dropped druing filter, even there are row names, 
  # set the column names by using a temp column
  proteinGroups_temp <- proteinGroups
  proteinGroups_temp$temp.rownames <- rownames(proteinGroups_temp)
  
  for(filter_name in columnames)  {
    #print(paste("filtering by",filter_name))
    filter_criteria <- lazyeval::interp(quote(x != "+"), x = as.name(filter_name))
    # note:
    # though the underscored version dplyr verbs are deprecated, still, in some cases, only the underscored version works
    # use this try will try filter_ first, which is more likely to work, otherwise try filter, which in most cases not needed
    proteinGroups_filtered <- try(dplyr::filter_(proteinGroups_temp, filter_criteria)) 
    if(class(proteinGroups_filtered)  == "try-error"){
      proteinGroups_filtered <- try(dplyr::filter(proteinGroups_temp, filter_criteria)) 
    }
    number_filter_out <- nrow(proteinGroups_temp) - nrow(proteinGroups_filtered)
    proteinGroups_temp <- proteinGroups_filtered
    
  
    print(paste0(filter_name, ": ", number_filter_out))
    
  }
  # change back the row names
  rownames(proteinGroups_temp) <- proteinGroups_temp$temp.rowname
  proteinGroups_temp <- proteinGroups_temp[,-ncol(proteinGroups_temp)] # remove the temp column
  number_of_rows_filtered_out <- nrow(proteinGroups) -  nrow(proteinGroups_temp)
  
  return(list(
    proteinGroups_filtered = proteinGroups_temp,
    number_of_rows_filtered_out = number_of_rows_filtered_out
  )
  )  
}







# http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r
#' Add grids to a scatterplot3d
#' 
#' @description The goal of this function is to add grids on an existing
#'  plot created using the package scatterplot3d
#' @param x,y,z numeric vectors specifying the x, y, z coordinates of points.
#'  x can be a matrix or a data frame containing 3 columns corresponding to
#'  the x, y and z coordinates. In this case the arguments y and z are optional
#' @param grid specifies the facet(s) of the plot on which grids should be drawn.
#'  Possible values are the combination of "xy", "xz" or "yz".
#'  Example: grid = c("xy", "yz"). The default value is TRUE to add grids only on xy facet.
#' @param col.grid,lty.grid color and line type to be used for grids
#' @param lab a numerical vector of the form c(x, y, len).
#'  The values of x and y give the (approximate) number of tickmarks on the x and y axes.
#' @param lab.z the same as lab, but for z axis
#' @param scale.y of y axis related to x- and z axis
#' @param angle angle between x and y axis
#' @param "xlim, ylim, zlim" the x, y and z limits (min, max) of the plot.
#' 
#' @note
#' Users who want to extend an existing scatterplot3d graphic with the
#'  function addgrids3d, should consider to set the arguments scale.y, angle, ...,
#'  to the value used in scatterplot3d.
#' 
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' 
#' @example
#' library(scatterplot3d)
#' data(iris)
#' scatterplot3d(iris[, 1:3], pch = 16, grid=T, box=F)
#' addgrids3d(iris[, 1:3], grid = c("xy", "xz", "yz"))
addgrids3d <- function(x, y=NULL, z=NULL, grid = TRUE,
                       col.grid = "grey", lty.grid = par("lty"),
                       lab = par("lab"), lab.z = mean(lab[1:2]),
                       scale.y = 1, angle = 40,
                       xlim=NULL, ylim=NULL, zlim=NULL){
  
  
  if(inherits(x, c("matrix", "data.frame"))){
    x <- as.data.frame(x)
    y <- unlist(x[,2])
    z <- unlist(x[,3])
    x <- unlist(x[,1])
  }
  
  p.lab <- par("lab")
  
  angle <- (angle%%360)/90
  yz.f <- scale.y * abs(if (angle < 1) angle else if (angle >3) angle - 4 else 2 - angle)
  yx.f <- scale.y * (if (angle < 2) 1 - angle else angle - 3)
  
  
  # x axis range
  x.range <- range(x[is.finite(x)], xlim)
  x.prty <- pretty(x.range, n = lab[1], min.n = max(1, min(0.5 *lab[1], p.lab[1])))
  x.scal <- round(diff(x.prty[1:2]), digits = 12)
  x <- x/x.scal
  x.range <- range(x.prty)/x.scal
  x.max <- ceiling(x.range[2])
  x.min <- floor(x.range[1])
  if (!is.null(xlim)) {
    x.max <- max(x.max, ceiling(xlim[2]/x.scal))
    x.min <- min(x.min, floor(xlim[1]/x.scal))
  }
  x.range <- range(x.min, x.max)
  
  # y axis range
  y.range <- range(y[is.finite(y)], ylim)
  y.prty <- pretty(y.range, n = lab[2], min.n = max(1, min(0.5 *lab[2], p.lab[2])))
  y.scal <- round(diff(y.prty[1:2]), digits = 12)
  y.add <- min(y.prty)
  y <- (y - y.add)/y.scal
  y.max <- (max(y.prty) - y.add)/y.scal
  if (!is.null(ylim))
    y.max <- max(y.max, ceiling((ylim[2] - y.add)/y.scal))
  
  # Z axis range
  z.range <- range(z[is.finite(z)], zlim)
  z.prty <- pretty(z.range, n = lab.z, min.n = max(1, min(0.5 *lab.z, p.lab[2])))
  z.scal <- round(diff(z.prty[1:2]), digits = 12)
  z <- z/z.scal
  z.range <- range(z.prty)/z.scal
  z.max <- ceiling(z.range[2])
  z.min <- floor(z.range[1])
  if (!is.null(zlim)) {
    z.max <- max(z.max, ceiling(zlim[2]/z.scal))
    z.min <- min(z.min, floor(zlim[1]/z.scal))
  }
  z.range <- range(z.min, z.max)
  
  # Add grid
  if ("xy" %in% grid || grid == TRUE) {
    i <- x.min:x.max
    segments(i, z.min, i + (yx.f * y.max), yz.f * y.max + 
               z.min, col = col.grid, lty = lty.grid)
    i <- 0:y.max
    segments(x.min + (i * yx.f), i * yz.f + z.min, x.max + 
               (i * yx.f), i * yz.f + z.min, col = col.grid, lty = lty.grid)
  }
  
  if ("xz" %in% grid) {
    i <- x.min:x.max
    segments(i + (yx.f * y.max), yz.f * y.max + z.min, 
             i + (yx.f * y.max), yz.f * y.max + z.max, 
             col = col.grid, lty = lty.grid)
    temp <- yx.f * y.max
    temp1 <- yz.f * y.max
    i <- z.min:z.max
    segments(x.min + temp,temp1 + i, 
             x.max + temp,temp1 + i , col = col.grid, lty = lty.grid)
    
  }
  
  if ("yz" %in% grid) {
    i <- 0:y.max
    segments(x.min + (i * yx.f), i * yz.f + z.min,  
             x.min + (i * yx.f) ,i * yz.f + z.max,  
             col = col.grid, lty = lty.grid)
    temp <- yx.f * y.max
    temp1 <- yz.f * y.max
    i <- z.min:z.max
    segments(x.min + temp,temp1 + i, 
             x.min, i , col = col.grid, lty = lty.grid)
  }
  
}


add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
  # demo
  # color <-  c("#00B3FA","#00B3FA80")
  # add.alpha(color, 0.5)
}




withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}




# A, B is a vector, elemment is a also a vector
# A <-c("a,b,c", "d,e,f")
# B <-c("a,b,c,d", "d,e,f,g", "a,b,c,d,e")
# expression <- data.frame(protein = letters[1:7], abundance =  sample(1:100, 7))
## expression has to be two columns data.frame
# r <- compare_two_vectorlist_with_value(A, B, expression)
# 


compare_two_vectorlist_with_value <- function(A, B, expression, sep = ","){
  result_matrix_num <-  matrix(data = NA, ncol = length(A), nrow = length(B))
  rownames(result_matrix_num) <- names(B)
  colnames(result_matrix_num) <- names(A)
  
  result_matrix_ele <- result_matrix_expression <-result_matrix_num
  
  
  
  for(i in 1: length(A)){
    A_element<- strsplit(A[i], sep)[[1]]
    for(j in 1: length(B)){
      B_element<- strsplit(B[j], sep)[[1]]
      elements <- intersect(A_element, B_element)
      result_matrix_ele[j,i] <- toString(elements)
      result_matrix_expression[j,i] <- sum(expression[match(elements,expression[,1]),2])
      
      result_matrix_num[j,i] <- length(intersect(A_element, B_element))
    }
  }
  
  return(list(result_matrix_num = result_matrix_num,
              result_matrix_expression = result_matrix_expression,
              result_matrix_ele = result_matrix_ele
  )
  )
  
}





remove_allNA_rows <- function(df){
  
  if(any(apply(df,1,function(x)all(is.na(x))))){ # if there is any all NA rows
    df[-which(apply(df,1,function(x)all(is.na(x)))),]
  }else{
    df
  }
  
  # this function can deal with data.frame and data matrix
  # note that NA has to be NA, otherwise, precess the data first
}

remove_allNA_columns <- function(df){
  if(any(apply(df,2,function(x)all(is.na(x))))){ # if there is any all NA columns
    df[,-which(apply(df,2,function(x)all(is.na(x))))]
  }else{
    df
  }
  # this function can deal with data.frame and data matrix
  # note that NA has to be NA, otherwise, precess the data first
}




remove_1st_column <- function(df){
  if(dim(df)[2] >=2 && length(unique(df[,1])) == 1){
    #print(length(unique(df[,1])) == 1)
    df <- df[,-1, drop =  FALSE]
    df <- remove_1st_column(df)
    df
  }else{
    df
  }
  # this function deals with hirarchically structured data.frame, for better display using treemap like method 
  # if the first column only has one unique value, remove it
  # if the following one still has one unique value,do it again, untill the remaing first column 
  # example
  # d <- data.frame(a = "A", b = "B", c = LETTERS[1:5])
  # remove_1st_column(d)
}



range_standarize_1 <- function(x){
  (x-min(x))/(max(x)-min(x))
}

range_standarize_100 <- function(x){
  round(100*(x-min(x))/(max(x)-min(x)))
}



# subfunctions, as the function name says
combine_list_to_matrix <-function(vector_list){
  if(class(vector_list) == "list"){
    all <- unique(unlist(vector_list))
    match_list <- lapply(vector_list,function(x) all %in% x)
    presence_matrix <- matrix(unlist(match_list), ncol =length(match_list) , byrow = FALSE)
    colnames(presence_matrix) <- names(vector_list)
    rownames(presence_matrix) <-all
    presence_matrix[which(presence_matrix == FALSE)] <- 0
    presence_matrix[which(presence_matrix == TRUE)] <- 1
    return(presence_matrix)
    
  }else if (class(vector_list) == "character"){
    m <- matrix(1:length(vector_list))
    rownames(m) <- vector_list
    colnames(m) <- "inputlist"
    return(m)
  }
  
  # this function convert a vector list into a expression matrix, with presence as 1, and absence as 0
  
  #test_list <- list(A = letters[1:10], B =  letters[5:15])
  #combine_list_to_matrix(test_list)
}



recode_for_sankey <- function(df){
  install.packages.auto(car) # use the recode function
  nodes_names <- union(df[[1]], df[[2]])
  code <- 0:length(nodes_names)
  nodes <- data.frame(name = nodes_names)
  
  expression <- toString(paste0("'",nodes_names,"'"," = ",code,collapse=";"))
  df_recode<- as.data.frame(apply(df, 2, function(x) {x <- car::recode(x,expression)}))
  
  return(list(df_links = df_recode,
              df_nodes = nodes
  ))
  
  # selfmade function @ 20171130
  # general interaction network function/package accept data.frame data structure
  # with three columns, first two columns are node names with intereactiton direction
  # 3rd column is numeric value 
  # however, currently, sankeyNetwork {networkD3} only accepts Links with data.frame with node number/index , staring from 0 
  # and a Nodes argument with data.frame structure, with the node id and propertie
  # use ?sankeyNetwork for more help
  
  # this fucntion preprocess the edge data.frame and retures two data.frames in one variable, ready for plot
  # links data.frame format, the same as input data.frame, with node names converted to id/index (numeric)
  # the first col is the from/soruce column#
  # the second col is the to/target column
  # node data.frame format:
  # one column, name of node id 
  
  # example
  # df <- data.frame(from = sample(letters,10), to = sample(letters,10), value = sample(1:100,10))
  # df_s <- recode_for_sankey(df)
  
  
}


# this function can wrap long sentence into \n separated multilines, based on words
# this bult in function has the same function strwrap

wrap_sentence <- function(string, width) {
  words <- unlist(strsplit(string, " "))
  fullsentence <- ""
  checklen <- ""
  for(i in 1:length(words)) {
    checklen <- paste(checklen, words[i])
    if(nchar(checklen)>(width+1)) {
      fullsentence <- paste0(fullsentence, "\n")
      checklen <- ""
    }
    fullsentence <- paste(fullsentence, words[i])
  }
  fullsentence <- sub("^\\s", "", fullsentence)
  fullsentence <- gsub("\n ", "\n", fullsentence)
  return(fullsentence)
}




# A, B is a vector, elemment is a also a vector
# A <-c("a,b,c", "d,e,f")
# B <-c("a,b,c,d", "d,e,f,g", "a,b,c,d,e")
# r <- compare_two_vector_list(A, B)

compare_two_vector_list <- function(A, B, sep = ","){
  result_matrix_num <-  matrix(data = NA, ncol = length(A), nrow = length(B))
  rownames(result_matrix_num) <- names(B)
  colnames(result_matrix_num) <- names(A)
  
  result_matrix_ele <-result_matrix_num
  
  
  
  for(i in 1: length(A)){
    A_element<- strsplit(A[i], sep)[[1]]
    for(j in 1: length(B)){
      B_element<- strsplit(B[j], sep)[[1]]
      result_matrix_ele[j,i] <- toString(intersect(A_element, B_element))
      result_matrix_num[j,i] <- length(intersect(A_element, B_element))
    }
  }
  
  return(list(result_matrix_num = result_matrix_num,
              result_matrix_ele = result_matrix_ele
  )
  )
  
}





# turan a 2d data matrix into a column based percentage 

table2lPercents_by_col <- function (data_matrix, digits = 2){
  sums <- colSums(data_matrix)
  per <- t(apply(data_matrix, 1, function(x) x/sums))
  per <- round(100 * per, digits)
  per
  
}





ggsimpleline <- function(data_matrix, 
                         plot.by = "Row", 
                         x_cord = NULL, # has to be a numeric vector, with the same length with either row (for column plot) or column (for row plot)
                         index = c(1,2),
                         linetype = "solid",
                         linewidth = 1,
                         spline_smooth = FALSE,
                         # down is from ggplot2_prettier
                         maintitle = "",
                         xlab = "",
                         ylab = "",
                         axis.text.angle.x = 0,
                         axis.text.angle.y = 0,
                         vertical =  FALSE
){
  
  install.packages.auto(reshape2)
  data<- as.matrix(data_matrix)
  
  
  if(plot.by == "Row"){
    df <- t(data[index, ,drop = FALSE])
    xlabels <- colnames(data)
  } else if(plot.by == "Column"){
    df <- data[,index,drop = FALSE]
    xlabels <- rownames(data)
  }
  
  if(!is.null(x_cord)){
    rownames(df) <- x_cord
  }
  
  m<- melt(as.matrix(df))
  
  
  if(spline_smooth){
    # generate a new df for geom_line
    if(!is.null(x_cord)){
      
      df_smooth<- data.frame(apply(df, 2,function(x){spline(as.numeric(x_cord),x)$y}))
      m_smooth<- melt(as.matrix(df_smooth))
      #m_smooth$Var1 <- spline(as.numeric(x_cord))$y 
      m_smooth$Var1 <- spline(as.numeric(x_cord),df[,1])$x
      
    }else{
      df_smooth<- data.frame(apply(df, 2,function(x){spline(x)$y}))
      m_smooth<- melt(as.matrix(df_smooth))
      m_smooth$Var1 <- spline(1:nrow(df))$y # spline will generate 3 times point smoother by default ( change by n)
    }
    
    
    p <-   ggplot(m, aes(Var1, value, group=Var2, color = Var2)) + 
      geom_point()+
      geom_line(
        data=m_smooth,
        linetype = linetype,
        size = linewidth) +
      scale_colour_discrete(name = "")
    
    
  }else{
    p <- ggplot(m, aes(Var1, value, group=Var2, color = Var2)) + 
      geom_line(linetype = linetype,
                size = linewidth)  + 
      scale_colour_discrete(name = "")
    
  }
  
  p <- ggplot2_prettier(p,
                        maintitle = maintitle,
                        xlab = xlab,
                        ylab = ylab,
                        axis.text.angle.x = axis.text.angle.x,
                        axis.text.angle.y = axis.text.angle.y,
                        vertical =  FALSE
  )
  
  return(p)
  
}





ggplot2_prettier <- function(ggplot2_object,
                             maintitle = "",
                             xlab = "",
                             ylab = "",
                             axis.text.angle.x = 0,
                             axis.text.angle.y = 0,
                             vertical =  FALSE)
{
  
  p <- ggplot2_object + 
    labs(title = maintitle, 
         x = xlab, 
         y = ylab ) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = axis.text.angle.x, hjust = 1),
          axis.text.y = element_text(angle = axis.text.angle.y, hjust = 1),
          panel.grid = element_blank()) 
  
  if(vertical){
    p <- p+ coord_flip()
  }
  
  return(p)
  
}





#________________________________________________________________________________________
#     matrix_quick_heatmap
#________________________________________________________________________________________

# ___Description___: 
# 1: 
# 2: 

# ___Arguments___:
# data matrix
# factor (usually the grouping information) to be color grouped in either column  or row

#____Usage____;

# ___Values___:
# 
# 


matrix_quick_heatmap <- function(matrix, 
                                 scale = "row",
                                 col_groupcolor_factor = NULL , 
                                 row_groupcolor_factor = NULL, 
                                 Col_tree = TRUE){
  
  install.packages.auto(gplots)
  svg(tempfile(),onefile = TRUE)
  dev.control('enable')  
  
  if(is.null(row_groupcolor_factor) & is.null(col_groupcolor_factor)){
    # if no grouping information provided
    if(Col_tree){
      heatmap.2(matrix, col=bluered, trace = "none",
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = "") 
    }else{
      heatmap.2(matrix, col=bluered, trace = "none",
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                Colv  = FALSE,
                dendrogram = "row",
                key.title  = "",
                key.xlab = "",
                key.ylab = "") 
    }
    
    
    
  }else if((length(col_groupcolor_factor) > 0) & is.null(row_groupcolor_factor)){
    # if only colum grouping information provided
    
    col_groupcolor_factor <- col_groupcolor_factor[order(col_groupcolor_factor[,2]),]
    matrix<- matrix[,match(col_groupcolor_factor[,1],colnames(matrix))]
    
    color_labeling <- factor(col_groupcolor_factor[,2]) # removing unused factors
    levels(color_labeling) <- rainbow(length(levels(color_labeling))) # rename the 
    color_labeling <- as.vector(color_labeling)
    if(Col_tree){
      
      heatmap.2(matrix, 
                col=bluered, 
                ColSideColors = color_labeling,
                trace = "none", 
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = "") 
      
      
    }else{
      heatmap.2(matrix, 
                col=bluered, 
                ColSideColors = color_labeling,
                trace = "none", 
                scale = scale,
                Colv  = FALSE, 
                dendrogram = "row",
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = ""
      ) 
    }
    
  }else if((length(row_groupcolor_factor) > 0) & is.null(col_groupcolor_factor)){
    # if only row grouping information provided
    
    row_groupcolor_factor <- row_groupcolor_factor[match(rownames(matrix), row_groupcolor_factor[,1]),]
    
    row_groupcolor_factor <- row_groupcolor_factor[order(row_groupcolor_factor[,2]),]
    matrix<- matrix[match(row_groupcolor_factor[,1],rownames(matrix)),]
    
    color_labeling <- factor(row_groupcolor_factor) # removing unused factors
    levels(color_labeling) <- rainbow(length(levels(color_labeling))) # rename the 
    color_labeling <- as.vector(color_labeling)
    if(Col_tree){
      heatmap.2(matrix,
                col=bluered, 
                ColSideColors = color_labeling,
                trace = "none", 
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = "" ) 
      
    }else{
      heatmap.2(matrix,
                col=bluered, 
                ColSideColors = color_labeling,
                trace = "none", 
                scale = scale,
                Colv  = FALSE,
                dendrogram = "row",
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = ""
      ) 
    }
  }
  p1 <- recordPlot()
  dev.off()
  return(p1)
}

