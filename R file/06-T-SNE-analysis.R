#Tsne

#need set seed


#if dataset is duplicated then need to transform the data
#method as below:

# test <- flavors_of_cacao_without_na %>% 
#   select(rating, cocoapercent) %>% 
#   as.matrix()
# transform <- test + matrix(
#   rnorm(length(test), mean = 0, sd = 1e-2),
#   nrow = nrow(test)
# ) 


tsne_function <- function(
    #--- first layer (calculation) ---
    database = NULL,
    name = NULL, #if no assigned name then use database as name
    
    #optional and adjustable
    dimension = NULL, # this used to set intended dimension to reduce. 
    point_to_consider = NULL, # refer to how many neighbors each point consider when building the low-dimensional map
    blur_rate = NULL,
    number_of_try = NULL,
    
    #--- second layer (plotting) ---
    group_col = NULL,
    
    #adjustable
    seed = NULL,
    show_iteration_msg  = TRUE, # This use to show message
    checking = F
){
  #defined
  name <- if(is.null(quo_get_expr(enquo(name)))){ as_name(enquo(database)) }else{ as_name(enquo(name)) }
  
  #assign value if is null
  if(is.null(point_to_consider)){point_to_consider <- nrow(database)/3}   #perplexity ideally set as n/3
  if(is.null(dimension)){dimension <- 2}
  if(is.null(blur_rate)){blur_rate <- 0.5} #this range from 0 ~ 1 (the lower the value, the higher the accuracy)
  if(is.null(number_of_try)){number_of_try <- 1000}
  if(is.null(seed)){seed <- 205}
    
  
  set.seed(seed)
  #tsne calculation======================================================
  
  numeric_data <- database[sapply(database, is.numeric)] %>% as.matrix()
  
  #Transforming data
  if(anyDuplicated(numeric_data) > 0){
    if(checking == T){
      transforming <- "Yes"
    }
    numeric_data <- numeric_data + matrix(
      rnorm(length(numeric_data), mean = 0, sd =1e-3),
      nrow = nrow(numeric_data)
    )
  }else{
    transforming <- "No"
  }
  
  #TSNE
  tsne_result <- Rtsne(
    numeric_data,
    dim = dimension,
    perplexity = point_to_consider,
    theta = blur_rate,
    verbose = show_iteration_msg,
    max_iter = number_of_try
  )
 
  assign(paste0(name,"_tsne_result"), tsne_result, envir = .GlobalEnv)
  
  #plotting==============================================================
  group_col <- quo_get_expr(enquo(group_col))
  
  if(is.null(group_col)){
    df <- data.frame(
      X = tsne_result$Y[,1],
      Y = tsne_result$Y[,2]
    )
    
    fig <-
      ggplotly(
        ggplot(
          data = df,
          aes(
            x = X,
            y = Y
          ))+  
          geom_point(alpha = 0.7) +
          labs(title = "t-SNE Result") +
          theme_minimal()
      )
    
  }else{
    group_col <- as.name(group_col)
    df <- data.frame(
      category = database[[group_col]],
      X = tsne_result$Y[,1],
      Y = tsne_result$Y[,2]
    )
    
    fig <-
    ggplotly(
      ggplot(
        data = df,
        aes(
          x = X,
          y = Y,
          color = category
        ))+  
        geom_point(alpha = 0.7) +
        labs(title = "t-SNE Result") +
        theme_minimal()
    )
  }
  
  
  if(checking == T){
    
    result <- paste0("Number of Considering: ", point_to_consider, "\n",
                     "Data Transformation: ", transforming)
    return(cat(result))
  }
  
  fig
}






