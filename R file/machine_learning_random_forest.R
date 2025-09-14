install.packages("randomForest",repos = "https://cloud.r-project.org")
library("randomForest")


#statquest = https://www.youtube.com/watch?v=6EXPYzbfLCE
#data.imputed used to assign value to NA cell


#USE to estimate best random forest setting for forecasting
random_forest_function <- function(
  seed = NULL, #optional
  database = NULL,
  result_col = NULL, #col name in database that represent result  
  nvariable = NULL, #how many variables used in random forest
  ntree = NULL
){
  
  if(is.null(seed)){}else{
    set.seed(seed)
  }
  fig_col <- substitute(result_col)
  result_col <- substitute(result_col)
  name <- as_name(enquo(database))

  #checking for oob range
  random_forest_estimator(database = database, seed = seed, name = name, result_col = result_col, ntree = ntree)
  
  rf_formula <- as.formula(paste(deparse(result_col),"~ ."))
  result <- randomForest(rf_formula, data = database, mtry = nvariable, ntree = ntree)
    assign(paste0(name,"_model"), result, envir =.GlobalEnv)
  
  #plot graph for error  
  name_for_model <- paste0(name,"_model")
  error_graph_function(database = name_for_model, ntree = ntree)  
    
  full_name <- paste0(name,"_oob_range")
  values <- lapply(full_name, get, envir = .GlobalEnv)
  return(list(result = result, oob_from_possible_mtry = unlist(values)))
}


random_forest_estimator <- function(
    database = NULL,
    name = NULL,
    seed = NULL,
    result_col = NULL,
    ntree = NULL
    ){
  
  if(is.null(seed)){}else{
    set.seed(seed)
  }

  max_col <- ncol(database) - 1
  oob_values <- vector(length = max_col)
  #result_col <- substitute(result_col)
  rf_formula <- as.formula(paste(deparse(result_col),"~ ."))

  for(i in 1:max_col){
    temp.model <- randomForest(rf_formula, data = database, mtry = i, ntree = ntree)
    oob_values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
  }  
  
  assign(paste0(name,"_oob_range"), oob_values, envir = .GlobalEnv)
}

error_graph_function <- function(
    database = NULL,
    ntree = NULL
){
  col <- "err.rate"
  df <- get(database, envir = .GlobalEnv)[[col]]
  cname <- (colnames(df))
  
  new_df <- data.frame(
    Tree = rep(1:nrow(df), times = 3),
    Type = rep(cname, each = nrow(df)),
    Error = c(df[,cname[1]], df[,cname[2]],df[,cname[3]])
  )
  

  err_fig <- ggplot( new_df, aes(x = Tree, y = Error))+
    geom_line(aes(color = Type))
    #geom_vline(xintercept = ntree,linetype = "dashed", color ="red", linewidth =1)
  
  final_err_fig <<- ggplotly(err_fig)
}

