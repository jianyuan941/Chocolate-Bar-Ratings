#pca analysis used to reduce multiple dimension and display dimension which is 
#essential to analysis


#source from:
#https://www.youtube.com/watch?v=5vgP05YpKdE&t=276s
#https://www.youtube.com/watch?v=5vgP05YpKdE statquest - Principal Component Analysis (PCA) - easy and practical explanation
#https://www.youtube.com/watch?v=PFDu9oVAE-g - Eigenvectors and eigenvalues | Chapter 14, Essence of linear algebra
#calculation as per below
#lets say i got a dataset: iris 

#use function prcomp to extract following information



#pca rotation mean each column contribute to pc
pca_function<- function(
    att_for_correlation = NULL, #if this being assigned then will show relationship between variable
    database = NULL, #sort data only left numerical data in the dataset before usd
    name = NULL # in format "" | as_name()
    ){
  att_checking <- quo_get_expr(enquo(att_for_correlation))
  
  #compute standard deviations, PCs, rotation, Xs
  df <- prcomp(database, center = T, scale = T)
  
  
  assign(name, df, envir = .GlobalEnv) #setup raw data
  assign(paste0(name,"_loading"), as.data.frame(df$rotation), envir =.GlobalEnv) # formula for pc
  assign(paste0(name,"_score"), as.data.frame(df$x), envir =.GlobalEnv) #setup for score for ggplot
  
  if(is.null(att_checking)){
    summary(df)  
  }else{
    
    df_for_plot <- as.data.frame(df$rotation)
    df_for_plot$att <- att_for_correlation
    
    ggplot(df_for_plot, aes(x = PC1, y= PC2, label = att))+
      geom_point(color = "blue", size = 3) +
      geom_text_repel() +
      theme_minimal() +
      labs(title = "Loadings Plot (PC1 vs PC2)",
           x = "PC1",
           y = "PC2") +
      geom_hline(yintercept = 0, linetype="dashed") +
      geom_vline(xintercept = 0, linetype="dashed")
  }
}



