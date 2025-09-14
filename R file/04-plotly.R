#SCATTER PLOT 
scatterplot_plotly <- function(
    database = NULL,
    x_col = NULL,
    y_col = NULL,
    fill_col = NULL,
    height = 380){
  
  x_col <- as_name(enquo(x_col))
  y_col <- as_name(enquo(y_col))
  fill_col <- quo_get_expr(enquo(fill_col))
  
  if(is.null(fill_col)){
    
    plot_ly(
      data = database,
      x = ~.data[[x_col]],
      y = ~.data[[y_col]],
      type = "scatter",
      mode = "markers+line",
      height = 380
    )
  }else{
    
    fill_col <- as_name(fill_col)
    plot_ly(
      data = database,
      x = ~.data[[x_col]],
      y = ~.data[[y_col]],
      color = ~.data[[fill_col]],
      type = "scatter",
      mode = "markers+line",
      height = 380
    )
  }
}
scatter_plotlyv2 <- function(
    database = NULL,
    x_col = NULL,
    y_col = NULL,
    #optional
    fill_col = NULL,
    text_col = NULL,
    checking = F,
    display = T, # to set visibility = T or F
    #For subplot purpose - the assign button to graph both matched_group_col and ordered_group is essential
    matched_group_col = NULL,
    ordered_group = NULL
    ){
  
  x_col <- as_name(enquo(x_col))
  y_col <- as_name(enquo(y_col))
  fill_col <- quo_get_expr(enquo(fill_col))
  text_col <- quo_get_expr(enquo(text_col))
  matched_group_col <- quo_get_expr(enquo(matched_group_col))
  ordered_group_checking <- quo_get_expr(enquo(ordered_group)) 
  
  
  fig <- plot_ly()

  if(is.null(ordered_group_checking)){
    if(checking == T){ path <- "path 1"}
  
      if(is.null(fill_col)){
        if(checking == T){calculation <- "calculation 1"}
        
        if(is.null(text_col)){
          if(checking == T){text <- "no text"}
          
          fig <- fig %>% add_trace(
            data = database,
            x = ~.data[[x_col]],
            y = ~.data[[y_col]],
            type = "scatter",
            mode = "markers+lines"
          )
        }else{
          if(checking == T){text <- "got text"}
          text_col <- as_name(text_col)
          
          fig <- fig %>% add_trace(
            data = database,
            x = ~.data[[x_col]],
            y = ~.data[[y_col]],
            text = ~.data[[text_col]],
            type = "scatter",
            mode = "markers+lines+text",
            textposition = "top center"
          )
        }
      }else{
        if(checking == T){calculation <- "calculation 2"}
        fill_col <- as_name(fill_col)
        
        if(is.null(text_col)){
          if(checking == T){text <- "no text"}
          
          fig <- fig %>% add_trace(
            data = database,
            x = ~.data[[x_col]],
            y = ~.data[[y_col]],
            type = "scatter",
            mode = "markers+lines",
            color = ~.data[[fill_col]]
          ) 
        }else{
          if(checking == T){text <- "got text"}
          text_col <- as_name(text_col)
          
          fig <- fig %>% add_trace(
            data = database,
            x = ~.data[[x_col]],
            y = ~.data[[y_col]],
            text = ~.data[[text_col]],
            type = "scatter",
            mode = "markers+lines+text",
            textposition = "top center",
            color = ~.data[[fill_col]]
          ) 
        }
  
      }
      display <- "no display"
  } else {
    ordered_group_checking <- as_name(ordered_group_checking)
    matched_group_col <- as_name(matched_group_col)
     if(checking == T){path <- "path 2"}
       
      if(is.null(fill_col)){
        if(checking == T){calculation <- "calculation 3"}
        
        if(is.null(text_col)){
          if(checking == T){text <- "no text"}
          
            if(display == T){
              display <- "got display"
              
              for(i in ordered_group){
                group_data <- subset(database, database[[matched_group_col]] == i)
                fig <- fig %>% 
                  add_trace(
                    data = group_data,
                    x = ~.data[[x_col]],
                    y = ~.data[[y_col]],
                    type = "scatter",
                    mode = "markers+lines",
                    name = paste0(i,"_line"),
                    legendgroup = i,
                    visible = T
                  )}
            }else{
              display <- "no display"
              
              for(i in ordered_group){
                group_data <- subset(database, database[[matched_group_col]] == i)
                fig <- fig %>% 
                  add_trace(
                    data = group_data,
                    x = ~.data[[x_col]],
                    y = ~.data[[y_col]],
                    type = "scatter",
                    mode = "markers+lines",
                    name = paste0(i,"_line"),
                    legendgroup = i,
                    visible = F
                  )}
              
            }
          
        }else{
          if(checking == T){text <- "got text"}
          text_col <- as_name(text_col)
          
          if(display == T){
            display <- "got display"
            
            for(i in ordered_group){
              group_data <- subset(database, database[[matched_group_col]] == i)
              fig <- fig %>% 
                add_trace(
                  data = group_data,
                  x = ~.data[[x_col]],
                  y = ~.data[[y_col]],
                  text = ~.data[[text_col]],
                  textposition = "top center",
                  type = "scatter",
                  mode = "markers+lines+text",
                  name = paste0(i, "_line"),
                  legendgroup = i,
                  visible = T
                )}   
          }else{
            display <- "no display"
            
            for(i in ordered_group){
              group_data <- subset(database, database[[matched_group_col]] == i)
              fig <- fig %>% 
                add_trace(
                  data = group_data,
                  x = ~.data[[x_col]],
                  y = ~.data[[y_col]],
                  text = ~.data[[text_col]],
                  textposition = "top center",
                  type = "scatter",
                  mode = "markers+lines+text",
                  name = paste0(i, "_line"),
                  legendgroup = i,
                  visible = F
                )}   
            
          }
          
        }
        
      }else{
        if(checking == T){calculation <- "calculation 4"}
        fill_col <- as_name(fill_col)
        
        if(is.null(text_col)){
          if(checking == T){text <- "no text"}
          
          if(display == T){
            display <- "got display"
            
            for(i in ordered_group){
              group_data <- subset(database, database[[matched_group_col]] == i)
              fig <- fig %>% 
                add_trace(
                  data = group_data,
                  x = ~.data[[x_col]],
                  y = ~.data[[y_col]],
                  color = ~.data[[fill_col]],
                  type = "scatter",
                  mode = "markers+lines",
                  name = paste0(i,"_line"),
                  legendgroup = i,
                  visible = T
                )}
          }else{
            display <- "no display"
            
            for(i in ordered_group){
              group_data <- subset(database, database[[matched_group_col]] == i)
              fig <- fig %>% 
                add_trace(
                  data = group_data,
                  x = ~.data[[x_col]],
                  y = ~.data[[y_col]],
                  color = ~.data[[fill_col]],
                  type = "scatter",
                  mode = "markers+lines",
                  name = paste0(i,"_line"),
                  legendgroup = i,
                  visible = F
                )}
          }
          
        }else{
          
          if(checking == T){text <- "got text"} 
          text_col <- as_name(text_col)
          
          if(display == T){
            display <- "got display"
            
            for(i in ordered_group){
              group_data <- subset(database, database[[matched_group_col]] == i)
              fig <- fig %>% 
                add_trace(
                  data = group_data,
                  x = ~.data[[x_col]],
                  y = ~.data[[y_col]],
                  color = ~.data[[fill_col]],
                  text = ~.data[[text_col]],
                  textposition = "top center",
                  type = "scatter",
                  mode = "markers+lines+text",
                  name = paste0(i,"_line"),
                  legendgroup = i,
                  visible = T
                )}
          }else{
            display <- "no display"
            
            for(i in ordered_group){
              group_data <- subset(database, database[[matched_group_col]] == i)
              fig <- fig %>% 
                add_trace(
                  data = group_data,
                  x = ~.data[[x_col]],
                  y = ~.data[[y_col]],
                  color = ~.data[[fill_col]],
                  text = ~.data[[text_col]],
                  textposition = "top center",
                  type = "scatter",
                  mode = "markers+lines+text",
                  name = paste0(i,"_line"),
                  legendgroup = i,
                  visible = F
                )}
          }
          

        }
        
      }
      
  }
  if(checking == T){
    result <- paste0("path - ",path, "\n",
                     "calculation - ",calculation,"\n",
                     "text - ",text,"\n",
                     "display - ",display,"\n")
    return(cat(result))
  }else{
    fig
  }
  
}


barchart_plotly <- function(
    database = NULL,
    x_col = NULL,
    y_col = NULL,
    matched_group_col = NULL, 

    #optional
    fill_col = NULL, #if use ordered group then can ignore this
    text_col = NULL,
    checking = F,
    display = T, # to set visibility = T or F
    auto_color = F,
    display_selected = NULL,  
    
    #For subplot purpose - the assign button to graph both matched_group_col and ordered_group is essential
    ordered_group = NULL
    ){
  
  x_col <- substitute(x_col)
  y_col <- substitute(y_col)
  text_col <- substitute(text_col)
  fill_col <- substitute(fill_col)
  matched_group_col <- substitute(matched_group_col)
  display_selected <- substitute(display_selected)
  
  # this calculation for without matched group col
  if(is.null(matched_group_col)){
    
    sub_barchart_plotlyv4_without_matched_group_col(
      database = database,
      x_col = x_col,
      y_col = y_col,
      text_col = text_col,
      fill_col = fill_col,
      auto_color = auto_color
    )
    
  # this calculation for with matched group col  
  }else{
    
    sub_barchart_plotlyv4_with_matched_group_col(
    database = database,
    x_col = x_col,
    y_col = y_col,
    text_col = text_col,
    fill_col = fill_col,
    matched_group_col = matched_group_col,
    display_selected = display_selected,
    ordered_group = ordered_group,
    display = display,
    checking = checking
  )
    
  }
}  
    #Sub function for barchart plotly
    sub_barchart_plotlyv4_without_matched_group_col<- function(
      database = NULL,
      x_col = NULL,
      y_col = NULL,
      #optional
      fill_col = NULL,
      text_col = NULL,
      
      #optional
      checking = F,
      auto_color = F
    ){
      x_col <- as_name(x_col)
      y_col <- as_name(y_col)
      
      
      # when this = T assign color can exceed 8 limits
      if(auto_color == F){
        cols <- as.formula(paste0("~",fill_col))
      }else{
        cols <- viridis(nrow(database), option = "D")
      }
      
      # this is without fill col
      if(is.null(fill_col)){
        path_one <- "without fill"
        
        if(is.null(text_col)){
          path_two <- "without text"
          
          plot_ly(
            data = database,
            x = ~.data[[x_col]],
            y = ~.data[[y_col]],
            type = "bar",
            orientation = "v"
          )
        }else{
          path_two <- "with text"
          text_col <- as_name(text_col)
          
          plot_ly(
            data = database,
            x = ~.data[[x_col]],
            y = ~.data[[y_col]],
            text = ~.data[[text_col]],
            textposition = "outside",
            textfont = list(color = "black"),
            type = "bar",
            orientation = "v"
          )
        }
        
      # this is with fill col  
      }else{
        
        path_one <- "with fill"
        fill_col <- as_name(fill_col)
        
          # this is without text col
          if(is.null(text_col)){
            path_two <- "without text"
            
            plot_ly(
              data = database,
              x = ~.data[[x_col]],
              y = ~.data[[y_col]],
              color = cols,
              name = ~.data[[fill_col]],
              type = "bar",
              orientation = "v"
            )
          
          # this is with text col
          }else{
            path_two <- "with text"
            text_col <- as_name(text_col)
            
            plot_ly(
              data = database,
              x = ~.data[[x_col]],
              y = ~.data[[y_col]],
              color = cols,
              name = ~.data[[fill_col]],
              text = ~.data[[text_col]],
              textposition = "outside",
              textfont = list(color = "black"),
              type = "bar",
              orientation = "v"
            )
          }
      }
      
      
    }

    #Sub function for barchart plotly
    sub_barchart_plotlyv4_with_matched_group_col<- function(
        database = NULL,
        x_col = NULL,
        y_col = NULL, #optional
        
        fill_col = NULL, #if use ordered group then can ignore this
        text_col = NULL,
        checking = F,
        auto_color = F,
        display = T, # to set visibility = T or F
        display_selected = NULL, #For subplot purpose - the assign button to graph both matched_group_col and ordered_group is essential
        
        matched_group_col = NULL,
        ordered_group = NULL
    ){
      
      x_col <- as_name(x_col)
      y_col <- as_name(y_col)
      matched_group_col <- as_name(matched_group_col)
      
      fig <- plot_ly()
      
      # this is show overall graph
      if(is.null(display_selected)){
      path_one <- "summary graph"
        
        # this is without text col
        if(is.null(text_col)){
          path_two <- "without text"
          
          # this is without fill col
          if(is.null(fill_col)){
            path_three <- "without fil"
            
            # this is for for visible graph
            if(display == T){
              path_four <- "visible graph"
              
              for(i in ordered_group){
                dat <- subset(database, database[[matched_group_col]] == i)
                fig <- fig %>% 
                  add_trace(
                    data = dat,
                    x = ~.data[[x_col]],
                    y = ~.data[[y_col]],
                    type = "bar",
                    orientation = "v",
                    name = paste0(i,"_bar"),
                    legendgroup = i,
                    visible = T
                  )
              }
              
            # this is for for invisible graph
            }else{
              path_four <- "invisible graph"
              
              for(i in ordered_group){
                dat <- subset(database, database[[matched_group_col]] == i)
                fig <- fig %>% 
                  add_trace(
                    data = dat,
                    x = ~.data[[x_col]],
                    y = ~.data[[y_col]],
                    type = "bar",
                    orientation = "v",
                    name = paste0(i,"_bar"),
                    legendgroup = i,
                    visible = F
                  )
              }
            }
            
          }else{
            path_three <- "with fil"
            fill_col <- as_name(fill_col)
            
            if(display == T){
              path_four <- "visible graph"

              for(i in ordered_group){
              dat <- subset(database, database[[matched_group_col]] == i)
              fig <- fig %>% 
                add_trace(
                  data = dat,
                  x = ~.data[[x_col]],
                  y = ~.data[[y_col]],
                  color = ~.data[[fill_col]],
                  type = "bar",
                  orientation = "v",
                  name = paste0(i,"_bar"),
                  legendgroup = i,
                  visible = T
                )
              }           
            }else{
              path_four <- "invisible graph"
              
              for(i in ordered_group){
                dat <- subset(database, database[[matched_group_col]] == i)
                fig <- fig %>% 
                  add_trace(
                    data = dat,
                    x = ~.data[[x_col]],
                    y = ~.data[[y_col]],
                    color = ~.data[[fill_col]],
                    type = "bar",
                    orientation = "v",
                    name = paste0(i,"_bar"),
                    legendgroup = i,
                    visible = F
                  )
            }
          }
          }
        # this is with text col
        }else{
          path_two <- "with text"
          text_col <- as_name(text_col)
          
          #this is without fill col
          if(is.null(fill_col)){
            path_three <- "without fill"
            
            if(display == T){
              path_four <- "visible graph"

                for(i in ordered_group){
                dat <- subset(database, database[[matched_group_col]] == i)
                fig <- fig %>% 
                  add_trace(
                    data = dat,
                    x = ~.data[[x_col]],
                    y = ~.data[[y_col]],
                    text = ~round(.data[[text_col]],2),
                    textposition = "outside",
                    type = "bar",
                    orientation = "v",
                    name = paste0(i,"_bar"),
                    legendgroup = i,
                    visible = T
                  )
              }          
              
            }else{
              path_four <- "invisible graph"

              for(i in ordered_group){
                dat <- subset(database, database[[matched_group_col]] == i)
                fig <- fig %>% 
                  add_trace(
                    data = dat,
                    x = ~.data[[x_col]],
                    y = ~.data[[y_col]],
                    text = ~round(.data[[text_col]],2),
                    textposition = "outside",
                    type = "bar",
                    orientation = "v",
                    name = paste0(i,"_bar"),
                    legendgroup = i,
                    visible = F
                  )
              }                
            }
          
          #this is with fill col
          }else{
            path_three <- "with fill"
            fill_col <- as_name(fill_col)
            
            if(display == T){
              path_four <- "visible graph"
              
              for(i in ordered_group){
                dat <- subset(database, database[[matched_group_col]] == i)
                fig <- fig %>% 
                  add_trace(
                    data = dat,
                    x = ~.data[[x_col]],
                    y = ~.data[[y_col]],
                    color =~.data[[fill_col]],
                    text = ~round(.data[[text_col]],2),
                    textposition = "outside",
                    type = "bar",
                    orientation = "v",
                    name = paste0(i,"_bar"),
                    legendgroup = i,
                    visible = T
                  )
              }
            }else{
              path_four <- "invisible graph"
              
              for(i in ordered_group){
                dat <- subset(database, database[[matched_group_col]] == i)
                fig <- fig %>% 
                  add_trace(
                    data = dat,
                    x = ~.data[[x_col]],
                    y = ~.data[[y_col]],
                    color =~.data[[fill_col]],
                    text = ~round(.data[[text_col]],2),
                    textposition = "outside",
                    type = "bar",
                    orientation = "v",
                    name = paste0(i,"_bar"),
                    legendgroup = i,
                    visible = F
                  )
              }
            }
            
   
          }
          
          
        }

      
      # display only selective graph  
      }else{
      path_one <- "singular graph"  
        
        # this is without text col
        if(is.null(text_col)){
          path_two <- "without text"
          
          # this is without fil col
          if(is.null(fill_col)){
            path_three <- "without fill"
            
          display_selected <- as_name(display_selected)
          dat <- subset(database, database[[matched_group_col]] == display_selected)
            fig <- fig %>% 
              add_trace(
                data = dat,
                x = ~.data[[x_col]],
                y = ~.data[[y_col]],
                type = "bar",
                orientation = "v",
                name = display_selected
              )
            
          # this is with fill col
          }else{
            path_three <- "with fill"
            fill_col <- as_name(fill_col)
            
            display_selected <- as_name(display_selected)
            dat <- subset(database, database[[matched_group_col]] == display_selected)
            fig <- fig %>% 
              add_trace(
                data = dat,
                x = ~.data[[x_col]],
                y = ~.data[[y_col]],
                color = ~.data[[fill_col]],
                type = "bar",
                orientation = "v",
                name = display_selected
              )
          }
        
        #this is with text col
        }else{
          path_two <- "with text"
          text_col <- as_name(text_col)
          
          if(is.null(fill_col)){
            path_three <- "without fill"
  
            display_selected <- as_name(display_selected)
            dat <- subset(database, database[[matched_group_col]] == display_selected)
            fig <- fig %>% 
              add_trace(
                data = dat,
                x = ~.data[[x_col]],
                y = ~.data[[y_col]],
                text = ~round(.data[[text_col]],2),
                textposition = "outside",
                type = "bar",
                orientation = "v",
                name = display_selected
              )         
          
          }else{
            path_three <- "with fill"
            fill_col <- as_name(fill_col)
            
            display_selected <- as_name(display_selected)
            dat <- subset(database, database[[matched_group_col]] == display_selected)
            fig <- fig %>% 
              add_trace(
                data = dat,
                x = ~.data[[x_col]],
                y = ~.data[[y_col]],
                text = ~round(.data[[text_col]],2),
                color = ~.data[[fill_col]],
                textposition = "outside",
                type = "bar",
                orientation = "v",
                name = display_selected
              )      
            
          }
          
        }
      path_four <- "invalid"
      }
      

      if(checking == T){
        
        result <- 
        paste0(
          "sub_barchart_plotlyv4_with_matched_group_col \n",
          "path 1 - ", path_one, "\n",
          "path 2 = ", path_two, "\n",
          "path 3 = ", path_three, "\n",
          "path 4 = ", path_four, "\n"
        )
        return(cat(result))
      }else{
        return(fig)
      }
      
}

hist_bell_plotly <-function(
database = NULL,
x_col = NULL,
bin_size = NULL,
matched_group_col = NULL,
ordered_group = NULL,

#drop down list generator
number_of_curve = NULL
){
  matched_group_col <- as_name(enquo(matched_group_col))
  x_col <- as_name(enquo(x_col))
  
  fig <- plot_ly()
  for(i in ordered_group){
    dat <- subset(database, database[[matched_group_col]] == i)
    
    # bell curve generator
    mean_result <<- mean(dat[[x_col]])
    std_result <<- sd(dat[[x_col]])
    sample_size <<- nrow(dat)
    
    bell_curve_generator(
      name = "calculation",
      mu = mean_result,
      sigma = std_result,
      sample_size = sample_size
    )
    
    # histogram generator
    fig <- fig %>% 
      add_trace(
        data = dat,
        x = ~.data[[x_col]],
        opacity = 0.5,
        xbins = list(start = min(dat[[x_col]]),
                     end = max(dat[[x_col]]),
                     size = bin_size),
        type = "histogram",
        histnorm = "probability density", #This set Y axis follow density, else it base on count
        legendgroup = i,
        name = paste0(x_col,"_", i)
      ) %>% 
      add_lines(
        x = calculation_x,
        y = calculation_y,
        line = list(color = "darkred",
                    width = 2),
        name = paste0(x_col,"_", i,"_line"),
        legendgroup = i,
      )
    
  }
  
  if(is.null(number_of_curve)){
    fig %>% layout(
      barmode = "overlay"
      #xaxis = list(range = c(0, max(database[[x_col]])+5))
    )  
  }else{
    fig %>% layout(
      barmode = "overlay"
      #xaxis = list(range = c(0, max(database[[x_col]])+5))
    ) %>% 
      bell_curve_selection_generator(
        number_of_curve = number_of_curve
      )
    
  }
}    


#bell_curve_generator
bell_curve_generator <- function(
    name= NULL, # in "" format| as_name() format
    mu = NULL, #mean
    sigma = NULL, #sd
    sample_size =NULL
    ){

      mu <- as.double(mu)
      sigma <- as.double(sigma)
      
      df<- seq(mu - 4*sigma, mu +4*sigma, length.out = sample_size)
      df1<- dnorm(df, mean = mu, sd = sigma)
      
      assign(paste0(name,"_x"), df, envir = .GlobalEnv)
      assign(paste0(name,"_y"), df1, envir = .GlobalEnv)
    }
#BELL curve + histrogram
  hist_plotly <- function(
    database = NULL,
    x = NULL,
    name = NULL, #optional| if no assign name, name will be the same as x
    #==================
    x_bell_curve = NULL, #bell curve x
    y_bell_curve = NULL, #bell curve y
    #==================
    color = "Blue",
    opacity = 0.5, #0~1
    #==================
    legendgroup = NULL, #optional | this for subplot purpose & legend name in "" format
    showlegendgroup = F, #optional | this for subplot purpose
    #==================
    checking = F #check which path to use
    ){
    name_checking <- quo_get_expr(enquo(name))
    name_checking2 <- quo_get_expr(enquo(legendgroup))
    x_name<- if(is.null(name_checking)){as_name(enquo(x))}else{as_name(enquo(name))}
    
    
    #set color for graph and bell curve
    hist_color <- paste0("light",color)
    bell_color <- paste0("dark",color)
    
    
    #here got 3 layer
    #1st layer - check whether legend name exist
    #2nd layer - check whether need to show legend group
    #3rd layer - check for calculation flow
    if(is.null(name_checking2)){
      if(checking == T){
        start_with <- "no legendgroup assigned"
      }
      df<- database %>%
        #histogram for existing data
        add_histogram(x = x,
                      histnorm = "probability density",
                      marker = list(color = hist_color),
                      opacity = opacity,
                      name = x_name) %>% 
        #bell curve on top of histogram
        add_lines(x = x_bell_curve, 
                  y = y_bell_curve, 
                  line = list(color = bell_color,
                              width = 2),
                  name = paste0(x_name," Curve"),
                  showlegend = FALSE)
    }else{
      if(checking == T){
        start_with <- "has legendgroup assigned"
      }
      
      if(showlegendgroup ==T){
        
        if(checking == T){
          calculation <- "calculation 1"
          }
      df<- database %>%
          #histogram for existing data
          add_histogram(x = x,
                        histnorm = "probability density",
                        marker = list(color = hist_color),
                        opacity = opacity,
                        name = x_name,
                        legendgroup = legendgroup,
                        showlegend = showlegendgroup) %>% 
          #bell curve on top of histogram
          add_lines(x = x_bell_curve, 
                    y = y_bell_curve, 
                    line = list(color = bell_color,
                                width = 2),
                    name = paste0(x_name," Curve"),
                    legendgroup = legendgroup,
                    showlegend = FALSE)
      }else if(showlegendgroup == F){
        
        if(checking == T){
          calculation <- "calculation 2"
          }
      df<- database %>%
          #histogram for existing data
          add_histogram(x = x,
                        histnorm = "probability density",
                        marker = list(color = hist_color),
                        opacity = opacity,
                        name = x_name,
                        legendgroup = legendgroup) %>% 
          #bell curve on top of histogram
          add_lines(x = x_bell_curve, 
                    y = y_bell_curve, 
                    line = list(color = bell_color,
                                width = 2),
                    name = paste0(x_name," Curve"),
                    legendgroup = legendgroup,
                    showlegend = FALSE)       
      }
      
    }
    if(checking == T){
      return(paste0(start_with," > ",calculation))
    }else{
      return(df)
    }
  }
#==========================================================
    #TITLE FUNCTION
    title_function <- function(
    title = NULL,
    size = NULL,
    color = NULL){
      
      color <- if(as.character(enquo(color)[2])== "NULL"){"darkblue"}else{color}
      list(
        text = title,
        x = 0.5,
        xanchor = "center",
        font = list(size = size, color = color)
      )
    }
    
    #LABEL FUNCTION
    labs_plotly_function <- function(
        data_base = NULL, #optinal to declare
        x_title = NULL, #input in "" format
        y_title = NULL  #input in "" format
    ){
      
      data_base %>% 
        layout(
          xaxis = list(title = x_title),
          yaxis = list(title = y_title)
        )
      
    }
    #VERTICAL LINE FUNCTION - LINE
    vertical_line_function<- function(
    x = NULL,
    y = NULL,
    color = NULL
    ){
      
      assign_color <- if(as.character(enquo(color)[2])== "NULL"){"red"}else{color}
      list(
        type = "line",
        x0 = x, x1 = x,
        y0 = 0, y1 = y, #y1 = max_y
        line = list(color =assign_color, width = 2, dash = "dash")
      )
      #data_base %>% 
      #  layout(
      #    shapes = list(
      #      list(
      #        type = "line",
      #        x0 = vertical_line, x1 = vertical_line,
      #        y0 = 0, y1 = vertical_line_length,
      #        line = list(color ="red", width = 2, dash = "dash")
      #      )
      #    ),
      #    annotations = list(
      #      list(
      #        x = vertical_line,
      #        y = vertical_line_length,
      #        text = vertical_description,
      #        showarrow = TRUE,
      #        arrowhead = 2
      #      )
      #    )
      #  )
    }
    
    #VERTICAL LINE LABEL
    horizontal_line_function <- function(
    x = NULL,
    y = NULL,
    color = NULL
    ){
      assign_color <- if(as.character(enquo(color)[2])=="NULL"){"red"}else{color}
      
      list(
        type = "line",
        x0 = 0, x1 = x, #x1 = max_x
        y0 = y, y1 = y,
        line = list(color = assign_color, width = 2, dash = "dash")
      )
    }
    
    #BUTTON FOR SUBPLOT
    button_generator <- function(
    data =NULL,
    sec_graph_display = T,
    #database = NULL, #need explicit declare
    #group = NULL, # defined group column 
    ordered_group = NULL, #defined vector in global and assign here
    num_of_plot = NULL
    ){
      
      menu_group <- sort(as.character(ordered_group))
      
      #checking for prerequisition
      if(is.null(num_of_plot)){
        num_of_plot <- 2
      }else{
        
        if(num_of_plot > 2){
          return("kindly adjust the visibility part")
        }
      }
      
      if(num_of_plot == 1){
        
        # --- Visibility vectors ---
        visibility <- lapply(seq_along(ordered_group), function(i){
          vis <- rep(FALSE, length(ordered_group))
          vis[i] <- TRUE
          vis
        })
        visibility <- c(visibility, list(rep(TRUE, length(ordered_group))))  # Show All
        
        # --- Buttons with sorted menu names, Show All first ---
        buttons <- lapply(seq_along(c("Show All", menu_group)), function(i){
          if (i == 1) {
            # Show All button
            vis <- rep(TRUE, length(ordered_group))
          } else {
            # Find index of group in original traces
            trace_index <- match(c("Show All", menu_group)[i], ordered_group)
            vis <- visibility[[trace_index]]
          }
          list(
            method = "update",
            args = list(list(visible = vis)),
            label = c("Show All", menu_group)[i]
          )
        })
        
      }else if(num_of_plot == 2){
        
        #visibility
        visibility <- lapply(seq_along(ordered_group), function(i){
          vis <- rep(FALSE, num_of_plot*length(ordered_group))
          vis[i] <- TRUE
          vis[i + length(ordered_group)] <- TRUE
          vis
        })
        
        #return("yes")
        #generate random calculation 
        visibility <- c(visibility, list(rep(TRUE, num_of_plot*length(ordered_group))))
        buttons <- lapply(seq_along(c("Show All", menu_group)), function(i){
          if(i == 1){
            # Show All button
            if(sec_graph_display == T){
              vis <- rep(TRUE, num_of_plot * length(ordered_group))
            }else{
              vis <- c(rep(TRUE,(num_of_plot * length(ordered_group))/2),rep(FALSE,(num_of_plot * length(ordered_group))/2))
            }
          } else {
            # Match only group name, exclude "Show All"
            trace_index <- match(menu_group[i - 1], ordered_group)
            vis <- visibility[[trace_index]]
          }
          
          list(
            method = "update",
            args = list(list(visible = vis)),
            label = c("Show All", menu_group)[i]
          )
        })
        
      }
      
      data %>% 
        layout(
          updatemenus = list(
            list(
              y = 1.1,
              xanchor = "center",
              pad = list(r = 0, t = 0, l = 0, b = 0),
              buttons = buttons
            )
          )
        )
    }
    

    bell_curve_selection_generator <- function(
    data = NULL,
    number_of_curve = NULL,
    hist_bell = F
    ){
      labels <- c("Overall", "Histogram", "Bell Curve")
      visibility <- lapply(seq_along(labels), function(i){
        
        # trigger display for all curve
        if(i == 1){
          vis <- rep(TRUE, number_of_curve)
        }else if(i == 2){
          vis <- rep(c(TRUE,FALSE), number_of_curve/2) 
        }else if(i == 3){
          vis <- rep(c(FALSE,TRUE), number_of_curve/2)
        }
        
        list(method ="update",
             args = list(list(visible = vis)),
             label = labels[i])
      })
      
      data %>% 
        layout(
          updatemenus = list(
            list(
              y = 1.1,
              xanchor = "center",
              pad = list(r = 0, t = 0, l = 0, b = 0),
              buttons = visibility
            )
          )
        )
    } 
    subplot_function <- function(
    f1,
    f2,
    x1 = 0.5,
    y1 = NULL,
    y2 = NULL){
      
      y1 <- if(is.null(y1)){1.05}else{y1}
      y2 <- if(is.null(y2)){0.45}else{y2}
      
      subplot(f1, f2, shareX = F, shareY = F, nrows = 2) %>% 
        layout(
          height = 900,
          annotations = list(
            list(
              text = "Title for Subplot 1", 
              x = x1, 
              y = y1,   # position above first subplot
              xref = "paper", 
              yref = "paper", 
              showarrow = FALSE,
              font = list(size = 14)
            ),
            list(
              text = "Title for Subplot 2", 
              x = x1, 
              y = y2,   # position above second subplot
              xref = "paper", 
              yref = "paper", 
              showarrow = FALSE,
              font = list(size = 14)
            )
          )
        )
        
    }
    
    