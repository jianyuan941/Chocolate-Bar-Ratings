#PIE CHART -GGPLOTV2 (SINGLE)  => only graphing
piechart_ggv2_custom1 <- function(data, column, col_count, label, leg_tit, tit){
  
  ggplot(data, aes(x = "", 
                   y = {{col_count}}, 
                   fill = {{column}})) +
    
    #apply 1st layer: type of graph
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    
    #apply 2nd layer: labs
    labs(fill = leg_tit , title = tit) +
    geom_text_repel(aes(label = round({{label}},2)), 
              position = position_stack(vjust = 0.5), size = 4) +
    
    #apply 3rd layer: adjustment for label, title, legend
    theme_void() +
    theme(legend.position = "right")
}

