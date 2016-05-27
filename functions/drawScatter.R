df_full = NULL

drawScatter <- function (input, values) {
  xaxis = subset(values$parameter_table, get(input$pick_var, envir = as.environment(values$parameter_table)) == input$x_scatter)
  GR1 = unique(xaxis[[input$pick_var]])
  
  for(i in length(input$y_scatter)) {
    yaxis = subset(values$parameter_table, get(input$pick_var, envir = as.environment(values$parameter_table)) == input$y_scatter[i])
    GR2 = unique(yaxis[[input$pick_var]])
    yaxis$cross = paste(GR1, GR2, sep = ' x ')
    xaxis$cross = paste(GR1, GR2, sep = ' x ')
    
    compare_col = which(groupingColumns == input$pick_var)
    merge_cols = c(groupingColumns[-compare_col], 'cross')
    merge_text_cols = c(groupingColumns[-compare_col])
    tmp = xaxis[,merge_cols, drop = F]
    tmp2 = xaxis[,merge_text_cols, drop = F]
    merge_paste = (apply(tmp,1, function(x) (paste(x,collapse=" "))))
    merge_paste2 = (apply(tmp2,1, function(x) (paste(x,collapse=" "))))
    xaxis$merge = merge_paste
    xaxis$merge_text = merge_paste2
    tmp = yaxis[,merge_cols, drop = F]
    merge_paste = (apply(tmp,1, function(x) (paste(x,collapse=" "))))
    yaxis$merge = merge_paste
    df = merge(xaxis, yaxis, by = 'merge')
    df_full <<- rbind(df, df_full)
  }
  df_full$cross.x <<- factor(df_full$cross.x)
  df_sub <<- df_full
  
  parameter_choice = input$pick_parameter
  if(parameter_choice == 'GR50') {
    parameter_choice = 'log10(GR50)'
  } else if(parameter_choice == 'Hill') {
    parameter_choice = 'log2(Hill)'
  }
  padding = 0.05
  scatter_values = values$parameter_table[,parameter_choice]
  x_min = min(scatter_values, na.rm = T)
  x_max = max(scatter_values, na.rm = T)
  y_min = min(scatter_values, na.rm = T)
  y_max = max(scatter_values, na.rm = T)
  all_max = max(abs(c(x_max, y_max, x_min, y_min)), na.rm = T)
  all_range = 2*all_max
  all_max = all_max + padding*all_range
  all_min = -all_max
  X = get(paste0(parameter_choice,'.x'), envir = as.environment(df_sub))
  Y = get(paste0(parameter_choice,'.y'), envir = as.environment(df_sub))
  p = ggplot(data = df_sub, aes(x = X, y = Y, colour = cross.x, text = merge_text)) + geom_point(size=2)+ geom_abline(slope = 1, intercept = 0, size = .25) + scale_x_continuous(limits = c(all_min, all_max)) + scale_y_continuous(limits = c(all_min, all_max)) + coord_fixed()
  if(parameter_choice == 'log10(GR50)') {
    p = p + ggtitle("GR50 Scatterplot (log10)") + labs(colour = "") + xlab("log10(GR50)") + ylab("log10(GR50)")
  } else if(parameter_choice == 'log2(Hill)') {
    p = p + ggtitle("Hill Scatterplot (log2)") + labs(colour = "") + xlab("log2(Hill)") + ylab("log2(Hill)")
  } else {
    p = p + ggtitle(paste(parameter_choice, "Scatterplot")) + labs(colour = "") + xlab(parameter_choice) + ylab(parameter_choice)
  }
  plotScatter <<- p
  # modify x and y names for hovertext
  p = plotly_build(p)
  for(i in 1:length(p$data)){
    p$data[[i]]$text = gsub('cross.x: ', '', p$data[[i]]$text)
  }
  return(p)
}