drawBox <- function(input, values) {
  parameter_choice = input$pick_box_y
  if(parameter_choice == 'GR50') {
    parameter_choice = 'log10(GR50)'
  }
  if(parameter_choice == 'Hill') {
    parameter_choice = 'log2(Hill)'
  }
  full_data = values$parameter_table
  boxplot_data = full_data[full_data[[ input$pick_box_x ]] %in% input$pick_box_factors,]
  x_factor = factor(get(input$pick_box_x, envir = as.environment(boxplot_data)))
  y_variable = get(parameter_choice, envir = as.environment(boxplot_data))
  point_color = factor(get(input$pick_box_point_color, envir = as.environment(boxplot_data)))
  # check that the data frame boxplot has data in it
  if(dim(boxplot_data)[1] > 0) {
    p <- ggplot(boxplot_data, aes(x = x_factor, y = y_variable, text = experiment))
    p = p + geom_boxplot(aes(fill = x_factor, alpha = 0.3), outlier.color = NA, show.legend = F) + geom_jitter(width = 0.5, show.legend = F, aes(colour = point_color)) + xlab('') + ylab(parameter_choice)
    q <- ggplot(boxplot_data, aes(x = x_factor, y = y_variable))
    q = q + geom_boxplot(aes(fill = x_factor, alpha = 0.3), outlier.color = NA, show.legend = F) + geom_jitter(width = 0.5, aes(colour = point_color)) + xlab('') + ylab(parameter_choice)
    q$labels$colour = input$pick_box_point_color
    plotScatter_box <<- q
    # modify x and y names for hovertext
    #test_gg <<- plotly_build(p)
    #test_gg<<- q
    p = plotly_build(p)
    for(i in 1:length(p$data)){
      p$data[[i]]$text = gsub('x_factor', input$pick_box_x, p$data[[i]]$text)
      p$data[[i]]$text = gsub('y_variable', parameter_choice, p$data[[i]]$text)
    }
    p$layout$xaxis$tickangle = -90
    p$layout$margin$b = 200
    return(p) 
  }
}