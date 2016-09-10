drawBox <- function(input, values) {
  parameter_choice = input$pick_box_y
  if(parameter_choice == 'GR50') {
    parameter_choice = 'log10(GR50)'
  }
  if(parameter_choice == 'h_GR') {
    parameter_choice = 'log2(h_GR)'
  }
  if(parameter_choice == 'IC50') {
    parameter_choice = 'log10(IC50)'
  }
  if(parameter_choice == 'h') {
    parameter_choice = 'log2(h)'
  }
  full_data = values$parameter_table
  
  boxplot_data = full_data[full_data[[ input$pick_box_x ]] %in% input$pick_box_factors,]
  boxplot_data[[ input$pick_box_x ]] = factor(boxplot_data[[ input$pick_box_x ]])
  if(!is.null(input$factorB) & !is.null(input$factorA)) {
    for(i in 1:length(input$factorB)) {
      boxplot_data[[ input$pick_box_x ]] = relevel(boxplot_data[[ input$pick_box_x ]], input$factorB[i])
    }
    for(i in 1:length(input$factorA)) {
      boxplot_data[[ input$pick_box_x ]] = relevel(boxplot_data[[ input$pick_box_x ]], input$factorA[i])
    }
  }

  x_factor = factor(get(input$pick_box_x, envir = as.environment(boxplot_data)))
  y_variable = get(parameter_choice, envir = as.environment(boxplot_data))
  point_color = factor(get(input$pick_box_point_color, envir = as.environment(boxplot_data)))
  # check that the data frame boxplot has data in it
  if(dim(boxplot_data)[1] > 0) {
    p <- ggplot(boxplot_data, aes(x = x_factor, y = y_variable, text = experiment))
    p = p + geom_boxplot(aes(fill = x_factor, alpha = 0.3), outlier.color = NA, show.legend = F) + geom_jitter(width = 0.5, show.legend = F, aes(colour = point_color)) + xlab('') + ylab(parameter_choice)
    q <- ggplot(boxplot_data, aes(x = x_factor, y = y_variable))
    q = q + geom_boxplot(aes(fill = x_factor, alpha = 0.3), outlier.color = NA, show.legend = F) + geom_jitter(width = 0.5, aes(colour = point_color)) + xlab('') + ylab(parameter_choice) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    q$labels$colour = input$pick_box_point_color
    
    # modify x and y names for hovertext
    #test_gg <<- plotly_build(p)
    #test_gg<<- q
    p1 = plotly_build(p)
    test_box <<- p1
   
    if(!is.null(values$wilcox)) {
      # Get y range:
      top_y = p1[[2]]$yaxis$range[2]
      print('topy1')
      print(top_y)
      bottom_y = p1[[2]]$yaxis$range[1]
      total_y_range = top_y - bottom_y
      # Get top of boxplot whiskers
      whiskers = NULL
      #for(i in 1:length(levels(x_factor))) {
      len = length(input$factorA) + length(input$factorB)
      for(i in 1:len) {
        whiskers[i] = fivenum(p1[[1]][[i]]$y)[5]
      }
      top_whisker = max(whiskers, na.rm = TRUE)
      y_range = (top_y - top_whisker)/total_y_range
      if(y_range < .25) {
        top_y = top_whisker + .25*total_y_range
        #y_range = top_y - top_whisker
      }
      lh = top_whisker + total_y_range*(.1)
      bump = total_y_range*(.05)
      ll = lh - bump
      lenA = length(input$factorA)
      lenB = length(input$factorB)
      if(lenA == 1 & lenB == 1) {
        p = p + annotate("text", x = 1.5, y = lh + bump/2, label = paste("p =",values$wilcox)) + geom_segment(x = 1, y = lh, xend = 2, yend = lh) + geom_segment(x = 1, y = ll, xend = 1, yend = lh) + geom_segment(x = 2, y = ll, xend = 2, yend = lh)
        
        df1 <- data.frame(a = c(1,1,2,2), b = c(ll,lh,lh,ll))
        q = q + annotate("text", x = 1.5, y = lh, label = values$wilcox) + geom_line(data = df1, aes(x = a, y = b))
      } else if(lenA > 1 & lenB == 1) {
        p = p + annotate("text", x = ((lenA + 1) + ((lenA+1)/2))/2, y = lh + 2*bump, label = paste("p =",values$wilcox)) +
          geom_segment(x = 1, y = lh, xend = lenA, yend = lh) + geom_segment(x = 1, y = ll, xend = 1, yend = lh) + geom_segment(x = lenA, y = ll, xend = lenA, yend = lh) +
          geom_segment(x = (lenA+1)/2, y = lh + bump, xend = lenA + 1, yend = lh + bump) + geom_segment(x = (lenA+1)/2, y = lh, xend = (lenA+1)/2, yend = lh + bump) + geom_segment(x = lenA+1, y = ll, xend = lenA+1, yend = lh + bump)

        # df1 <- data.frame(a = c(1,1,2,2), b = c(ll,lh,lh,ll))
        # q = q + annotate("text", x = 1.5, y = lh, label = values$wilcox) + geom_line(data = df1, aes(x = a, y = b))
      } else if(lenA == 1 & lenB > 1) {
        p = p + annotate("text", x = 1.25 + .25*lenB, y = lh + 2*bump, label = paste("p =",values$wilcox)) + 
          geom_segment(x = 1, y = lh+bump, xend = .5*lenB + 1.5, yend = lh+bump) + geom_segment(x = 1, y = ll, xend = 1, yend = lh+bump) + geom_segment(x = 1.5+.5*lenB, y = lh, xend = 1.5+.5*lenB, yend = lh+bump) +
          geom_segment(x = 2, y = lh, xend = lenB + 1, yend = lh) + geom_segment(x = 2, y = ll, xend = 2, yend = lh) + geom_segment(x = lenB+1, y = ll, xend = lenB+1, yend = lh)
        
        # df1 <- data.frame(a = c(1,1,2,2), b = c(ll,lh,lh,ll))
        # q = q + annotate("text", x = 1.5, y = lh, label = values$wilcox) + geom_line(data = df1, aes(x = a, y = b))
      } else if(lenA > 1 & lenB > 1) {
        p = p + annotate("text", x = .25*(lenB-1)+.75*(lenA+1), y = lh + 2*bump, label = paste("p =",values$wilcox)) + 
          geom_segment(x = 1, y = lh, xend = lenA, yend = lh) + geom_segment(x = 1, y = ll, xend = 1, yend = lh) + geom_segment(x = lenA, y = ll, xend = lenA, yend = lh) +
          geom_segment(x = lenA+1, y = lh, xend = lenA+lenB, yend = lh) + geom_segment(x = lenA+1, y = ll, xend = lenA+1, yend = lh) + geom_segment(x = lenA+lenB, y = ll, xend = lenA+lenB, yend = lh) +
          geom_segment(x = (lenA+1)/2, y = lh+bump, xend = (lenA+1)+((lenB-1)/2), yend = lh+bump) + geom_segment(x = (lenA+1)/2, y = lh, xend = (lenA+1)/2, yend = lh+bump) + geom_segment(x = (lenA+1)+((lenB-1)/2), y = lh, xend = (lenA+1)+((lenB-1)/2), yend = lh+bump)
        
        # df1 <- data.frame(a = c(1,1,2,2), b = c(ll,lh,lh,ll))
        # q = q + annotate("text", x = 1.5, y = lh, label = values$wilcox) + geom_line(data = df1, aes(x = a, y = b))
      }
      
      p = plotly_build(p)
      p[[2]]$yaxis$range[2] = top_y
    } else {
      p = plotly_build(p)
    }
    plotScatter_box <<- q
    
    # Current CRAN version of plotly (3.6.0) uses p$data
    # Latest github version of plotly (4.3.5) uses p$x$data
    if(is.null(p$data)) {
      for(i in 1:length(p$x$data)) {
        if(!is.null(p$x$data[[i]]$text)) {
          p$x$data[[i]]$text = gsub('x_factor', input$pick_box_x, p$x$data[[i]]$text)
          p$x$data[[i]]$text = gsub('y_variable', parameter_choice, p$x$data[[i]]$text)
        }
      }
      p$x$layout$xaxis$tickangle = -90
      p$x$layout$margin$b = 200
    } else {
      for(i in 1:length(p$data)){
        if(!is.null(p$data[[i]]$text)) {
          p$data[[i]]$text = gsub('x_factor', input$pick_box_x, p$data[[i]]$text)
          p$data[[i]]$text = gsub('y_variable', parameter_choice, p$data[[i]]$text)
        }
      }
      p$layout$xaxis$tickangle = -90
      p$layout$margin$b = 200
    }
    return(p)
  }
}