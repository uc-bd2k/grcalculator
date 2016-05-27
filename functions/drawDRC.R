drawDRC <- function (input, values)
{
  q<-renderPlotly({
    
    all_inputs <- names(input)
    print(all_inputs)
    
    point_plot <- values$GR_table
    
    for(col_name in all_inputs[grep("^param_", all_inputs)])
    {
      if (length(input[[col_name]])>0) {
        sel_values_list <- input[[col_name]]
        df_colname <- gsub("^param_(.*)","\\1",col_name)
        point_plot_tmp <- point_plot[which(point_plot[[df_colname]] %in% sel_values_list),]
        point_plot <- point_plot_tmp
      }
    }
    experiments = unique(point_plot$experiment)
    curve_plot = values$parameter_table
    min_conc = min(point_plot$concentration, na.rm = T)
    max_conc = max(point_plot$concentration, na.rm = T)
    print(log10(min_conc))
    print(log10(max_conc))
    Concentration = 10^(seq(log10(min_conc) - 1, log10(max_conc) + 1, length.out = 1000))
    curve_data_all = NULL
    print(6)
    for(exp in experiments) {
      row = which(curve_plot$experiment == exp)
      EC50 = curve_plot$GEC50[row]
      GRinf = curve_plot$GRinf[row]
      Hill = curve_plot$Hill[row]
      logistic_3u = function(c){GRinf + (1 - GRinf)/(1 + (c/EC50)^Hill)}
      curve_data = as.matrix(Concentration)
      colnames(curve_data) = "Concentration"
      if(curve_plot$fit[row] == "sigmoid") {
        GR = apply(curve_data, 1, logistic_3u)
      } else {
        GR = curve_plot$flat_fit[row]
      }
      curve_data = cbind(curve_data, GR)
      curve_data = as.data.frame(curve_data)
      curve_data$experiment = exp
      if(is.null(curve_data_all)){
        curve_data_all = curve_data
      } else {
        curve_data_all = rbind(curve_data_all, curve_data)
      }
    }
    curve_data_all$experiment = as.factor(curve_data_all$experiment)

    print(7)
    if(input$plot_options == 1) {
      p = ggplot(data = point_plot, aes(x = log10(concentration), y = GR, colour = experiment)) + geom_point()
    } else if(input$plot_options == 2) {
      p = ggplot(data = curve_data_all, aes(x = log10(Concentration), y = GR, colour = experiment)) + geom_line()
    } else if(input$plot_options == 3) {
      p = ggplot() + geom_line(data = curve_data_all, aes(x = log10(Concentration), y = GR, colour = experiment)) + geom_point(data = point_plot, aes(x = log10(concentration), y = GR, colour = experiment))
    }
    p = p + coord_cartesian(xlim = c(log10(min_conc)-0.1, log10(max_conc)+0.1), ylim = c(-1, 1.5), expand = T) + ggtitle("Concentration vs. GR values") + xlab('Concentration (log10 scale)') + ylab('GR value') + labs(colour = "") + geom_hline(yintercept = 1, size = .25) + geom_hline(yintercept = 0, size = .25) + geom_hline(yintercept = -1, size = .25)

    print(10)
    plotDRC <<- p
  })
  print(12)
  return(q)
}