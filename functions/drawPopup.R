drawPopup <- function(curve_plot, curve_type, min_conc, max_conc) {
  Concentration = 10^(seq(min_conc, max_conc, length.out = 200))
  curve_data_all = NULL
  len = dim(curve_plot)[1]
  for(row in 1:len) {
    if(curve_type == "GR") {
      GEC50 = curve_plot$GEC50[row]
      GRinf = curve_plot$GRinf[row]
      h_GR = curve_plot$h_GR[row]
      logistic_3u = function(c){GRinf + (1 - GRinf)/(1 + (c/GEC50)^h_GR)}
      curve_data = as.matrix(Concentration)
      colnames(curve_data) = "Concentration"
      if(curve_plot$fit_GR[row] == "sigmoid") {
        GR = apply(curve_data, 1, logistic_3u)
      } else {
        GR = curve_plot$GRinf[row]
      }
      curve_data = cbind(curve_data, GR)
    } else if(curve_type == "IC") {
      EC50 = curve_plot$EC50[row]
      Einf = curve_plot$Einf[row]
      h = curve_plot$h[row]
      logistic_3u = function(c){Einf + (1 - Einf)/(1 + (c/EC50)^h)}
      curve_data = as.matrix(Concentration)
      colnames(curve_data) = "Concentration"
      if(curve_plot$fit_IC[row] == "sigmoid") {
        rel_cell_count = apply(curve_data, 1, logistic_3u)
      } else {
        rel_cell_count = curve_plot$Einf[row]
      }
      curve_data = cbind(curve_data, rel_cell_count)
    }
    
    curve_data = as.data.frame(curve_data)
    curve_data$experiment = curve_plot$experiment[row]
    if(is.null(curve_data_all)){
      curve_data_all = curve_data
    } else {
      curve_data_all = rbind(curve_data_all, curve_data)
    }
  }
  curve_data_all$experiment = as.factor(curve_data_all$experiment)
  if(curve_type == "GR") {
    p = ggplot(data = curve_data_all, aes(x = log10(Concentration), y = GR, colour = experiment)) + geom_line() + ggtitle("Concentration vs. GR values") + xlab('Concentration (log10 scale)') + ylab('GR value') + labs(colour = "") + geom_hline(yintercept = 1, size = .25) + geom_hline(yintercept = 0, size = .25) + geom_hline(yintercept = -1, size = .25)
  } else if(curve_type == "IC") {
    p = ggplot(data = curve_data_all, aes(x = log10(Concentration), y = rel_cell_count, colour = experiment)) + geom_line() + ggtitle("Concentration vs. Relative cell count") + xlab('Concentration (log10 scale)') + ylab('Relative cell count') + labs(colour = "") + geom_hline(yintercept = 1, size = .25) + geom_hline(yintercept = 0, size = .25) + geom_hline(yintercept = -1, size = .25)
  }
  #try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
  ggplotly(p)
}