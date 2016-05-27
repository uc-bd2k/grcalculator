calculate_GR = function(input_data,groupingColumns) {
  log2nn = with(input_data, log2(cell_count/cell_count__time0))
  log2nn_ctrl = with(input_data, log2(cell_count__ctrl/cell_count__time0))
  GR = 2^(log2nn/log2nn_ctrl) - 1
  input_edited = input_data
  input_edited$log10_concentration = log10(input_edited$concentration)
  input_edited$GR = GR
  tmp<-input_edited[,groupingColumns, drop = F]
  experimentNew = (apply(tmp,1, function(x) (paste(x,collapse=" "))))
  if(length(groupingColumns) > 0) {
    input_edited$experiment = as.factor(experimentNew)
  } else {
    input_edited$experiment = as.factor("All Data")
  }
  return(input_edited)
}
