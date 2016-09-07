parseLabel = function(input, values) {
  graphParams <- input$'dose-response-grid-main'
  print('graphparams')
  graphParams = URLdecode(graphParams)
  graphParams = trimws(graphParams)
  if(graphParams == ',') {
    graphParams = ''
  }
  print(graphParams)
  if (graphParams != '') {
    cols = strsplit(graphParams, split = '=')[[1]][1]
    cols_vector = unlist(strsplit(cols, split = ' '))
    if(',' %in% cols_vector) {
      cols_vector = cols_vector[-which(cols_vector == ',')]
    }
    vals = strsplit(graphParams, split = '=')[[1]][2]
    # values are separated with two spaces just in case, for example, cell line names have a space in them.
    vals_vector = unlist(strsplit(vals, split = '  '))
    print(cols_vector)
    print(vals_vector)
    if(length(cols_vector) == length(vals_vector)) {
      popupData = values$parameter_table
      for(i in 1:length(cols_vector)) {
        print(cols_vector[i])
        print(vals_vector[i])
        keep = popupData[[ cols_vector[i] ]] == vals_vector[i]
        popupData = popupData[keep,]
      }
    } else {
      pick = cols_vector[1] #first item is choice(s) from box
      cols_vector = cols_vector[-1] #second item is columns to use
      pick_vector = unlist(strsplit(pick, split = ','))
      print('pick')
      print(pick_vector)
      print('cols_vector')
      print(cols_vector)
      
      popupData = values$parameter_table
      for(i in 1:length(cols_vector)) {
        print(cols_vector[i])
        print(vals_vector[i])
        keep = popupData[[ cols_vector[i] ]] == vals_vector[i]
        popupData = popupData[keep,]
      }
      print('i1')
      if(pick_vector %in% popupData[[input$choiceVar]]) {
        popupData = popupData[popupData[[input$choiceVar]] %in% pick_vector,]
      }
      print('i2')
    }
    point_plot <- values$GR_table
    min_conc = log10(min(point_plot$concentration, na.rm = T))
    max_conc = log10(max(point_plot$concentration, na.rm = T))
    q = drawPopup(popupData, input$curve_type_grid, min_conc, max_conc)
    return(q)
    
  }
}