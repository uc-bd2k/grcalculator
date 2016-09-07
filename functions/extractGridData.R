extractGridData <- function(input, output, data, choiceVar, groupingVars) {
  
  if (!(is.null(input$choiceVar) || is.null(input$xgroupingVars) )) {
    data1 <- data
    data1[[choiceVar]]<-data1[[choiceVar]]
    # Separate names with two spaces just in case there are single spaces in cell line names, for example
    print('groupingVars')
    print(groupingVars)
    data1[[paste(groupingVars,collapse = ' ')]] <- do.call(paste, c(as.data.frame(data[groupingVars], stringsAsFactors=FALSE),sep='  '))
    data1['EC50'] <- data['GEC50']
    data1['Einf'] <- data['GRinf']
    data1['HillSlope'] <- data['h_GR']
    data1['log10[EC50]'] <- lapply(data1['EC50'], log10)
    data1
  } else {
    NULL
  }
}