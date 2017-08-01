library(shiny)
library(shinyjs)
library(plotly)
library(ggplot2)
library(readr)
library(drc)
library(GRmetrics)
library(S4Vectors)
library(stringr)
library(DT)
library(formattable)

source('functions/drawPopup.R')
source('functions/drawDRC.R', local = T)
source('functions/extractGridData.R')
source('functions/drawScatter.R', local = T)
source('functions/drawBox.R')
source('functions/parseLabel.R')

shinyServer(function(input, output,session) {

  values <- reactiveValues(inData=NULL, input_case = NULL, GR_table = NULL, GR_table_show = NULL, 
                           parameter_table = NULL, parameter_table_show = NULL, 
                           df_scatter = NULL, showanalyses=0, showdata=0, 
                           showanalyses_multi=0, data_dl = NULL, wilcox = NULL,
                           clearScatter = F, separator = NULL, div_rate = NULL)
  isolate(values$inData)
  
  # Save the input format (case A vs. case C)
  observeEvent(c(input$caseA_div, input$caseA_time0), { values$input_case = "A" })
  observeEvent(c(input$caseC_div, input$caseC_time0), { values$input_case = "C" })
  
  # Save the input format (comma vs. tab separated)
  observeEvent(input$comma_input,{ values$separator = ","})
  observeEvent(input$tab_input,{ values$separator = "\t"})
  
  # Save the input format (initial cell count vs. division rate)
  observeEvent(c(input$caseA_time0, input$caseC_time0), { values$div_rate = F })
  observeEvent(c(input$caseA_div, input$caseC_div), { values$div_rate = T })

  observeEvent(input$loadExample, {
    values$input_case = "A"
    values$data_dl = 'example'
    output$input_error = renderText("")
    session$sendCustomMessage(type = "resetFileInputHandler", "uploadData")
    values$inData <- read_tsv('resources/toy_example_input1_edited.tsv')
    values$GR_table_show = NULL
    values$parameter_table_show = NULL
    values$showanalyses=0
    values$showanalyses_multi=0
    if(values$showdata) updateTabsetPanel(session,"tabs",selected="tab-data")
    output$input_table <- renderDataTable(datatable(values$inData, rownames = F))
  })
  
  observeEvent(input$loadExampleC, {
    values$input_case = "C"
    values$data_dl = 'example'
    output$input_error = renderText("")
    session$sendCustomMessage(type = "resetFileInputHandler", "uploadData")
    values$inData <- read_tsv('resources/toy_example_input4_edited.tsv')
    values$GR_table_show = NULL
    values$parameter_table_show = NULL
    values$showanalyses=0
    values$showanalyses_multi=0
    if(values$showdata) updateTabsetPanel(session,"tabs",selected="tab-data")
    output$input_table <- renderDataTable(datatable(values$inData, rownames = F))
  })
  
  observeEvent(input$uploadData, {
    values$data_dl = 'input'
    values$showanalyses=0
    values$showanalyses_multi=0
    if(values$showdata) updateTabsetPanel(session,"tabs",selected="tab-data")
    values$GR_table_show = NULL
    values$parameter_table_show = NULL
    inFile <- input$uploadData
    if (is.null(inFile)) {return(NULL)
      } else if(values$separator == '\t'){
      if(input$euro_in == T) {values$inData <- read_tsv(inFile$datapath, locale = locale(decimal_mark = ','))
      } else {values$inData <- read_tsv(inFile$datapath)}
      # Get rid of blank rows at the end of a file
      values$inData <- values$inData[rowSums(is.na(values$inData)) != ncol(values$inData),]
    } else if(values$separator == ',') {
      if(input$euro_in == T) {values$inData <- read_csv2(inFile$datapath)
      } else {values$inData <- read_csv(inFile$datapath)}
      # Get rid of blank rows at the end of a file
      values$inData <- values$inData[rowSums(is.na(values$inData)) != ncol(values$inData),]
    }
    output$input_table <- renderDataTable(datatable(values$inData, rownames = F))
  })
  
  observeEvent(input$fetchURLData, {
    # Somehow add test to make sure that input$url is a proper url/text file
    if(input$url != "") {
      values$data_dl = 'input'
      values$showanalyses=0
      values$showanalyses_multi=0
      if(values$showdata) updateTabsetPanel(session,"tabs",selected="tab-data")
      values$GR_table_show = NULL
      values$parameter_table_show = NULL
      inFile <- input$url
      if(grepl("dropbox.com",inFile) & grepl("dl=0", inFile)) {
        inFile = gsub("dl=0", "dl=1", inFile)
      } else if(grepl("dropbox.com",inFile) & !grepl("dl=", inFile)) {
        inFile = paste0(inFile,"?dl=1")
      } else if(grepl("basecamp.com", inFile) & !grepl("uploads", inFile)) {
        basecamp = readLines(inFile)
        line = grep(inFile, basecamp)[1]
        inFile = strsplit(basecamp[line], '"')[[1]][2]
      }
      if (is.null(inFile)) {
        return(NULL)
      } else if(values$separator == '\t'){
        if(input$euro_in == T) {
          values$inData <- read_tsv(inFile, locale = locale(decimal_mark = ','))
        } else {
          values$inData <- read_tsv(inFile)
        }
        # Get rid of blank rows at the end of a file
        values$inData <- values$inData[rowSums(is.na(values$inData)) != ncol(values$inData),]
      } else if(values$separator == ',') {
        if(input$euro_in == T) {
          values$inData <- read_csv2(inFile)
        } else {
          values$inData <- read_csv(inFile)
        }
        # Get rid of blank rows at the end of a file
        values$inData <- values$inData[rowSums(is.na(values$inData)) != ncol(values$inData),]
      }
      output$input_table <- renderDataTable(datatable(values$inData, rownames = F))
    }
  })
  
  observe({
    toggle(condition = values$showdata, selector = "#tabs li a[data-value=tab-data]")
    toggle(condition = values$showanalyses, selector = "#tabs li a[data-value=tab-drc]")
    toggle(condition = values$showanalyses_multi, selector = "#tabs li a[data-value=tab-drc-grid]")
    toggle(condition = values$showanalyses_multi, selector = "#tabs li a[data-value=tab-gr-metric]")
  })
  observe({
    if(values$showdata) updateTabsetPanel(session,"tabs",selected="tab-data")
  })
  getData <- reactive({
    input$loadExample
    if(!is.null(input$uploadData)) return('ok')
    if(length(values$inData)>0) return('ok')
    return(NULL)
  })
  
  observeEvent(values$inData, {
    output$fileUploaded <- reactive({
      output$input_error = renderText("")
      
      caseA_params = c('concentration', 'cell_count', 'cell_count__ctrl')
      time0_param = 'cell_count__time0'
      div_params = c('treatment_duration', 'division_time')
      if(values$input_case == "A") {
        if(!time0_param %in% colnames(values$inData)) {
          
        }
          delete_cols = which(colnames(values$inData) %in% c('concentration', 'cell_count', 'cell_count__ctrl', 'cell_count__time0', 'treatment_duration', 'division_time'))
          updateSelectizeInput(
            session, 'groupingVars',
            choices = colnames(values$inData)[-delete_cols],
            selected = colnames(values$inData)[-delete_cols],
            options = c()
          )
      } else if(values$input_case == "C") {
        delete_cols = which(colnames(values$inData) %in% c('concentration', 'cell_count', 'treatment_duration', 'division_time'))
        updateSelectizeInput(
          session, 'groupingVars',
          choices = colnames(values$inData)[-delete_cols],
          selected = colnames(values$inData)[-delete_cols],
          options = c()
        )
        } else {
          if(!is.null(getData())) {
              print("bad input")
              output$input_error = renderText("Your data is not in the correct form. Please read the 'Getting Started' Section.")
          }
      }
      if(!is.null(getData())) values$showdata=1
      return(!is.null(getData()))
    })
    
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  })
  
  output$input_table <- renderDataTable(datatable(values$inData, rownames = F))
  
  observeEvent(input$pick_data, {
    if(input$pick_data == 1) {output$input_table <- renderDataTable(datatable(values$inData, rownames = F))}
    if(input$pick_data == 2) {
      # if(!is.null(values$GR_table_show)) {
      #   colnames(values$GR_table_show)[which(colnames(values$GR_table_show)=="GR")]<-"GR_value"
      # }
      output$input_table <- renderDataTable(datatable(values$GR_table_show, rownames = F))
    }
    if(input$pick_data == 3) {output$input_table <- renderDataTable(datatable(values$parameter_table_show, rownames = F))}
  })
  
  ###### Hide pop-up "modals" when next choice is made
  observeEvent(c(input$initialCellCount, input$divisionRate), {
    toggleModal(session, 'importDialog1', toggle = "close")
  }, ignoreInit = T)
  observeEvent(c(input$caseA_time0, input$caseC_time0), {
    toggleModal(session, 'importDialog2_time0', toggle = "close")
  }, ignoreInit = T)
  observeEvent(c(input$caseA_div, input$caseC_div), {
    toggleModal(session, 'importDialog2_div_rate', toggle = "close")
  }, ignoreInit = T)
  observeEvent(c(input$comma_input, input$tab_input), {
    toggleModal(session, 'importDialog3', toggle = "close")
  }, ignoreInit = T)
  observeEvent(c(input$uploadData,input$fetchURLData), {
    toggleModal(session, 'importDialog4', toggle = "close")
  }, ignoreInit = T)
  observeEvent(c(input$loadExample, input$loadExampleC),{
    toggleModal(session, 'loadExamples', toggle = "close")
  }, ignoreInit = T)
  # Add other triggers for comma/tab separator input popup
  observeEvent(c(input$caseA_div, input$caseC_time0, input$caseC_div), {
    toggleModal(session, 'importDialog3', toggle = "open")
  }, ignoreInit = T)
  # Add other trigger for final input popup
  observeEvent(input$tab_input, {
    toggleModal(session, 'importDialog4', toggle = "open")
  })
  
  # Function to check for slightly misspelled column names
  check_col_names = function(col_names, expected) {
    check_cols = col_names[!col_names %in% expected]
    missing_cols = expected[!expected %in% col_names]
    n <- max(length(check_cols), length(missing_cols))
    length(check_cols) <- n                      
    length(missing_cols) <- n
    df = data.frame(missing = missing_cols, cols = check_cols, check.names = F)
    for(i in 1:length(check_cols)) {
      hits = agrep(check_cols[i], missing_cols, value = T)
      df$missing[i] = hits[1]
    }
    df = df[!is.na(df$missing),]
    test_cols = NULL
    print(df)
    if(dim(df)[1] > 0) {
      for(i in 1:dim(df)[1]) {
        test_cols = c(test_cols, agrepl(df$missing[i], df$cols[i])[1])
      }
    df = df[test_cols,]
    }
    return(df)
  }
  
  observeEvent(input$uploadData, {
    # Check input table column names for slight misspellings
    caseA_cols = c("cell_line", "drug", "concentration", "cell_count", "cell_count__ctrl", "cell_count__time0")
    caseC_cols = c("cell_line", "drug", "concentration", "cell_count", "time")
    if(values$input_case == "A" & !values$div_rate) {cols = caseA_cols}
    if(values$input_case == "A" & values$div_rate) {cols = caseA_cols[1:5]}
    if(values$input_case == "C") {cols = caseC_cols}
    
    print(cols)
    incols = colnames(values$inData)
    print(incols)
    if(length(intersect(cols, incols)) != 4) {
      sug = check_col_names(incols, cols)
      print(sug)
      print(dim(sug))
      if(dim(sug)[1] > 0) {
        message = NULL
        for(i in 1:dim(sug)[1]) {
          message = paste(message, 'Column ', '<font color="navy"><b>', sug[i,1], '</b><font color="black">', ' is missing.', ' It may be misnamed as ', '<b><font color="maroon">', sug[i,2], '</b><font color="black">', '.', '<br>','<br>', sep = '')
        }
        output$col_suggest = renderText(message)
      }
    }
    
    # Check that necessary columns of input table exist, check other table properties
    # Display output table showing pass/fail for these properties
    df = data.frame(matrix(nrow = length(cols), ncol = 2), check.names = F)
    colnames(df) = c("Necessary column", "pass/fail")
    for(i in 1:length(cols)) {
      df[i,1]= cols[i]
      df[i,2] = ifelse(cols[i] %in% colnames(values$inData), T, F)
    }
    delete_cols = which(colnames(values$inData) %in% cols)
    check1 = ifelse (dim(values$inData[,-delete_cols, drop = F])[2] > 0, T, F)
    df = rbind(df, c("Additional grouping variables", check1))
    formats = list(`pass/fail` = formatter("span", style = x ~ style(color = ifelse(x, "green", "red")),
                               x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))))
    
    df2 = data.frame(matrix(nrow = 1, ncol = 2), check.names = F)
    colnames(df2) = c("check", "pass/fail")
    if(values$input_case == "A") {
      df2[1,1] = "Concentration values are all greater than 0"
      df2[1,2] = ifelse (sum(values$inData$concentration <= 0) == 0, T, F)
    } else {
      df2[1,1] = "Concentration values are all non-negative"
      df2[1,2] = ifelse (sum(values$inData$concentration < 0) == 0, T, F)
      df2[2,1] = "Rows with concentration equal to zero (control cell counts)"
      df2[2,2] = ifelse (sum(values$inData$concentration == 0) > 0, T, F)
      df2[2,1] = "Rows with time equal to zero (initial cell counts)"
      df2[2,2] = ifelse (sum(values$inData$time == 0) > 0, T, F)
    }

    output$input_check = renderFormattable({
      formattable(df, formats, align = "l")
    })
    output$input_check2 = renderFormattable({
      formattable(df2, formats, align = "l")
    })
  })
    
#========== Download button data tables =======
  output$downloadData <- downloadHandler(
    filename = function() {
      if(values$data_dl == 'input') {
        if(input$pick_data == 1) {
          return(paste(sub("^(.*)[.].*", "\\1", input$uploadData), ".", input$download_type, sep=''))
        } else if(input$pick_data == 2) {
          return(paste(sub("^(.*)[.].*", "\\1", input$uploadData), '_GRvalues', ".", input$download_type, sep=''))
        } else {
          return(paste(sub("^(.*)[.].*", "\\1", input$uploadData),'_FittedParameters', ".", input$download_type, sep = ''))
        }
      } else if(values$data_dl == 'example') {
        if(input$pick_data == 1) {
          return(paste("example.", input$download_type, sep = ""))
        } else if(input$pick_data == 2) {
          return(paste("example_GRvalues.", input$download_type, sep = ""))
        } else {
          return(paste("example_FittedParameters.", input$download_type, sep = ""))
        }
      }
    },
    content = function(filename) {
      if(input$pick_data == 1) {
        data_output = values$inData
      } else if(input$pick_data == 2) {
        data_output = values$GR_table_show
        # if(!is.null(data_output)) {
        #   colnames(data_output)[which(colnames(data_output)=="GR")]<-"GR_value"
        # }
      } else {
        data_output = values$parameter_table_show
      }
      if(input$download_type == "tsv") {
        if(input$euro_out == T) {
          write.table(data_output, file = filename, quote = F, sep = '\t', row.names = F, col.names = T, dec = ',')
        } else {
          write.table(data_output, file = filename, quote = F, sep = '\t', row.names = F, col.names = T)
        }
      } else if(input$download_type == "csv") {
        if(input$euro_out == T) {
          write.table(data_output, file = filename, quote = F, sep = ';', row.names = F, col.names = T, dec = ',')
        } else {
          write.table(data_output, file = filename, quote = F, sep = ',', row.names = F, col.names = T)
        }
      }
      
    }
    #,
    #contentType = paste('text/', input$download_type, sep = "")
  )

#========== Download buttons for DRC plots =======
  output$downloadDRC = downloadHandler(
      filename = function() {
        if(input$drcImageType == '.pdf') {
          if(!is.null(input$uploadData)) {
            return(paste(sub("^(.*)[.].*", "\\1", input$uploadData), "_GR_DRC.pdf", sep=''))
          } else {
            return("example_GR_DRC.pdf")
          }
        } else {
          if(!is.null(input$uploadData)) {
            return(paste(sub("^(.*)[.].*", "\\1", input$uploadData), "_GR_DRC.tiff", sep=''))
          } else {
            return("example_GR_DRC.tiff")
          }
        }
      },
      content = function(filename) {
        if(input$drcImageType == '.pdf') {
          ggsave(filename = filename, plot = plotDRC, device = "pdf")
        } else {
          ggsave(filename = filename, plot = plotDRC, device = "tiff", units = "in", width = 7, height = 7, dpi = 300)
        }
      }
  )

 
#========== Download button for scatterplot images =======
  output$downloadScatter = downloadHandler(
    filename = function() {
      if(input$scatterImageType == '.pdf') {
        if(!is.null(input$uploadData)) {
          if(input$box_scatter == "Scatter plot") {
            return(paste(sub("^(.*)[.].*", "\\1", input$uploadData), "_GR_scatter.pdf", sep=''))
          } else {
            return(paste(sub("^(.*)[.].*", "\\1", input$uploadData), "_GR_box.pdf", sep=''))
          }
        } else {
          if(input$box_scatter == "Scatter plot") {
            return("example_GR_scatter.pdf")
          } else {
            return("example_GR_box.pdf")
          }
        }
      } else {
        if(!is.null(input$uploadData)) {
          if(input$box_scatter == "Scatter plot") {
            return(paste(sub("^(.*)[.].*", "\\1", input$uploadData), "_GR_scatter.tiff", sep=''))
          } else {
            return(paste(sub("^(.*)[.].*", "\\1", input$uploadData), "_GR_box.tiff", sep=''))
          }
        } else {
          if(input$box_scatter == "Scatter plot") {
            return("example_GR_scatter.tiff")
          } else {
            return("example_GR_box.tiff")
          }
        }
      }
    },
    content = function(filename) {
      if(input$scatterImageType == '.pdf') {
        ggsave(filename = filename, plot = plotScatter_box, device = "pdf")
      } else {
        ggsave(filename = filename, plot = plotScatter_box, device = "tiff", units = "in", width = 7, height = 7, dpi = 300)
      }
    }
  )

#============ Plotly popups =====================
  observeEvent(input$'dose-response-grid-main', {
    if (input$'dose-response-grid-main' != '' && str_count(input$'dose-response-grid-main', '=') == 1) {
      q = parseLabel(input, values)
      output$graphPopupPlot <- renderPlotly(q)
      toggleModal(session,"graphPopup")
    }
  })
  
  observeEvent(input$plot_scatter, {
    print('clearScatter = F')
    values$clearScatter = F
  })
 
observeEvent(c(input$plot_scatter,input$pick_parameter), {
  output$plotlyScatter1 <- renderPlotly({
    if(values$clearScatter) {
       return()
     } else {
      isolate(drawScatter(input, values))
    }
  })
}, ignoreInit = T)

    
#================= analyzeButton ================================
  observeEvent(input$analyzeButton, {
    df_full <<- NULL
    values$clearScatter = T
  })

  observeEvent(input$clear, {
    df_full <<- NULL
    values$clearScatter = T
  })
  
  observeEvent(input$pick_var, {
    df_full <<- NULL
    values$clearScatter = T
  })


  observeEvent(input$analyzeButton,{
    df_full <<- NULL
    all_inputs <- names(input)
    print(all_inputs)
    groupingColumns <<- input$groupingVars
    print("groupingColumns")
    print(groupingColumns)
    print("groupingColumns")
    
    tables <- try(GRfit(values$inData, groupingColumns, force = input$force, cap = input$cap, case = values$input_case))
    
    if(class(tables)!="try-error") {
      values$parameter_table <- GRgetMetrics(tables)
      values$GR_table <- GRgetValues(tables)
      parameters_show <- GRgetMetrics(tables)
      
      values$GR_table_show <- values$GR_table
      values$GR_table_show$GRvalue <- as.numeric(prettyNum(values$GR_table_show$GRvalue, digits = 3))
      values$GR_table_show$log10_concentration <- as.numeric(prettyNum(values$GR_table_show$log10_concentration, digits = 3))
      values$GR_table_show$rel_cell_count <- as.numeric(prettyNum(values$GR_table_show$rel_cell_count, digits = 3))
      test_gr <<- values$GR_table
      print("finishedGR")
      
      values$parameter_table$GR50[is.infinite(values$parameter_table$GR50)] = NA
      values$parameter_table$h_GR[values$parameter_table$h_GR == 0.01] = NA
      values$parameter_table$`log10(GR50)` = log10(values$parameter_table$GR50)
      values$parameter_table$`log2(h_GR)` = log2(values$parameter_table$h_GR)
      
      values$parameter_table$IC50[is.infinite(values$parameter_table$IC50)] = NA
      values$parameter_table$h[values$parameter_table$h == 0.01] = NA
      values$parameter_table$`log10(IC50)` = log10(values$parameter_table$IC50)
      values$parameter_table$`log2(h)` = log2(values$parameter_table$h)
      
      #values$parameter_table$`log10(EC50)` = log10(values$parameter_table$GEC50)
      
      # For compatibility with shinyLi dose-response grid visualization
      #values$parameter_table$Hill = values$parameter_table$h_GR
      #values$parameter_table$`log2(Hill)` = log2(values$parameter_table$h_GR)
  
      test_ref <<- values$parameter_table
      #values$parameter_table_show <- temp_parameter_table[[2]]
      
      parameters_show$GR50 = as.numeric(prettyNum(parameters_show$GR50, digits = 3))
      parameters_show$GRmax = as.numeric(prettyNum(parameters_show$GRmax, digits = 3))
      parameters_show$GR_AOC = as.numeric(prettyNum(parameters_show$GR_AOC, digits = 3))
      parameters_show$GEC50 = as.numeric(prettyNum(parameters_show$GEC50, digits = 3))
      parameters_show$GRinf = as.numeric(prettyNum(parameters_show$GRinf, digits = 3))
      parameters_show$h_GR = as.numeric(prettyNum(parameters_show$h_GR, digits = 3))
      parameters_show$r2_GR = as.numeric(prettyNum(parameters_show$r2_GR, digits = 3))
      parameters_show$pval_GR = as.numeric(prettyNum(parameters_show$pval_GR, digits = 3))
      parameters_show$flat_fit_GR = as.numeric(prettyNum(parameters_show$flat_fit_GR, digits = 3))
      
      parameters_show$IC50 = as.numeric(prettyNum(parameters_show$IC50, digits = 3))
      parameters_show$Emax = as.numeric(prettyNum(parameters_show$Emax, digits = 3))
      parameters_show$AUC = as.numeric(prettyNum(parameters_show$AUC, digits = 3))
      parameters_show$EC50 = as.numeric(prettyNum(parameters_show$EC50, digits = 3))
      parameters_show$Einf = as.numeric(prettyNum(parameters_show$Einf, digits = 3))
      parameters_show$h = as.numeric(prettyNum(parameters_show$h, digits = 3))
      parameters_show$r2_rel_cell = as.numeric(prettyNum(parameters_show$r2_rel_cell, digits = 3))
      parameters_show$pval_rel_cell = as.numeric(prettyNum(parameters_show$pval_rel_cell, digits = 3))
      parameters_show$flat_fit_rel_cell = as.numeric(prettyNum(parameters_show$flat_fit_rel_cell, digits = 3))
      
      values$parameter_table_show <- parameters_show
      #=========================
      test_ref_show <<- values$parameter_table_show
      print("finishedParams")
    } else {
      # When the GRfit function fails for some reason
      output$input_error = renderText("There was an error in the GR value calculation. Please check that your data is in the correct form.")
    }
    
    #=========== data loaded (start) ================
    if (length(values$inData)>0) {
      values$showanalyses<-1
      if (length(input$groupingVars)>0) {values$showanalyses_multi<-1
      } else {values$showanalyses_multi<-0}
      
      output$plot.ui <- renderUI({
        plotlyOutput("drc2", height = input$height)
      })
      
      output$plot.ui2 <- renderUI({
        if(input$box_scatter == "Box plot") {
          plotlyOutput('boxplot', height = input$scatter_height)
        } else {
          plotlyOutput("plotlyScatter1", height = input$scatter_height)
        }
      })
      
      observeEvent(input$scatter_height, {
        output$plot.ui2 <- renderUI({
          if(input$box_scatter == "Box plot") {
            plotlyOutput('boxplot', height = input$scatter_height)
          } else {
            plotlyOutput("plotlyScatter1", height = input$scatter_height)
          }
        })
      })

      output$drc2<-drawDRC(input, values)
      output$ui <- renderUI({
        n <- length(input$groupingVars)
        if (n>0) {
          code_output_list <- lapply(1:n, function(i) {
            # name the input choice based on the grouping variable names, prefix with "param_" to aviod conflict
            codeOutput <- paste("param_", input$groupingVars[i], sep="")
            verbatimTextOutput(codeOutput)
            drc_choices = sort(unique(values$GR_table[[ input$groupingVars[i] ]]))
            selectizeInput(codeOutput, input$groupingVars[i], choices = drc_choices, 
                           multiple = TRUE, selected = drc_choices[1])
          })
        } else code_output_list <- list()
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, code_output_list)
      })
      
      observeEvent(groupingColumns, {
          updateSelectInput(
            session, 'pick_var',
            choices = input$groupingVars
          )
      })
      
      observeEvent(input$pick_var, {
        scatter_choices = unique(values$GR_table[[input$pick_var]])
        updateSelectInput(
          session, 'x_scatter',
          choices = scatter_choices,
          selected = NULL
        )
        updateSelectizeInput(
          session, 'y_scatter',
          choices = scatter_choices,
          selected = NULL
        )
      })

      output$boxplot <- renderPlotly({
        box = drawBox(input, values)
        if(!is.null(box)) {
          box
        } else {stop()}
      })
    }
    #=============== data loaded (end) ===================
  })
#======= analyzeButton (end) ===========
  
  observeEvent(input$analyzeButton, {
      outVar <- reactive({
        vars <- all.vars(parse(text=as.character(input$pick_box_x)), functions = FALSE, unique = TRUE)
        return(vars)
      })
      
      observeEvent(input$factorA, ignoreNULL = FALSE, {
        picks = sort(input$pick_box_factors)
        picks1 = setdiff(picks, input$factorA)
        updateSelectizeInput(
          session, 'factorB',
          choices = picks1,
          selected = input$factorB
        )
      })
      
      observeEvent(input$factorB, ignoreNULL = FALSE, {
        picks = sort(input$pick_box_factors)
        picks2 = setdiff(picks, input$factorB)
        updateSelectizeInput(
          session, 'factorA',
          choices = picks2,
          selected = input$factorA
        )
      })
      
      observeEvent(input$pick_box_x, {
        picks = unique(values$GR_table[[outVar()]])
        updateSelectizeInput(
          session, 'pick_box_factors',
          choices = picks,
          selected = picks[1:min(10, length(picks))]
        )
      })
      
      observeEvent(input$box_scatter, {
        if(!is.null(input$pick_box_x)) {
          picks = unique(values$GR_table[[outVar()]])
          updateSelectizeInput(
            session, 'pick_box_factors',
            choices = picks,
            selected = picks[1:min(10, length(picks))]
          )
        }
      })
      
      observeEvent(c(input$box_scatter, input$pick_box_x, input$pick_box_factors), {
        if(!is.null(input$pick_box_x)) {
          picks = sort(input$pick_box_factors)
          updateSelectizeInput(
            session, 'factorA',
            choices = picks,
            selected = NULL
          )
          updateSelectizeInput(
            session, 'factorB',
            choices = picks,
            selected = NULL
          )
        }
      })
      
      observeEvent(input$box_scatter, {
        observeEvent(input$pick_var, {
          updateSelectInput(
            session, 'x_scatter',
            choices = unique(values$inData[[input$pick_var]]),
            selected = NULL
          )
          updateSelectizeInput(
            session, 'y_scatter',
            choices = unique(values$inData[[input$pick_var]]),
            selected = NULL
          )
        })
      })
  })
  
  observeEvent(c(input$curve_type_grid, input$choiceVar, input$xgroupingVars), {
    output$'dose-response-grid-main' <- renderLiDoseResponseGrid(
      input="",
      xmin = min(log10(values$GR_table$concentration), na.rm = T),
      xmax = max(log10(values$GR_table$concentration), na.rm = T),
      factors=c(paste(isolate(input$xgroupingVars),collapse = ' '), isolate(input$choiceVar)),
      toggle=0,
      {
        #input$plot_gr50grid
        isolate(extractGridData(input, output, values$parameter_table, isolate(input$choiceVar), isolate(input$xgroupingVars)))
      }
      )
  })
  
      box_scatter_choices = c('GR50', 'GRmax', 'GRinf', 'h_GR', 'GR_AOC', 'IC50','Emax', 'Einf', 'h', 'AUC')

      output$scatter <- renderUI({
        if(input$box_scatter == "Scatter plot") {
          fluidRow(
            selectInput('pick_parameter', 'Select parameter', choices = box_scatter_choices),
            selectInput('pick_var', 'Select variable', choices = input$groupingVars),
            selectInput('x_scatter', 'Select x-axis value', choices = unique(values$inData[[input$pick_box_x]])),
            selectizeInput('y_scatter', 'Select y-axis value', choices = unique(values$inData[[input$pick_box_x]])),
            bsButton('plot_scatter', 'Add', size = 'small'),
            bsButton('clear', 'Clear', size = 'small')
          )
        } else {
          fluidRow(
            selectInput('pick_box_y', 'Select parameter', choices = box_scatter_choices),
            selectInput('pick_box_x', 'Select grouping variable', choices = input$groupingVars),
            selectInput('pick_box_point_color', 'Select additional point coloring', choices = input$groupingVars),
            selectizeInput('pick_box_factors', 'Show/hide data', choices = c(), multiple = T),
            actionLink('wilcox_panel', 'Compare boxplots'),
            conditionalPanel(condition = "input.wilcox_panel%2==1",
                            selectizeInput('factorA', 'Wilcoxon rank-sum test', choices = c(), multiple = T),
                            selectizeInput('factorB', '', choices = c(), multiple = T),
                            radioButtons('wilcox_method', label = "",choices = c("One-sided", "Two-sided"), selected = "Two-sided", inline = F),
            # selectizeInput('factorA', 'Choose factors with which to perform a one-sided Wilcoxon rank-sum test', choices = c(), multiple = T),
            # selectizeInput('factorB', '', choices = c(), multiple = T),
            textOutput("wilcox")
            )
          )
        }
      })
      
      observeEvent(c(input$factorA, input$factorB, input$pick_box_y, input$wilcox_method), {
        wil_data = values$parameter_table
        if(!is.null(input$factorA) & !is.null(input$factorB)) {
          rowsA = wil_data[[input$pick_box_x]] %in% input$factorA
          rowsB = wil_data[[input$pick_box_x]] %in% input$factorB
          wil_dataA = wil_data[rowsA,input$pick_box_y]
          wil_dataB = wil_data[rowsB,input$pick_box_y]
          print(head(wil_dataA))
          print(head(wil_dataB))
          wil_less = wilcox.test(x = wil_dataA, y = wil_dataB, alternative = "less")
          wil_greater = wilcox.test(x = wil_dataA, y = wil_dataB, alternative = "greater")
          wil_two_sided = wilcox.test(x = wil_dataA, y = wil_dataB, alternative = "two.sided")$p.value
          wil_one_sided = min(wil_less$p.value,wil_greater$p.value)
          wil_pval = ifelse(input$wilcox_method == "One-sided", wil_one_sided, wil_two_sided)
          values$wilcox = prettyNum(wil_pval, digits = 2)
          output$wilcox = renderText({
            paste("P-value:", prettyNum(wil_pval, digits = 2))
          })
        } else {
          output$wilcox = renderText({
            paste("P-value: ")
          })
          values$wilcox = NULL
        }
      })
      
    observeEvent(input$analyzeButton, {
      updateSelectizeInput(session, 'choiceVar', choices = input$groupingVars, server = TRUE, selected=input$groupingVars[1])
      if (length(input$groupingVars)==1) {
         updateSelectizeInput(session, 'xgroupingVars', choices = input$groupingVars, server = TRUE, selected=input$groupingVars[1])
      } else {
         updateSelectizeInput(session, 'xgroupingVars', choices = input$groupingVars, server = TRUE, selected=input$groupingVars[2])
      }
      
#       observeEvent(input$plot_gr50grid, {
#         output$'dose-response-grid-main' <- renderLiDoseResponseGrid(
#           input="",
#           xmin = min(log10(values$GR_table$concentration), na.rm = T),
#           xmax = max(log10(values$GR_table$concentration), na.rm = T),
# 	  factors=c(paste(isolate(input$xgroupingVars),collapse = ' '), isolate(input$choiceVar)),
# 	  toggle=0,
#           {
#             input$plot_gr50grid
#             isolate(extractGridData(input, output, values$parameter_table, isolate(input$choiceVar), isolate(input$xgroupingVars)))
#           }
#         )
#         print('test')
#         print(input$xgroupingVars)
#         print(input$choiceVar)
#         print(input$plot_gr50grid)
#       })
    })

#================================================
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    graphics.off()
    print('devices off')
  })
})
