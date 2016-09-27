library(shiny)
library(shinyjs)
library(plotly)
library(ggplot2)
library(readr)
library(drc)
library(GRmetrics)
library(S4Vectors)
library(stringr)

source('functions/drawPopup.R')
source('functions/drawDRC.R', local = T)
source('functions/extractGridData.R')
source('functions/drawScatter.R', local = T)
source('functions/drawBox.R')
source('functions/parseLabel.R')

shinyServer(function(input, output,session) {

  values <- reactiveValues(inData=NULL, case = "A", GR_table = NULL, GR_table_show = NULL, parameter_table = NULL, parameter_table_show = NULL, df_scatter = NULL, showanalyses=0, showdata=0, showanalyses_multi=0, data_dl = NULL, wilcox = NULL)
  isolate(values$inData)

  observeEvent(input$loadExample, {
    values$data_dl = 'example'
    output$input_error = renderText("")
    session$sendCustomMessage(type = "resetFileInputHandler", "uploadData")
    values$inData <- read_tsv('toy_example_input1_edited.tsv')
    values$GR_table_show = NULL
    values$parameter_table_show = NULL
    values$showanalyses=0
    values$showanalyses_multi=0
    if(values$showdata) updateTabsetPanel(session,"tabs",selected="tab-data")
    output$input_table <- DT::renderDataTable(DT::datatable({
      x<-values$inData
      data.frame(x)
    }, rownames= FALSE))
  })
  
  observeEvent(input$loadExampleC, {
    values$data_dl = 'example'
    output$input_error = renderText("")
    session$sendCustomMessage(type = "resetFileInputHandler", "uploadData")
    values$inData <- read_tsv('toy_example_input4_edited.tsv')
    values$GR_table_show = NULL
    values$parameter_table_show = NULL
    values$showanalyses=0
    values$showanalyses_multi=0
    if(values$showdata) updateTabsetPanel(session,"tabs",selected="tab-data")
    output$input_table <- DT::renderDataTable(DT::datatable({
      x<-values$inData
      data.frame(x)
    }, rownames= FALSE))
  })
  
  observeEvent(input$uploadData, {
    values$data_dl = 'input'
    values$showanalyses=0
    values$showanalyses_multi=0
    if(values$showdata) updateTabsetPanel(session,"tabs",selected="tab-data")
    values$GR_table_show = NULL
    values$parameter_table_show = NULL
    inFile <- input$uploadData
    if (is.null(inFile)) {
      return(NULL)
    } else if(input$sep == '\t'){
      if(input$euro_in == T) {
        values$inData <- read_tsv(inFile$datapath, locale = locale(decimal_mark = ','))
      } else {
        values$inData <- read_tsv(inFile$datapath)
      }
      # Get rid of blank rows at the end of a file
      values$inData <- values$inData[rowSums(is.na(values$inData)) != ncol(values$inData),]
    } else if(input$sep == ',') {
      if(input$euro_in == T) {
        values$inData <- read_csv2(inFile$datapath)
      } else {
        values$inData <- read_csv(inFile$datapath)
      }
      # Get rid of blank rows at the end of a file
      values$inData <- values$inData[rowSums(is.na(values$inData)) != ncol(values$inData),]
    }
    output$input_table <- DT::renderDataTable(DT::datatable({
      x<-values$inData
      data.frame(x)
    }, rownames= FALSE))
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
      if (is.null(inFile)) {
        return(NULL)
      } else if(input$sep == '\t'){
        if(input$euro_in == T) {
          values$inData <- read_tsv(inFile, locale = locale(decimal_mark = ','))
        } else {
          values$inData <- read_tsv(inFile)
        }
        # Get rid of blank rows at the end of a file
        values$inData <- values$inData[rowSums(is.na(values$inData)) != ncol(values$inData),]
      } else if(input$sep == ',') {
        if(input$euro_in == T) {
          values$inData <- read_csv2(inFile)
        } else {
          values$inData <- read_csv(inFile)
        }
        # Get rid of blank rows at the end of a file
        values$inData <- values$inData[rowSums(is.na(values$inData)) != ncol(values$inData),]
      }
      output$input_table <- DT::renderDataTable(DT::datatable({
        x<-values$inData
        data.frame(x)
      }, rownames= FALSE))
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
  
  output$fileUploaded <- reactive({
    output$input_error = renderText("")
    if(length(intersect(colnames(values$inData), c('concentration', 'cell_count', 'cell_count__ctrl', 'cell_count__time0'))) == 4) {
      print('Input Case A')
      delete_cols = which(colnames(values$inData) %in% c('concentration', 'cell_count', 'cell_count__ctrl', 'cell_count__time0'))
      updateSelectizeInput(
        session, 'groupingVars',
        choices = colnames(values$inData)[-delete_cols],
        selected = colnames(values$inData)[-delete_cols],
        options = c()
      )
      values$case = "A"
    } else if(length(intersect(colnames(values$inData), c('concentration', 'cell_count', 'time'))) == 3) {
      print('Input Case C')
      delete_cols = which(colnames(values$inData) %in% c('concentration', 'cell_count'))
      updateSelectizeInput(
        session, 'groupingVars',
        choices = colnames(values$inData)[-delete_cols],
        selected = colnames(values$inData)[-delete_cols],
        options = c()
      )
      values$case = "C"
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
  
  output$input_table <- DT::renderDataTable(DT::datatable({
    x<-values$inData
    data.frame(x)
  }, rownames= FALSE))
  
  observeEvent(input$pick_data, {
    if(input$pick_data == 1) {
      output$input_table <- DT::renderDataTable(DT::datatable({
        x<-values$inData
        data.frame(x)
      }, rownames= FALSE))
    }
    
    if(input$pick_data == 2) {
      output$input_table <- DT::renderDataTable(DT::datatable({
        x<-values$GR_table_show
        if(!is.null(x)) {
          colnames(x)[which(colnames(x)=="GR")]<-"GR_value"
        }
        data.frame(x)
      }, rownames= FALSE))
    }
    
    if(input$pick_data == 3) {
      output$input_table <- DT::renderDataTable(DT::datatable({
        x<-values$parameter_table_show
        data.frame(x)
      }, rownames= FALSE))
    }
  })
  
  observeEvent(input$uploadData, {
    if(!is.null(input$uploadData)) {
      toggleModal(session, 'importDialog', toggle = "close")
    }
  })
  
  observeEvent(input$fetchURLData, {
    if(input$fetchURLData > 0) {
      toggleModal(session, 'importDialog', toggle = "close")
    }
  })
      
  observeEvent(c(input$loadExample, input$loadExampleC),{
    if(input$loadExample > 0 | input$loadExampleC > 0) {
      toggleModal(session, 'loadExamples', toggle = "close")
    }
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
        if(!is.null(data_output)) {
          colnames(data_output)[which(colnames(data_output)=="GR")]<-"GR_value"
        }
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
 
observeEvent(input$analyzeButton, { 
# Make scatterplot reactive to "pick_parameter" after first plot.
  observeEvent(input$pick_parameter, {
    if(input$plot_scatter > 0) {
      output$plotlyScatter1 <- renderPlotly({
        #try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
        plot1 = isolate(drawScatter(input, values))
      })
    }
  })
})

    
#================= analyzeButton ================================
  observeEvent(input$analyzeButton,{
    df_full <<- NULL
    all_inputs <- names(input)
    print(all_inputs)
    groupingColumns <<- input$groupingVars
    print("groupingColumns")
    print(groupingColumns)
    print("groupingColumns")
    
    ###### For use with Bioconductor package #####
    tables <- GRfit(values$inData, groupingColumns, force = input$force, cap = input$cap, case = values$case)
    values$parameter_table <- cbind(as.data.frame(colData(tables)), as.data.frame(t(assay(tables))))
    values$GR_table <- S4Vectors::metadata(tables)[[1]]
    parameters_show <- cbind(as.data.frame(colData(tables)), as.data.frame(t(assay(tables))))
    ##############################################
    
    # ###### For use with old package #####
    # tables <- GRfit(values$inData, groupingColumns, GRtable = 'both', force = input$force, cap = input$cap, case = values$case)
    # values$GR_table <- tables[[1]]
    # values$parameter_table <- tables[[2]]
    # parameters_show <- tables[[2]]
    # #####################################
    
    #values$GR_table <- calculate_GR(values$inData,groupingColumns)
    values$GR_table_show <- values$GR_table
    values$GR_table_show$GR <- as.numeric(prettyNum(values$GR_table_show$GR, digits = 3))
    values$GR_table_show$log10_concentration <- as.numeric(prettyNum(values$GR_table_show$log10_concentration, digits = 3))
    values$GR_table_show$rel_cell_count <- as.numeric(prettyNum(values$GR_table_show$rel_cell_count, digits = 3))
    test_gr <<- values$GR_table
    print("finishedGR")
    #temp_parameter_table = logistic_fit_GR(values$GR_table,groupingColumns)
    #values$parameter_table <- temp_parameter_table[[1]]
    
    
    #values$parameter_table$GEC50[values$parameter_table$GEC50 == 0] = NA
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
    parameters_show$r2_IC = as.numeric(prettyNum(parameters_show$r2_IC, digits = 3))
    parameters_show$pval_IC = as.numeric(prettyNum(parameters_show$pval_IC, digits = 3))
    parameters_show$flat_fit_IC = as.numeric(prettyNum(parameters_show$flat_fit_IC, digits = 3))
    
    values$parameter_table_show <- parameters_show
    #=========================
    test_ref_show <<- values$parameter_table_show
    print("finishedParams")
    
    if (length(values$inData)>0) {
print(1)
      
      values$showanalyses<-1
      if (length(input$groupingVars)>0) {
          values$showanalyses_multi<-1
      } else {
          values$showanalyses_multi<-0
      }
      
print(2)      
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

print(3)      
      output$drc2<-drawDRC(input, values)
print(4)      
      output$ui <- renderUI({
        n <- length(input$groupingVars)
        if (n>0) {
          code_output_list <- lapply(1:n, function(i) {
            # name the input choice based on the grouping variable names, prefix with "param_" to aviod conflict
            codeOutput <- paste("param_", input$groupingVars[i], sep="")
            verbatimTextOutput(codeOutput)
            drc_choices = sort(unique(subset(values$GR_table,select=c(input$groupingVars[i]))[,1])[[1]])
            selectizeInput(
              codeOutput, input$groupingVars[i], choices = drc_choices, multiple = TRUE, selected = drc_choices[1]
            )
          })
        } else code_output_list <- list()
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, code_output_list)
      })
print(5)
      
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
        #try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
        box = drawBox(input, values)
        if(!is.null(box)) {
          box
        } else {stop()}
      })
      observeEvent(input$plot_scatter, {
        output$plotlyScatter1 <- renderPlotly({
          #try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
          plot1 = isolate(drawScatter(input, values))
        })
      })
    }
  })
  
  observeEvent(input$analyzeButton, {
      outVar <- reactive({
        vars <- all.vars(parse(text=as.character(input$pick_box_x)), functions = FALSE, unique = TRUE)
        return(vars)
      })
      
      observeEvent(input$factorA, {
        picks = sort(input$pick_box_factors)
        picks1 = setdiff(picks, input$factorA)
        updateSelectizeInput(
          session, 'factorB',
          choices = picks1,
          selected = input$factorB
        )
      })
      
      observeEvent(input$factorB, {
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
            #choices = unique(values$GR_table[[input$pick_box_x]]),
            choices = picks,
            selected = NULL
          )
          updateSelectizeInput(
            session, 'factorB',
            #choices = unique(values$GR_table[[input$pick_box_x]]),
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
            selectizeInput('pick_box_factors', 'Select factors of grouping variable', choices = c(), multiple = T),
            selectizeInput('factorA', 'Choose factors with which to perform a one-sided Wilcoxon rank-sum test', choices = c(), multiple = T),
            selectizeInput('factorB', '', choices = c(), multiple = T),
            textOutput("wilcox")
          )
        }
      })
      
      observeEvent(c(input$factorA, input$factorB, input$pick_box_y), {
        wil_data = values$parameter_table
        if(!is.null(input$factorA) & !is.null(input$factorB)) {
          rowsA = wil_data[[input$pick_box_x]] %in% input$factorA
          rowsB = wil_data[[input$pick_box_x]] %in% input$factorB
          wil_dataA = wil_data[rowsA,input$pick_box_y]
          wil_dataB = wil_data[rowsB,input$pick_box_y]
          print(head(wil_dataA))
          print(head(wil_dataB))
          wil = wilcox.test(x = wil_dataA, y = wil_dataB, alternative = "less")
          values$wilcox = prettyNum(wil$p.value, digits = 2)
          output$wilcox = renderText({
            paste("P-value:", prettyNum(wil$p.value, digits = 2))
          })
        } else {
          output$wilcox = renderText({
            paste("P-value: ")
          })
          values$wilcox = NULL
        }
      })
      
#==== Clear scatterplot on "analyze" =========
      
      observeEvent(input$analyzeButton, {
        output$plotlyScatter1 <- renderPlotly({
          parameter_choice = input$pick_parameter
          if(parameter_choice == 'GR50') {
            parameter_choice = 'log10(GR50)'
          } else if(parameter_choice == 'h_GR') {
            parameter_choice = 'log2(h_GR)'
          }
          padding = 0.05
          scatter_values = values$parameter_table[,parameter_choice]
          finite_values = which(is.finite(scatter_values))
          scatter_values = scatter_values[finite_values]
          x_min = min(scatter_values, na.rm = T)
          x_max = max(scatter_values, na.rm = T)
          y_min = min(scatter_values, na.rm = T)
          y_max = max(scatter_values, na.rm = T)
          all_max = max(abs(c(x_max, y_max, x_min, y_min)), na.rm = T)
          all_range = 2*all_max
          all_max = all_max + padding*all_range
          all_min = -all_max
          # plug in a filler data frame
          p = ggplot(data = mtcars, aes(x = mpg, y = wt)) + geom_abline(slope = 1, intercept = 0, size = .25) + scale_x_continuous(limits = c(all_min, all_max)) + scale_y_continuous(limits = c(all_min, all_max)) + coord_fixed() + xlab('') + ylab('') + ggtitle('') + geom_blank()
          
          df_full <<- NULL
          print(3)
          #try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
          ggplotly(p)
          print(4)
          layout(p, hovermode = FALSE)
        })
      })
      
#===== Clear button ========
      observeEvent(input$clear, {
        output$plotlyScatter1 <- renderPlotly({
          parameter_choice = input$pick_parameter
          if(parameter_choice == 'GR50') {
            parameter_choice = 'log10(GR50)'
          }
          if(parameter_choice == 'h_GR') {
            parameter_choice = 'log2(h_GR)'
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
          print(all_min)
          print(all_max)
          p = ggplot(data = df_sub, aes(x = get(paste0(parameter_choice,'.x'), envir = as.environment(df_sub)), y = get(paste0(parameter_choice,'.y'), envir = as.environment(df_sub)), colour = cross.x, text = merge_text)) + geom_abline(slope = 1, intercept = 0, size = .25) + scale_x_continuous(limits = c(all_min, all_max)) + scale_y_continuous(limits = c(all_min, all_max)) + coord_fixed() + xlab('') + ylab('') + ggtitle('') + geom_blank()
          df_full <<- NULL
          #try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
          ggplotly(p)
          layout(p, hovermode = FALSE)
        })
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
