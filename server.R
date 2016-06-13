library(shiny)
library(shinyjs)
library(plotly)
library(ggplot2)
library(readr)
library(drc)
library(GRmetrics)

source('functions/drawPopup.R')
source('functions/drawDRC.R', local = T)
source('functions/extractGridData.R')
source('functions/drawScatter.R', local = T)
#source('functions/calculate_GR.R')
#source('functions/logistic_fit_GR.R')
source('functions/parseLabel.R')
trim_mean = function(x, percent) {
  x = x[!is.na(x)]
  n = length(x)
  k = n*(percent/100)/2
  # round down if k is half an integer
  if(round(k) != k & round(k*2) == k*2) {
    lo = floor(k) + 1
    hi = n - lo + 1
  } else {
    lo = round(k) + 1
    hi = n - lo + 1
  }
  x = sort(x)[lo:hi]
  return(mean(x))
}

shinyServer(function(input, output,session) {
  
  values <- reactiveValues(inData=NULL, case = "A", GR_table = NULL, GR_table_show = NULL, parameter_table = NULL, parameter_table_show = NULL, df_scatter = NULL, showanalyses=0, showdata=0, showanalyses_multi=0, data_dl = NULL)
  isolate(values$inData)

  observeEvent(input$loadExample, {
    values$data_dl = 'example'
    output$input_error = renderText("")
    session$sendCustomMessage(type = "resetFileInputHandler", "uploadData")
    values$inData <- read_tsv('toy_example_input1.tsv')
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
      values$inData <- read_tsv(inFile$datapath)
      # Get rid of blank rows at the end of a file
      values$inData <- values$inData[rowSums(is.na(values$inData)) != ncol(values$inData),]
    } else if(input$sep == ',') {
      values$inData <- read_csv(inFile$datapath)
      # Get rid of blank rows at the end of a file
      values$inData <- values$inData[rowSums(is.na(values$inData)) != ncol(values$inData),]
    }
    output$input_table <- DT::renderDataTable(DT::datatable({
      x<-values$inData
      data.frame(x)
    }, rownames= FALSE))
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
    } else if(length(intersect(colnames(values$inData), c('concentration', 'cell_count'))) == 2) {
        print('Input Case C')
        delete_cols = which(colnames(values$inData) %in% c('concentration', 'cell_count'))
        keys = colnames(values$inData)[-delete_cols]
        time0 = values$inData[values$inData$time == 0, c(keys, 'cell_count')]
        ctrl = values$inData[values$inData$concentration == 0 & values$inData$time > 0, c(keys, 'cell_count')]
        data = values$inData[values$inData$concentration != 0 & values$inData$time > 0, ]
        time0_keys = NULL
        ctrl_keys = NULL
        for(i in 1:length(keys)) {
          time0_keys[i] = length(intersect(time0[[ keys[i] ]], data[[ keys[i] ]])) > 0
          ctrl_keys[i] = length(intersect(ctrl[[ keys[i] ]], data[[ keys[i] ]])) > 0
        }
        ctrl_keys = keys[ctrl_keys]
        time0_keys = keys[time0_keys]
        
        temp = ctrl[, ctrl_keys]
        ctrl$key = apply(temp, 1, function(x) paste(x, collapse = ' '))
        
        temp = time0[, time0_keys]
        time0$key = apply(temp, 1, function(x) paste(x, collapse = ' '))
        
        temp = data[, ctrl_keys]
        data$key_ctrl = apply(temp, 1, function(x) paste(x, collapse = ' '))
        
        temp = data[, time0_keys]
        data$key_time0 = apply(temp, 1, function(x) paste(x, collapse = ' '))
        
        data$cell_count__ctrl = NA
        data$cell_count__time0 = NA
        
        for(key in unique(ctrl$key)) {
          trimmed_mean = trim_mean(ctrl[ctrl$key == key,]$cell_count, 50)
          data[data$key_ctrl == key, 'cell_count__ctrl'] = trimmed_mean
        }
        
        for(key in unique(time0$key)) {
          trimmed_mean = trim_mean(time0[time0$key == key,]$cell_count, 50)
          data[data$key_time0 == key, 'cell_count__time0'] = trimmed_mean
        }
        
        delete_cols = which(colnames(data) %in% c('key_ctrl', 'key_time0'))
        data = data[, -delete_cols]
        
        row.names(data) = 1:dim(data)[1]
        values$inData = data
        delete_cols = which(colnames(values$inData) %in% c('concentration', 'cell_count', 'cell_count__ctrl', 'cell_count__time0'))
        updateSelectizeInput(
          session, 'groupingVars',
          choices = colnames(values$inData)[-delete_cols],
          selected = colnames(values$inData)[-delete_cols],
          options = c()
        )
        values$case = "A"
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
        write.table(data_output, file = filename, quote = F, sep = '\t', row.names = F, col.names = T)
      } else if(input$download_type == "csv") {
        write.table(data_output, file = filename, quote = F, sep = ',', row.names = F, col.names = T)
      }
      
    }
    #,
    #contentType = paste('text/', input$download_type, sep = "")
  )

#========== Download buttons for DRC plots =======
  output$downloadDRC = downloadHandler(
      filename = function() {
        if(input$drcImageType == '.eps') {
          if(!is.null(input$uploadData)) {
            return(paste(sub("^(.*)[.].*", "\\1", input$uploadData), "_GR_DRC.eps", sep=''))
          } else {
            return("example_GR_DRC.eps")
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
        if(input$drcImageType == '.eps') {
          ggsave(filename = filename, plot = plotDRC, device = "eps")
        } else {
          ggsave(filename = filename, plot = plotDRC, device = "tiff", units = "in", width = 7, height = 7, dpi = 300)
        }
      }
  )

#========== Download button for scatterplot images =======
  output$downloadScatter = downloadHandler(
    filename = function() {
      if(input$scatterImageType == '.eps') {
        if(!is.null(input$uploadData)) {
          return(paste(sub("^(.*)[.].*", "\\1", input$uploadData), "_GR_scatter.eps", sep=''))
        } else {
          return("example_GR_scatter.eps")
        }
      } else {
        if(!is.null(input$uploadData)) {
          return(paste(sub("^(.*)[.].*", "\\1", input$uploadData), "_GR_scatter.tiff", sep=''))
        } else {
          return("example_GR_scatter.tiff")
        }
      }
    },
    content = function(filename) {
      if(input$scatterImageType == '.eps') {
        ggsave(filename = filename, plot = plotScatter, device = "eps")
      } else {
        ggsave(filename = filename, plot = plotScatter, device = "tiff", units = "in", width = 7, height = 7, dpi = 300)
      }
    }
  )

#============ Plotly popups =====================
  observeEvent(input$'dose-response-grid-main', {
    if (input$'dose-response-grid-main' != '') {
      q = parseLabel(input, values)
      output$graphPopupPlot <- renderPlotly(q)
      toggleModal(session,"graphPopup")
    }
  })
  
  # Make scatterplot reactive to "pick_parameter" after first plot.
  observeEvent(input$pick_parameter, {
    if(input$plot_scatter > 0) {
      output$plotlyScatter1 <- renderPlotly({
        try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
        plot1 = isolate(drawScatter(input, values))
      })
    }
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
    
    tables <- GRfit(values$inData, groupingColumns, GRtable = 'both', force = input$force, cap = input$cap, case = values$case)
    values$GR_table <- tables[[1]]
    #values$GR_table <- calculate_GR(values$inData,groupingColumns)
    values$GR_table_show <- values$GR_table
    values$GR_table_show$GR <- as.numeric(prettyNum(values$GR_table_show$GR, digits = 3))
    values$GR_table_show$log10_concentration <- as.numeric(prettyNum(values$GR_table_show$log10_concentration, digits = 3))
    test_gr <<- values$GR_table
    print("finishedGR")
    #temp_parameter_table = logistic_fit_GR(values$GR_table,groupingColumns)
    #values$parameter_table <- temp_parameter_table[[1]]
    
    values$parameter_table <- tables[[2]]
    # log10(EC50) needed for passing
    #values$parameter_table$GEC50[values$parameter_table$GEC50 == 0] = NA
    values$parameter_table$GR50[is.infinite(values$parameter_table$GR50)] = NA
    values$parameter_table$Hill[values$parameter_table$Hill == 0.01] = NA
    values$parameter_table$`log10(GR50)` = log10(values$parameter_table$GR50)
    values$parameter_table$`log2(Hill)` = log2(values$parameter_table$Hill)
    #values$parameter_table$`log10(EC50)` = log10(values$parameter_table$GEC50)
    
    test_ref <<- values$parameter_table
    #values$parameter_table_show <- temp_parameter_table[[2]]
    parameters_show <- tables[[2]]
    parameters_show$GR50 = as.numeric(prettyNum(parameters_show$GR50, digits = 3))
    parameters_show$GRmax = as.numeric(prettyNum(parameters_show$GRmax, digits = 3))
    parameters_show$GR_AOC = as.numeric(prettyNum(parameters_show$GR_AOC, digits = 3))
    parameters_show$GEC50 = as.numeric(prettyNum(parameters_show$GEC50, digits = 3))
    parameters_show$GRinf = as.numeric(prettyNum(parameters_show$GRinf, digits = 3))
    parameters_show$Hill = as.numeric(prettyNum(parameters_show$Hill, digits = 3))
    parameters_show$r2 = as.numeric(prettyNum(parameters_show$r2, digits = 3))
    parameters_show$pval = as.numeric(prettyNum(parameters_show$pval, digits = 3))
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
            drc_choices = sort(unique(subset(values$inData,select=c(input$groupingVars[i]))[,1])[[1]])
            # Get rid of "-" for Case C
            delete_choice = which(drc_choices == '-')
            if(length(delete_choice) > 0) {
              drc_choices = drc_choices[-delete_choice]
            }
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
      
      observeEvent(input$plot_scatter, {
        output$plotlyScatter1 <- renderPlotly({
          try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
          plot1 = isolate(drawScatter(input, values))
        })
      })
      
      
      
      output$scatter1.ui <- renderUI({
        plotlyOutput("plotlyScatter1", height = input$scatter_height)
      })

#==== Clear scatterplot on "analyze" =========
      
      observeEvent(input$analyzeButton, {
        output$plotlyScatter1 <- renderPlotly({
          parameter_choice = input$pick_parameter
          if(parameter_choice == 'GR50') {
            parameter_choice = 'log10(GR50)'
          } else if(parameter_choice == 'Hill') {
            parameter_choice = 'log2(Hill)'
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
          try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
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
          if(parameter_choice == 'Hill') {
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
          print(all_min)
          print(all_max)
          p = ggplot(data = df_sub, aes(x = get(paste0(parameter_choice,'.x'), envir = as.environment(df_sub)), y = get(paste0(parameter_choice,'.y'), envir = as.environment(df_sub)), colour = cross.x, text = merge_text)) + geom_abline(slope = 1, intercept = 0, size = .25) + scale_x_continuous(limits = c(all_min, all_max)) + scale_y_continuous(limits = c(all_min, all_max)) + coord_fixed() + xlab('') + ylab('') + ggtitle('') + geom_blank()
          df_full <<- NULL
          try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
          ggplotly(p)
          layout(p, hovermode = FALSE)
        })
      })
      
      updateSelectizeInput(session, 'choiceVar', choices = input$groupingVars, server = TRUE, selected=input$groupingVars[1])
      if (length(input$groupingVars)==1) {
         updateSelectizeInput(session, 'xgroupingVars', choices = input$groupingVars, server = TRUE, selected=input$groupingVars[1])
      } else {
         updateSelectizeInput(session, 'xgroupingVars', choices = input$groupingVars, server = TRUE, selected=input$groupingVars[2])
      }
      
      observeEvent(input$plot_gr50grid, {
        output$'dose-response-grid-main' <- renderLiDoseResponseGrid(
          input="",
          xmin = min(log10(values$inData$concentration), na.rm = T),
          xmax = max(log10(values$inData$concentration), na.rm = T),
	  factors=c(paste(isolate(input$xgroupingVars),collapse = ' '), isolate(input$choiceVar)),
	  toggle=0,
          {
            input$plot_gr50grid
            isolate(extractGridData(input, output, values$parameter_table, isolate(input$choiceVar), isolate(input$xgroupingVars)))
          }
        )
        print('test')
        print(input$xgroupingVars)
        print(input$choiceVar)
        print(input$plot_gr50grid)
      })
      
    }
  })
#================================================
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    graphics.off()
    print('devices off')
  })
})
