library(shiny)
library(shinyjs)
library(plotly)
library(ggplot2)
library(readr)
library(drc)
library(GRmetrics)
library(magrittr)
library(S4Vectors)
library(stringr)
library(DT)
library(formattable)
library(plyr)
library(shinycssloaders)

source('functions/drawPopup.R')
#source('functions/drawDRC.R', local = T)
source('functions/extractGridData.R')
source('functions/drawScatter.R', local = T)
source('functions/drawBox.R')
source('functions/parseLabel.R')
source('functions/check_col_names.R')

zipped_csv <- function(df_list, zippedfile, filenames, stamp) {
  dir = tempdir()
  mkdir = paste0("mkdir ", dir, "/", stamp)
  system(mkdir)
  len = length(df_list)
  for(i in 1:len) {
    # filename in temp directory 
    assign(paste0("temp",i), paste0(dir, "/", stamp, "/", filenames[i], ".csv"))
    # write temp csv
    write_csv(df_list[[i]], path=get(paste0("temp",i)))
  }
  # zip temp csv
  print(dir)
  print(filenames)
  zip(zippedfile, paste0(dir,"/", stamp, "/", filenames, ".csv"), flags = "-j" )
  # delete temp csv
  for(i in 1:len) {
    unlink( paste0("temp",i) )
  }
}

about.modal.js = "$('.ui.small.modal')
.modal({
    blurring: true
})
$('#about_modal').modal('show')
;"
contact.modal.js = "$('.ui.mini.modal')
.modal({
    blurring: true
})
$('#contact_modal').modal('show')
;"
import.modal.js = "$('.ui.small.modal')
.modal({
    blurring: true
})
$('#import_modal').modal('show')
;"

example.modal.js = "$('.ui.mini.modal')
.modal({
    blurring: true
})
$('#example_modal').modal('show')
;"

start.modal.js = "$('.ui.mini.modal')
.modal({
    blurring: true
})
$('#start_modal').modal('show')
;"

instructions.modal.js = "$('.ui.small.modal')
.modal({
    blurring: true
})
$('#instructions_modal').modal('show')
;"

download_plot_drc.modal.js = "$('.ui.small.modal')
.modal({
    blurring: true
})
$('#download_plot_drc_modal').modal('show')
;"

tab.js = "$('.menu .item')
  .tab()
;"



shinyServer(function(input, output,session) {
  runjs(tab.js)
  # initialize variables for saving various user inputs, parameters, etc.
  values <- reactiveValues(inData=NULL, GR_table = NULL, GR_table_show = NULL, 
                           parameter_table = NULL, parameter_table_show = NULL, 
                           df_scatter = NULL, showanalyses=0, showdata=0, 
                           showanalyses_multi=0, data_dl = NULL, wilcox = NULL,
                           clearScatter = F, separator = NULL, div_rate = NULL,
                           init_count = logical(0), input_case = NULL,
                           cell_lines = NULL, div_rate_test = NULL, check_fail = NULL,
                           current_data = NULL, current_table_button = NULL,
                           tables = NULL)
  
  
  observeEvent(input$import_button, {
    runjs(import.modal.js)
  })
  observeEvent(input$example_button, {
    runjs(example.modal.js)
  })
  observeEvent(input$instructions_button, {
    runjs(instructions.modal.js)
  })
  observeEvent(c(input$loadExample, input$loadExampleB), {
    runjs("$('#example_modal').modal('hide')")
  })
  observeEvent(input$download_plot_drc.modal_button, {
    runjs(download_plot_drc.modal.js)
  })
  observeEvent(input$about, {
    runjs(about.modal.js)
  })
  observeEvent(input$start_button, {
    runjs(start.modal.js)
  })
  observeEvent(input$contact, {
    runjs(contact.modal.js)
  })
  
  ### initialize main data table and make sure it updates when hidden
  output$current_table = renderDataTable({ values$inData })
  outputOptions(output, "current_table", suspendWhenHidden = FALSE)
  ### initialize main plot and make sure it updates when hidden
  #output$drc2<- renderPlot(NULL)
  #outputOptions(output, "drc2", suspendWhenHidden = FALSE)
  ### start loader on analyze button first
  observeEvent(input$analyzeButton, {
    shinyjs::addClass(id = "analyze_loader", "active")
  }, ignoreInit = T, ignoreNULL = T, priority = 1000)
  ### then start loaders on plots
  shinyjs::onclick("analyzeButton", {
    shinyjs::click(id = "third_top")
    shinyjs::removeClass(id = "analyze_loader", "active")
  })
  ### on click of data table tab, "click" current data table button to make sure it shows up
  shinyjs::onclick("output_tables_top", {
    print("output_tables_top")
    print(values$current_table_button)
    shinyjs::click(values$current_table_button)
  })
  ### on click of drc plot tab, "click" plot button to make sure it shows up
  shinyjs::onclick("third_top", {
    print("third_top")
    #shinyjs::click(values$current_table_button)
    shinyjs::show("ui")
    shinyjs::show("plot.ui")
  })
  shinyjs::onclick("plot_options_button", {
    shinyjs::toggle(id = "plot_options", animType = "slide")
  })
  ### set which table to show in data table tab
  shinyjs::click(id = "input_table_button")
  
  observeEvent(values$inData, {
    shinyjs::click(id = "input_top")
    output$input_table = renderDataTable({ datatable(values$inData,  extensions = c('Buttons', 'FixedHeader'),
                                         filter = 'top',
                                         rownames = F, options = list(
                                           dom = 'lBfrtip',
                                           buttons = c('copy', 'csv', 'excel', 'colvis'),
                                           initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
                                             "}"),
                                           searchHighlight = TRUE,
                                           fixedHeader = TRUE,
                                           autoWidth = TRUE))
                                         
  }, server = FALSE)
    outputOptions(output, "input_table", suspendWhenHidden = FALSE)
  }, ignoreInit = T, ignoreNULL = T)
  
  observeEvent(input$input_table_button, {
    values$current_table_button = "input_table_button"
    values$current_data = values$inData
    ## emphasize the selected data table button
    shinyjs::addClass(id = "input_table_button", class = "green")
    shinyjs::removeClass(id = "gr_table_button", class = "green")
    shinyjs::removeClass(id = "parameter_table_button", class = "green")
  })
  observeEvent(input$gr_table_button, {
    values$current_table_button = "gr_table_button"
    values$current_data = values$GR_table_show
    ## emphasize the selected data table button
    shinyjs::removeClass(id = "input_table_button", class = "green")
    shinyjs::addClass(id = "gr_table_button", class = "green")
    shinyjs::removeClass(id = "parameter_table_button", class = "green")
  })
  observeEvent(input$parameter_table_button, {
    values$current_table_button = "parameter_table_button"
    values$current_data = values$parameter_table_show
    ## emphasize the selected data table button
    shinyjs::removeClass(id = "input_table_button", class = "green")
    shinyjs::removeClass(id = "gr_table_button", class = "green")
    shinyjs::addClass(id = "parameter_table_button", class = "green")
  })
  observeEvent(values$current_data, {
    output$current_table = renderDataTable({ datatable(values$current_data,  extensions = c('Buttons', 'FixedHeader'),
                   filter = 'top',
                   rownames = F, options = list(
                     dom = 'lBfrtip',
                     buttons = c('copy', 'csv', 'excel', 'colvis'),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
                       "}"),
                     searchHighlight = TRUE,
                     fixedHeader = TRUE,
                     autoWidth = TRUE))
    }, server = FALSE)
    outputOptions(output, "current_table", suspendWhenHidden = FALSE)
  }, ignoreNULL = F)

  
  observeEvent(c(input$nplots, values$tables), {
    output$plots_grid <- renderUI({
      print(input$drc2_color)
      print(input$drc2_facet)
      plots = try(GRdrawDRC(fitData = values$tables, metric = input$drc2_metric, curves = input$drc2_curves,
                        points = input$drc2_points, xrug = input$drc2_xrug, yrug = input$drc2_yrug,
                        facet = input$drc2_facet, bars = input$drc2_bars,
                        color = input$drc2_color, plot_type = input$drc2_plot_type,
                        output_type = "separate"))
      for (i in 1:length(plots)) {
        local({
          n <- i # Make local variable
          plotname <- paste("plot", n , sep="")
          output[[plotname]] <- renderPlotly({
            plots[[n]]
          })
        })
      }
      #col.width <- round(16/input$ncol) # Calculate bootstrap column width
      col.width <- 300
      #n.col <- ceiling(length(plots)/input$ncol) # calculate number of rows
      #n.row = input$nrow
      #n.col = input$ncol
      cnter <<- 0 # Counter variable
      #n_pages =  ceiling(n_total_plots/input$nplots)
      
      # Create row with columns
      #rows  <- lapply(1:n.row,function(row.num){
        cols  <- lapply(1:min(as.numeric(input$nplots), length(plots) ), function(i) {
          cnter    <<- cnter + 1
          plotname <- paste("plot", cnter, sep="")
          div(class = "ui column", style = paste0("flex: 0 0 ",col.width,"px;") , 
              plotlyOutput(plotname,width = col.width, height = col.width) %>% withSpinner(type = 3, color = "#009999", color.background = "#ffffff")
              # tags$img(src = "images/GRcalculator-logo.jpg", width = paste0(col.width, "px"), height = paste0(col.width, "px")),
              #tags$p(paste0("plot", i))
              )
        })
        do.call(tagList, cols)
    })
    output$plots_grid_pages = renderUI({
      n_pages = 10
      div(class = "ui pagination menu", id = "drc_pages",
                  tags$a(class = "item", type = "firstItem", "«"),
                  tags$a(class = "item", type = "prevItem", "⟨"),
                  tags$a(class = "active item", 1, id = "drc_page1"),
                  lapply(2:n_pages, function(x) tags$a(class = "item", x, 
                                                       id = paste0("drc_page", x)) ),
                  tags$a(class = "item", type = "nextItem", "⟩"),
                  tags$a(class = "item", type = "lastItem", "»")
      )
    })
    # if(input$drc2_plot_type == "interactive") {
    #   output$plot.ui <- renderUI({
    #     plotlyOutput("drc2_plotly", height = input$height, width = "800px")
    #   })
    # } else if(input$drc2_plot_type == "static") {
    #   output$plot.ui <- renderUI({
    #     plotOutput("drc2", height = input$height, width = "800px")
    #   })
    # }
  }, ignoreInit = T, ignoreNULL = T)
  
  # Code to show/hide descriptions of input cases (division rate vs. initial cell counts)
  observeEvent(values$div_rate, {
    if(values$div_rate) {
      showElement(id = "div_rate_desc")
      hideElement(id = "init_count_desc")
    }
  }, ignoreInit = T)
  observeEvent(values$init_count, {
    if(values$init_count) {
      showElement(id = "init_count_desc")
      hideElement(id = "div_rate_desc")
    }
  }, ignoreInit = T)
  
  # Code to show/hide descriptions of necessary columns for input
  observeEvent(c(values$input_case, values$div_rate, values$init_count), {
    if(!is.null(values$input_case) & !is.null(values$div_rate)) {
      div_names = c("caseA_initial_desc", "caseA_div_desc", "caseB_initial_desc", "caseB_div_desc")
      div_loc = NULL
      if(values$input_case == "A" & !values$div_rate) { div_loc = 1 }
      if(values$input_case == "A" & values$div_rate) { div_loc = 2 }
      if(values$input_case == "B" & !values$div_rate) { div_loc = 3 }
      if(values$input_case == "B" & values$div_rate) { div_loc = 4 }
      # Hide descriptions
      for(i in 1:4) {
        args = list(id = div_names[i])
        if(i != div_loc) {
          do.call(what = "hideElement", args = args)
        }
      }
      # Show relevant description
      args = list(id = div_names[div_loc])
      do.call(what = "showElement", args = args)
    }
  }, ignoreInit = T)
  # observeEvent(values$input_case, {
  #   div_names = c("caseA_initial_desc", "caseA_div_desc", "caseB_initial_desc", "caseB_div_desc")
  #   if(values$input_case == "A" & !values$div_rate) { div_loc = 1 }
  #   if(values$input_case == "A" & values$div_rate) { div_loc = 2 }
  #   if(values$input_case == "C" & !values$div_rate) { div_loc = 3 }
  #   if(values$input_case == "C" & !values$div_rate) { div_loc = 4 }
  #   # show correct description
  #   args = list(id = div_names[div_loc])
  #   do.call(what = "showElement", args = args)
  # }, ignoreInit = T, priority = -1)
  
  # Code to show caseA/caseB choice buttons
  observeEvent(c(input$divisionRate, input$initialCellCount), {
    showElement(id = "case_buttons", anim = T, animType = "fade")
  }, ignoreInit = T)
  # Code to show csv/tsv choice buttons
  observeEvent(c(input$caseA, input$caseB), {
    showElement(id = "comma_tab_buttons", anim = T, animType = "fade")
  }, ignoreInit = T)

  # Code to make import dialog csv/tsv buttons work like radiobuttons
  observeEvent(input$comma_input, {
    addClass(id = "comma_input", class = "active")
    removeClass(id = "tab_input", class = "active")
    values$separator = ","
    showElement(id = "upload_button", anim = T, animType = "fade")
    showElement(id = "advanced_input", anim = T, animType = "fade")
  }, ignoreInit = T)
  observeEvent(input$tab_input, {
    addClass(id = "tab_input", class = "active")
    removeClass(id = "comma_input", class = "active")
    values$separator = "\t"
    showElement(id = "upload_button", anim = T, animType = "fade")
    showElement(id = "advanced_input", anim = T, animType = "fade")
  }, ignoreInit = T)
  # Code to make import dialog initial cell count/division rate buttons work like radiobuttons
  observeEvent(input$initialCellCount, {
    addClass(id = "initialCellCount", class = "active")
    removeClass(id = "divisionRate", class = "active")
    values$div_rate = F
    values$init_count = T
  }, ignoreInit = T)
  observeEvent(input$divisionRate, {
    addClass(id = "divisionRate", class = "active")
    removeClass(id = "initialCellCount", class = "active")
    values$div_rate = T
    values$init_count = F
  }, ignoreInit = T)
  # Code to make import dialog caseA/caseB buttons work like radiobuttons
  observeEvent(input$caseA, {
    addClass(id = "caseA", class = "active")
    removeClass(id = "caseB", class = "active")
    values$input_case = "A"
  }, ignoreInit = T)
  observeEvent(input$caseB, {
    addClass(id = "caseB", class = "active")
    removeClass(id = "caseA", class = "active")
    values$input_case = "B"
  }, ignoreInit = T)

  # Code for closing input dialog when data is uploaded
# observeEvent(c(input$uploadData,input$fetchURLData), {
#   #observeEvent(values$check_fail, {
#     toggleModal(session, 'import_button', toggle = "close")
#   }, ignoreInit = T)
  observeEvent(c(input$loadExample, input$loadExampleB),{
    toggleModal(session, 'example_modal', toggle = "close")
  }, ignoreInit = T)
  observeEvent(input$div_rate_input,{
    toggleModal(session, 'importDialog_div', toggle = "close")
  }, ignoreInit = T)
  
  # Open division rate input dialog if necessary
#  observeEvent(c(input$uploadData, input$fetchURLData), {
  observeEvent(values$check_fail, {
    toggleModal(session, 'import_button', toggle = "close")
    if(!values$check_fail) {
      if(values$div_rate) {
        toggleModal(session, 'importDialog_div', toggle = "open")
      }
    } else {
      toggleModal(session, 'import_fail', toggle = "open")
    }
  }, ignoreInit = T)

  # Code for loading example data for input Case A
  observeEvent(input$loadExample, {
    print("load example a")
    values$input_case = "A"
    values$data_dl = 'example'
    output$input_error = renderText("")
    #session$sendCustomMessage(type = "resetFileInputHandler", "uploadData")
    values$inData <- read_tsv('resources/toy_example_input1_edited.tsv')
    values$GR_table_show = NULL
    values$parameter_table_show = NULL
    values$showanalyses=0
    values$showanalyses_multi=0
    #if(values$showdata) updateTabsetPanel(session,"tabs",selected="tab-data")
    # output$input_table <- DT::renderDataTable(datatable(values$inData, rownames = F))
    # print("load example a done")
  })
  
  # Code for loading example data for input Case B
  observeEvent(input$loadExampleB, {
    values$input_case = "B"
    values$data_dl = 'example'
    output$input_error = renderText("")
    #session$sendCustomMessage(type = "resetFileInputHandler", "uploadData")
    values$inData <- read_tsv('resources/toy_example_input4_edited.tsv')
    values$GR_table_show = NULL
    values$parameter_table_show = NULL
    values$showanalyses=0
    values$showanalyses_multi=0
    if(values$showdata) updateTabsetPanel(session,"tabs",selected="tab-data")
    # output$input_table <- renderDataTable(datatable(values$inData, rownames = F))
  })
  
  # Code for loading data from file
  observeEvent(input$uploadData, {
    values$check_fail = NULL
    print('data upload')
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
    # output$input_table <- renderDataTable(datatable(values$inData, rownames = F))
  })
  
  # Code for loading data from URL
  observeEvent(input$fetchURLData, {
    values$check_fail = NULL
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
      # output$input_table <- renderDataTable(datatable(values$inData, rownames = F))
    }
  })
  
  # Code for division rate case, showing cell lines
  observeEvent(c(input$uploadData,input$fetchURLData), {
    values$cell_lines = sort(unique(values$inData$cell_line))
    df = data.frame(cell_lines = values$cell_lines)
    print(df)
    output$cell_lines = renderTable(df)
  }, ignoreInit = T)
  
  # Code for adding division rates to input data
  observeEvent(input$div_rate_input, {
    # parse division rate input from text box
    div_rates = as.numeric(unlist(strsplit(input$div_rate, "\n")))
    values$div_rate_test = ifelse(sum(is.na(div_rates)) == 0, T, F)
    values$inData$division_time = as.numeric(mapvalues(values$inData$cell_line, values$cell_lines, div_rates))
    values$inData$treatment_duration = as.numeric(input$treatment_duration)
    print(head(values$inData))
    # output$input_table <- renderDataTable(datatable(values$inData, rownames = F))
  })
  
  # Code for showing/hiding advanced analysis options
  observeEvent(values$inData, {
    showElement(id = "advanced_analysis", anim = T, animType = "fade")
  })

  # Code for showing/hiding tabs
  observe({
    toggle(condition = values$showdata, selector = "#tabs li a[data-value=tab-data]")
    toggle(condition = values$showanalyses, selector = "#tabs li a[data-value=tab-drc]")
    toggle(condition = values$showanalyses_multi, selector = "#tabs li a[data-value=tab-drc-grid]")
    toggle(condition = values$showanalyses_multi, selector = "#tabs li a[data-value=tab-gr-metric]")
  })
  
  observe({
    if(values$showdata) updateTabsetPanel(session,"tabs",selected="tab-data")
  })
  
  observeEvent(values$inData, {
    values$showdata = 1
  })
  
  # Code for checking input and providing feedback to user
  observeEvent(c(input$uploadData,input$fetchURLData), {
    # Check input table column names for slight misspellings
    caseA_cols = c("cell_line","treatment", "concentration", "cell_count", "cell_count__ctrl", "cell_count__time0")
    caseB_cols = c("cell_line","treatment", "concentration", "cell_count", "time")
    if(values$input_case == "A" & !values$div_rate) {cols = caseA_cols}
    if(values$input_case == "A" & values$div_rate) {cols = caseA_cols[1:5]}
    if(values$input_case == "B") {cols = caseB_cols}
    
    print(cols)
    incols = colnames(values$inData)
    print(incols)
    # run check_col_names function to search for slightly misnamed columns
    # if all of the necessary column names are not in the input data and
    # there are extra columns
    if(length(intersect(cols, incols)) != length(cols) & length(incols) >= length(cols)) {
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
    print('df')
    print(df)
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
    check1_fail = F
    check2_fail = F
    if(sum(!df[,2]) > 0) {
      check1_fail = T
    } else {
      check1_fail = F
    }
    output$input_check = renderFormattable({
      formattable(df, formats, align = "l")
    })
    if(sum(!df2[,2]) > 0) {
      check2_fail = T
    } else {
      check2_fail = F
    }
    output$input_check2 = renderFormattable({
      formattable(df2, formats, align = "l")
    })
    # If either checks fail, take note
    if(sum(check1_fail, check2_fail) > 0) {
      values$check_fail = T
    } else {
      values$check_fail = F
    }
  }, ignoreInit = T)
  
  # Code to show/hide elements after data is uploaded
  # observeEvent(values$inData, {
  #   output$fileUploaded <- reactive(T)
  #   outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  # })
  
  # Code for updating grouping variable selection boxes
  observeEvent(values$inData, {
    caseA_params = c('concentration', 'cell_count', 'cell_count__ctrl')
    caseB_params = c('concentration', 'cell_count', 'time')
    time0_param = 'cell_count__time0'
    div_params = c('treatment_duration', 'division_time')
    if(values$input_case == "A") {
        delete_cols = which(colnames(values$inData) %in% c(caseA_params, time0_param, div_params))
        updateSelectizeInput(
          session, 'groupingVars',
          choices = colnames(values$inData)[-delete_cols],
          selected = colnames(values$inData)[-delete_cols],
          options = c()
        )
    } else {
      delete_cols = which(colnames(values$inData) %in% c(caseB_params, div_params))
      updateSelectizeInput(
        session, 'groupingVars',
        choices = colnames(values$inData)[-delete_cols],
        selected = colnames(values$inData)[-delete_cols],
        options = c()
      )
    }
  })
  
  # Define the table showing the input data
  # output$input_table <- renderDataTable(datatable(values$inData, rownames = F))
  
  # Change table shown from input to GR values to GR metrics based on radio buttons
  # observeEvent(input$pick_data, {
  #   if(input$pick_data == 1) {
  #     output$input_table <- renderDataTable(datatable(values$inData, rownames = F))
  #     }
  #   if(input$pick_data == 2) {
  #     output$input_table <- renderDataTable(datatable(values$GR_table_show, rownames = F))
  #   }
  #   if(input$pick_data == 3) {
  #     output$input_table <- renderDataTable(datatable(values$parameter_table_show, rownames = F))
  #     }
  # })
    
#========== Download button data tables =======
  output$downloadBind <- downloadHandler(
    filename = function() {
      return(paste0("GRdata_", format(Sys.time(), "%Y%m%d_%I%M%S"), 
                    ".zip", sep = ""))
    },
    content = function(filename) {
      files_all = list(values$inData,
                       values$GR_table_show,
                       values$parameter_table_show)
      # take only tables that exist
      drugs = NULL
      if(values$num_selected > 0) {
        files = files_all[1:values$num_selected]
        for(i in 1:3) {
          drugs = c(drugs, values[[paste0("selection.drug", i)]])
        }
      } else {
        files = NULL
        drugs = NULL
      }
      zipped_csv(files, filename, paste0("BindingData_", drugs), format(Sys.time(), "%Y%m%d_%I%M%S") )
    }, contentType = "application/zip"
  )
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     if(values$data_dl == 'input') {
  #       if(input$pick_data == 1) {
  #         return(paste(sub("^(.*)[.].*", "\\1", input$uploadData), ".", input$download_type, sep=''))
  #       } else if(input$pick_data == 2) {
  #         return(paste(sub("^(.*)[.].*", "\\1", input$uploadData), '_GRvalues', ".", input$download_type, sep=''))
  #       } else {
  #         return(paste(sub("^(.*)[.].*", "\\1", input$uploadData),'_FittedParameters', ".", input$download_type, sep = ''))
  #       }
  #     } else if(values$data_dl == 'example') {
  #       if(input$pick_data == 1) {
  #         return(paste("example.", input$download_type, sep = ""))
  #       } else if(input$pick_data == 2) {
  #         return(paste("example_GRvalues.", input$download_type, sep = ""))
  #       } else {
  #         return(paste("example_FittedParameters.", input$download_type, sep = ""))
  #       }
  #     }
  #   },
  #   content = function(filename) {
  #     if(input$pick_data == 1) {
  #       data_output = values$inData
  #     } else if(input$pick_data == 2) {
  #       data_output = values$GR_table_show
  #     } else {
  #       data_output = values$parameter_table_show
  #     }
  #     if(input$download_type == "tsv") {
  #       if(input$euro_out == T) {
  #         write.table(data_output, file = filename, quote = F, sep = '\t', row.names = F, col.names = T, dec = ',')
  #       } else {
  #         write.table(data_output, file = filename, quote = F, sep = '\t', row.names = F, col.names = T)
  #       }
  #     } else if(input$download_type == "csv") {
  #       if(input$euro_out == T) {
  #         write.table(data_output, file = filename, quote = F, sep = ';', row.names = F, col.names = T, dec = ',')
  #       } else {
  #         write.table(data_output, file = filename, quote = F, sep = ',', row.names = F, col.names = T)
  #       }
  #     }
  #     
  #   }
  # )

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
  
#=========== Scatterplot drawing/clearing code ===========
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
    
#================= analyzeButton ================================
  observeEvent(input$analyzeButton,{
    df_full <<- NULL
    all_inputs <- names(input)
    print(all_inputs)
    groupingColumns <<- input$groupingVars
    print("groupingColumns")
    print(groupingColumns)
    print("groupingColumns")
    
    values$tables <- try(GRfit(values$inData, groupingColumns, force = input$force, cap = input$cap, case = values$input_case))
    if(class(values$tables)!="try-error") {
      values$parameters_all = GRgetMetrics(values$tables)
      values$parameter_table = values$parameters_all$GR$sigmoid$normal
      values$parameter_table_show = values$parameter_table
      values$GR_table = GRgetValues(values$tables)
      values$GR_table_show = values$GR_table
      
      # values$parameter_table <- GRgetMetrics(tables)
      # values$GR_table <- GRgetValues(tables)
      # parameters_show <- GRgetMetrics(tables)
      # 
      # values$GR_table_show <- values$GR_table
      # values$GR_table_show$GRvalue <- as.numeric(prettyNum(values$GR_table_show$GRvalue, digits = 3))
      # values$GR_table_show$log10_concentration <- as.numeric(prettyNum(values$GR_table_show$log10_concentration, digits = 3))
      # values$GR_table_show$rel_cell_count <- as.numeric(prettyNum(values$GR_table_show$rel_cell_count, digits = 3))
      # test_gr <<- values$GR_table
      # print("finishedGR")
      # 
      # values$parameter_table$GR50[is.infinite(values$parameter_table$GR50)] = NA
      # values$parameter_table$h_GR[values$parameter_table$h_GR == 0.01] = NA
      # values$parameter_table$`log10(GR50)` = log10(values$parameter_table$GR50)
      # values$parameter_table$`log2(h_GR)` = log2(values$parameter_table$h_GR)
      # 
      # values$parameter_table$IC50[is.infinite(values$parameter_table$IC50)] = NA
      # values$parameter_table$h[values$parameter_table$h == 0.01] = NA
      # values$parameter_table$`log10(IC50)` = log10(values$parameter_table$IC50)
      # values$parameter_table$`log2(h)` = log2(values$parameter_table$h)
      # 
      # #values$parameter_table$`log10(EC50)` = log10(values$parameter_table$GEC50)
      # 
      # # For compatibility with shinyLi dose-response grid visualization
      # #values$parameter_table$Hill = values$parameter_table$h_GR
      # #values$parameter_table$`log2(Hill)` = log2(values$parameter_table$h_GR)
      # 
      # test_ref <<- values$parameter_table
      # #values$parameter_table_show <- temp_parameter_table[[2]]
      # 
      # parameters_show$GR50 = as.numeric(prettyNum(parameters_show$GR50, digits = 3))
      # parameters_show$GRmax = as.numeric(prettyNum(parameters_show$GRmax, digits = 3))
      # parameters_show$GR_AOC = as.numeric(prettyNum(parameters_show$GR_AOC, digits = 3))
      # parameters_show$GEC50 = as.numeric(prettyNum(parameters_show$GEC50, digits = 3))
      # parameters_show$GRinf = as.numeric(prettyNum(parameters_show$GRinf, digits = 3))
      # parameters_show$h_GR = as.numeric(prettyNum(parameters_show$h_GR, digits = 3))
      # parameters_show$r2_GR = as.numeric(prettyNum(parameters_show$r2_GR, digits = 3))
      # parameters_show$pval_GR = as.numeric(prettyNum(parameters_show$pval_GR, digits = 3))
      # parameters_show$flat_fit_GR = as.numeric(prettyNum(parameters_show$flat_fit_GR, digits = 3))
      # 
      # parameters_show$IC50 = as.numeric(prettyNum(parameters_show$IC50, digits = 3))
      # parameters_show$Emax = as.numeric(prettyNum(parameters_show$Emax, digits = 3))
      # parameters_show$AUC = as.numeric(prettyNum(parameters_show$AUC, digits = 3))
      # parameters_show$EC50 = as.numeric(prettyNum(parameters_show$EC50, digits = 3))
      # parameters_show$Einf = as.numeric(prettyNum(parameters_show$Einf, digits = 3))
      # parameters_show$h = as.numeric(prettyNum(parameters_show$h, digits = 3))
      # parameters_show$r2_rel_cell = as.numeric(prettyNum(parameters_show$r2_rel_cell, digits = 3))
      # parameters_show$pval_rel_cell = as.numeric(prettyNum(parameters_show$pval_rel_cell, digits = 3))
      # parameters_show$flat_fit_rel_cell = as.numeric(prettyNum(parameters_show$flat_fit_rel_cell, digits = 3))
      # 
      # values$parameter_table_show <- parameters_show
      # #=========================
      # test_ref_show <<- values$parameter_table_show
      # print("finishedParams")
    } else {
      # When the GRfit function fails for some reason
      err = attributes(tables)$condition
      output$input_error = renderText(paste("There was an error in the GR value calculation. Please check that your data is in the correct form.", "\n", err))
    }
    
    #=========== data loaded (start) ================
    if (length(values$inData)>0) {
      values$showanalyses<-1
      if (length(input$groupingVars)>0) {values$showanalyses_multi<-1
      } else {values$showanalyses_multi<-0}
      

      # observeEvent(input$drc2_plot_type, {
      #   if(input$drc2_plot_type == "interactive") {
      #     output$plot.ui <- renderUI({
      #       plotlyOutput("drc2_plotly", height = input$height, width = "800px")
      #     })
      #   } else if(input$drc2_plot_type == "static") {
      #     output$plot.ui <- renderUI({
      #       plotOutput("drc2", height = input$height, width = "800px")
      #     })
      #   }
      # })
      
      
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

      #output$drc2<-drawDRC(input, values)
      # observeEvent(c(input$drc2_metric, input$drc2_curves, input$drc2_points, input$drc2_bars,
      #                input$drc2_facet_row, input$drc2_facet_col, input$drc2_plot_type, input$drc2_xrug,
      #                input$drc2_yrug), {
        # values$drc2 = GRdrawDRC(fitData = tables, metric = input$drc2_metric, curves = input$drc2_curves,
        #                  points = input$drc2_points, xrug = input$drc2_xrug, yrug = input$drc2_yrug,
        #                  facet_row = input$drc2_facet_row, facet_col = input$drc2_facet_col)
        #drc2_plotly = ggplotly(drc2)
        #output$drc2<- renderPlot(drc2)
        # output$drc2<- renderPlot(
        #   GRdrawDRC(fitData = tables, metric = input$drc2_metric, curves = input$drc2_curves,
        #             points = input$drc2_points, xrug = input$drc2_xrug, yrug = input$drc2_yrug,
        #             facet = input$drc2_facet, bars = input$drc2_bars,
        #             color = input$drc2_color, plot_type = input$drc2_plot_type,
        #             output_type = "separate")
        #   )
        # 
        # output$drc2_plotly<- renderPlotly(ggplotly(
        #   GRdrawDRC(fitData = tables, metric = input$drc2_metric, curves = input$drc2_curves,
        #             points = input$drc2_points, xrug = input$drc2_xrug, yrug = input$drc2_yrug,
        #             facet = input$drc2_facet, bars = input$drc2_bars,
        #             color = input$drc2_color, plot_type = input$drc2_plot_type,
        #             output_type = "separate")
        #   ))
      # }, ignoreInit = T, ignoreNULL = T)
      
      
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
      
      
      observeEvent(input$drc2_metric, {
        if(input$drc2_metric == "GR") {
          updateSelectizeInput(session, 'drc2_xrug', choices = c("none", "GR50", "GEC50"))
          updateSelectizeInput(session, 'drc2_yrug', choices = c("none", "GRinf", "GRmax"))
        }
        if(input$drc2_metric == "rel_cell") {
          updateSelectizeInput(session, 'drc2_xrug', choices = c("none", "IC50", "EC50"))
          updateSelectizeInput(session, 'drc2_yrug', choices = c("none", "Einf", "Emax"))
        }
      })
    observeEvent(input$analyzeButton, {
      updateSelectizeInput(session, 'choiceVar', choices = input$groupingVars, server = TRUE, selected=input$groupingVars[1])
      if (length(input$groupingVars)==1) {
         updateSelectizeInput(session, 'xgroupingVars', choices = input$groupingVars, server = TRUE, selected=input$groupingVars[1])
      } else {
         updateSelectizeInput(session, 'xgroupingVars', choices = c("none", input$groupingVars), server = TRUE, selected=input$groupingVars[2])
      }
      ### update facets for main plot
      updateSelectizeInput(session, 'drc2_facet', choices = c("none", input$groupingVars), server = TRUE, selected=input$groupingVars[1])
      updateSelectizeInput(session, 'drc2_color', choices = c("experiment", input$groupingVars), server = TRUE, selected= "experiment")
      
      
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
