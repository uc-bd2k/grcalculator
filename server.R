library(shiny)
library(shinyjs)
library(plotly)
library(ggplot2)
library(readr)
library(GRmetrics)
library(magrittr)
library(stringr)
library(DT)
library(formattable)
library(shinycssloaders)
library(dplyr)
library(tictoc)

#source('functions/drawPopup.R')
#source('functions/drawDRC.R', local = T)
#source('functions/extractGridData.R')
#source('functions/drawScatter.R', local = T)
#source('functions/drawBox.R')
#source('functions/parseLabel.R')
source('functions/GRdrawDRC.app.R')
source('functions/GRdrawDRCV2.app.R')
source('functions/check_col_names.R')

prettyNumTable = function(df, digits = 3) {
  df %<>% dplyr::mutate_if(is.numeric, 
            function(x) base::prettyNum(x, digits = digits, drop0trailing = TRUE))
}

######## code for zipping output data into one file ##########
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
######################

####### javascript code for modals ##########
about.modal.js = "$('.ui.small.modal')
.modal({
    blurring: false
})
$('#about_modal').modal('show')
;"
contact.modal.js = "$('.ui.mini.modal')
.modal({
    blurring: false
})
$('#contact_modal').modal('show')
;"
import.modal.js = "$('.ui.small.modal')
.modal({
    blurring: false
})
$('#import_modal').modal('show')
;"

example.modal.js = "$('.ui.mini.modal')
.modal({
    blurring: false
})
$('#example_modal').modal('show')
;"

start.modal.js = "$('.ui.mini.modal')
.modal({
    blurring: false
})
$('#start_modal').modal('show')
;"

instructions.modal.js = "$('.ui.small.modal')
.modal({
    blurring: false
})
$('#instructions_modal').modal('show')
;"

download_plot_drc.modal.js = "$('.ui.small.modal')
.modal({
    blurring: false
})
$('#download_plot_drc_modal').modal('show')
;"

tab.js = "$('.menu .item')
  .tab()
;"

### code for file upload button
upload.js = "$('#divUpload').on('click', function() {
  $('#uploadData').click();
});"

# ### code for accordion

accordion.js = "$('.ui.accordion')
.accordion()
;"

###############################

shinyServer(function(input, output,session) {
  runjs(tab.js)
  runjs(accordion.js)
  # initialize variables for saving various user inputs, parameters, etc.
  values <- reactiveValues(inData=NULL, GR_table = NULL, GR_table_show = NULL, 
                           parameter_table = NULL, parameter_table_show = NULL, 
                           df_scatter = NULL, showanalyses=0, showdata=0, 
                           showanalyses_multi=0, data_dl = NULL, wilcox = NULL,
                           clearScatter = F, separator = NULL, div_rate = NULL,
                           init_count = logical(0), input_case = NULL,
                           cell_lines = NULL, div_rate_test = NULL, check_fail = NULL,
                           current_data = NULL, current_table_button = "gr_table_button",
                           no_dead = NULL, yes_dead = NULL,
                           tables = NULL, grid_vs_single = "single", filters_loaded = F,
                           group_vars = NULL)
  
  ### file upload button
  runjs(upload.js)
  #### update plot options for dose-response curve ##########
  observeEvent(input$analyzeButton, {
    if(values$input_case == "static_vs_toxic") {
      curve_choices = c("sigmoid","line", "none")
    } else {
      curve_choices = c("sigmoid", "line", "biphasic", "sigmoid_high", "sigmoid_low", "none") 
    }
    updateSelectizeInput(session, "drc2_curves", choices = curve_choices)
  }, ignoreNULL = T, ignoreInit = T)
  observeEvent(input$analyzeButton, {
    if(values$input_case == "static_vs_toxic") {
      curve_types = list(GR ="GR")
    } else {
      curve_types = list(GR ="GR", `Relative cell count` = "rel_cell")
    }
    updateSelectizeInput(session, "drc2_metric", choices = curve_types)
  }, ignoreNULL = T, ignoreInit = T)
  
  observeEvent(c(input$drc2_metric, input$drc2_curves), {
    if(input$drc2_metric == "GR") {
      if(input$drc2_curves == "biphasic") {
        updateSelectizeInput(session, 'drc2_xrug', choices = c("none", "GEC50_1", "GEC50_2"))
        updateSelectizeInput(session, 'drc2_yrug', choices = c("none", "GRinf_1", "GRinf_2", "GRmax"))
      } else {
        updateSelectizeInput(session, 'drc2_xrug', choices = c("none", "GR50", "GEC50"))
        updateSelectizeInput(session, 'drc2_yrug', choices = c("none", "GRinf", "GRmax"))
      }
    }
    if(input$drc2_metric == "rel_cell") {
      if(input$drc2_curves == "biphasic") {
        updateSelectizeInput(session, 'drc2_xrug', choices = c("none", "EC50_1", "EC50_2"))
        updateSelectizeInput(session, 'drc2_yrug', choices = c("none", "Einf_1", "Einf_2", "Emax"))
      } else {
        updateSelectizeInput(session, 'drc2_xrug', choices = c("none", "IC50", "EC50"))
        updateSelectizeInput(session, 'drc2_yrug', choices = c("none", "Einf", "Emax"))
      }
      
    }
  })
  ### update facets for main plot
  observeEvent(input$analyzeButton, {
    ### select first grouping variable
    # updateSelectizeInput(session, 'drc2_facet', choices = c(input$groupingVars), 
    #                    server = TRUE, selected=input$groupingVars[1])
    ### select all grouping variables
    updateSelectizeInput(session, 'drc2_facet', choices = c(input$groupingVars), 
                         server = TRUE, selected=input$groupingVars)
  }, ignoreInit = T, ignoreNULL = T, priority = 1000)
  ### update color variable
  observeEvent(c(input$analyzeButton, input$single_button, input$grid_button), {
    req(values$grid_vs_single, values$input_case, input$groupingVars)
    if(values$grid_vs_single == "grid") {
      if(values$input_case == "static_vs_toxic") {
        updateSelectizeInput(session, 'drc2_color', choices = "GR_metric", 
                             server = TRUE, selected= "GR_metric")
        
      } else {
        updateSelectizeInput(session, 'drc2_color', choices = c("experiment", input$groupingVars), 
                             server = TRUE, selected= "experiment")
      }
    } else {
      updateSelectizeInput(session, 'drc2_color', choices = c("experiment", input$groupingVars), 
                           server = TRUE, selected= "experiment")
    }
  }, ignoreInit = T, ignoreNULL = T, priority = 900)
  ###############
  
  ########### code for input breadcrumbs ################
  observeEvent(input$import_button, {
    removeClass(id = "example_button", class = "active")
    addClass(id = "import_button", class = "active")
    #runjs(import.modal.js)
    hideElement(id = "bc1_content")
    hideElement(id = "bc1_text")
    showElement(id = "bc1_link")
    showElement(id = "bc2_upload")
    showElement(id = "bc2_upload_text")
    removeClass(id = "bc1", class = "active")
    addClass(id = "bc2_upload", class = "active")
    showElement(id = "bc2_upload_content")
  })
  observeEvent(input$example_button, {
    #runjs(example.modal.js)
    removeClass(id = "import_button", class = "active")
    addClass(id = "example_button", class = "active")
    hideElement(id = "bc1_content")
    hideElement(id = "bc1_text")
    showElement(id = "bc1_link")
    showElement(id = "bc2_ex")
    showElement(id = "bc2_ex_text")
    removeClass(id = "bc1", class = "active")
    addClass(id = "bc2_ex", class = "active")
    showElement(id = "bc2_ex_content")
  })
  observeEvent(input$bc1_link, {
    #runjs(import.modal.js)
    addClass(id = "bc1", class = "active")
    hideElement(id = "bc2_upload_content")
    hideElement(id = "bc2_ex_content")
    hideElement(id = "bc2_ex_link")
    hideElement(id = "bc2_upload_link")
    hideElement(id = "bc2_upload")
    hideElement(id = "bc2_ex")
    hideElement(id = "bc3")
    hideElement(id = "bc3_link")
    hideElement(id = "bc3_content")
    hideElement(id = "bc4")
    hideElement(id = "bc4_link")
    hideElement(id = "bc4_content")
    hideElement(id = "bc1_link")
    hideElement(id = "upload_button")
    showElement(id = "bc1_text")
    showElement(id = "bc1_content")
  })
  observeEvent(input$bc2_upload_link, {
    addClass(id = "bc2_upload", class = "active")
    hideElement(id = "bc3_content")
    hideElement(id = "bc3")
    hideElement(id = "bc3_link")
    hideElement(id = "bc2_upload_link")
    hideElement(id = "upload_button")
    showElement(id = "bc2_upload_text")
    showElement(id = "bc2_upload_content")
    hideElement(id = "bc4")
    hideElement(id = "bc4_link")
    hideElement(id = "bc4_content")
  })
  
  observeEvent(input$bc2_ex_link, {
    addClass(id = "bc2_ex", class = "active")
    hideElement(id = "bc3_content")
    hideElement(id = "bc3")
    hideElement(id = "bc3_link")
    hideElement(id = "bc2_ex_link")
    showElement(id = "bc2_ex_text")
    showElement(id = "bc2_ex_content")
    hideElement(id = "bc4")
    hideElement(id = "bc4_link")
    hideElement(id = "bc4_content")
  })
  
  observeEvent(input$bc3_link, {
    addClass(id = "bc3", class = "active")
    hideElement(id = "bc3_link")
    showElement(id = "bc3_text")
    showElement(id = "bc3_content")
    
    hideElement(id = "bc4")
    hideElement(id = "bc4_link")
    hideElement(id = "bc4_content")
  })
  
  
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
    if(!is.null(values$input_case) && !is.null(values$div_rate)) {
      if(identical(values$input_case, "A") || identical(values$input_case, "B")) {
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
  
  # Code to show division rate vs. initial cell count choice buttons
  observeEvent(input$no_dead, {
    #hideElement(id = "upload_button", anim = F)
    hideElement(id = "advanced_input", anim = F)
    hideElement(id = "static_vs_toxic_req", anim = F)
    hideElement(id = "comma_tab_buttons", anim = F)
    hideElement(id = "case_desc", anim = F)
    hideElement(id = "case_buttons", anim = F)
    showElement(id = "calc_method_buttons", anim = F)
    showElement(id = "calc_method_desc", anim = F)
    addClass(id = "no_dead", class = "active")
    removeClass(id = "yes_dead", class = "active")
    values$yes_dead = F
    values$no_dead = T
  }, ignoreInit = T)
  # Code to show GR static vs. toxic columns needed
  observeEvent(input$yes_dead, {
    #hideElement(id = "upload_button", anim = F)
    hideElement(id = "bc2_upload_content")
    hideElement(id = "bc2_upload_text")
    showElement(id = "bc2_upload_link")
    showElement(id = "bc3_content")
    removeClass(id = "bc2_upload", class = "active")
    addClass(id = "bc3", class = "active")
    showElement(id = "bc3", anim = F)
    showElement(id = "bc3_text", anim = F)
    
    hideElement(id = "advanced_input", anim = F)
    hideElement(id = "calc_method_buttons", anim = F)
    hideElement(id = "case_buttons", anim = F)
    hideElement(id = "calc_method_desc", anim = F)
    hideElement(id = "case_desc", anim = F)
    showElement(id = "static_vs_toxic_req", anim = F)
    showElement(id = "comma_tab_buttons", anim = F)
    addClass(id = "yes_dead", class = "active")
    removeClass(id = "no_dead", class = "active")
    values$input_case = "static_vs_toxic"
    values$yes_dead = T
    values$no_dead = F
  }, ignoreInit = T)
  # Code to show caseA/caseB choice buttons
  observeEvent(input$initialCellCount, {
    removeClass(id = "divisionRate", class = "active")
    addClass(id = "initialCellCount", class = "active")
    showElement(id = "case_buttons", anim = F)
    values$div_rate = F
    values$init_count = T
  }, ignoreInit = T)
  observeEvent(input$divisionRate, {
    removeClass(id = "initialCellCount", class = "active")
    addClass(id = "divisionRate", class = "active")
    showElement(id = "case_buttons", anim = F)
    values$div_rate = T
    values$init_count = F
  }, ignoreInit = T)
  # Code to show csv/tsv choice buttons
  observeEvent(input$caseA, {
    removeClass(id = "caseB", class = "active")
    addClass(id = "caseA", class = "active")
    
    hideElement(id = "bc2_upload_content")
    hideElement(id = "bc2_upload_text")
    showElement(id = "bc2_upload_link")
    showElement(id = "bc3_content")
    removeClass(id = "bc2_upload", class = "active")
    addClass(id = "bc3", class = "active")
    showElement(id = "bc3", anim = F)
    showElement(id = "bc3_text", anim = F)
    showElement(id = "case_desc", anim = F)
    #hideElement(id = "static_vs_toxic_req", anim = F)
    
    showElement(id = "comma_tab_buttons", anim = F)
    values$input_case = "A"
  }, ignoreInit = T)
  observeEvent(input$caseB, {
    removeClass(id = "caseA", class = "active")
    addClass(id = "caseB", class = "active")
    
    hideElement(id = "bc2_upload_content")
    hideElement(id = "bc2_upload_text")
    showElement(id = "bc2_upload_link")
    showElement(id = "bc3_content")
    removeClass(id = "bc2_upload", class = "active")
    addClass(id = "bc3", class = "active")
    showElement(id = "bc3", anim = F)
    showElement(id = "bc3_text", anim = F)
    showElement(id = "case_desc", anim = F)
    #hideElement(id = "static_vs_toxic_req", anim = F)
    
    showElement(id = "comma_tab_buttons", anim = F)
    values$input_case = "B"
  }, ignoreInit = T)
  
  # Code to make import dialog csv/tsv buttons work like radiobuttons
  observeEvent(input$comma_input, {
    removeClass(id = "tab_input", class = "active")
    addClass(id = "comma_input", class = "active")
    values$separator = ","
    showElement(id = "upload_button", anim = F)
    #showElement(id = "advanced_input", anim = F)
  }, ignoreInit = T)
  observeEvent(input$tab_input, {
    removeClass(id = "comma_input", class = "active")
    addClass(id = "tab_input", class = "active")
    values$separator = "\t"
    showElement(id = "upload_button", anim = F)
    #showElement(id = "advanced_input", anim = F)
  }, ignoreInit = T)
  
  # Code for closing input dialog when data is uploaded
  # observeEvent(c(input$uploadData,input$fetchURLData), {
  #   #observeEvent(values$check_fail, {
  #     toggleModal(session, 'import_button', toggle = "close")
  #   }, ignoreInit = T)
  # observeEvent(c(input$loadExample, input$loadExampleB),{
  #   toggleModal(session, 'example_modal', toggle = "close")
  # }, ignoreInit = T)
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
  ####################
  
  
  
  ############ initialize modals ################
  observeEvent(input$instructions_button, {
    runjs(instructions.modal.js)
  })

  observeEvent(input$download_plot_drc.modal_button, {
    runjs(download_plot_drc.modal.js)
  })
  observeEvent(input$about, {
    runjs(about.modal.js)
  })
  observeEvent(input$start_button, {
    #runjs(start.modal.js)
    shinyjs::show(id = "input_top")
    shinyjs::click(id = "input_top")
  })
  observeEvent(input$contact, {
    runjs(contact.modal.js)
  })
  ##############################
  
  ##### show and hide parts of UI #########
  ## hide boxplot/scatterplot buttons for now
  shinyjs::hideElement("scatter_button")
  shinyjs::hideElement("boxplot_button")
  #shinyjs::click(id = "single_button")
  
  ### hide example modal when example is picked
  # observeEvent(c(input$loadExample, input$loadExampleB), {
  #   runjs("$('#example_modal').modal('hide')")
  # })
  # observeEvent(input$uploadData, {
  #   runjs("$('#import_modal').modal('hide')")
  # })
  
  ### start loader on analyze button first
  observeEvent(input$analyzeButton, {
    shinyjs::addClass(id = "analyze_loader", "active")
  }, ignoreInit = T, ignoreNULL = T, priority = 1000)
  ### then jump to plots tab
  shinyjs::onclick("analyzeButton", {
    #shinyjs::click(id = "single_button")
    shinyjs::show(id = "drc_top")
    #shinyjs::show(id = "comparison_top")
    shinyjs::show(id = "output_top")
    shinyjs::click(id = "drc_top")
    shinyjs::removeClass(id = "analyze_loader", "active")
  })
  ### on click of data table tab, "click" current data table button to make sure it shows up
  shinyjs::onclick("output_top", {
    print("output tables top")
    print(values$current_table_button)
    shinyjs::click(values$current_table_button)
  })
  ### on click of drc plot tab, "click" plot button to make sure it shows up
  shinyjs::onclick("drc_top", {
    # shinyjs::show("ui")
    # shinyjs::show("plot.ui")
    if(values$grid_vs_single == "grid") shinyjs::show("plots_grid")
    if(values$grid_vs_single == "single") shinyjs::show("single_drc")
    
  })
  shinyjs::onclick("plot_options_button", {
    shinyjs::toggle(id = "plot_options", animType = "slide")
    toggleElement(id = "caret_down")
    toggleElement(id = "caret_right")
  })
  ### set which table to show in data table tab
  shinyjs::click(id = "input_table_button")
  
  
  ## record whether we want a grid plot or a single plot for curves
  observeEvent(input$grid_button, {
    shinyjs::removeClass(id = "single_button", class = "active")
    shinyjs::addClass(id = "grid_button", class = "active")
  })
  observeEvent(input$single_button, {
    shinyjs::removeClass(id = "grid_button", class = "active")
    shinyjs::addClass(id = "single_button", class = "active")
  })
  observeEvent(c(input$grid_button, input$single_button), {
    #shinyjs::toggleElement("grid_button")
    #shinyjs::toggleElement("single_button")
    #shinyjs::toggleElement("plots_grid")
    #shinyjs::toggleElement("plots_grid_legend")
    shinyjs::toggleElement("grid_segment")
    shinyjs::toggleElement("height")
    #shinyjs::toggleElement("drc2_facet")
    shinyjs::toggleElement("nplots")
    shinyjs::toggleElement("single_segment")
    
    
    values$grid_vs_single = ifelse(values$grid_vs_single == "grid", "single", "grid")
  }, ignoreInit = T, ignoreNULL = F, priority = 1000)
  
  observeEvent(c(values$grid_vs_single, values$input_case), {
    req(values$grid_vs_single, values$input_case)
    if(values$grid_vs_single == "grid") {
      if(values$input_case != "static_vs_toxic") {
        shinyjs::showElement("drc2_facet")
      } else {
        shinyjs::hideElement("drc2_facet")
      }
    } else {
      shinyjs::hideElement("drc2_facet")
    }
  }, ignoreInit = T, ignoreNULL = T)
  
  observeEvent(c(input$scatter_button, input$boxplot_button), {
    #shinyjs::toggleElement("scatter_button")
    #shinyjs::toggleElement("boxplot_button")

    shinyjs::toggleElement("scatter_options")
    shinyjs::toggleElement("boxplot_options")
    
    shinyjs::toggleElement("boxplot")
    shinyjs::toggleElement("scatterplot")
  }, ignoreInit = T, ignoreNULL = F, priority = 1000)
  
  ### show/hide parameter table button
  # shinyjs::onclick("parameter_table_button", {
  #   shinyjs::show("parameter_table_select")
  # })
  shinyjs::onclick("gr_table_button", {
    shinyjs::hide("parameter_table_select")
  })
  shinyjs::onclick("input_table_button", {
    shinyjs::hide("parameter_table_select")
  })
  
#######################
  
  
  ##### observe input data ##########
  observeEvent(values$inData, {
    ### reset initial state just in case 
    shinyjs::hide(id = "output_top")
    shinyjs::hide(id = "comparison_top")
    shinyjs::hide(id = "drc_top")
    values$filters_loaded = F
    
    # shinyjs::show(id = "input_top")
    # shinyjs::click(id = "input_top")
    output$input_table = renderDataTable({ datatable(values$inData,  
       extensions = c('Buttons'
                      #, 'FixedHeader'
       ),
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
  
  # Code for showing/hiding advanced analysis options
  # observeEvent(values$inData, {
  #   showElement(id = "advanced_analysis", anim = T, animType = "fade")
  # })
  ####################
  
  
  ######### show correct output data table on button click #############
  observeEvent(input$analyzeButton, {
    if(identical(values$input_case, "A") || identical(values$input_case, "B")) {
      curve_choices = 1:8
      names(curve_choices) = c("GR sigmoid normal", "GR sigmoid low", "GR sigmoid high", "GR biphasic",
                               "Traditional sigmoid normal", "Traditional sigmoid low", 
                               "Traditional sigmoid high", "Traditional biphasic")
    }
    if(identical(values$input_case, "static_vs_toxic")) {
      curve_choices = 1:2
      names(curve_choices) = c("GR static", "GR toxic")
    }
    updateSelectizeInput(session, inputId = "parameter_table_select", choices = curve_choices,
                         selected = 1)
  })
  observeEvent(input$gr_table_button, {
    values$current_table_button = "gr_table_button"
    #values$current_data = values$GR_table_show
    ## emphasize the selected data table button
    #shinyjs::removeClass(id = "input_table_button", class = "green")
    shinyjs::hide(id = "parameter_table")
    shinyjs::show(id = "gr_table")
    shinyjs::addClass(id = "gr_table_button", class = "green")
    shinyjs::removeClass(id = "parameter_table_button", class = "green")
  })
  observeEvent(input$parameter_table_button, {
    values$current_table_button = "parameter_table_button"
    shinyjs::hide(id = "gr_table")
    shinyjs::show(id = "parameter_table")
    #values$current_data = values$parameter_table_show
    ## emphasize the selected data table button
    #shinyjs::removeClass(id = "input_table_button", class = "green")
    shinyjs::removeClass(id = "gr_table_button", class = "green")
    shinyjs::addClass(id = "parameter_table_button", class = "green")
  })
  ### define current output (parameter) data table
  observeEvent(input$parameter_table_button, {
    print("parameter table button clicked")
    shinyjs::show("parameter_table_select")
  }, ignoreInit = T, ignoreNULL = T)
  observeEvent(c(input$parameter_table_select, input$parameter_table_button), {
    req(input$parameter_table_select, values$input_case, values$parameters_all)
    pp = input$parameter_table_select
    print(pp)
    if(identical(values$input_case, "A") || identical(values$input_case, "B")) {
      # curve_choices = 1:8
      # names(curve_choices) = c("GR sigmoid normal", "GR sigmoid low", "GR sigmoid high", "GR biphasic",
      #                          "Traditional sigmoid normal", "Traditional sigmoid low", 
      #                          "Traditional sigmoid high", "Traditional biphasic")
      if(pp == 1) values$parameter_table = values$parameters_all$GR$sigmoid$normal
      if(pp == 2) values$parameter_table = values$parameters_all$GR$sigmoid$low
      if(pp == 3) values$parameter_table = values$parameters_all$GR$sigmoid$high
      if(pp == 4) values$parameter_table = values$parameters_all$GR$biphasic$normal
      if(pp == 5) values$parameter_table = values$parameters_all$rel_cell$sigmoid$normal
      if(pp == 6) values$parameter_table = values$parameters_all$rel_cell$sigmoid$low
      if(pp == 7) values$parameter_table = values$parameters_all$rel_cell$sigmoid$high
      if(pp == 8) values$parameter_table = values$parameters_all$rel_cell$biphasic$normal
    }
    if(identical(values$input_case, "static_vs_toxic")) {
      # curve_choices = 1:2
      # names(curve_choices) = c("GR static", "GR toxic")
      if(pp == 1) values$parameter_table = values$parameters_all$GR$static
      if(pp == 2) values$parameter_table = values$parameters_all$GR$toxic
    }
    values$parameter_table_show = prettyNumTable(values$parameter_table)
    values$current_data = values$parameter_table_show
  }, ignoreInit = T, ignoreNULL = T)
  #### render current data table
  # observeEvent(values$current_data, {
  #   output$current_table = renderDataTable({ datatable(values$current_data,  extensions = c('Buttons'#, 'FixedHeader'
  #                 ),
  #    filter = 'top',
  #    rownames = F, options = list(
  #      dom = 'lBfrtip',
  #      buttons = c('copy', 'csv', 'excel', 'colvis'),
  #      initComplete = JS(
  #        "function(settings, json) {",
  #        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
  #        "}"),
  #      searchHighlight = TRUE,
  #      fixedHeader = TRUE,
  #      autoWidth = TRUE))
  #   }, server = FALSE)
  #   outputOptions(output, "current_table", suspendWhenHidden = FALSE)
  # }, ignoreNULL = F)
  
  output$gr_table = renderDataTable({ datatable(values$GR_table_show,  extensions = c('Buttons'#, 'FixedHeader'
  ),
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
  outputOptions(output, "gr_table", suspendWhenHidden = FALSE)
  
  output$parameter_table = renderDataTable({ datatable(values$parameter_table_show,  extensions = c('Buttons'#, 'FixedHeader'
  ),
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
  outputOptions(output, "parameter_table", suspendWhenHidden = FALSE)
  
########################
  
  ### initialize listener for filter boxes
  toListen_drc <- reactive({
    req(input$groupingVars, values$tables)
    if(length(input$groupingVars) > 0) {
      filters = lapply(1:length(input$groupingVars), function(i) input[[ paste("param_", input$groupingVars[i], sep="") ]] )
      names(filters) = paste("param_", input$groupingVars, sep="")
      filters
    } else { list() }
    })
  toListen_drc2 = toListen_drc %>% debounce(400)
  facet_throttle = reactive({ input$drc2_facet }) %>% debounce(100)
  button_throttle = reactive({
    input$analyzeButton
    values$grid_vs_single 
  }) %>% throttle(500)
  color_throttle = reactive({ input$drc2_color }) %>% throttle(100)
  
  filter_data = reactive({
      req(values$input_case, values$tables, input$groupingVars, toListen_drc2())
    print("tolisten")
    print(toListen_drc2())
    filtered_drc = NULL
      if(identical(values$input_case,"static_vs_toxic")) {
        filtered_drc = values$tables
        n <- length(toListen_drc2())
        if (n>0) {
          filter_list = toListen_drc2()
          print("filter")
          print(filter_list)
          for(i in 1:length(filter_list)) {
            vals = filter_list[[i]]
            #do.call("freezeReactiveValue", args = list(x = input, name = filter_vars[i]))
            df_colname = sym(names(filter_list)[i])
            #print(head(filtered_drc$metadata$gr_table))
            if(length(vals) > 0) {
              if(sum(!vals %in% filtered_drc$metadata$gr_table[[df_colname]]) == 0) {
                filtered_drc$assays$GR$static %<>%
                  dplyr::filter(!!df_colname %in% vals)
                filtered_drc$assays$GR$toxic %<>%
                  dplyr::filter(!!df_colname %in% vals)
                filtered_drc$metadata$gr_table %<>%
                  dplyr::filter(!!df_colname %in% vals)
              }
            }
          }
        }
      } else {
          gr_table = GRmetrics::GRgetValues(values$tables)
          filtered_drc = values$tables
          n <- length(toListen_drc2())
          if (n>0) {
            filter_list = toListen_drc2()
            print("filter")
            print(filter_list)
            tic()
            for(i in 1:length(filter_list)) {
              vals = filter_list[[i]]
              #do.call("freezeReactiveValue", args = list(x = input, name = filter_vars[i]))
              df_colname = sym(names(filter_list)[i])
              test <<- filtered_drc

              if(length(vals) > 0) {
                if(sum(!vals %in% filtered_drc$metadata$gr_table[[df_colname]]) == 0) {
                  if(input$drc2_metric == "GR") {
                    if(input$drc2_curves == "sigmoid") {
                      filtered_drc$assays$GR$sigmoid$normal %<>%
                        dplyr::filter(!!df_colname %in% vals)
                    } else if(input$drc2_curves == "sigmoid_high") {
                      filtered_drc$assays$GR$sigmoid$high %<>%
                        dplyr::filter(!!df_colname %in% vals)
                    } else if(input$drc2_curves == "sigmoid_low") {
                      filtered_drc$assays$GR$sigmoid$low %<>%
                        dplyr::filter(!!df_colname %in% vals)
                    } else if(input$drc2_curves == "biphasic") {
                      filtered_drc$assays$GR$biphasic$normal %<>%
                        dplyr::filter(!!df_colname %in% vals)
                    }
                  } else {
                    if(input$drc2_curves == "sigmoid") {
                      filtered_drc$assays$rel_cell$sigmoid$normal %<>%
                        dplyr::filter(!!df_colname %in% vals)
                    } else if(input$drc2_curves == "sigmoid_low") {
                      filtered_drc$assays$rel_cell$sigmoid$low %<>%
                        dplyr::filter(!!df_colname %in% vals)
                    } else if(input$drc2_curves == "sigmoid_high") {
                      filtered_drc$assays$rel_cell$sigmoid$high %<>%
                        dplyr::filter(!!df_colname %in% vals)
                    } else if(input$drc2_curves == "biphasic") {
                      filtered_drc$assays$rel_cell$biphasic$normal %<>%
                        dplyr::filter(!!df_colname %in% vals)
                    }
                  }
                  filtered_drc$metadata$gr_table %<>%
                    dplyr::filter(!!df_colname %in% vals)
                }
              }
            }
            toc()
          }
      }
    filtered_drc
  })
  
  output$single_drc = renderPlotly({
    req(filter_data(), input$drc2_curves, input$drc2_metric, input$drc2_points, input$drc2_xrug, input$drc2_yrug,
        input$drc2_bars, color_throttle(), values$input_case)
    if(values$input_case != "static_vs_toxic") {
      single_plot = try(GRdrawDRC.app(fitData = filter_data(), 
                                      metric = input$drc2_metric, 
                                      curves = input$drc2_curves,
                                      points = input$drc2_points,
                                      xrug = input$drc2_xrug,
                                      yrug = input$drc2_yrug,
                                      facet = "none", 
                                      bars = input$drc2_bars,
                                      color = color_throttle(),
                                      plot_type = "static",
                                      output_type = "together"))
        if(class(single_plot) != "try-error") {
          ggplotly(single_plot$plot, tooltip = "text")
        }
    } else {
      single_plot = try(GRdrawDRCV2.app(fitData = filter_data(), 
                                      #metric = input$drc2_metric, 
                                      curves = input$drc2_curves,
                                      points = input$drc2_points,
                                      #xrug = input$drc2_xrug,
                                      #yrug = input$drc2_yrug,
                                      #facet = "none", 
                                      #bars = input$drc2_bars,
                                      color = color_throttle(),
                                      plot_type = "static",
                                      output_type = "together"))
      if(class(single_plot) != "try-error") {
        ggplotly(single_plot$plot, tooltip = "text")
      }
    }
  })
  outputOptions(output, "single_drc", suspendWhenHidden = FALSE)
  
  
  
  ###### begin render main dose-response curve plot(s) ########
  # observeEvent(c(input$nplots, values$tables, input$analyzeButton, toListen_drc2(),
  #                values$grid_vs_single, facet_throttle(),
  #                color_throttle(), input$drc2_metric, input$drc2_curves,
  #                input$drc2_points, input$drc2_xrug, input$drc2_yrug, input$drc2_bars
  #                ), {
  # observeEvent(c(toListen_drc2(), button_throttle()), {
  #   req(input$groupingVars, input$drc2_metric, input$drc2_points, input$drc2_facet,
  #       input$drc2_xrug, input$drc2_yrug, input$drc2_bars, values$grid_vs_single, input$drc2_color)
  #   #col = isolate({ input$drc2_color })
  #   # for(x in c("groupingVars", "drc2_color", "drc2_metric", "drc2_points",
  #   #             "drc2_xrug", "drc2_yrug", "drc2_bars") ) {
  #   #   print(x)
  #   #   print(input[[x]])
  #   # }
  #   # for(x in "grid_vs_single") {
  #   #   print(x)
  #   #   print(values[[x]])
  #   # }
  #                  
  #   if(identical(values$input_case,"static_vs_toxic")) {
  #     filtered_drc = values$tables
  #     n <- length(input$groupingVars)
  #     if (n>0) {
  #       filter_vars = paste0("param_", input$groupingVars)
  #       filter_list = lapply(filter_vars, function(x) input[[x]] )
  #       names(filter_list) = input$groupingVars
  #       print(filter_list)
  #       for(i in 1:length(filter_list)) {
  #         vals = filter_list[[i]]
  #         #do.call("freezeReactiveValue", args = list(x = input, name = filter_vars[i]))
  #         df_colname = sym(names(filter_list)[i])
  #         print(head(filtered_drc$metadata$gr_table))
  #         if(length(vals) > 0) {
  #           if(sum(!vals %in% filtered_drc$metadata$gr_table[[df_colname]]) == 0) {
  #             filtered_drc$assays$GR$static %<>% 
  #               dplyr::filter(!!df_colname %in% vals)
  #             filtered_drc$assays$GR$toxic %<>% 
  #               dplyr::filter(!!df_colname %in% vals)
  #             filtered_drc$metadata$gr_table %<>%
  #               dplyr::filter(!!df_colname %in% vals)
  #           }
  #         }
  #       }
  #     }
  #     print(dim(filtered_drc$assays$GR$toxic))
  #     ######### begin render plots for static vs toxic case ##########
  #     if(identical(values$grid_vs_single, "single")) {
  #       #shinyjs::click(id = "grid_button")
  #       single_plot = try(GRdrawDRCV2.app(fitData = filtered_drc, 
  #                                         #metric = input$drc2_metric, 
  #                                         curves = input$drc2_curves,
  #                                         points = input$drc2_points, 
  #                                         #xrug = input$drc2_xrug, yrug = input$drc2_yrug,
  #                                         #facet = input$drc2_facet, 
  #                                         #bars = input$drc2_bars,
  #                                         color = color_throttle(), 
  #                                         plot_type = "static",
  #                                         output_type = "together"))
  #       if(class(single_plot) != "try-error") {
  #         output$single_drc = renderPlotly(ggplotly(single_plot$plot, tooltip = "text"))
  #         outputOptions(output, "single_drc", suspendWhenHidden = FALSE)
  #       }
  #     }
  #     if(identical(values$grid_vs_single, "grid")) {
  #       #### render grid of plots
  #       
  #       output$plots_grid <- renderUI({
  #         plots = try(GRdrawDRCV2.app(fitData = filtered_drc, 
  #                                 #metric = input$drc2_metric, 
  #                                 curves = input$drc2_curves,
  #                                 points = input$drc2_points, 
  #                                 #xrug = input$drc2_xrug, yrug = input$drc2_yrug,
  #                                 #facet = input$drc2_facet, 
  #                                 #bars = input$drc2_bars,
  #                                 #color = color_throttle(), 
  #                                 plot_type = "static",
  #                                 output_type = "separate"))
  #         if(class(plots) != "try-error") {
  #           legend = plots$legend
  #           plots = plots$plot
  #           output$plots_grid_legend = renderPlot(legend)
  #         }
  #         for (i in 1:length(plots)) {
  #           local({
  #             n <- i # Make local variable
  #             plotname <- paste("plot", n , sep="")
  #             output[[plotname]] <- renderPlotly({
  #               ggplotly(plots[[n]], tooltip = "text")
  #             })
  #           })
  #         }
  #         #col.width <- round(16/input$ncol) # Calculate bootstrap column width
  #         col.width <- 300
  #         #n.col <- ceiling(length(plots)/input$ncol) # calculate number of rows
  #         #n.row = input$nrow
  #         #n.col = input$ncol
  #         cnter <<- 0 # Counter variable
  #         #n_pages =  ceiling(n_total_plots/input$nplots)
  #         
  #         # Create row with columns
  #         #rows  <- lapply(1:n.row,function(row.num){
  #         cols  <- lapply(1:min(as.numeric(input$nplots), length(plots) ), function(i) {
  #           cnter    <<- cnter + 1
  #           plotname <- paste("plot", cnter, sep="")
  #           div(class = "ui column", style = paste0("flex: 0 0 ",col.width,"px; padding: 0px;") , 
  #               plotlyOutput(plotname,width = col.width, height = col.width) %>% withSpinner(type = 3, color = "#009999", color.background = "#ffffff")
  #               # tags$img(src = "images/GRcalculator-logo.jpg", width = paste0(col.width, "px"), height = paste0(col.width, "px")),
  #               #tags$p(paste0("plot", i))
  #           )
  #         })
  #         do.call(tagList, cols)
  #         #} else {
  #         #  div(class = "ui basic segment")
  #         #}
  #       })
  #       #### render page buttons
  #       output$plots_grid_pages = renderUI({
  #         n_pages = 10
  #         div(class = "ui pagination menu", id = "drc_pages",
  #             tags$a(class = "item", type = "firstItem", "«"),
  #             tags$a(class = "item", type = "prevItem", "⟨"),
  #             tags$a(class = "active item", 1, id = "drc_page1"),
  #             lapply(2:n_pages, function(x) tags$a(class = "item", x, 
  #                                                  id = paste0("drc_page", x)) ),
  #             tags$a(class = "item", type = "nextItem", "⟩"),
  #             tags$a(class = "item", type = "lastItem", "»")
  #         )
  #         #} #else {
  #         #div(class = "ui basic segment")
  #         #}
  #       })
  #     }
  #     ######## end render plots for static vs toxic case #######
  #   } else if(identical(values$input_case, "A") || identical(values$input_case, "B")) {
  #     ######### begin render plots for case A and case B ##########
  #     if(values$filters_loaded && !is.null(values$tables) ) {
  #       gr_table = GRmetrics::GRgetValues(values$tables)
  #       filtered_drc = values$tables
  #       n <- length(input$groupingVars)
  #       if (n>0) {
  #         filter_vars = paste0("param_", input$groupingVars)
  #         filter_list = lapply(filter_vars, function(x) input[[x]] )
  #         names(filter_list) = input$groupingVars
  #         print(filter_list)
  #         print("filtering tables")
  #         tic()
  #         for(i in 1:length(filter_list)) {
  #           vals = filter_list[[i]]
  #           #do.call("freezeReactiveValue", args = list(x = input, name = filter_vars[i]))
  #           df_colname = sym(names(filter_list)[i])
  #           test <<- filtered_drc
  #           
  #           if(length(vals) > 0) {
  #             if(sum(!vals %in% filtered_drc$metadata$gr_table[[df_colname]]) == 0) {
  #               filtered_drc$assays$GR$sigmoid$normal %<>%
  #                 dplyr::filter(!!df_colname %in% vals)
  #               filtered_drc$assays$GR$sigmoid$high %<>%
  #                   dplyr::filter(!!df_colname %in% vals)
  #               filtered_drc$assays$GR$sigmoid$low %<>%
  #                 dplyr::filter(!!df_colname %in% vals)
  #               filtered_drc$assays$GR$biphasic$normal %<>%
  #                 dplyr::filter(!!df_colname %in% vals)
  #               filtered_drc$metadata$gr_table %<>%
  #                 dplyr::filter(!!df_colname %in% vals)
  #               filtered_drc$assays$rel_cell$sigmoid$normal %<>%
  #                 dplyr::filter(!!df_colname %in% vals)
  #               filtered_drc$assays$rel_cell$sigmoid$high %<>%
  #                 dplyr::filter(!!df_colname %in% vals)
  #               filtered_drc$assays$rel_cell$sigmoid$low %<>%
  #                 dplyr::filter(!!df_colname %in% vals)
  #               filtered_drc$assays$rel_cell$biphasic$normal %<>%
  #                 dplyr::filter(!!df_colname %in% vals)
  #               filtered_drc$metadata$gr_table %<>%
  #                 dplyr::filter(!!df_colname %in% vals)
  #             }
  #           }
  #         }
  #         toc()
  #         
  #       }
  #       if(values$grid_vs_single == "grid") {
  #         #### render grid of plots
  #         output$plots_grid <- renderUI({
  #           #if(values$grid_vs_single == "grid") {
  #           print("generating plots")
  #           tic()
  #           plots = try(GRdrawDRC.app(fitData = filtered_drc, metric = input$drc2_metric, 
  #                                      curves = input$drc2_curves, points = input$drc2_points, 
  #                                      xrug = input$drc2_xrug, yrug = input$drc2_yrug,
  #                                      facet = input$drc2_facet, bars = input$drc2_bars,
  #                                      color = color_throttle(), plot_type = "static",
  #                                      output_type = "separate"))
  #           toc()
  #           if(class(plots) != "try-error") {
  #             legend = plots$legend
  #             plots = plots$plot
  #             output$plots_grid_legend = renderPlot(legend)
  #           }
  #           print("ggplotly and plot layout")
  #           tic()
  #           for (i in 1:length(plots)) {
  #             local({
  #               n <- i # Make local variable
  #               plotname <- paste("plot", n , sep="")
  #               output[[plotname]] <- renderPlotly({
  #                 ggplotly(plots[[n]], tooltip = "text")
  #               })
  #             })
  #           }
  #           toc()
  #           #col.width <- round(16/input$ncol) # Calculate bootstrap column width
  #           col.width <- 250
  #           #n.col <- ceiling(length(plots)/input$ncol) # calculate number of rows
  #           #n.row = input$nrow
  #           #n.col = input$ncol
  #           cnter <<- 0 # Counter variable
  #           #n_pages =  ceiling(n_total_plots/input$nplots)
  #           
  #           # Create row with columns
  #           #rows  <- lapply(1:n.row,function(row.num){
  #           cols  <- lapply(1:min(as.numeric(input$nplots), length(plots) ), function(i) {
  #             cnter    <<- cnter + 1
  #             plotname <- paste("plot", cnter, sep="")
  #             div(class = "ui column", style = paste0("flex: 0 0 ",col.width,"px; padding: 0px;") , 
  #                 plotlyOutput(plotname,width = col.width, height = col.width) %>% withSpinner(type = 3, color = "#009999", color.background = "#ffffff")
  #                 # tags$img(src = "images/GRcalculator-logo.jpg", width = paste0(col.width, "px"), height = paste0(col.width, "px")),
  #                 #tags$p(paste0("plot", i))
  #             )
  #           })
  #           do.call(tagList, cols)
  #           #} else {
  #           #  div(class = "ui basic segment")
  #           #}
  #         })
  #         #### render page buttons
  #         output$plots_grid_pages = renderUI({
  #           n_pages = 10
  #           div(class = "ui pagination menu", id = "drc_pages",
  #               tags$a(class = "item", type = "firstItem", "«"),
  #               tags$a(class = "item", type = "prevItem", "⟨"),
  #               tags$a(class = "active item", 1, id = "drc_page1"),
  #               lapply(2:n_pages, function(x) tags$a(class = "item", x, 
  #                                                    id = paste0("drc_page", x)) ),
  #               tags$a(class = "item", type = "nextItem", "⟩"),
  #               tags$a(class = "item", type = "lastItem", "»")
  #           )
  #           #} #else {
  #           #div(class = "ui basic segment")
  #           #}
  #         })
  #       }
  #       
  #       ####### render single plot output
  #       if(values$grid_vs_single == "single") {
  #         tic("single_plot")
  #         single_plot = try(GRdrawDRC.app(fitData = filtered_drc, metric = input$drc2_metric, 
  #                                          curves = input$drc2_curves, points = input$drc2_points,
  #                                          xrug = input$drc2_xrug, yrug = input$drc2_yrug,
  #                                          facet = "none", bars = input$drc2_bars,
  #                                          color = color_throttle(), plot_type = "interactive"))
  #         if(class(single_plot) != "try-error") {
  #           output$single_drc = renderPlotly(ggplotly(single_plot$plot, tooltip = "text"))
  #           outputOptions(output, "single_drc", suspendWhenHidden = FALSE)
  #         }
  #         toc()
  #       }
  #     }
  #   }
  # }, ignoreInit = T, ignoreNULL = T)
  # ######### end render main dose-response curve plot(s) ########
  # #### make a list of the variables to filter over
  # # filter_params = reactiveValues()
  # # observeEvent(reactiveValuesToList(input), {
  # #   if(length(values$group_vars) > 0) {
  # #     for(x in values$group_vars) {
  # #       print(x)
  # #       filter_params[[x]] = input[[paste0("param_", x)]]
  # #     }
  # #   }
  # # })
  # # observeEvent(reactiveValuesToList(filter_params), {
  # #   print("filteringgggg")
  # # })
  # #### render curve filter options
  observeEvent(input$analyzeButton, {
    values$group_vars = input$groupingVars
    output$drc_filter <- renderUI({
      n <- length(input$groupingVars)
      if (n>0) {
        code_output_list <- lapply(1:n, function(i) {
          # name the input choice based on the grouping variable names, prefix with "param_" to aviod conflict
          codeOutput <- paste("param_", input$groupingVars[i], sep="")
          verbatimTextOutput(codeOutput)
          drc_choices = sort(unique(values$GR_table[[ input$groupingVars[i] ]]))
          div(class = "ui column", #style = "flex: 0 0 100px; padding: 0px;",
              style = "min-width:150px;",
          selectizeInput(codeOutput, input$groupingVars[i], choices = drc_choices,
                         width = "140px",
                         multiple = TRUE, selected = drc_choices[1:min(5, length(drc_choices))])
          )
        })
      } else code_output_list <- list()
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, code_output_list)
    })

    values$filters_loaded = T
  }, ignoreInit = T, ignoreNULL = T, priority = 1000)
  
  ###############
  
  #### download buttons for example data
  output$dl_caseA = downloadHandler(
    filename = function() {"caseA_example.csv"},
    content = function(con) {
      temp = read_csv("resources/caseA_example.csv")
      return( write.table(temp, file = con, quote = T, row.names = F, sep = ",") )
    }
  )
  output$dl_caseB = downloadHandler(
    filename = function() {"caseB_example.csv"},
    content = function(con) {
      temp = read_csv("resources/caseB_example.csv")
      return( write.table(temp, file = con, quote = T, row.names = F, sep = ",") )
    }
  )
  output$dl_caseA_div = downloadHandler(
    filename = function() {"caseA_div_example.csv"},
    content = function(con) {
      temp = read_csv("resources/caseA_div_example.csv")
      return( write.table(temp, file = con, quote = T, row.names = F, sep = ",") )
    }
  )
  output$dl_caseB_div = downloadHandler(
    filename = function() {"caseB_div_example.csv"},
    content = function(con) {
      temp = read_csv("resources/caseB_div_example.csv")
      return( write.table(temp, file = con, quote = T, row.names = F, sep = ",") )
    }
  )
  output$dl_case_static_vs_toxic = downloadHandler(
    filename = function() {"gr_static_vs_toxic_input_small.csv"},
    content = function(con) {
      temp = read_csv("resources/case_static_vs_toxic_example.csv")
      return( write.table(temp, file = con, quote = T, row.names = F, sep = ",") )
    }
  )
  
  ####### render box and scatter plots ##############
  #### update curve fit choice for box/scatterplots
  # curve_choices = c("GR sigmoid normal", "GR sigmoid low", "GR sigmoid high", "GR biphasic",
  #                   "Traditional sigmoid normal", "Traditional sigmoid low", 
  #                   "Traditional sigmoid high", "Traditional biphasic")
  # observeEvent(input$box_scatter_fit, {
  #   if(input$box_scatter_fit %in% 1:4) values$box_scatter_metric = "GR"
  #   if(input$box_scatter_fit %in% 5:8) values$box_scatter_metric = "rel_cell"
  #   if(input$box_scatter_fit %in% c(1, 5)) values$box_scatter_fit = "sigmoid"
  #   if(input$box_scatter_fit %in% c(2, 6)) values$box_scatter_fit = "sigmoid_low"
  #   if(input$box_scatter_fit %in% c(3, 7)) values$box_scatter_fit = "sigmoid_high"
  #   if(input$box_scatter_fit %in% c(4, 8)) values$box_scatter_fit = "biphasic"
  # }, ignoreInit = F, ignoreNULL = T, priority = 1000)
  # ### render boxplot
  # observeEvent(c(values$tables, input$pick_box_x, input$pick_box_y,
  #                input$pick_box_point_color, input$pick_box_factors, 
  #                input$factorA, input$factorB, input$wilcox_method), {
  #  output$boxplot <- renderPlotly({
  #    if(identical(values$input_case, "A") || identical(values$input_case, "B")) {
  #     plot = try(GRbox(fitData = values$tables, metric = values$box_scatter_metric, 
  #                 fit = values$box_scatter_fit,
  #                 parameter = input$pick_box_y, groupVariable = input$pick_box_x,
  #                 pointColor = input$pick_box_point_color, 
  #                 factors = input$pick_box_factors, 
  #                 wilA = input$factorA, wilB = input$factorB, plotly = TRUE))
  #    if(class(plot) != "try-error") return(plot)
  #    } else {
  #      return(ggplot())
  #    }
  #  })
  #                  outputOptions(output, "boxplot", suspendWhenHidden = FALSE)
  #                }, ignoreInit = T, ignoreNULL = T)
  
  #### render scatterplot
  # observeEvent(c(values$tables, input$pick_parameter, input$x_scatter, input$y_scatter), {
  #   output$scatterplot <- renderPlotly({
  #     if(identical(values$input_case, "A") || identical(values$input_case, "B")) {
  #       plot = try(GRscatter(fitData = values$tables, metric = input$pick_parameter, xaxis = input$x_scatter,
  #                            yaxis = input$y_scatter, # curves = "sigmoid",
  #                            plotly = TRUE))
  #       if(class(plot) != "try-error") return(plot)
  #     } else {
  #       return(ggplot())
  #     }
  #   })
  #   outputOptions(output, "scatterplot", suspendWhenHidden = FALSE)
  # }, ignoreInit = T, ignoreNULL = T)
  
  ################
  
  #### update plot options for box/scatter plots ##########
  observeEvent(input$analyzeButton, {
    ### scatter plot
    updateSelectInput(session, 'pick_var', 'Select variable', choices = input$groupingVars)
    ### box plot
    updateSelectInput(session, 'pick_box_x', 'Select grouping variable', choices = input$groupingVars)
    updateSelectInput(session, 'pick_box_point_color', 'Select additional point coloring', choices = input$groupingVars)

  }, ignoreInit = T, ignoreNULL = T)
  #### Update x and y axis options for scatterplot
  observeEvent(input$pick_var, {
    scatter_choices = unique(values$inData[[input$pick_var]])
    updateSelectInput(session, 'x_scatter', 'Select x-axis value', choices = scatter_choices)
    updateSelectizeInput(session, 'y_scatter', 'Select y-axis value', choices = scatter_choices)
  }, ignoreInit = T, ignoreNULL = T)
  #### Update x-axis choices for boxplot
  observeEvent(input$pick_box_x, {
    box_choices = unique(values$inData[[input$pick_box_x]])
    updateSelectizeInput(session, 'pick_box_factors', 'Show/hide data', choices = box_choices,
                         selected = box_choices[1:min(10, length(box_choices)) ])
  }, ignoreInit = T, ignoreNULL = T)
  #### Update boxplot selections for p-value comparison
  observeEvent(c(input$factorA,input$pick_box_factors), {
    picks = sort(input$pick_box_factors)
    picks1 = setdiff(picks, input$factorA)
    updateSelectizeInput(session, 'factorB', choices = picks1, selected = input$factorB)
  }, ignoreInit = T,  ignoreNULL = F)
  
  observeEvent(c(input$factorB,input$pick_box_factors), {
    picks = sort(input$pick_box_factors)
    picks1 = setdiff(picks, input$factorB)
    updateSelectizeInput(session, 'factorA', choices = picks1, selected = input$factorA)
  }, ignoreInit = T,  ignoreNULL = F)
  
  ##################
  
  ####### p-values for boxplots ###########
  observeEvent(c(input$factorA, input$factorB, input$pick_box_y, input$wilcox_method), {
    wil_data = values$parameter_table
    if(!is.null(input$factorA) & !is.null(input$factorB)) {
      rowsA = wil_data[[input$pick_box_x]] %in% input$factorA
      rowsB = wil_data[[input$pick_box_x]] %in% input$factorB
      wil_dataA = wil_data[rowsA,input$pick_box_y]
      wil_dataB = wil_data[rowsB,input$pick_box_y]
      #wil_less = wilcox.test(x = wil_dataA, y = wil_dataB, alternative = "less")
      #wil_greater = wilcox.test(x = wil_dataA, y = wil_dataB, alternative = "greater")
      wil_two_sided = wilcox.test(x = wil_dataA, y = wil_dataB, alternative = "two.sided")$p.value
      #wil_one_sided = min(wil_less$p.value,wil_greater$p.value)
      wil_pval = wil_two_sided
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
  ###############
  
############ code for loading data ##############
  # Code for loading example data for input Case A
  observeEvent(input$loadExample, {
    removeClass(id = "loadExample_SvT", class = "active")
    addClass(id = "loadExample", class = "active")
    hideElement("bc2_ex_content")
    hideElement("bc2_ex_text")
    showElement("bc2_ex_link")
    removeClass(id = "bc2_ex", class = "active")
    
    ### show current breadcrumb
    addClass(id = "bc4", class = "active")
    shinyjs::show("bc4")
    shinyjs::show("bc4_content")
    
    print("load example a")
    values$input_case = "A"
    values$data_dl = 'example'
    output$input_error = renderText("")
    #session$sendCustomMessage(type = "resetFileInputHandler", "uploadData")
    values$inData <- read_csv('resources/caseA_example.csv')
    values$GR_table_show = NULL
    values$parameter_table_show = NULL
    # print("load example a done")
  })
  # Code for loading example data for input static vs. toxic case
  observeEvent(input$loadExample_SvT, {
    removeClass(id = "loadExample", class = "active")
    addClass(id = "loadExample_SvT", class = "active")
    hideElement("bc2_ex_content")
    hideElement("bc2_ex_text")
    showElement("bc2_ex_link")
    removeClass(id = "bc2_ex", class = "active")
    
    ### show current breadcrumb
    addClass(id = "bc4", class = "active")
    shinyjs::show("bc4")
    shinyjs::show("bc4_content")
    
    values$input_case = "static_vs_toxic"
    values$data_dl = 'example'
    output$input_error = renderText("")
    #session$sendCustomMessage(type = "resetFileInputHandler", "uploadData")
    values$inData <- read_csv('resources/gr_static_vs_toxic_input_small.csv')
    values$GR_table_show = NULL
    values$parameter_table_show = NULL
  })
  
  # Code for loading example data for input Case B
  # observeEvent(input$loadExampleB, {
  #   values$input_case = "B"
  #   values$data_dl = 'example'
  #   output$input_error = renderText("")
  #   #session$sendCustomMessage(type = "resetFileInputHandler", "uploadData")
  #   values$inData <- read_csv('resources/caseB_example.csv')
  #   values$GR_table_show = NULL
  #   values$parameter_table_show = NULL
  # })
  
  ### hide and show correct things on data input
  observeEvent(c(input$uploadData, input$fetchURLData), {
    ### hide previous breadcrumb
    shinyjs::hide("bc3_text")
    shinyjs::show("bc3_link")
    removeClass(id = "bc3", class = "active")
    addClass(id = "bc4", class = "active")
    shinyjs::hide("bc3_content")
    shinyjs::hide("upload_button")
    ### show current breadcrumb
    shinyjs::show("bc4")
    shinyjs::show("bc4_content")
  }, ignoreInit = T, ignoreNULL = T)
  
  # Code for loading data from file
  observeEvent(input$uploadData, {
    values$check_fail = NULL
    print('data upload')
    values$data_dl = 'input'
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
  ##############################
  
  
  ############ Code for checking input and providing feedback to user ###############
  observeEvent(c(input$uploadData,input$fetchURLData), {
    
    if(identical(values$input_case, "A") || identical(values$input_case, "B")) {
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
    }
  }, ignoreInit = T)
  
  # Code for updating grouping variable selection boxes
  observeEvent(values$inData, {
    static_vs_toxic_params = c("concentration", "cell_count", "cell_count__ctrl", 
                               "cell_count__time0", "dead_count", "dead_count__ctrl",
                               "dead_count__time0")
    caseA_params = c('concentration', 'cell_count', 'cell_count__ctrl')
    caseB_params = c('concentration', 'cell_count', 'time')
    time0_param = 'cell_count__time0'
    div_params = c('treatment_duration', 'division_time')
    if(identical(values$input_case, "A")) {
        delete_cols = which(colnames(values$inData) %in% c(caseA_params, time0_param, div_params))
        updateSelectizeInput(
          session, 'groupingVars',
          choices = colnames(values$inData)[-delete_cols],
          selected = colnames(values$inData)[-delete_cols],
          options = c()
        )
    }
    if(identical(values$input_case, "B")) {
      delete_cols = which(colnames(values$inData) %in% c(caseB_params, div_params))
      updateSelectizeInput(
        session, 'groupingVars',
        choices = colnames(values$inData)[-delete_cols],
        selected = colnames(values$inData)[-delete_cols],
        options = c()
      )
    }
    if(identical(values$input_case, "static_vs_toxic")) {
      delete_cols = which(colnames(values$inData) %in% static_vs_toxic_params)
      updateSelectizeInput(
        session, 'groupingVars',
        choices = colnames(values$inData)[-delete_cols],
        selected = colnames(values$inData)[-delete_cols],
        options = c()
      )
    }
  })
  ################################
  
  ############## old download button code ##########
  
#========== Download button data tables
  files_all = list()
  observeEvent(input$dl_output_button, {
    if(identical(values$input_case, "A") || identical(values$input_case, "B")) {
      GR = values$tables$assays$GR
      trad = values$tables$assays$rel_cell
      files_all <<- list(
        input_data = values$inData,
        GR_table = values$GR_table,
        GR_sigmoid_normal = GR$sigmoid$normal, 
        GR_sigmoid_low = GR$sigmoid$low,
        GR_sigmoid_high = GR$sigmoid$high,
        GR_biphasic_normal = GR$biphasic$normal,
        trad_sigmoid_normal = trad$sigmoid$normal, 
        trad_sigmoid_low = trad$sigmoid$low,
        trad_sigmoid_high = trad$sigmoid$high,
        trad_biphasic_normal = trad$biphasic$normal
      )
    }
    if(identical(values$input_case, "static_vs_toxic")) {
      GR = values$tables$assays$GR
      files_all <<- list(
        input_data = values$inData,
        GR_table = values$GR_table,
        GR_static = GR$static,
        GR_toxic = GR$toxic
      )
    }
    print("files for download:")
    print(names(files_all))
    
    output$dl_output_tables <<- downloadHandler(
      filename = function() {
        return(paste0("GRdata_", format(Sys.time(), "%Y%m%d_%I%M%S"),
                      ".zip", sep = ""))
      },
      content = function(con) {
        zipped_csv(df_list = files_all, zippedfile = con, 
                   filenames = names(files_all), stamp = format(Sys.time(), "%Y%m%d_%I%M%S") )
      }, contentType = "application/zip"
    )
    jsinject <- "setTimeout(function(){window.open($('#dl_output_tables').attr('href'))}, 100);"
    session$sendCustomMessage(type = 'jsCode', list(value = jsinject))     
  })


#========== Download buttons for DRC plots
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

 
#========== Download button for scatterplot images
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
######################

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
  ##################
    
######## Run GRfit when analyze button is clicked ########
  observeEvent(input$analyzeButton, {
    #df_full <<- NULL
    #all_inputs <- names(input)
    values$tables <- try(GRfit(values$inData, input$groupingVars, force = input$force, cap = input$cap, case = values$input_case))
    if(class(values$tables)!="try-error") {
      values$parameters_all = GRgetMetrics(values$tables)
      values$parameter_table = values$parameters_all$GR$sigmoid$normal
      values$parameter_table_show = values$parameter_table
      values$GR_table = GRgetValues(values$tables)
      values$GR_table_show = prettyNumTable(values$GR_table)
      ######### edit parameter tables ##########
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
      ###############
    } else {
      # When the GRfit function fails for some reason
      err = attributes(values$tables)$condition
      output$input_error = renderText(paste("There was an error in the GR value calculation. Please check that your data is in the correct form.", "\n", err))
    }
  }, ignoreInit = T, ignoreNULL = T, priority = 100)
  ###############

  #### allow large files to be uploaded
  options(shiny.maxRequestSize=100*1024^2)
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    graphics.off()
    print('devices off')
  })
  
})
