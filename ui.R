library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)
library(ggplot2)
library(shinyLi)


shinyUI(
  fluidPage(
    #adding head section to html with links to CSS files
    tags$head(
      tags$link(href="css/ilincs.css",rel="stylesheet")
    ),
    useShinyjs(),
    #displaying header
    includeHTML("www/html/nav.html"),
    titlePanel("GR Calculator"),
    # side column
    column(2,
           br(),
           wellPanel(
             tags$script('Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {   
                      var el = $("#" + x);
                      el.replaceWith(el = el.clone(true));
                      var id = "#" + x + "_progress";     
                      $(id).css("visibility", "hidden");
                      });
                      '),
             fileInput('uploadData', 'Open data file (.tsv, .csv)', multiple = FALSE, accept = NULL, width = NULL),
             tags$style(type='text/css', "#uploadData {width: 80px}"),
             radioButtons('sep', 'Separator',
                          c(Comma=',',
                          Tab='\t'),
                          selected = ',', inline = F),
              actionLink('loadExample', 'Load Example'),
              hr(),
              conditionalPanel(
                condition = "output.fileUploaded",
                selectizeInput('groupingVars', 'Select grouping variables', choices = c(), multiple = TRUE),
                actionButton("analyzeButton", "Analyze")
              )
           )
    ),
    # main column
    column(10,
           tags$style(".nav-tabs  li  a {font-size:14px; padding:10px 20px 10px 20px;} "),
           tabsetPanel(id = "tabs",
                       tabPanel(value="tab-starting",
                                "Getting Started",
                   					    tags$div(tags$link(href="css/AboutGRMetrics.css",rel="stylesheet"),
                   					             includeHTML("www/GRCGettingStarted.html")),
                   					    conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div(class="bigdiv",tags$img(src="gif-loading.gif",width=800,height=426)))
                       ),
                       # Data Tables tab
                       tabPanel(value="tab-data",
    					                  "Data Tables",
         					              textOutput(outputId = 'input_error'),
         					              tags$head(tags$style("#input_error{color: red; font-size: 20px; }")),
    					                  fluidRow(column(6,radioButtons(inputId = "pick_data", label = "", choices = list("Input Data" = 1, "GR Values" = 2, "Fitted Parameters" = 3), selected = 1, inline = T)),
                  				      column(3, downloadButton('downloadData', 'Download data table'),
                         		    tags$style(type='text/css', "#downloadData { width:100%; margin-top: 10px; margin-bottom: 20px;}")),
                  				      radioButtons('download_type', label = "", choices = c("csv", "tsv"), inline = T)
    					                  ),
    					                  tags$head(tags$style("#input_table  {white-space: nowrap;  }")),
    					                  DT::dataTableOutput("input_table")
    					                  ),
                       # Dose response curve tab
                       tabPanel(value="tab-drc",
                                "Dose Response by Condition",
                                fluidRow(
                                  column(4,
                                         radioButtons("plot_options", label = "", choices = list("Data Points" = 1, "Fitted Curves" = 2, "Both" = 3), selected = 3, inline = T)),
                                  column(2,
                                         downloadButton('downloadDRC', label = "Download image")),   
                                  column(1,
                                  	     radioButtons('drcImageType', label = '', choices = c('.eps', '.tiff')), inline = T),             
                                  column(1, offset=1, tags$div(id='plotBoxL1',"Plot height")),
                                  column(2, textInput('height', NULL, value = 700)),
                                  tags$style(type='text/css', "#height { width:50px; margin-top: 20px; margin-left: 0px; margin-right: 0px;}"),
                                  tags$style(type='text/css', "#plotBoxL1 { white-space: nowrap; margin-top: 25px; margin-left: 0px; margin-right: 0px;}"),
                                  tags$style(type='text/css', "#drcImageType { margin-top: 0px; margin-bottom: 30px;margin-right: 0px;margin-left: 0px;}"),
                                  tags$style(type='text/css', "#downloadDRC { margin-top: 20px; margin-bottom: 0px;}")),
                                fluidRow(
                                  column(2,uiOutput("ui")),
                                  column(10,uiOutput("plot.ui"))
                                  )
                       ),
                       # Dose-response grid tab
                       tabPanel(value="tab-drc-grid",
                                "Dose Response Grid",
                                fluidRow(
                                  column(3, selectizeInput('choiceVar', 'Choose selector variable', choices = NULL)),
                                  column(3, selectizeInput('xgroupingVars', 'Choose grid variables', choices = NULL, multiple = TRUE)),
                                  column(3, br(), actionButton('plot_gr50grid', 'Plot')),
                                   width=12
                                ),
                                fluidRow(
                                  
                                  liDoseResponseGrid("dose-response-grid-main"),
                                  bsModal("graphPopup", "Graph Popup", "triggerGraphPopup",
                                          size = "large",
                                          plotlyOutput("graphPopupPlot")),
                                  width=12
                                )
                       ),
                       # GR
                       tabPanel(value="tab-gr-metric",
                                "GR Metric Comparison",
                                fluidRow(
                                  column(2, selectInput('pick_parameter', 'Select parameter', choices = c('GR50', 'GRmax', 'GRinf', 'Hill', 'GR_AOC'))),
                                  column(2, offset=2, downloadButton('downloadScatter', label = "Download image")),
                                  column(1,radioButtons('scatterImageType', label = '', choices = c('.eps', '.tiff')), inline = T),
                                  tags$style(type='text/css', "#downloadScatter { margin-top: 20px; margin-bottom: 0px; margin-left: 0px; margin-right: 0px}"),
                                  tags$style(type='text/css', "#scatterImageType { margin-top: 0px; margin-bottom: 20px; margin-left: 0px}"),
                                  column(1,offset=1, tags$div(id='plotBoxL2',"Plot height")),
                                  column(2, textInput('scatter_height', NULL, value = 700)),
                                  tags$style(type='text/css', "#plotBoxL2 { white-space: nowrap; margin-top: 25px; margin-left: 10px; margin-right: 0px}"),
                                  tags$style(type='text/css', "#scatter_height { width:50px; margin-top: 20px; margin-left: 20px; margin-right: 0px}"),
                                  tags$style(type='text/css', "#clear { margin-top: 10px; margin-bottom: 10px; float: center;}"),
                                  tags$style(type='text/css', "#plot_scatter { margin-top: 10px; margin-bottom: 10px; float: center}")
                                  ),
                                fluidRow(
                                  column(2,
                                         selectInput('pick_var', 'Select variable', choices = c()),
                                         selectInput('x_scatter', 'Select x-axis value', choices = c()),
                                         selectizeInput('y_scatter', 'Select y-axis value', choices = c()),
                                         bsButton('plot_scatter', 'Add', size = 'small'),
                                         bsButton('clear', 'Clear', size = 'small')
                                  ),
                                  column(10, uiOutput("scatter1.ui"))
                                )
                       )
           )
    )
  )
)


