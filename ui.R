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
    tags$head(tags$style(".leftColWidth{max-width: 225px;}")),
    useShinyjs(),
    #displaying header
    includeHTML("www/html/nav.html"),
    titlePanel("GR Calculator"),
    # side column
    column(2, class="leftColWidth",
           br(),
           wellPanel(
             bsModal("importDialog", "Import Data", "importData",
                tags$script('Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {   
                      var el = $("#" + x);
                      el.replaceWith(el = el.clone(true));
                      var id = "#" + x + "_progress";     
                      $(id).css("visibility", "hidden");
                      });
                      '),
                br(),
                fluidRow(
                  column(6,
                    fileInput('uploadData', 'Open data file (.tsv, .csv)', multiple = FALSE, accept = NULL, width = NULL),
                    tags$style(type='text/css', "#uploadData {width: 80px}"),
                    wellPanel(
                      radioButtons('sep', 'Separator',
                                   c(Comma=',',Tab='\t'), selected = ',', inline = T),
                      checkboxInput('euro_in', "Input with commas as decimal points", value = F)
                    )
                    ),
                  column(6,
                         textInput("url", "URL data file (.tsv, .csv)"),
                         actionButton("fetchURLData", "Fetch Data")
                  )
                )
           ),
           tags$div(
             actionButton('importData', 'Open data file'),
             tags$hr(),
             actionLink('examples', "Load Example")
           ),
           bsModal('loadExamples', "Load Example", "examples",
                   actionButton('loadExample', 'Load Example (Case A)'),br(),
                   actionButton('loadExampleC', 'Load Example (Case C)')
                   ),
           tags$style(type='text/css', "#loadExampleC { margin-top: 10px;}")
        ),
        conditionalPanel(
          condition = "output.fileUploaded",
          actionButton('advanced', 'Advanced options')
        ),
        tags$style(type='text/css', "#advanced { margin: 10px;}"),
        conditionalPanel(
          condition = "input.advanced % 2 == 1",
          checkboxInput('cap', "Cap GR values below 1", value = F),
          checkboxInput('force', "Force sigmoidal fit", value = F)
        ),
        conditionalPanel(
          condition = "output.fileUploaded",
          wellPanel(
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
                   					    includeHTML("www/GettingStartedRMD.html")
                   					    ),
                   					    conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div(class="bigdiv",tags$img(src="gif-loading.gif",width=800,height=426)))
                       ),
                       # Data Tables tab
                       tabPanel(value="tab-data",
    					                  "Data Tables",
         					              textOutput(outputId = 'input_error'),
         					              tags$head(tags$style("#input_error{color: red; font-size: 20px; }")),
    					                  fluidRow(
    					                    column(6,
    					                       wellPanel(
    					                           tags$style(type='text/css', "#pick_data { margin-bottom: 0px; margin-top: 0px;}"),
    					                           radioButtons(inputId = "pick_data", label = NULL, choices = list("Input Data" = 1, "GR Values" = 2, "Fitted Parameters" = 3), selected = 1, inline = T)
    					                           , style = "padding-top: 5px;margin-top: 10px; padding-bottom:5px; margin-bottom: 0px;")
    					                       ),
                  				      column(3, 
                  				             tags$style(type='text/css', "#shareData { width:200px; margin-top: 10px; margin-bottom: 20px;}"),
                  				             actionButton("shareData", "Download Data File"),
                  				             bsModal("shareDataDialog", "", "shareData",
                  				                     fluidRow(
                  				                       column(6,
                  				                downloadButton('downloadData', 'Download data table'),
                         		              tags$style(type='text/css', "#downloadData { width:200px; margin-top: 10px; margin-bottom: 20px;}")
                  				                ,
                  				                radioButtons('download_type', label = "", choices = c("csv", "tsv"), inline = T)
                  				                ),
                  				                column(6,
                  				                checkboxInput('euro_out', 'Export commas as decimal points', value = F))
    					                            # hr(),
    					                            # actionButton("generateURL", "Generate URL to Share"),
    					                            # textInput("prepopulatedURL", label = "", value = "")
                  				                     )
                                       )
                  				          )
                  				      ),
                               tags$head(tags$style("#input_table  {white-space: nowrap;  }")),
                               DT::dataTableOutput("input_table")
    					         ),
                       # Dose response curve tab
                       tabPanel(value="tab-drc",
                                "Dose Response by Condition",
                                fluidRow(
                                  column(6,
                                         wellPanel(
                                            tags$style(type='text/css', "#plot_options { margin-bottom: 0px; margin-top: 0px;}"),
                                            radioButtons("plot_options", label = NULL, choices = list("Data Points" = 1, "Fitted Curves" = 2, "Both" = 3), selected = 3, inline = T)
                                            , style = "padding-top: 5px;margin-top: 10px; padding-bottom:5px; margin-bottom: 0px;")
                                         ),
                                  column(2,
                                         tags$style(type='text/css', "#shareImageData { margin-top: 10px; margin-bottom: 20px;}"),
                                         actionButton("shareImageData", "Download Image File"),
                                         bsModal("shareImageDataDialog", "", "shareImageData",
                                            fluidRow(
                                              column(4,
                                                downloadButton('downloadDRC', label = "Download image")),
                                              column(4,
                                                radioButtons('drcImageType', label = '', choices = c('.pdf', '.tiff'),
                                                             inline = F)
                                              #   hr(),
                                              #   actionButton("generateURL2", "Generate URL to Share"),
                                              #   textInput("prepopulatedURL2", label = "", value = "")
                                              )
                                            )
                                         )
                                  ),
                                  column(1, offset=1, tags$div(id='plotBoxL1',"Plot height")),
                                  column(2, textInput('height', NULL, value = 500)),
                                  tags$style(type='text/css', "#height { width:50px; margin-top: 20px; margin-left: 0px; margin-right: 0px;}"),
                                  tags$style(type='text/css', "#plotBoxL1 { white-space: nowrap; margin-top: 25px; margin-left: 0px; margin-right: 0px;}"),
                                  tags$style(type='text/css', "#drcImageType { margin-top: 0px; margin-bottom: 30px;margin-right: 0px;margin-left: 0px;}"),
                                  tags$style(type='text/css', "#downloadDRC { margin-top: 20px; margin-bottom: 0px;}")),
                                fluidRow(
                                  column(2,
                                         radioButtons("curve_type", label = "Curve type", choices = c("GR","IC"), inline = TRUE),
                                         uiOutput("ui")),
                                  column(10,uiOutput("plot.ui"))
                                  )
                       ),
                       # Dose-response grid tab
                       tabPanel(value="tab-drc-grid",
                                "Dose Response Grid",
                                fluidRow(
                                  column(3, radioButtons("curve_type_grid", label = "Curve type", choices = c("GR","IC"), inline = TRUE)),
                                  column(3, selectizeInput('choiceVar', 'Choose selector variable', choices = NULL)),
                                  column(3, selectizeInput('xgroupingVars', 'Choose grid variables', choices = NULL, multiple = TRUE)),
                                  #column(3, br(), actionButton('plot_gr50grid', 'Plot')),
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
                       # Boxplot and scatterplot tab
                       tabPanel(value="tab-gr-metric",
                                "GR Metric Comparison",
                                fluidRow(
                                  column(4, 
                                    wellPanel(
                                      tags$style(type='text/css', "#box_scatter { margin-bottom: 0px; margin-top: 0px;}"),
                                      radioButtons('box_scatter', label = NULL, choices = c("Box plot", "Scatter plot"), inline = T)
                                      , style = "padding-top: 5px;margin-top: 10px; padding-bottom:5px; margin-bottom: 0px;")
                                    ),
                                  column(2, # offset=2, 
                                         tags$style(type='text/css', "#shareImageData2 { margin-top: 10px; margin-bottom: 20px;}"),
                                         actionButton("shareImageData2", "Download Image File"),
                                         bsModal("shareImageDataDialog2", "", "shareImageData2",
                                            fluidRow(
                                                column(4,
                                                   downloadButton('downloadScatter', label = "Download image")),
                                                column(4,
                                                   radioButtons('scatterImageType', label = '', choices = c('.pdf', '.tiff')),
                                                   tags$style(type='text/css', "#downloadScatter { margin-top: 20px; margin-bottom: 0px; margin-left: 0px; margin-right: 0px}"),
                                                   tags$style(type='text/css', "#scatterImageType { margin-top: 0px; margin-bottom: 20px; margin-left: 0px}")
                                                   # hr(),
                                                   # actionButton("generateURL3", "Generate URL to Share"),
                                                   # textInput("prepopulatedURL3", label = "", value = "")
                                                   )
                                                )
                                            )
                                         ),
                                  
                                  column(1,offset=1, tags$div(id='plotBoxL2',"Plot height")),
                                  column(2, textInput('scatter_height', NULL, value = 700)),
                                  tags$style(type='text/css', "#plotBoxL2 { white-space: nowrap; margin-top: 25px; margin-left: 10px; margin-right: 0px}"),
                                  tags$style(type='text/css', "#scatter_height { width:50px; margin-top: 20px; margin-left: 20px; margin-right: 0px}"),
                                  tags$style(type='text/css', "#clear { margin-top: 10px; margin-bottom: 10px; float: center;}"),
                                  tags$style(type='text/css', "#plot_scatter { margin-top: 10px; margin-bottom: 10px; float: center}")
                                  ),
                                  fluidRow(column(2,
                                                  uiOutput("scatter")),
                                           column(10, uiOutput("plot.ui2"))
                                  )
                       )
           )
    )
  )
)


