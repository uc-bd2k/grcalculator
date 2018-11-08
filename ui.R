library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)
library(ggplot2)
library(shinyLi)
library(formattable)


shinyUI(
  fluidPage(
    #adding head section to html with links to CSS files
    tags$head(
      tags$link(href="css/ilincs.css",rel="stylesheet"),
      tags$link(href="css/dose_response_grid.css",rel="stylesheet")
    ),
    tags$head(tags$style(".leftColWidth{max-width: 225px;}")),
    useShinyjs(),
    #displaying header
    includeHTML("www/html/nav.html"),
    # side column
    column(2, class="leftColWidth",
           img(src = "images/GRcalculator-logo.jpg", width = "100%"),
           wellPanel(
             bsModal("importDialog1", "", "importData", size = "large",
                     fluidRow(column(12,
                      p("Step 1: Choose GR calculation method"),
                      div(class = "btn-group",
                      bsButton('initialCellCount', label = "Initial cell counts (default)",
                              style = "primary"),
                      bsButton('divisionRate', label = "Cell division times", style = "primary")))),
                     br(),
                     fluidRow(column(12,
                                     shinyjs::hidden(
                      p("Use cell line division times (instead of initial cell count) to calculate GR values.", id = "div_rate_desc"),
                      p("Use initial (Time 0) cell counts - the measure of cell number in untreated wells grown in parallel until the time of treatment - for GR value calculation.", id = "init_count_desc")
                     ))),br(),
                     fluidRow(column(12,
                              shinyjs::hidden(
                                div(class = "btn-group", `data-toggle` = "buttons", id = "case_buttons",
                                    p("Step 2: Choose input file format"),
                                    bsButton("caseA", label = "Case A (multiple cell counts per row)", value = "caseA", style = "primary"),
                                    bsButton("caseC", label = "Case B (one cell count per row)", value = "caseC", style = "primary")
                                    )
                              )
                              )
                     ),
                     fluidRow(
                       column(12,
                                     shinyjs::hidden(
                                       div(id = "caseA_div_desc",
                                           includeMarkdown("www/caseA_div.md")
                                       ),
                                       div(id = "caseA_initial_desc",
                                           includeMarkdown("www/caseA_initial.md")
                                       ),
                                       div(id = "caseC_initial_desc",
                                           includeMarkdown("www/caseC_initial.md")
                                       ),
                                       div(id = "caseC_div_desc",
                                           includeMarkdown("www/caseC_div.md")
                                       )
                                     ))),br(),
                     fluidRow(column(12,
                                     shinyjs::hidden(
                                       
                                       div(class = "btn-group", `data-toggle` = "buttons", id = "comma_tab_buttons",
                                           p("Step 3: Select file type"),
                                           bsButton("comma_input", label = "comma-separated (.csv)", value = "comma", style = "primary"),
                                           bsButton("tab_input", label = "tab-separated (.tsv)", value = "tab", style = "primary"))))
                     ), br(),shinyjs::hidden(
                       div(id = 'upload_button', p("Step 4: Upload data file"),
                     fluidRow(column(6, p('From your computer:'),
                     fileInput('uploadData', '', multiple = FALSE, accept = NULL, width = NULL),
                     # The following tag allows for the same file path to be used twice in a row for upload
                     tags$script('$( "#uploadData" ).on( "click", function() { this.value = null; });'),
                     tags$style(type='text/css', "#uploadData {width: 80px}")),
                     column(6,
                     p("Or from a URL:"),
                     textInput("url", ""),
                     actionButton("fetchURLData", "Fetch Data")
                     )))), br(),
                     fluidRow(column(12,
                                     shinyjs::hidden(
                                       div(id = 'advanced_input',
                                     actionButton('import_options', 'Advanced Options'),
                                     conditionalPanel(
                                       condition = "input.import_options % 2 == 1",
                                       checkboxInput('euro_in', "Input with commas as decimal points", value = F)
                                     )))
                                     ))
                     ),
           #   bsModal("importDialog4", "Import Data", "",
           #      tags$script('Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {
           #            var el = $("#" + x);
           #            el.replaceWith(el = el.clone(true));
           #            var id = "#" + x + "_progress";
           #            $(id).css("visibility", "hidden");
           #            });
           #            '),
           #      br(),
           #      fluidRow(
           #        column(6,
           #          fileInput('uploadData', 'Open data file (.tsv, .csv)', multiple = FALSE, accept = NULL, width = NULL),
           #          # The following tag allows for the same file path to be used twice in a row for upload
           #          tags$script('$( "#uploadData" ).on( "click", function() { this.value = null; });'),
           #          tags$style(type='text/css', "#uploadData {width: 80px}"),
           #          textInput("url", "URL data file (.tsv, .csv)"),
           #          actionButton("fetchURLData", "Fetch Data"),
                    # wellPanel(
                    #   actionButton('import_options', 'Advanced Options'),
                    #   conditionalPanel(
                    #     condition = "input.import_options % 2 == 1",
                    #     checkboxInput('euro_in', "Input with commas as decimal points", value = F)
                    #   )
                    # )
           #          )
           #      )
           # ),
           bsModal("import_fail", "File formatting issues", "do_not_open",
                   fluidRow(column(12,
                p("Your input file is not in the correct format."),
                p("Please fix the following issues and re-upload your file."))),
                fluidRow(column(12,
                         formattableOutput('input_check'))
                ),
                fluidRow(column(12,
                         formattableOutput('input_check2'))
                ),
                fluidRow(column(12,
                         htmlOutput('col_suggest'))
                )
                   ),
           bsModal("importDialog_div", "Add division times and assay duration", "do_not_open",
                   fluidRow(column(12,
                                   p("Type the length of treatment below. (Do not include units)"),
                                   textInput('treatment_duration', '',width = '200px')
                                   )),
                   fluidRow(column(12,
                    p("Type or copy/paste the time it takes for each (untreated) cell line to undergo one division."),
                    p("Each line should contain one number (no units), in the order listed.")
                                   )),
                   fluidRow(
                     column(6,
                            tableOutput('cell_lines')
                     ),
                     column(6,
                            textAreaInput('div_rate','Division times', height = "100px",
                                          placeholder = "23.1 \n13.2 \n...\netc.")
                     )
                   ),
                   fluidRow(column(12,
                    p("Note that although units (hours, days, etc.) are not included above, the division times and duration of treatment must all be measured in the same units."),
                   actionButton('div_rate_input', "Submit"))
                   )
           ),
           tags$div(
             actionButton('importData', 'Import data file'),
             tags$hr(),
             actionLink('examples', "Load Example")
           ),
           bsModal('loadExamples', "Load Example", "examples",
                   actionButton('loadExample', 'Load Example (Case A)'),
                   span("control values assigned to treated measurements"),
                   br(),
                   actionButton('loadExampleC', 'Load Example (Case B)'),
                   span("control values stacked with treated measurements")
                   ),
           tags$style(type='text/css', "#loadExampleC { margin-top: 10px;}")
        ),
        br(),
        shinyjs::hidden(
        div(id = 'advanced_analysis',
        actionButton('advanced', 'Advanced options'),
        tags$style(type='text/css', "#advanced { margin: 10px;}"),
        conditionalPanel(
          condition = "input.advanced % 2 == 1",
          checkboxInput('cap', "Cap GR values below 1", value = F),
          checkboxInput('force', "Force sigmoidal fit", value = F)
        ),
          wellPanel(
            selectizeInput('groupingVars', 'Select grouping variables', choices = c(), multiple = TRUE),
            actionButton("analyzeButton", "Analyze")
          )
        ))
        # conditionalPanel(
        #   condition = "output.fileUploaded",
        #   actionButton('advanced', 'Advanced options')
        # ),
        # tags$style(type='text/css', "#advanced { margin: 10px;}"),
        # conditionalPanel(
        #   condition = "input.advanced % 2 == 1",
        #   checkboxInput('cap', "Cap GR values below 1", value = F),
        #   checkboxInput('force', "Force sigmoidal fit", value = F)
        # ),
        # conditionalPanel(
        #   condition = "output.fileUploaded",
        #   wellPanel(
        #     selectizeInput('groupingVars', 'Select grouping variables', choices = c(), multiple = TRUE),
        #     actionButton("analyzeButton", "Analyze")
        #   )
        # )
    ),
    # main column
    column(10,
           # Opacity transitions keep the "working" indicator hidden unless the
           # server is busy for more than 0.5s (otherwise the indicator is
           # blinking all the time).
           br(),
           tags$style("
             .nav-tabs  li  a { font-size:14px; padding:10px 20px 10px 20px; }
             #busy-working {
               opacity: 0;
                       transition: opacity .1s linear 0s;
               -webkit-transition: opacity .1s linear 0s;
             }
             html.shiny-busy #busy-working {
               opacity: 1;
                       transition: opacity .1s linear .5s;
               -webkit-transition: opacity .1s linear .5s;
             }
           "),
           tags$div(id="busy-working", class="btn btn-warning",
                    style="position: absolute; top: -40px; cursor: inherit;",
                    "Working..."),
           tabsetPanel(id = "tabs",
                       tabPanel(value="tab-starting",
                                "Getting Started",
                                                            # Hide second and subsequent tabs as soon as possible. Hiding the
                                                            # 'a' elements replicates the behavior of toggle() on the server side
                                                            # so that later calls to toggle() will properly unhide the tabs.
                                                            tags$script('$("#tabs li a[data-value!=tab-starting]").hide();'),
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
    					                 # fluidRow(
    					                 # column(3,
    					                 # formattableOutput('input_check', width = "300px")
    					                 # ),
    					                 # column(1),
    					                 # column(3,
    					                 # formattableOutput('input_check2', width = "300px")
    					                 # ),
    					                 # column(3,
    					                 # htmlOutput('col_suggest')
    					                 # )
    					                 # ),
                               tags$head(tags$style("#input_table  {white-space: nowrap;  }")),
                               DT::dataTableOutput("input_table")
    					         ),
                       # Dose-response curve tab
                       tabPanel(value="tab-drc",
                                "Dose-Response by Condition",
                                fluidRow(
                                  # column(6,
                                  #        wellPanel(
                                  #           tags$style(type='text/css', "#plot_options { margin-bottom: 0px; margin-top: 0px;}"),
                                  #           radioButtons("plot_options", label = NULL, choices = list("Data Points" = 1, "Fitted Curves" = 2, "Both" = 3), selected = 3, inline = T)
                                  #           , style = "padding-top: 5px;margin-top: 10px; padding-bottom:5px; margin-bottom: 0px;")
                                  #        ),
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
                                column(2,
                                       fluidRow(
                                  
     selectizeInput("drc2_metric", label = "Metric", choices = list(GR ="GR", `Relative cell count` = "rel_cell")),
     selectizeInput("drc2_curves", label = "Curves", choices = c("fit", "line", "none") )
     ),
     fluidRow(
     selectizeInput("drc2_points", label = "Points", choices = c("average", "all", "none") ),
     selectizeInput("drc2_bars", label = "Error bars", choices = c("none", "sd", "se") )
     ),
     fluidRow(
     selectizeInput("drc2_xrug", label = "x-axis rug", choices = c("none", "GR50", "GEC50", "IC50", "EC50") ),
     selectizeInput("drc2_yrug", label = "y-axis rug", choices = c("none", "GRinf", "GRmax", "Einf", "Emax") ),
     selectizeInput("drc2_facet", label = "Facet variable", choices = "none"),
     selectizeInput("drc2_plot_type", label = "Static or interactive plot", choices = c("interactive", "static")),
                                         uiOutput("ui"))
     ),
                                  column(9,uiOutput("plot.ui"))
                                  
                       ),
                       # Dose-response grid tab
                       tabPanel(value="tab-drc-grid",
                                "Dose-Response Grid",
                                fluidRow(
                                  column(3, radioButtons("curve_type_grid", label = "Curve type", choices = c("GR","Relative cell count"), inline = F)),
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


