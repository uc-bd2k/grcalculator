library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)
library(ggplot2)
library(formattable)
library(shiny.semantic)
library(shinycssloaders)

#library(dplyr)
#library(readr)
library(DT)
#library(crosstalk)
#library(magrittr)
#library(markdown)
#library(clipr)
#library(rclipboard)
#library(aws.s3)
curve_choices = 1:8
names(curve_choices) = c("GR sigmoid normal", "GR sigmoid low", "GR sigmoid high", "GR biphasic",
                  "Traditional sigmoid normal", "Traditional sigmoid low", 
                  "Traditional sigmoid high", "Traditional biphasic")


shinyUI(
  semanticPage(
    title = "Online Dose-response GR Calculator",
    shinyjs::useShinyjs(),
    suppressDependencies("bootstrap"),
    # Fix for mobile viewing
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    # CSS for sizing of data table search boxes
    inlineCSS(".form-control {
              box-sizing: border-box;
              }"),
    # CSS for hiding border on horizontal segments
    tags$style(type = "text/css", "
               .ui.noshadow.segments {
               box-shadow: none;
               border: none;
               margin-top: 0px;
               margin-bottom: 0px;
               }"
    ),
    div(class = "ui small modal", id = "about_modal",
        div(class = "header", "About",
            div(class = "actions", style = "float: right; display: inline-block; vertical-align: top;",
                div(class = "ui red basic circular cancel icon button", uiicon(type = "window close"))
            )
        ),
        div(class = "ui center aligned basic segment",
            includeMarkdown("www/about.md")
        )
    ),
    div(class = "ui mini modal", id = "contact_modal",
        div(class = "header", "Contact us",
            div(class = "actions", style = "float: right; display: inline-block; vertical-align: top;",
                div(class = "ui red basic circular cancel icon button", uiicon(type = "window close"))
            )
        ),
        div(class = "ui center aligned basic segment",
            includeMarkdown("www/contact.md")
        )
    ),
    div(class = "ui small modal", id = "import_modal",
        div(class = "ui basic segment",
        div(class = "twelve wide column",
            p("Step 1: Choose GR calculation method"),
            div(class = "ui buttons",
                div(class = "ui positive button action-button", id = 'initialCellCount', "Initial cell counts (default)"),
                div(class = "or"),
                div(class = "ui button action-button", id = 'divisionRate', "Cell division times")
            )
        ),
        br(),
        div(class = "twelve wide column",
            shinyjs::hidden(
              p("Use cell line division times (instead of initial cell count) to calculate GR values.", id = "div_rate_desc"),
              p("Use initial (Time 0) cell counts - the measure of cell number in untreated wells grown in parallel until the time of treatment - for GR value calculation.", id = "init_count_desc")
            )
        ),br(),
        shinyjs::hidden( 
        div(class = "twelve wide column", `data-toggle` = "buttons", id = "case_buttons",
            p("Step 2: Choose input file format"),
            div(class = "ui buttons",
            div(class = "ui positive button action-button", id = "caseA", "Case A (multiple cell counts per row)", value = "caseA"),
            div(class = "or"),
            div(class = "ui button action-button", id = "caseB", "Case B (one cell count per row)", 
                value = "caseB")
            )
        )
      ),
        div(class = "twelve wide column",
         shinyjs::hidden(
           div(id = "caseA_div_desc",
               includeMarkdown("www/caseA_div.md")
           ),
           div(id = "caseA_initial_desc",
               includeMarkdown("www/caseA_initial.md")
           ),
           div(id = "caseB_initial_desc",
               includeMarkdown("www/caseB_initial.md")
           ),
           div(id = "caseB_div_desc",
               includeMarkdown("www/caseB_div.md")
           )
          )
         ),br(),
        shinyjs::hidden(
        div(class = "twelve wide column", `data-toggle` = "buttons", id = "comma_tab_buttons",
            p("Step 3: Select file type"),
            div(class = "ui buttons",
                div(class = "ui positive button action-button", id = "comma_input",
                    "comma-separated (.csv)", value = "comma"),
                div(class = "or"),
                div(class = "ui button action-button", id = "tab_input", 
                    "tab-separated (.tsv)", value = "tab")
            )
          )
        ), br(),
      shinyjs::hidden(
        div(id = 'upload_button', p("Step 4: Upload data file"),
      div(class = "ui two column grid",
      div(class = "row",
        div(class = "six wide column",
          tags$b('From your computer:'),
          br(), br(),
          tags$input(type="file", id = "uploadData"),
          #fileInput('uploadData', "", multiple = FALSE, accept = NULL, width = NULL),
          # The following tag allows for the same file path to be used twice in a row for upload
          tags$script('$( "#uploadData" ).on( "click", function() { this.value = null; });')
        ),
        div(class = "ten wide column",
          div(class = "ui form", style = "width: 200px; display: inline-block;",
              div(class = "field",
                  tags$label("Or from a URL:"),
                  tags$input(type = "text", id = "url")
              )
          ),
          div(class = "ui button action-button" , id = "fetchURLData", "Fetch Data",
              style="width: 120px; vertical-align: bottom; display: inline-block;")
        )
      )
      ))), br(),
      div(class = "twelve wide column",
      shinyjs::hidden(
        div(id = 'advanced_input',
          div(class = "ui button action-button", id = 'import_options', 'Advanced Options'),
          conditionalPanel(
            condition = "input.import_options % 2 == 1",
            br(),
            div(class = "ui toggle checkbox", 
                tags$input(type = "checkbox", name = "public", 
                           id = 'euro_in', tags$label("Input with commas as decimal points"))
            )
          )
          )
        ) 
      )
      )
    ),
    div(class = "ui mini modal", id = 'example_modal', 
        div(class = "ui center aligned basic segment",
        #"Load Example", "examples",
        p("Case A: control values assigned to treated measurements"),
        p("Case B: control values stacked with treated measurements"),
            div(class = "ui buttons", "Load example:",
                div(class = "ui positive button action-button", id = 'loadExample',
                    'Case A'),
                div(class = "or"),
                div(class = "ui button action-button", id = 'loadExampleB', 'Case B')
        )
    )),
    div(class = "ui mini modal", id = 'start_modal', 
        div(class = "ui basic center aligned segment",
            div(class = "ui buttons", "Load example:", style = "vertical-align: middle;",
                div(class = "ui positive button action-button", id = 'example_button',
                    'Load example data'),
                div(class = "or"),
                div(class = "ui button action-button", id = 'import_button', 'Import data')
            )
        )),
    div(class = "ui small modal", id = 'instructions_modal', 
        div(class = "ui basic segment",
            includeMarkdown("www/GettingStartedModal.md")
            )
        ),
    div(class = "ui small modal", id = "download_plot_drc_modal", 
      div(class = "ui basic segment",
        downloadButton('downloadDRC', label = "Download image"),
        radioButtons('drcImageType', label = '', choices = c('.pdf', '.tiff'), inline = F)
        )
    ),
    #div(class = "ui container",
        div(class = "ui top attached inverted seven item stackable menu",
            div(class = "ui center aligned container",
                a(class = "item", img(class = "logo", src = "dcic.png"),
                  href = "http://lincs-dcic.org/"),
                a(class = "item", "Home", href = "/grtutorial/Home.html", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                a(class = "item", "About GR Metrics", href = "/grtutorial/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                a(class = "item", "Online GR Calculator", href = "/grcalculator/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                a(class = "item", "LINCS Dose-Response Datasets", href = "/grbrowser/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                a(class = "item", "Support", href = "/grtutorial/support.html", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                a(class = "item", img(class = "logo", src = "logo_harvard_150.png"),
                  href = "http://sorger.med.harvard.edu" )
            )
        ),
        div(class = "ui main basic segment",
          div(class="ui top basic secondary pointing menu", id = "tabs",
              a(class="active item", `data-tab`="first", "Getting Started"),
              shinyjs::hidden(
              a(class="item", `data-tab`="input", "Input data", id = "input_top"),
              a(class="item", `data-tab`="third", "Dose-Response by Condition", id = "drc_top"),
              a(class="item", `data-tab`="fourth", "GR Metric Comparison", id = "comparison_top"),
              a(class="item", `data-tab`="output_tables", "Output Tables", id = "output_top")
              )
          ),
          div(class="ui active bottom center basic tab segment", `data-tab`="first",
                  div(class = "ui basic segment",
                      tags$img(src = "images/GRcalculator-logo.jpg", width = "250px",
                               style = "float: right;"),
                      includeMarkdown("www/GettingStarted.md"),
                      div(class = "ui bottom attached buttons",
      tags$button(class="ui teal button action-button", "Start", id = "start_button"),
      tags$button(class = "ui grey button action-button", id = "instructions_button", "Instructions")
                      )
                  )
              ),
      div(class="ui bottom center basic tab segment", `data-tab`="input", id = "input_bottom",
      div(class = "ui basic center aligned segment",
          div(class = "ui three column grid",
              div(class = "four wide column"),
              div(class = "eight wide column",
              selectizeInput('groupingVars', 'Select grouping variables', choices = c(), multiple = TRUE, width = "100%"),
         div(class = "ui buttons",
              div(class = "ui bottom attached primary button action-button", 
                  id = "analyzeButton",
                  "Analyze",
                  div(class="ui dimmer", id = "analyze_loader",
                    div(class="ui loader"), "Analyze")
              ),
             div(class = "ui bottom attached button action-button", id = 'advanced', 'Advanced options')
         ), br(), br(),
             conditionalPanel(
               condition = "input.advanced % 2 == 1",
               div(class = "ui checkbox", 
                   tags$input(type = "checkbox", name = "public", 
                              id = 'cap', tags$label("Cap GR values below 1"))
               ),
               div(class = "ui checkbox", 
                   tags$input(type = "checkbox", name = "public", 
                              id = 'force', tags$label("Force sigmoidal fit"))
               )
             )
         ),
         div(class = "four wide column")
         )),
      div(class = "ui basic segment",
          tags$style(type='text/css', "#input_table { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;}"),
          DT::dataTableOutput("input_table") %>% withSpinner(type = 3, color = "#009999", color.background = "#ffffff")
      )
      ),
      
          div(class="ui bottom center basic tab segment", `data-tab`="output_tables", id = "output_tables_bottom",
              div(class="ui two top attached buttons",
                  # tags$button(class = "ui button action-button", id = "input_table_button", "Input data"),
                  # div(class = "or"),
                  tags$button(class = "ui active green button action-button", id = "gr_table_button", "GR values"),
                  div(class = "or"),
                  tags$button(class = "ui button action-button", id = "parameter_table_button", "Parameter values")
                  ),
              tags$style(type = "text/css", "#parameter_table_select { display: inline-block; text-align: center; }"),
              shinyjs::hidden(selectInput(inputId = "parameter_table_select", "Curve",
                                          choices = curve_choices,
                                          selected = 1
              )
              ),
              div(class = "ui basic center aligned segment", style = "min-height: 500px;",
                DT::dataTableOutput("gr_table") %>% withSpinner(type = 3, color = "#009999", color.background = "#ffffff"),
                shinyjs::hidden(DT::dataTableOutput("parameter_table")) %>% withSpinner(type = 3, color = "#009999", color.background = "#ffffff"),
                tags$style(type='text/css', "#gr_table { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;}"),
                tags$style(type='text/css', "#parameter_table { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;}")
                # div(class = "ui primary button action-button",
                #     downloadLink("download_button", "Download Data File", style = "color: white;")
                #     )
              )
          ),
          div(class="ui bottom center basic tab segment", `data-tab`="third",
            div(class = "ui basic segment", id = "drc_tabs",
              div(class = "ui black top attached button action-button","Plot options",id="plot_options_button"),
              shinyjs::hidden(
              div( id = "plot_options",
                div(class = "ui stackable three column grid",
                  div(class = "five wide column",
  selectizeInput("drc2_metric", label = "Metric", choices = list(GR ="GR", `Relative cell count` = "rel_cell")),
  selectizeInput("drc2_curves", label = "Curves", choices = c("sigmoid", "line", "biphasic", "sigmoid_high", "sigmoid_low", "none") ),
  selectizeInput("drc2_points", label = "Points", choices = c("average", "all", "none") )
),
                  div(class = "five wide column",
                      selectizeInput("drc2_bars", label = "Error bars", choices = c("none", "sd", "se") ),
                      selectizeInput("drc2_xrug", label = "x-axis rug", choices = c("none") ),
                      selectizeInput("drc2_yrug", label = "y-axis rug", choices = c("none") )
                  ),
                  div(class = "five wide column",
                      selectizeInput("drc2_color", label = "Color", choices = "none")#,
                      #selectizeInput("drc2_plot_type", label = "Static or interactive plot", choices = c("interactive", "static"))
                  )
                )
                )
                )
                ),
                div(class = "ui basic center aligned segment",
                  div(class = "ui three column center aligned grid",
                    div(class = "six wide column", style = "padding: 0px;",
                    # tags$head(tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: inline-block!important; }")),
                   # tags$style(type='text/css', 
                               #".selectize-control.single { vertical-align: middle; display: inline-block;}",
                    #           ".form-group { vertical-align: middle; display: inline-block;}"),
                    #p("Grid variables", style = "vertical-align: middle; display: inline-block;"),
                    selectizeInput("drc2_facet", label = "Grid variables", choices = "none", multiple = T)#,
                    #selectizeInput("nplots", label = "Number of plots per page", choices = c(5, 10, 25, 50), selected = 10)
                    ),
                    div(class = "four wide column",
                      div(class = "ui primary bottom attached button action-button",
                          id = "single_button", style = "width: 100%; display: inline-block;",
                          "Single plot"
                          ),
                      shinyjs::hidden(
                        div(class = "ui secondary bottom attached button action-button",
                            id = "grid_button", style = "width: 100%; display: inline-block;",
                            "Grid plot"
                        )#,
                        #sliderInput('height', label = "Plot size (pixels)", min = 200, max = 1000, step = 50, value = 500)
                      )
                        ),
                    div(class = "six wide column")
                  )
                ),
                  div(class = "ui two column grid",
                    div(class = "three wide column",
                      div(class = "ui medium header", "Show/hide curves"),
                      uiOutput("drc_filter"),
                      tags$button(class = "ui secondary bottom attached button action-button", id = "update_button", "Update Plot")
                        ),
                    div(class = "thirteen wide column",
                        div(class = "ui basic center aligned segment", id = "grid_segment",
                          div(class = "ui two column grid",
                            div(class = "twelve wide column",
                                uiOutput("plots_grid", class = "ui doubling four column grid",
                                         style = "min-height: 500px;")#,
                                #uiOutput("plots_grid_pages"),
                                #tags$button(class = "ui button action-button", id = "download_plot_drc_button", 
                                #            "Download Image File")
                            ),
                            div(class = "four wide column", id = "plots_grid_legend",
                                plotOutput("plots_grid_legend", height = "500px")
                            )
                        )
                        ),
                        shinyjs::hidden(
                          div(class = "ui basic center aligned segment", id = "single_segment",
                              tags$style(type='text/css', "#single_drc { display: inline-block }"),
                              div(class = "twelve wide column",
                                  plotlyOutput("single_drc", width = "800px", height = "500px") %>% withSpinner(type = 3, color = "#009999", color.background = "#ffffff")
                              )
                          )
                        )
                        )
                
          )
),
          div(class="ui bottom center basic tab segment", `data-tab`="fourth",
              div(class = "ui basic center aligned segment",
                  div(class = "ui primary bottom attached button action-button",
                      id = "scatter_button", style = "width: 50%; display: inline-block;",
                      "Scatterplot"
                  ),
                  shinyjs::hidden(
                    div(class = "ui secondary bottom attached button action-button",
                        id = "boxplot_button", style = "width: 50%; display: inline-block;",
                        "Boxplot"
                    )
                  ),
                  selectInput(inputId = "box_scatter_fit", "Select fit type",
                              choices = curve_choices,
                              selected = 1
                  )
              ),
              div(class = "ui two column center aligned grid",
                div(class = "four wide column",
          shinyjs::hidden(
                  div(id = "scatter_options",
        selectInput('pick_parameter', 'Select parameter', choices = c('GR50', 'GRmax', 'GRinf', 'h_GR', 'GR_AOC', 'IC50','Emax', 'Einf', 'h', 'AUC')),
        selectInput('pick_var', 'Select variable', choices = ""),
        selectInput('x_scatter', 'Select x-axis value', choices = ""),
        selectizeInput('y_scatter', 'Select y-axis value', choices = ""),
        bsButton('plot_scatter', 'Add', size = 'small'),
        bsButton('clear', 'Clear', size = 'small')
                  )
        ),
            div(id = "boxplot_options",
        selectInput('pick_box_y', 'Select parameter', choices = c('GR50', 'GRmax', 'GRinf', 'h_GR', 'GR_AOC', 'IC50','Emax', 'Einf', 'h', 'AUC')),
        selectInput('pick_box_x', 'Select grouping variable', choices = ""),
        selectInput('pick_box_point_color', 'Select additional point coloring', choices = ""),
        selectizeInput('pick_box_factors', 'Show/hide data', choices = c(), multiple = T),
        actionLink('wilcox_panel', 'Compare boxplots'),
        conditionalPanel(condition = "input.wilcox_panel%2==1",
                         selectizeInput('factorA', 'Wilcoxon rank-sum test', choices = c(), multiple = T),
                         selectizeInput('factorB', '', choices = c(), multiple = T),
                         #radioButtons('wilcox_method', label = "",choices = c("One-sided", "Two-sided"), selected = "Two-sided", inline = F),
                         textOutput("wilcox")
        )
          )
        ),
                div(class = "twelve wide column",
                      ### box/scatter plot
                  div(class = "ui basic center aligned segment",
  plotlyOutput('boxplot', height = "500px", width = "500px") %>% withSpinner(type = 3, color = "#009999", color.background = "#ffffff"),
  shinyjs::hidden(plotlyOutput("scatterplot", height = "500px", width = "500px") %>% withSpinner(type = 3, color = "#009999", color.background = "#ffffff"))
                  )
                )
              )
          )
        ),
        div(class = "ui bottom attached inverted footer segment", style = "margin: 0px;",
            div(class = "ui center aligned container",
                div(class = "ui horizontal inverted large divided link list",
                    a(class = "item", div(class = "action-button", "About", id = "about") ),
                    a(class = "item", div(class = "action-button", "Contact Us", id = "contact")),
                    a(class = "item", "Github", uiicon("github"), href = "https://github.com/uc-bd2k/grcalculator/")
                )
            )
        )
    )
    #)
  )