library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)
library(ggplot2)
library(formattable)
library(shiny.semantic)
library(shinycssloaders)
library(DT)

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
    
    #### about modal start #########
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
    ######## about modal end #########
    #### contact modal start #########
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
    ######## contact modal end #########
    #### import modal start #########
    div(class = "ui small modal", #id = "import_modal",
        div(class = "ui basic segment",
          # div(class = "twelve wide column",
          #   h3("Do you have live and dead cell counts?"),
          #     div(class = "ui buttons",
          #         div(class = "ui toggle button action-button", id = 'no_dead', "No"),
          #         div(class = "or"),
          #         div(class = "ui toggle button action-button", id = 'yes_dead', "Yes")
          #     )
          # ),
          # br(),
        #   shinyjs::hidden(
        #   div(class = "twelve wide column", id = "static_vs_toxic_req",
        #           includeMarkdown("www/static_vs_toxic_req.md")
        #   ) ),
        #   
        #   shinyjs::hidden(
        # div(class = "twelve wide column", `data-toggle` = "buttons", id = "calc_method_buttons",
        #       h3("Do you have initial (time 0) cell counts?"),
        #     div(class = "ui buttons",
        #         div(class = "ui toggle button action-button", id = 'initialCellCount', "Yes, I do"),
        #         div(class = "or"),
        #         div(class = "ui toggle button action-button", id = 'divisionRate', "No, but I have untreated cell division times")
        #     )
        #     )
        # ),
        # br(),
        # shinyjs::hidden(
        # div(class = "twelve wide column", id = "calc_method_desc",
        #       p("Use cell line division times (instead of initial cell count) to calculate GR values.", id = "div_rate_desc"),
        #       p("Use initial (Time 0) cell counts - the measure of cell number in untreated wells grown in parallel until the time of treatment - for GR value calculation.", id = "init_count_desc")
        #     )
        # ),
      #   shinyjs::hidden( 
      #   div(class = "twelve wide column", `data-toggle` = "buttons", id = "case_buttons",
      #       h3("Choose input file format"),
      #       div(class = "ui buttons",
      #       div(class = "ui toggle button action-button", id = "caseA", "Case A (multiple cell counts per row)"),
      #       div(class = "or"),
      #       div(class = "ui toggle button action-button", id = "caseB", "Case B (one cell count per row)")
      #       )
      #   )
      # ),
      # shinyjs::hidden(  div(class = "twelve wide column", id = "case_desc",
      #    shinyjs::hidden(
      #      div(id = "caseA_div_desc",
      #          includeMarkdown("www/caseA_div.md")
      #      ),
      #      div(id = "caseA_initial_desc",
      #          includeMarkdown("www/caseA_initial.md"),
      #          tags$img(src = "images/data_examples/caseA_blur.png", width = "100%")
      #      ),
      #      div(id = "caseB_initial_desc",
      #          includeMarkdown("www/caseB_initial.md")
      #      ),
      #      div(id = "caseB_div_desc",
      #          includeMarkdown("www/caseB_div.md")
      #      )
      #     )
      #    )
      #    ),br(),
      #   shinyjs::hidden(
      #   div(class = "twelve wide column", `data-toggle` = "buttons", id = "comma_tab_buttons",
      #       h3("Select file type"),
      #       div(class = "ui buttons",
      #           div(class = "ui toggle button action-button", id = "comma_input",
      #               "comma-separated (.csv)", value = "comma"),
      #           div(class = "or"),
      #           div(class = "ui toggle button action-button", id = "tab_input", 
      #               "tab-separated (.tsv)", value = "tab")
      #       )
      #     )
      #   ), br(),
      # shinyjs::hidden(
      #   div(id = 'upload_button', h3("Upload data file"),
      # div(class = "ui two column grid",
      # div(class = "row",
      #   div(class = "six wide column",
      #     tags$b('From your computer:'),
      #     br(),
      #     div(class="ui icon button", id="divUpload",
      #       tags$i(class="cloud icon"), "Choose file..."
      #     ),
      #     tags$input(type="file", id = "uploadData", style="display: none"),
      #     #fileInput('uploadData', "", multiple = FALSE, accept = NULL, width = NULL),
      #     # The following tag allows for the same file path to be used twice in a row for upload
      #     tags$script('$( "#uploadData" ).on( "click", function() { this.value = null; });')
      #   ),
      #   div(class = "ten wide column",
      #     div(class = "ui form", style = "width: 200px; display: inline-block;",
      #         div(class = "field",
      #             tags$label("Or from a URL:"),
      #             tags$input(type = "text", id = "url")
      #         )
      #     ),
      #     div(class = "ui button action-button" , id = "fetchURLData", "Fetch Data",
      #         style="width: 120px; vertical-align: bottom; display: inline-block;")
      #   )
      # )
      # ))), br(),
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
    ######## import modal end #########
    #### example modal start #########
    # div(class = "ui mini modal", #id = 'example_modal', 
    #     div(class = "ui center aligned basic segment",
    #     #"Load Example", "examples",
    #     p("Case A: control values assigned to treated measurements"),
    #     p("Case B: control values stacked with treated measurements"),
    #         div(class = "ui buttons", "Load example:",
    #             div(class = "ui positive button action-button", id = 'loadExample',
    #                 'Case A'),
    #             div(class = "or"),
    #             div(class = "ui button action-button", id = 'loadExampleB', 'Case B')
    #     )
    # )),
    ######## example modal end #########
    #### start modal start #########
    # div(class = "ui small modal", #id = 'start_modal', 
    #     div(class = "ui basic center aligned segment",
    #         div(class = "ui buttons", "Load example:", style = "vertical-align: middle;",
    #             div(class = "ui positive button action-button", id = 'example_button',
    #                 'Load example data'),
    #             div(class = "or"),
    #             div(class = "ui button action-button", id = 'import_button', 'Import your own data')
    #         )
    #     )),
    ######## start modal end #########
    #### instructions modal start #########
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
    ######## instructions modal end #########
   div(class = "ui container", style = "width: inherit!important; display: flex; min-height: 100vh; flex-direction: column;",
        ######### top menu start ########
        div(class = "ui top attached inverted seven item stackable menu",  style = "flex: 0.1;",
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
        ######### top menu end ########
        div(class = "ui main basic segment", style = "min-height: 70vh",
          div(class="ui top basic secondary pointing menu", id = "tabs",
              a(class="active item", `data-tab`="first", "Getting Started"),
              shinyjs::hidden(
              a(class="item", `data-tab`="input", "Input data", id = "input_top"),
              a(class="item", `data-tab`="third", "Dose-Response by Condition", id = "drc_top"),
              a(class="item", `data-tab`="fourth", "GR Metric Comparison", id = "comparison_top"),
              a(class="item", `data-tab`="output_tables", "Output Tables", id = "output_top")
              )
          ),
      ######### first tab start #########
          div(class="ui active bottom center basic tab segment", `data-tab`="first",
                  div(class = "ui container", style = "max-width: 750px!important;",
                      tags$img(src = "images/GRcalculator-logo_v2.png", width = "250px", style = "float: left;"),
                      includeMarkdown("www/GettingStarted.md"),
                      div(class = "ui bottom attached buttons",
                          tags$button(class = "ui grey button action-button", id = "instructions_button", "Instructions"),
                          tags$button(class="ui teal labeled icon button action-button", tags$i(class="right arrow icon"), "Start", id = "start_button")
                      )
                )
              
          ),
      ######### first tab end #########
    ######### input tab start #########
      div(class="ui bottom center basic tab segment", `data-tab`="input", id = "input_bottom",
              div(class = "ui basic segment",
                  div(class="ui breadcrumb",
                      div(class="active section", tags$span("Choose upload or example data", id = "bc1_text"), 
                          hidden(actionLink("bc1_link", "Choose upload or example data")), id = "bc1"),
                      hidden(
                        div(class="section", tags$i(class="right angle icon divider"),
                            tags$span("Choose example data", id = "bc2_ex_text"), 
                              hidden(actionLink("bc2_ex_link", "Choose example data")), id = "bc2_ex"),
                        div(class="section", tags$i(class="right angle icon divider"),
                            tags$span("Data format", id = "bc2_upload_text"), 
                            hidden(actionLink("bc2_upload_link", "Data format")), id = "bc2_upload"),
                        div(class="section", tags$i(class="right angle icon divider"),
                            tags$span("File format", id = "bc3_text"), 
                            hidden(actionLink("bc3_link", "File format")), id = "bc3"),
                        div(class="section", tags$i(class="right angle icon divider"),
                            tags$span("Choose groups", id = "bc4_text"), 
                            hidden(actionLink("bc4_link", "Choose groups")), id = "bc4")
                      )
                  )
              ),
          ##### start bc1_content ######
              div(class = "ui basic center aligned segment", id = "bc1_content",
                  div(class = "ui buttons", "Load example:", style = "vertical-align: middle;",
                      div(class = "ui toggle button action-button", id = 'example_button',
                          'Load example data'),
                      div(class = "or"),
                      div(class = "ui toggle button action-button", id = 'import_button', 'Import your own data')
                  )
              ),
          ########### end bc1_content ######
          
          ##### start bc2_ex_content ######
  hidden(
    div(class = "ui center aligned basic segment", id = "bc2_ex_content",
        div(class = "ui buttons", "Load example:",
            div(class = "ui toggle button action-button", id = 'loadExample',
                'Live cell data only (GR curves)'),
            div(class = "or"),
            div(class = "ui toggle button action-button", id = 'loadExample_SvT', 'Live and dead cell data (static and toxic GR curves)')
        )
    )
  ),
          ########## end bc2_ex_content ######
          
          ##### start bc2_upload_content ######
            hidden(
              div(class = "ui center aligned basic segment", id = "bc2_upload_content",
              h3("Do you have live and dead cell counts?"),
              div(class = "ui buttons",
                  div(class = "ui toggle button action-button", id = 'no_dead', "No, I only have live cell counts"),
                  div(class = "or"),
                  div(class = "ui toggle button action-button", id = 'yes_dead', "Yes, I have both")
                ),
            hidden(
              div(class = "ui basic center aligned segment", id = "calc_method_buttons",
                h3("Do you have initial (time 0) cell counts?"),
                div(class = "ui buttons",
                    div(class = "ui toggle button action-button", id = 'initialCellCount', "Yes, I do"),
                    div(class = "or"),
                    div(class = "ui toggle button action-button", id = 'divisionRate', "No, but I have untreated cell division times")
                )
              )
            ),
            shinyjs::hidden(
              div(class = "ui basic center aligned segment", `data-toggle` = "buttons", id = "case_buttons",
                  h3("Choose input file format"),
                  div(class = "ui buttons",
                      div(class = "ui toggle button action-button", id = "caseA", "Case A (multiple cell counts per row)"),
                      div(class = "or"),
                      div(class = "ui toggle button action-button", id = "caseB", "Case B (one cell count per row)")
                  )
              )
            )
          )
          ),
          ######### end bc2_upload_content ######
          
          ##### start bc3_content ########
          hidden(
          div(class = "ui basic segment", id = "bc3_content",
              shinyjs::hidden(
                div(class = "ui basic center aligned segment", `data-toggle` = "buttons", id = "comma_tab_buttons", style = "padding-top: 0px; margin-top: 0px;",
                    h3("Select file type"),
                    div(class = "ui buttons",
                        div(class = "ui toggle button action-button", id = "comma_input",
                            "comma-separated (.csv)", value = "comma"),
                        div(class = "or"),
                        div(class = "ui toggle button action-button", id = "tab_input", 
                            "tab-separated (.tsv)", value = "tab")
                    )
                )
              ),
              shinyjs::hidden(
                div(id = 'upload_button', h3("Upload data file"),
                    div(class = "ui two column grid",
                        div(class = "row",
                            div(class = "six wide column",
                                tags$b('From your computer:'),
                                br(),
                                div(class="ui icon button", id="divUpload",
                                    tags$i(class="upload icon"), " Choose file",
                                    style = "width:140px"
                                ),
                                tags$input(type="file", id = "uploadData", style="display: none"),
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
                                div(class = "ui button action-button" , id = "fetchURLData", tags$i(class="cloud upload icon"), " Fetch Data",
                                    style="width: 140px; vertical-align: bottom; display: inline-block;")
                            )
                        )
                    ))),
          shinyjs::hidden(
            div(class = "ui basic segment", id = "static_vs_toxic_req",
                downloadLink(outputId = "dl_case_static_vs_toxic", "Download example data for static vs. toxic curves"),
                div(class = "ui active accordion",
                    div(class = "title active", 
                        tags$i(class = "dropdown icon"), "Static vs. toxic case example"
                    ),
                    div(class = "content active",
                        tags$img(src = "images/data_examples/case_static_vs_toxic_blur.png", 
                                 width = "100%",
                                 style = "float: center;")
                    )
                ),
                div(class = "ui active accordion",
                    div(class = "title active", 
                        tags$i(class = "dropdown icon"), "Static vs. toxic case column descriptions"
                    ),
                    div(class = "content active",
                        includeMarkdown("www/static_vs_toxic_req.md")
                    )
                )
            )
          ),
          shinyjs::hidden(  div(class = "twelve wide column", id = "case_desc",
            shinyjs::hidden(
              div(id = "caseA_div_desc",
                  downloadLink(outputId = "dl_caseA_div", "Download example data for case A"),
                  div(class = "ui active accordion",
                      div(class = "title active", 
                          tags$i(class = "dropdown icon"), "Case A Example"
                      ),
                      div(class = "content active",
                          tags$img(src = "images/data_examples/caseA_div_blur.png", width = "90%",
                                   style = "float: center;")
                      )
                  ),
                  div(class = "ui active accordion",
                      div(class = "title active", 
                          tags$i(class = "dropdown icon"), "Case A column descriptions"
                      ),
                      div(class = "content active",
                          includeMarkdown("www/caseA_div.md")
                      )
                  )
              ),
              div(id = "caseA_initial_desc",
                  downloadLink(outputId = "dl_caseA", "Download example data for case A"),
                  div(class = "ui active accordion",
                    div(class = "title active", 
                        tags$i(class = "dropdown icon"), "Case A Example"
                    ),
                    div(class = "content active",
                        tags$img(src = "images/data_examples/caseA_blur.png", width = "90%",
                                 style = "float: center;")
                    )
                  ),
                  div(class = "ui active accordion",
                      div(class = "title active", 
                          tags$i(class = "dropdown icon"), "Case A column descriptions"
                      ),
                      div(class = "content active",
                          includeMarkdown("www/caseA_initial.md")
                      )
                  )
              ),
              div(id = "caseB_initial_desc",
                  downloadLink(outputId = "dl_caseB", "Download example data for case B"),
                  div(class = "ui active accordion",
                      div(class = "title active", 
                          tags$i(class = "dropdown icon"), "Case B Example"
                      ),
                      div(class = "content active",
                          tags$img(src = "images/data_examples/caseB_blur.png", width = "80%",
                                   style = "float: center;")
                      )
                  ),
                  div(class = "ui active accordion",
                      div(class = "title active", 
                          tags$i(class = "dropdown icon"), "Case B column descriptions"
                      ),
                      div(class = "content active",
                          includeMarkdown("www/caseB_initial.md")
                      )
                  )
              ),
              div(id = "caseB_div_desc",
                  downloadLink(outputId = "dl_caseB_div", "Download example data for case B"),
                  div(class = "ui active accordion",
                      div(class = "title active", 
                          tags$i(class = "dropdown icon"), "Case B Example"
                      ),
                      div(class = "content active",
                          tags$img(src = "images/data_examples/caseB_blur_div.png", width = "90%",
                                   style = "float: center;")
                      )
                  ),
                  div(class = "ui active accordion",
                      div(class = "title active", 
                          tags$i(class = "dropdown icon"), "Case B column descriptions"
                      ),
                      div(class = "content active",
                          includeMarkdown("www/caseB_div.md")
                      )
                  )
                )
            )
            )
          )
          )
          
          ),
          ########## end bc3_content ########
          
          ##### bc4 content start ########
          hidden(
      div(class = "ui basic center aligned segment", id = "bc4_content",
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
         ),
      div(class = "ui basic segment",
          tags$style(type='text/css', "#input_table { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;}"),
          DT::dataTableOutput("input_table") %>% withSpinner(type = 3, color = "#009999", color.background = "#ffffff")
      )
      )
          )
          ########## end bc4_content ########
      ),
    ######### input tab end #########
      ######### output tables tab start #########
          div(class="ui bottom center basic tab segment", `data-tab`="output_tables", id = "output_tables_bottom",
              div(class="ui two top attached buttons",
                  # tags$button(class = "ui button action-button", id = "input_table_button", "Input data"),
                  # div(class = "or"),
                  tags$button(class = "ui active green button action-button", id = "gr_table_button", "GR values"),
                  div(class = "or"),
                  tags$button(class = "ui button action-button", id = "parameter_table_button", "Parameter values")
                  ),
              #tags$style(type = "text/css", "#parameter_table_select { display: inline-block; text-align: center; }"),
              br(),
              div(class = "ui centered grid",
                div(class = "basic center aligned segment",
                div(class = "column",
                    div(class="ui primary icon button action-button", id="dl_output_button",
                        tags$i(class="download icon"), " Download GR data",
                        style = "width:200px"
                    ),
                    #tags$script('$( "#dl_output_button" ).on( "click", function() { this.value = null; });'),
                    tags$head(tags$script(HTML('
                           Shiny.addCustomMessageHandler("jsCode",
                           function(message) {
                           eval(message.value);
                           });'))),
                    downloadLink(outputId = "dl_output_tables", label = "")
                ),
                br(),
                div(class = "column",
                  shinyjs::hidden(selectInput(inputId = "parameter_table_select", "",
                                          choices = "", width= "250px"))
                )
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
      ######### output tables tab end #########
      ######### drc/third tab start #########
          div(class="ui bottom center basic tab segment", `data-tab`="third",
            div(class = "ui basic segment", id = "drc_tabs",
                h4(class="ui horizontal divider header",
                   div(class = "item action-button shiny-bound-input", id = "plot_options_button",
                       a(class = "action-button", 
                         hidden(uiicon("caret down", id = "caret_down")),
                         uiicon(type = "caret right", id = "caret_right"),
                         "Plot options", uiicon(type = "info_circle")), href = "#")
                   ),
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
                  div(class = "ui grid",
                    div(class = "row",
                    div(class = "two wide column", style = "min-width: 125px;",
                      div(class = "ui icon button action-button", tags$i(class = "square icon", style = "font-weight: 0"), id = "single_button"),
                        div(class = "ui active icon button action-button", tags$i(class = "th large icon", style = "font-weight: 0"), id = "grid_button")
                    ),
                    div(class = "three wide column",
                      selectizeInput("drc2_facet", label = "Grid variables", choices = "none", multiple = T, width = "150px"),
                      tags$button(class = "ui secondary bottom attached button action-button", id = "update_button", "Update Plot")
                    ),
                    div(class = "column", style = "width:600px",
                        
                      div(class = "row",
                          div(class = "ui medium header", "Show/hide curves")
                      ),
                      div(class = "row",
                          uiOutput("drc_filter", class = "ui grid")
                          )
                      )
                    )
                  )
                ),
                  #div(class = "ui two column grid",
                    #div(class = "three wide column",
                        #),
                    #div(class = "thirteen wide column",
                        div(class = "ui basic center aligned segment", id = "grid_segment",
                          div(class = "ui two column grid",
                            div(class = "fourteen wide column", style = "padding:0px;",
                                uiOutput("plots_grid", class = "ui doubling three column grid",
                                         style = "min-height: 500px; margin:0px;")#,
                                #uiOutput("plots_grid_pages"),
                                #tags$button(class = "ui button action-button", id = "download_plot_drc_button", 
                                #            "Download Image File")
                            ),
                            div(class = "two wide column", id = "plots_grid_legend", style = "padding:0px;",
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
                       # )
                
          #)
      ),
      ######### drc/third tab end #########
      ######### scatter/box/fourth tab start #########
          div(class="ui bottom center basic tab segment", `data-tab`="fourth",
              style = "padding: 0px;",
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
                              choices = "")
              ),
              div(class = "ui two column centered grid",
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
      ######### scatter/box/fourth tab end #########
        )
,
          ######### footer start #########
        div(class = "ui bottom attached inverted footer segment", style = "margin: 0px; flex: 1;",
            div(class = "ui center aligned container",
                div(class = "ui horizontal inverted large divided link list",
                    a(class = "item", div(class = "action-button", "About", id = "about") ),
                    a(class = "item", div(class = "action-button", "Contact Us", id = "contact")),
                    a(class = "item", "Github", uiicon("github"), href = "https://github.com/uc-bd2k/grcalculator/")
                )
            )
        )
        ######### footer end #########
    )
    )
  )