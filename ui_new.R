library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)
library(ggplot2)
library(shinyLi)
library(formattable)
library(shiny.semantic)
#library(dplyr)
#library(readr)
#library(DT)
#library(crosstalk)
#library(magrittr)
#library(markdown)
#library(clipr)
#library(rclipboard)
#library(aws.s3)

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
    div(class = "ui container",
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
        div(class = "ui main attached segment",
          div(class="ui top attached tabular menu", id = "tabs",
              a(class="active item", `data-tab`="first", "Getting Started"),
              a(class="item", `data-tab`="second", "Data Tables"),
              a(class="item", `data-tab`="third", "Dose-Response by Condition"),
              a(class="item", `data-tab`="fourth", "Dose-Response Grid"),
              a(class="item", `data-tab`="fifth", "GR Metric Comparison")
          ),
          div(class="ui active bottom center attached tab segment", `data-tab`="first",
              div(class="ui primary button action-button", "Import data", id = "import_button")
          ),
          div(class="ui bottom center attached tab segment", `data-tab`="second",
              #includeMarkdown("www/home.md")
              p("second")
          ),
          div(class="ui bottom center attached tab segment", `data-tab`="third",
              #includeMarkdown("www/home.md")
              p("third")
          ),
          div(class="ui bottom center attached tab segment", `data-tab`="fourth",
              #includeMarkdown("www/home.md")
              p("fourth")
          ),
          div(class="ui bottom center attached tab segment", `data-tab`="fifth",
              #includeMarkdown("www/home.md")
              p("fifth")
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
    )
  )