##Packman UI
##initialized March 24, 2014
##Takes in full report, cleans aggregations
## makes it client facing.

require('shiny')
require('datasets')
require('data.table')
require('plyr')
require('devtools')
require('stringr')


shinyUI(pageWithSidebar(
  headerPanel("Packman"),
  sidebarPanel(
    textInput("job_id", h4("Enter a Job ID or IDs"), 0),
    p("separate multiple job ids with a comma ,"),
    fileInput("files", h4("Select a full report:"), multiple=T, accept = 
               c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    h4("***"),
    htmlOutput("pacman")
  ), #close sidebarPanel
  mainPanel(
      tabsetPanel(
        tabPanel("Operations",
                 tabsetPanel(
                   tabPanel("Reorder Columns",
                            actionButton("get_reorder", "Submit?"),
                            uiOutput("columnSelector"),
                            tableOutput("new_file")),
                   tabPanel("Row Data Cleanup",
                            actionButton("get_clean", "Submit?"),
                            uiOutput("rowProperCase"),
                            uiOutput("showPropers"),
                            htmlOutput("showPropsWarning")),
                   tabPanel("Row Data Dedupe",
                            uiOutput("rowDedupeKey"),
                            actionButton("get_dedupe", "Submit?")),
                   tabPanel("Rename Columns",
                            htmlOutput("new_column_names"),
                            actionButton("get_rename", "Submit?")),
                   tabPanel("Built File:",
                           htmlOutput("editFileWarning"),
                           uiOutput("editedFile"),
                           tags$style(type="text/css", ".shiny-datatable-output { overflow: scroll; }")
                           )
                 )),
        tabPanel("Download Results",
                 tabsetPanel(
                   tabPanel("Summary",
                            h4("Summary Data Here..."),
                            htmlOutput("createReportCard"),
                            textInput("download_name", "Name for the output file (uft-8 without the .csv)", "output"),
                            br(),
                            downloadButton('downloadOutput', 'Download Built File')),
                   tabPanel("View Low Confidence Units",
                            dataTableOutput("displayLowUnits"))
                 )),
        tabPanel("Compare to Source",
                 fileInput("source_file", h4("Upload a job source file:"), multiple=F, accept = 
                             c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                 tabsetPanel(
                   tabPanel("Missing Units?",
                            p("Missing Units"),
                            p(strong("Warning:"),
                            span("Make sure that some source column names and agg column names overlap! We use these to make a key.",
                                 style="color:red")),
                            dataTableOutput("missingUnits")
                            ),
                   tabPanel("Source Viewer",
                   dataTableOutput("sourceFile"))
                 )),
        tabPanel("View Original File",
                 dataTableOutput("sample_file"),
                 tags$style(type="text/css", ".data { overflow: scroll; }")
                 )
    ) #close overall tabset
  ) #close mainPanel
)) #close shiny ui