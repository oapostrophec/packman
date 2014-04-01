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
                   tabPanel("Rename Columns",
                            htmlOutput("new_column_names"),
                            actionButton("get_rename", "Submit?")),
                   tabPanel("Row Data Cleanup",
                            actionButton("get_clean", "Submit?"),
                            h5("Select cells for proper casing"),
                            uiOutput("rowProperCase"),
                            uiOutput("showPropers")),
                   tabPanel("Row Data Dedupe",
                            uiOutput("rowDedupeKey"),
                            actionButton("get_dedupe", "Submit?")),
                   tabPanel("Built File:",
                           uiOutput("editedFile"),
                           tags$style(type="text/css", ".shiny-datatable-output { overflow: scroll; }")
                           )
                 )),
        tabPanel("Merge with Source",
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
        tabPanel("Report Card",
                 tabsetPanel(
                   tabPanel("Summary",
                            h4("Summary Data Here..."),
                            htmlOutput("createReportCard")),
                   tabPanel("View Low Confidence Units",
                            dataTableOutput("displayLowUnits"))
                 )),
        tabPanel("View Original File",
                 dataTableOutput("sample_file"),
                 tags$style(type="text/css", ".data { overflow: scroll; }")
                 )
    ) #close overall tabset
  ) #close mainPanel
)) #close shiny ui