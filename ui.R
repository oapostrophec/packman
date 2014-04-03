##Packman UI
##initialized March 24, 2014
##Takes in a report, cleans aggregations
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
    br(),
    span(strong("separate multiple job ids with a comma ,"), style="color:blue"),
    fileInput("files", h4("Select an agg report:"), multiple=T, accept = 
               c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    h4("***"),
    h5("Complete the step by step submissions under the numerated tab. Once you've completed that you
       can analyze and download the final product under the Download Results tab."),
    htmlOutput("summaryMessage"),
    htmlOutput("pacman")
  ), #close sidebarPanel
  mainPanel(
      tabsetPanel(
        tabPanel("Operations",
                 tabsetPanel(
                   tabPanel("1. Reorder Columns",
                            actionButton("get_reorder", "Submit?"),
                            uiOutput("columnSelector"),
                            tableOutput("reorderTabTable")),
                   tabPanel("2. Row Data Cleanup",
                            actionButton("get_clean", "Submit?"),
                            uiOutput("rowProperCase"),
                            uiOutput("dataCleanTabTable"),
                            htmlOutput("dataCleanTabWarning")),
                   tabPanel("3. Row Data Dedupe",
                            uiOutput("rowDedupeKey"),
                            actionButton("get_dedupe", "Submit?"),
                            htmlOutput("dedupeTabWarning"),
                            uiOutput("dedupeTabTable")),
                   tabPanel("4. Rename Columns",
                            htmlOutput("renameTabTable"),
                            actionButton("get_rename", "Submit?")),
                   tabPanel("5. View Built File:",
                           htmlOutput("builtTabWarning"),
                           uiOutput("builtTabTable"),
                           tags$style(type="text/css", ".shiny-datatable-output { overflow: scroll; }")
                           )
                 )),
        tabPanel("Download Results",
                 tabsetPanel(
                   tabPanel("Summary",
                            textInput("download_name", "Name for the output file (uft-8 without the .csv)", "output"),
                            br(),
                            downloadButton('downloadOutput', 'Download Built File'),
                            br(),
                            htmlOutput("createReportCard")
                            ),
                   tabPanel("View Low Confidence Units",
                            dataTableOutput("displayLowUnits"))
                 )),
        tabPanel("Compare to Source",
                 fileInput("source_file", h4("Upload a job source file:"), multiple=F, accept = 
                             c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                 tabsetPanel(
                   tabPanel("Missing Units",
                            p(strong("Warning:"),
                            span("Make sure that some source column names and agg column names overlap! We use these to make a key.",
                                 style="color:red")),
                            htmlOutput("missingUnitsText"),
                            br(),
                            dataTableOutput("missingUnits")
                            ),
                   tabPanel("Source Viewer",
                   dataTableOutput("sourceFile"))
                 )),
        tabPanel("View Original File",
                 dataTableOutput("originalFileTabTable"),
                 tags$style(type="text/css", ".data { overflow: scroll; }")
                 ),
        tabPanel("Logic Aware Aggregation",
                 fileInput("files_logic", h4("Upload your FULL report here (required):"), multiple=FALSE),
                 textInput("job_id_logic", h4("Add a job id if it's not contained in the name of the file (optional):"), 0),
                 h4(textOutput("sample_skip_text")),
                 h4("If the lines above turned grey, your file is being processed.
                 You can download your file when it's done."),
                 h4(textOutput("logic_agg_ready")),
                 downloadButton('downloadAgg', 'Download your Logic-Aware Agg report!')
                 )
    ) #close overall tabset
  ) #close mainPanel
)) #close shiny ui