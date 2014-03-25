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
    numericInput("job_id", h4("Enter a Job ID or IDs"), 0),
    p("separate multiple job ids with a comma ,"),
    fileInput("files", h4("Select a full report:"), multiple=T, accept = 
               c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    h4("***"),
    uiOutput("sourceSelector"),
    uiOutput("jobSelector"),
    uiOutput("cmlSelector")
  ), #close sidebarPanel
  mainPanel(
    tabsetPanel(
      tabPanel("Column Names",
               #htmlOutput("get_job_names"),
               h4("***"),
               dataTableOutput("new_file")
               ),
      tabPanel("View File",
               h4("Shows first few rows of the uploaded file:"),
               dataTableOutput("sample_file"))
    ) #close 1st tabset
  ) #close mainPanel
)) #close shiny ui