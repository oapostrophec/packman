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
    htmlOutput("pacman")
    #uiOutput("jobSelector"),
    #uiOutput("cmlSelector")
  ), #close sidebarPanel
  mainPanel(
    tabsetPanel(
      tabPanel("Select Fields",
              tabsetPanel( 
                tabPanel("View Output Fields",
                         uiOutput("columnSelector"),
                         h4("***"),
                         dataTableOutput("new_file")),
                tabPanel("View Original File",
                         dataTableOutput("sample_file"))
                )
               ),
      tabPanel("Column Ops",
               tabsetPanel(
                 tabPanel("Edit Column Names",
                          htmlOutput("new_column_names")),
                 tabPanel("Reorder Columns",
                          htmlOutput("new_column_order"))
                 )
               ),
      tabPanel("Row Ops",
               h4("Nothing yet")),
      tabPanel("Report Card",
               h4("Summary Data Here...")),
      tabPanel("Looks Legit",
               h4("check nrows in output to source rows"),
               h4("find & display missing units"),
               h4("show source columns and compare to output cols"))
     
    ) #close main tabset
  ) #close mainPanel
)) #close shiny ui