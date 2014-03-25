##Packman Server
##initialized March 24, 2014
##Takes in full report, cleans aggregations
## makes it client facing.

require('shiny')
require('datasets')
require('data.table')
require('plyr')
require('devtools')
require('stringr')

options(stringsAsFactors = F)
options(shiny.maxRequestSize=150*1024^2)

shinyServer(function(input, output){
  full <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      job_ids <- input$job_id
      split_ids = str_split(job_ids, ",")
      split_ids = unlist(split_ids)
      
      #trim ids
      for (i in 1:length(split_ids)){
        split_ids[i] = gsub(split_ids[i], pattern=" ", replacement="")
      }
      ##need to figure out how to grab multiple files from s3 and store.
      #
      #
      #
      
      inFile <- input$files
      full = read.csv(inFile$datapath, na.strings="NaN", stringsAsFactors=FALSE)
      #full$X_created_at = as.POSIXct(full$X_created_at,
      #                               format='%m/%d/%Y %H:%M:%S')
      return(full)
    }
  })
  
  get_names <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full = full()
      names = names(full)
      names
    }
  })
  
#   get_job_names <- reactive({
#     if (is.null(input$files[1]) || is.na(input$files[1])) {
#       # User has not uploaded a file yet
#       return(NULL)
#     } else {
#       names = get_names()
#       job_cols = grepl("(X_).+", names)
#       job_cols_names = names[job_cols]
#     }
#   })
  
#   get_cml_names <- reactive({
#     if (is.null(input$files[1]) || is.na(input$files[1])) {
#       # User has not uploaded a file yet
#       return(NULL)
#     } else {
#       names = get_names()
#       gold_cols = grepl(".(_gold)$", names)
#       gold_cols_names = names[gold_cols]
#       cml_names = gsub(pattern="(_gold)$", replacement="", gold_cols_names)
#       cml_names
#     }
#   })
  
#   get_source_names <- reactive ({
#     if (is.null(input$files[1]) || is.na(input$files[1])) {
#       # User has not uploaded a file yet
#       return(NULL)
#     } else {
#       all_names = get_names()
#       gold_cols = grepl(".(_gold)$", all_names)
#       gold_names = all_names[gold_cols]
#       job_names = get_job_names()
#       answer_names = get_cml_names()
#     
#       not_source_names = c(gold_names, job_names, answer_names)
#     
#      source_names = all_names[!(all_names %in% not_source_names)]
#     }
#   })
  
  output$columnSelector <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      columns = get_names()
      
      selectInput("cols_chosen", 
                   "Select the source columns to be included in the output",
                   choices = columns,
                   multiple = T)
      
    }
  })
  
#   output$jobSelector <- renderUI({
#     if (is.null(input$files[1]) || is.na(input$files[1])) {
#       # User has not uploaded a file yet
#       return(NULL)
#     } else {
#       columns = get_job_names()
#       
#       selectInput("job_cols_chosen", 
#                   "Select the job info columns to be included in the output",
#                   choices = columns,
#                   multiple = T)
#   
#     }
#   })
#   
#   output$cmlSelector <- renderUI({
#     if (is.null(input$files[1]) || is.na(input$files[1])) {
#       # User has not uploaded a file yet
#       return(NULL)
#     } else {
#       columns = get_cml_names()
#       
#       selectInput("cml_cols_chosen", 
#                   "Select the cml columns to be included in the output",
#                   choices = columns,
#                   multiple = T)
#       
#     }
#   })
  
  output$sample_file <- renderDataTable({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      table = full()
      #table = head(table)
      table
    }
  })
  
  output$new_column_names <- renderText({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      column_names=new_file_columns()
      if (!(is.null(column_names))) {
        #job_id = job_id()
        table = "<table border=1>"
        #worker_table$last_submit = as.character(worker_table$last_submit)
        column_names = names(column_names)
        for (i in 0:length(column_names)) {
          this_row =  column_names[i]
          table = paste(table, '<tr>', sep="\n")
          if (i == 0) {
              table = paste(table, '<td>', sep="\n")
              table = paste(table, paste("<b>",'Name', "</b>"),
                      sep="\n") # pastes value!
              table = paste(table, '</td>', sep="\n")
            table = paste(table, '<td>', sep="\n")
            table = paste(table, 'Edit?', sep="\n")  
            table = paste(table, '</td>', sep="\n")
            table = paste(table, '<td>', sep="\n")
            table = paste(table, 'New Name', sep="\n")
            table = paste(table, '</td>', sep="\n")
            table = paste(table, '</tr>', sep="\n")
          } else {
            for (value_id in 1:length(this_row)) {
            value = this_row[value_id]
            table = paste(table, '<td>', sep="\n")
            table = paste(table, value, "&nbsp;&nbsp;", sep="\n") # pastes value!
            table = paste(table, '</td>', sep="\n")
          }
          table = paste(table, '<td>', sep="\n")
          table = 
            paste(table,'<button class="btn btn-info action-button shiny-bound-input" data-toggle="button" id="get', this_row[1], '" type="button">Change?</button>' , sep="")
          table = paste(table, '</td>', sep="\n")
          
          table = paste(table, '<td>', sep="\n")
          table = paste(table, '<input type="text" placeholder=', this_row[value_id], '>', sep="")
          table = paste(table, '</td>', sep="\n")
        }
        table = paste(table, '</tr>', sep="\n")
      }
      table = paste(table,"</table>", sep="\n")
      paste(table)
      } else {
        paste("<b>No data to see here. Make sure you selected at least 2 columns in the previous tab.</b>")
      }
    }
  })
  
  new_file_columns <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      old_file = full()
      
      columns = input$cols_chosen
      #jobs_columns = input$jobs_cols_chosen
      #cml_columns = input$cml_cols_chosen
      
      #combined = c(source_columns, jobs_columns, cml_columns)
      
      new_file = old_file[,(names(old_file) %in% columns)]
      new_file
    }
  })
  
  output$new_file <- renderDataTable({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      table = new_file_columns()
      table
    }
  })
  
})
























