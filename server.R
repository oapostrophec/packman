##Packman Server
##initialized March 24, 2014
##Takes in a report, cleans aggregations
## makes it client facing.

require('shiny')
require('datasets')
require('data.table')
require('plyr')
require('devtools')
require('stringr')
require('reshape2')

source('proper_case.R')
source('reorder_columns.R')
source('create_hash_key.R')

options(stringsAsFactors = F)
options(shiny.maxRequestSize=150*1024^2)
options(scipen=999)
shinyServer(function(input, output){
  
  output$pacman <- renderText ({
    image_path = "http://cf-public-view.s3.amazonaws.com/coolstuff/pacman.png"
    html_image = paste("<img src=", image_path, " width=\"65%\"/>", sep="")
    paste(html_image)
  })
  
  output$summaryMessage <- renderText({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return("<p>You have not uploaded any files yet</p>")
    } else {
      agg = agg_file()
      num_gold_units = length(unique(agg$X_unit_id[agg$X_golden=='true']))
      num_nongold_units = length(unique(agg$X_unit_id)) - num_gold_units
      if(num_nongold_units == 0){
        num_nongold_units = nrow(agg)
      }
      cols = names(agg)
      num_cols = length(cols)
      cols_list = list(cols)
      
      column_names = paste("<p>The following are all of the columns in your file: <br>", 
                           cols_list, "</p>", sep="")
      
      column_warning = paste("<p><b>Note that the job columns (like _unit_id) are appended with an X 
                             in the app. Like so: X_unit_id</b></p>")
      
      overall_message = paste("<p>The report you uploaded has:<br>",
                              num_gold_units, " gold units,<br>",
                              num_nongold_units, " ordinary units,<br>",
                              num_cols, " column headers.<br>",
                              "</p>", sep="")
      paste(overall_message, column_names, column_warning, sep="<br>")
    } 
  })
  
  agg_file <- reactive({
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
      agg_file = read.csv(inFile$datapath, na.strings="NaN", stringsAsFactors=FALSE)
      
      return(agg_file)
    }
  })
  
  source_file <- reactive({
    if(is.null(input$source_file[1]) || is.na(input$source_file[1])){
      return(NULL) 
    } else{
      
      inSource <- input$source_file
      source_file = read.csv(inSource$datapath, na.strings="NaN", stringsAsFactors=FALSE)
      return(source_file)
      
    }
  })
  
  output$sourceFile <- renderDataTable({
    if(is.null(input$source_file[1]) || is.na(input$source_file[1])){
      return(NULL) 
    } else{
      output = source_file()
      output
    }
  })
  
  
  get_names <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      agg_file = agg_file()
      names = names(agg_file)
      names
    }
  })
  
  get_job_names <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      names = get_names()
      job_cols = grepl("(X_).+", names)
      job_cols_names = names[job_cols]
    }
  })
  
  get_cml_names <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      names = get_names()
      gold_cols = grepl(".(_gold)$", names)
      gold_cols_names = names[gold_cols]
      cml_names = gsub(pattern="(_gold)$", replacement="", gold_cols_names)
      cml_names
    }
  })
  
  get_source_names <- reactive ({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      all_names = get_names()
      gold_cols = grepl(".(_gold)$", all_names)
      gold_names = all_names[gold_cols]
      job_names = get_job_names()
      answer_names = get_cml_names()
      
      not_source_names = c(gold_names, job_names, answer_names)
      
      source_names = all_names[!(all_names %in% not_source_names)]
      drop = grepl(pattern=".+(confidence)", source_names)
      
      source_names = source_names[!(drop)]
      
    }
  })
  
  ##Step 1 - Reorder and Drop Columns
  ##Selector Outputs
  output$columnSelector <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      columns = get_names()
      source_cols = get_source_names()
      cml_cols = get_cml_names()
      default_choices = c('X_unit_id', source_cols, cml_cols)
      
      selectInput("cols_chosen", 
                  "Select the columns to be included in the output (enter them in the order you want):",
                  choices = columns,
                  multiple = TRUE,
                  selected = default_choices,
                  selectize = TRUE)
    }
  })
  
  ##Reorder Columns
  col_reorder_drop <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      change_file = agg_file()  
      reorder_cols_input = input$cols_chosen
      
      if(!(is.null(reorder_cols_input))){
        change_file = reorder_columns(change_file, reorder_cols_input)
        change_file
      }
      
      change_file  
    }
  })
  
  ##Step 2 - Data Clean
  ##Selector Outputs
  output$rowProperCase <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1]) || input$get_reorder == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      file = col_reorder_drop()
      columns = names(file)
      
      selectInput("propers_chosen",
                  "Select the cells that should be proper cased",
                  choices = columns,
                  multiple=TRUE,
                  selectize = TRUE)
      
    }
  })
  
  output$urlFix <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      columns = get_names()
      
      selectInput("webs_chosen",
                  "Select the url cells that should be standardized",
                  choices = columns,
                  multiple=TRUE,
                  selectize = TRUE)
      
    }
  })
  
  ##Date Clean - titlecase
  row_proper_case <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1]) || input$get_reorder == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      change_file = col_reorder_drop()
      proper_input = input$propers_chosen
      
      if(!(is.null(proper_input))){
        change_file = proper_case(change_file, proper_input)
        change_file
      }  
      change_file
    }
  })
  
  ##Step 3 - Dedupe rows
  ##Selector Outputs
  output$rowDedupeKey <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      columns = row_proper_case()
      columns = names(columns)
      default_cols = get_source_names()
      
      if (length(default_cols > 3)){
        default_cols = default_cols[1:3]
      }
      
      selectInput("key_chosen",
                  "Select the columns we should use to create a key for row deduplification",
                  choices = columns,
                  multiple=TRUE,
                  selected = default_cols,
                  selectize = TRUE)
      
    }
  })
  
  ##Dedupe Rows
  row_dedupe <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1]) || input$get_clean == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      change_file = row_proper_case()
      key = input$key_chosen
      
      
      if(!(is.null(key))){
        change_file$hash_key = create_hash_key(change_file, key)
        dup_lists = duplicated(change_file$hash_key)
        if(!(is.null(dup_lists))){
          change_file = change_file[!(dup_lists),]
        }
        
        change_file = change_file[,names(change_file)!= "hash_key"]
        
      }
      change_file
    }
  })
  
  ###Step 4
  col_rename <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1]) || input$get_dedupe == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      change_file = row_dedupe()
      names = names(change_file)
      update_names = {}
      
      for(i in 1:length(names)){
        new_name = input[[paste(names[i])]]
        update_names[i] <- new_name
      }
      
      if(input$get_rename != 0){
        colnames(change_file) <- update_names
      }
      change_file
    } 
  })
  
  ##Displaying Table Outputs
  #Step 1 View
  output$reorderTabTable <- renderTable({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      table = col_reorder_drop()
      
      if (nrow(table) > 15){
        max_count = min(15, nrow(table))
        table = table[1:max_count,]
      }
      table
    }
  })
  
  #Step 2 View
  output$dataCleanTabTable <- renderTable({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      if(input$get_reorder != 0){
        output = row_proper_case()
        if (nrow(output) > 15){
          max_count = min(15, nrow(output))
          output = output[1:max_count,]
        }
        output
      } else{
        return(NULL)
      }
    }
  })  
  
  output$dataCleanTabWarning <- renderText({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      if(input$get_reorder == 0){
        paste("<span style=\"color:red\">We have not received any data on this page. Make sure to press the submit button in the previous tab, Reorder Columns.</span>")
      } else {
        return(NULL) 
      }
    }
  })
  
  #Step 3 View
  output$dedupeTabTable <- renderTable ({
    if (is.null(input$files[1]) || is.na(input$files[1]) || input$get_clean == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      table = row_proper_case()
      
      if(input$get_dedupe != 0){
        table = row_dedupe()
      }
      
      if(nrow(table)>15){
        max_count = min(15, nrow(table))
        table = table[1:max_count,]
      }
      table
    }
  })
  
  output$dedupeTabWarning <- renderText({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      if(input$get_clean == 0){
        paste("<span style=\"color:red\">We have not received any data on this page. Make sure to press the submit button in the previous tab, Row Data Cleanup.</span>")
      } else {
        return(NULL) 
      }
    } 
  })
  
  #Step 4 View
  output$renameTabTable <- renderText({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      column_names=col_rename()
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
            table = paste(table, 'New Name', sep="\n")  
            table = paste(table, '</td>', sep="\n")
          } else {
            for (value_id in 1:length(this_row)) {
              value = this_row[value_id]
              table = paste(table, '<td>', sep="\n")
              table = paste(table, "&nbsp;", value, "&nbsp;&nbsp;", sep="\n") # pastes value!
              table = paste(table, '</td>', sep="\n")
            }
            table = paste(table, '<td>', sep="\n")
            table = paste(table, '<input id=', this_row[value_id],
                          ' type="text" value="', this_row[value_id], 
                          '" class="shiny-bound-input">', sep="")
            table = paste(table, '</td>', sep="\n")
          }
          table = paste(table, '</tr>', sep="\n")
        }
        table = paste(table,"</table>", sep="\n")
        paste(table)
      } else {
        paste("<span style=\"color:red\">No data to see here. Make sure you selected at least 2 columns in the previous tab and pressed the <u>Submit?</u> button.</span>")
      }
    }
  })  
  
  #Built Tab View
  output$builtTabTable <- renderTable({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      if (input$get_rename != 0){
        output = col_rename()
        #output = head(output)
        if (nrow(output) > 15){
          max_count = min(15, nrow(output))
          output = output[1:max_count,]
        }
        output
      } else{
        return(NULL)
      }
    }
  })
  
  output$builtTabWarning <- renderText({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      if(input$get_rename == 0){
        paste("<span style=\"color:red\">We have not received any data on this page. Make sure to press the submit button in the previous tab, Rename Columns.</span>")
      } else {
        return(NULL) 
      }
    } 
  })
  
  find_missing_units <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1]) || is.null(input$source_file[1]) || is.na(input$source_file[1])){
      return(NULL)
    } else {
      source_file = source_file()
      agg_file = agg_file()
      
      source_names = names(source_file)
      agg_names = names(agg_file)
      
      key_names = intersect(source_names, agg_names)
      
      if (!is.null(key_names)){
        source_file$source_hash = create_hash_key(source_file, key_names)
        #   apply(source_file[, key_names], 1, function(x) paste(x,collapse="\t"))
        
        agg_file$agg_hash = create_hash_key(agg_file, key_names)
        #    apply(agg_file[, key_names], 1, function(x) paste(x,collapse="\t"))
        
        missing = agg_file[!(agg_file$agg_hash %in% source_file$source_hash),]
      }
      
    }
  })
  
  output$missingUnits <- renderDataTable({
    if (is.null(input$files[1]) || is.na(input$files[1]) || is.null(input$source_file[1]) || is.na(input$source_file[1])){
      return(NULL)
    } else {
      table = find_missing_units()
      table  
    }
  })
  
  output$missingUnitsText <- renderText({
    if (is.null(input$files[1]) || is.na(input$files[1]) || is.null(input$source_file[1]) || is.na(input$source_file[1])){
      return(NULL)
    } else {
      missing = nrow(find_missing_units())
      agg_file = nrow(agg_file())
      source_file = nrow(source_file())
      
      if(agg_file == source_file){
        message = paste("<div class=\"alerts alert-info\"><big>We did not detect in missing units. Source File:", source_file,
                        "<br> Agg File:", agg_file, "</big></div>", sep=" ")
      }
      
      if(missing != 0){
        message = paste("<div class=\"alerts alert-info\"><big>There are ", source_file, "total units in the source file.
                        The agg file contains", agg_file, "total units. There are", missing,
                        "units unaccounted for.</big></div>", sep=" ")
      } 
      else if(agg_file != source_file){
        difference = abs(agg_file - source_file)
        message = paste("<div class=\"alerts alert-info\"><big>We were unable to identify the missing units. However there
                        is a numeric difference between the agg file and source file.
                        <br> Make sure there is an overlap in header names before attempting any file merges.<br> Source File:", source_file,
                        "<br> Agg File:", agg_file,"<br> Difference:", difference, "</big></div>", sep=" ") 
      }
      
      message
    }
  })
  
  find_low_conf_units <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      file = agg_file()
      names = get_names()
      
      conf_index = grepl(names, pattern=".+(confidence)$")
      conf_columns = file[,conf_index]
      new_file = file[conf_columns < .4,]
      new_file
    }     
  })
  
  output$displayLowUnits <- renderDataTable({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      output = find_low_conf_units()
      output
    }
  })
  
  output$selectReportCardCols <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      columns = col_rename()
      columns = names(columns)
      
      selectInput("report_cols_chosen",
                  "Select the columns we should use to generate the report card.",
                  choices = columns,
                  multiple=TRUE,
                  selected = columns,
                  selectize = TRUE)
      
    } 
  })
  
  build_report_card <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1]) || input$get_report == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      file = col_rename()
      names = input$report_cols_chosen
      
      responses = lapply(names, function(x) {
        responses = table(file[,names(file)==x])
        responses/sum(responses)
      })
      
      list(r=responses, n=names)
    }
  })
  
  output$createReportCard <- renderText({
    if (is.null(input$files[1]) || is.na(input$files[1]) || input$get_report == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      inp =  build_report_card()
      report_list =inp$r
      
      question_names=inp$n
      output = paste("<html>")
      for(i in 1:length(report_list)){
        
        header = question_names[i]
        output = paste(output, "<h4>", header, "</h4>", sep="")
        item = names(report_list[[i]])
        num_item = length(item)
        answer = report_list[[i]]
        output = paste(output, "<table class=\"table table-bordered table-condensed table-striped\">", sep="")
        output = paste(output, "<tr>", sep="") # open names row
        
        if(num_item > 6){
          num_item = 6
          item[6] = "Other Non Empty Values"
          answer[6] = sum(answer[6:length(item)])
        }
        
        for (j in 1:num_item){
          if(item[j] == ""){
            item[j] = "\"\""
          }
          output = paste(output, "<td>", item[j], "</td>", sep ="")
        }
        output = paste(output, "</tr>", sep="") # close names row
        
        output = paste(output, "<tr>", sep="") # open values row
        for (j in 1:num_item){
          answer[j] = round(answer[j], digits=2)
          output = paste(output, "<td>", answer[j], "</td>", sep ="")
        }
        output = paste(output, "</tr>", sep="") # close the values row
        output = paste(output, "</table>", sep="")
        output = paste(output, "<hr>", sep="")
      }
      output = paste(output,"</html>", sep="")
      paste(output)
    }
  })
  
  build_report_card_csv <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1]) || input$get_report == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      inp =  build_report_card()
      report_list =inp$r
      question_names=inp$n
      output=paste("start of form")
      for(i in 1:length(report_list)){
        header = question_names[i]
        output_1 = paste(header, "_answer", sep="")
        output_2 = paste(header, "_value", sep="")
        outputs = paste(output_1, output_2, sep=",")
        output = paste(output, outputs, sep="\n")
      
        item = names(report_list[[i]])
        num_item = length(item)
        answer = report_list[[i]]
      
        if(num_item > 6){
          num_item = 6
          item[6] = "Other Non Empty Values"
          answer[6] = sum(answer[6:length(item)])
        }
      
        for (j in 1:num_item){
          if(item[j] == ""){
            item[j] = "no answer"
          }
          answer[j] = round(answer[j], digits=2)
          
          output_rows = paste(item[j], answer[j], sep =",")
          output=paste(output, output_rows, sep="\n")
        }
       ouput = paste(output, sep="\n")
       #print(output)
      }
      output
    }
  })
  
  output$downloadOutput <- downloadHandler(
    
    filename = function() { paste(input$download_name, '.csv', sep='') },
    content = function(file) {
      df=col_rename()
      write.csv(df, paste(file,sep=''), row.names=F, na="")
    }
  )
  
  
  output$downloadReport <- downloadHandler(
    
    filename = function() { paste('reportcard', '.csv', sep='') },
    content = function(file) {
      df=build_report_card_csv()
      write.csv(df, paste(file,sep=''), row.names=F, na="")
    }
  )
  
  output$originalFileTabTable <- renderDataTable({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      table = agg_file()
      table
    }
  })
  
})

