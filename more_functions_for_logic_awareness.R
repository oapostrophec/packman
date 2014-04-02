#############  make logic from cml: logic_df
library('plyr')

get_true_dependencies <- function(logic_df) {
  true_dependencies = list()
  for (q in unique(logic_df$question)) {
    true_dependencies[[paste(q)]] = unique(get_dependencies(q_name=q, logic_df = logic_df))
  }
  return(true_dependencies)
}

get_ordered_tds <- function(true_dependencies)  {
  unique_td = unique(true_dependencies)
  # by design, every true dependency contains all previous dependencies
  # so a shorter dependency will always be earlier then or independent of a longer dependency
  order_of_tds = order(unlist(lapply(unique_td, length)))
  ordered_tds = list()
  for (o in 1:length(order_of_tds)) {
    ordered_tds[[o]] = unique_td[[order_of_tds[o]]]
  }
  
  return(ordered_tds)
}


perform_chain_aggregations <- function(start_with_full, ordered_tds, true_dependencies) {
  already_agged  = c("") # this is a list of what has already been agged on
  new_df = start_with_full
  unit_ids = unique(start_with_full$X_unit_id)
  unit_ids = unit_ids[order(unit_ids)]
  for (i in 1:length(ordered_tds)) {
    matching_fields = unlist(lapply(true_dependencies, function(x) {
      setequal(x, ordered_tds[[i]]) # see if two sets are identical
    }))
    question_names = names(true_dependencies)[matching_fields]
    question_names = question_names[!(question_names %in% already_agged)]
    #print(paste("I aggregate", paste(question_names, collapse=","), "next"))
    # aggregate aggregate
    if (length(question_names) > 0) { 
      for (qn in question_names) {
        print(paste("Doing aggregation for", qn))
        new_list = lapply(unit_ids, function(x)  aggregate_one_slice(slice=new_df[new_df$X_unit_id==x,],
                                                                     field=qn))
        print("lapplying over units ready")
        new_rows = lapply(new_list, function(x) x$data)
        print("new rows are ready")
        new_df <- rbind.fill(new_rows)
        print("unlist is ready")
        print(dim(new_df))
      }
      already_agged = unique(c(already_agged, question_names))
    }
    #print(paste("already_agged becomes", paste(already_agged, collapse=",")))
  }
  return(new_df)
}

# 6 minutes for 4257 rows with 13 questions
# 

add_confidence_columns <- function(report, logic_df) {
  questions_names = unique(logic_df$question)
  questions_names = questions_names[questions_names!=""]
  print(questions_names)
  confidence_cols = paste(questions_names, ".confidence", sep="")
  print(confidence_cols)
  report[,confidence_cols] = ""
  return(report)
}

get_dependencies <- function(q_name, logic_df, already_got=c()) {
  print(paste("qname is", q_name))
  slice = logic_df[logic_df$question == q_name,]
  list_of_deps = slice$depends_on
  if (nrow(slice) == 0) {
    return("")
  } else if (nrow(slice) == 1 && slice$depends_on == "") {
    return(unique(list_of_deps))
  } else {
    for (i in slice$depends_on) {
      if (!(i %in% list_of_deps)) {
        print(paste("going deeper with", i))
        list_of_deps = c(list_of_deps, get_dependencies(i, already_got))
      }
    }
    already_got = c(already_got, list_of_deps)
    return(unique(list_of_deps))
  }
}

aggregate_one_slice <- function(slice, field, ...) {
  if (nrow(slice) == 0) {
    return(slice)
  } else {
    # print("start") # this one takes a little time 
    trust_weigths = ddply(slice, field, here(summarize), X_trust = sum(X_trust)/sum(slice$X_trust))
    # print("trust ready")
    winning_field = trust_weigths[[paste(field)]][trust_weigths$X_trust == max(trust_weigths$X_trust)][1] # this should depend on the aggregation method
    # print("winning_field ready")
    field_confidence = max(trust_weigths$X_trust)
    # print("field_confidence ready")
    new_slice = slice[slice[[as.character(as.name(field))]]==winning_field,]
    new_slice[[paste(field, ".confidence", sep="")]] = max(trust_weigths$X_trust)
    # print("new_slice ready")
    the_list = list(answer=winning_field, data=new_slice, confidence=field_confidence)
    # print("the_list ready")
    return(the_list)
  }
  # add confidences
}

