######### getting logic dependencies from jobs
# fetches cml, xml-parses, makes a df where every row is a combination of a single question
# and a single deoendency
###### Run as:
# job_id = 385529
# cml = get_cml_from_job(job_id)
# draft_df = get_depenencies_from_cml(cml)
# logic_df = make_logic_df(draft_df)

auth_key = "5b7d73e5e7eb06556f12b45f87b013fc419f45f2"

possible_elements_parent = c("textarea", "text", 
                             "checkbox", "checkboxes","radios",
                             "select",
                             "taxonomy", "group")

get_logic <- function(list_el) {
  logic = list_el["only-if"]
  if (is.na(logic)) {
    logic = ""
  }
  return(logic)
}

get_names <- function(list_el) {
  name = list_el["name"]
  if (is.na(name)) {
    label = list_el["label"]
    if (is.na(label)) {
      name=""
    } else {
      print("there was no name")
      name = gsub(pattern="[^[:alnum:] ]", replacement="",
                  label) # drop all non alphanumeric characters
      name = gsub(tolower(name), pattern=" ", replacement="_")
    }
  } else {
    # some crazy people don't follow proper naming convention
    name = gsub(tolower(name), pattern=" ", replacement="_")
  }
  print("The name is <<<<<<<<<<<<<")
  print(name)
  return(name)
}

##########################    scrape CML    #############################
############################ get job json

get_cml_from_job <- function(job_id) {
  json = paste(system(paste0("curl https://api.crowdflower.com/v1/jobs/",job_id,".json?key=",auth_key), intern=T), 
               collapse="")
  # get cml
  json_parsed = fromJSON(json_str = json)
  cml = json_parsed$cml
  return(cml)
}
############################# get question names and dependencies
get_depenencies_from_cml <- function(cml) {
  if (is.null(cml) || is.na(cml) || cml=="") {
    return("")
  } else {
    doc = xmlTreeParse(cml, useInternalNodes = T,
                       fullNamespaceInfo=F, isHTML=T)
    # "possible_elements" is defined on line 5
    element_vector = list()
    type_vector = list()
    ind =1 
    #find all elements
    for (el_name in possible_elements_parent) {
      radios_all = NULL # clear radios all
      if (el_name == "checkbox") {
        # get elements that are single checkbox
        radios_all = getNodeSet(doc, paste("//", el_name, sep=""), addFinalizer=T)
        # drop elements who are children of "checkboxes"
        if (length(radios_all) > 0) {
          for (ch_index in length(radios_all):1) {
            parent = xmlName(xmlParent(radios_all[[ch_index]]))
            if (parent == "checkboxes") {
              radios_all[[ch_index]] = NULL
            }
          }
        }
      } else {
        radios_all = getNodeSet(doc, paste("//", el_name, sep=""), addFinalizer=T)
      }
      if (length(radios_all)>0) {
        print(paste("There was a", el_name))
        element_vector[[ind]] = radios_all
        type_vector[[ind]] = el_name
        ind = ind+1
      }
    }
    names_vector = c()
    logic_vector = c()
    ind =1 
    #transform all elements
    if (length(element_vector) >0) {
      # first, update the elements and make reasons
      for (i in 1:length(element_vector)) {
        # we'll need special treatment for groups here
        # xmlName
        radios_all = element_vector[[i]]
        el_name = type_vector[[i]]
        print(paste("Found some", el_name))
        if (el_name == "group") {
          # special case: nested groups!
          element_vector_internal = list()
          # for each group
          for (gr in radios_all) {
            # get logic
            radios_attributes = xmlAttrs(gr)
            radios_logic = get_logic(radios_attributes)
            # get all children, and collect their names ####################################
            list_internal = list() # list of cml objects internal to the group
            ind_internal = 1
            for (el_name_internal in possible_elements_parent) {
              radios_all_internal = NULL # clear radios all
              if (el_name_internal == "checkbox") {
                print("element is CHECKBOXES")
                # get elements that are single checkbox
                radios_all_internal = getNodeSet(gr, paste("//", el_name_internal, sep=""), addFinalizer=T)
                # drop elements who are children of "checkboxes"
                if (length(radios_all_internal) > 0) {
                  for (ch_index_internal in length(radios_all_internal):1) {
                    parent_internal = xmlName(xmlParent(radios_all_internal[[ch_index_internal]]))
                    if (parent_internal == "checkboxes") {
                      radios_all_internal[[ch_index_internal]] = NULL
                    }
                  }
                }
              } else if (el_name_internal=="group") {
                print("element is GROUP")
                radios_all_internal = c()
              } else {
                print("element is SOMETHING ELSE")
                radios_all_internal = getNodeSet(gr, paste("//", el_name_internal, sep=""), addFinalizer=T)
              }
              print(length(radios_all_internal))
              if (length(radios_all_internal) > 0) {
                print("DECIDED TO TO name/logic extractions")
                radios_attributes = xmlApply(radios_all_internal,xmlAttrs)
                # store names 
                radios_names_internal = unlist(lapply(radios_attributes, function(x) get_names(x)))
                print("radios_names_internal")
                print(radios_names_internal)
                radios_logic_internal = rep(radios_logic, times= length(radios_names_internal))
                print("radios_logic_internal")
                print(radios_logic_internal)
                names_vector = c(names_vector, radios_names_internal)
                logic_vector = c(logic_vector, radios_logic_internal)
              }
            }
          }
          
          # attach group's logic to names
          
        } else {
          
          radios_attributes = xmlApply(radios_all,xmlAttrs)
          # store names 
          radios_names = unlist(lapply(radios_attributes, function(x) get_names(x)))
          radios_logic = unlist(lapply(radios_attributes, function(x) get_logic(x)))
          names_vector = c(names_vector, radios_names)
          logic_vector = c(logic_vector, radios_logic)
        }
        
      }
    } else {
      print("No elements found")
    }
    df = data.frame(element_names = names_vector, element_logic = logic_vector)
    return(df)
  }
}

##################################################
############## parse dependencies so each lives in a separate row (make logic_df)

make_logic_df <- function(short_df) {
  if (nrow(short_df)==0) {
    return(NULL)
  } else {
    logic_df = data.frame(NA, NA, NA)[0,]
    names(logic_df) = c("question", "depends_on", "depends_how")
    # this table is small so a plain loop is okay
    for (i in 1:nrow(short_df)) {
      name = short_df$element_names[i]
      dependency = short_df$element_logic[i]
      if (dependency != "") {
        logic_components = strsplit(dependency, split="\\+\\+|\\|\\|")[[1]]
        logic_names = sapply(logic_components, function(x) 
          gsub(gsub(x, pattern="!", replacement="\\!"), pattern="(:.*)", replacement="")
        )
        ####### make rows for new df
        question_vector = rep(name, times = length(logic_names))
        depends_on_vector = logic_names
        depends_how_vector = rep(dependency, times = length(logic_names))
        new_slice = data.frame(question=question_vector, depends_on=depends_on_vector,
                               depends_how=depends_how_vector)
      } else {
        new_slice = data.frame(question=name, depends_on=dependency,
                               depends_how=dependency)
      }
      # add to logic_df
      logic_df = rbind(logic_df, new_slice)
    }
    logic_df1 = logic_df[0,]
    for (q in unique(logic_df$question)) {
      print(q)
      sl = logic_df[logic_df$question==q,]
      sl = sl[sl$question != sl$depends_on,]
      sl =  sl[!duplicated(sl$depends_on),]
      if (nrow(sl) > 1) { 
        sl = sl[sl$depends_on != "",]
      }
      logic_df1 = rbind(logic_df1, sl)
    }
    # TODO: check for uniqueness
    logic_df1 = logic_df1[order(logic_df1$question),]
    row.names(logic_df1) = 1:nrow(logic_df1)
    return(logic_df1)
  }
}
