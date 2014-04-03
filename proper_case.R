toproper <- function(x) {
    # Makes Proper Capitalization out of a string or collection of strings.
    sapply(x, function(strn) {
        s <- strsplit(strn, "\\s")[[1]]
        return(paste0(toupper(substring(s, 1,1)),
        tolower(substring(s, 2)),
        collapse=" "))
    }, USE.NAMES=FALSE)
}


proper_case <- function(file, cols){
  # cols is a character vector, subsetting needs to be done with a number
    for(j in 1:length(cols)){
      column_name = cols[j]
      column = which(names(file) == column_name)
        for(i in 1:nrow(file)){
            row = i
            file[row, column] = toproper(as.character(file[row, column]))
        }
    }
    return(file)
}