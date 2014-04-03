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
    for(j in 1:length(cols)){
        for(i in 1:nrow(file)){
            row = i
            column = cols[j]
            file[row, column] = toproper(file[row, column])
        }
    }
    return(file)
}