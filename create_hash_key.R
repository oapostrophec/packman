create_hash_key <- function(file, key){
	file$hash_key = 
		apply(file[, key], 1, function(x) paste(x, collapse="\t"))

}