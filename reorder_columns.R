reorder_columns <- function(file, columns){
	new_file = file[,(columns)]
	return(new_file)

}