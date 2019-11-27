#' Your own packages
#'
#' This function prints your packages on screen or in a file.
#' @param file The file path to write down your packages 
#' @return A string with the names of your installed packages
#' @export
#' @examples
#' my_packages()

my_packages = function(file=NULL){
	my_packs = rownames(utils::installed.packages())
	if(is.null(file)){
		my_packs
	}
	else{
		write(my_packs,file)
	}
}
