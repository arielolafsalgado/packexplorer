#' Provide your packages to the authors, for further research
#'
#' This function sends your packages to the authors, using the 'mailR' package.
#' @return Nothing, it just sends the e-mail.
#' @export
#' @examples
#' send_your_packages()

send_your_packages <- function(){
	checkMailR = is.element('mailR',my_packages())
	if(!checkMailR){
		askInst = readinteger(prompt="You will need to install 'mailR' to continue
1: I agree, install it
2: Do not install it
")
		if(askInst==1){
			install.packages('mailR')
		}else{
			return('Ok!')
		}
	}
	packages = my_packages()
	mail = readline(prompt="I will need your email, we promise not to use this information for referring data
")
	areas = readline(prompt="And maybe you can say me 3 areas where you work.
Example: Physics, Statistics, Networks
")
	favpack = readline(prompt="Also, I would ask you your favorite R-packages (no more than 10).
Example: leaflet, igraph, stringr, Matrix
")
	mailBody = paste(paste("Areas: ", areas),paste("Favorite packages: ", favpack),paste("Packages: ",paste(packages,collapse=", ")),sep="\n")
	cat("This is the mail body\n")
	cat(mailBody)
	askSend = readinteger(prompt="Do you agree to send it?
1:YES
2:NO
")
	if(askSend==1){
		mailR::send.mail(from = mail, 
				to = "arielolafsalgado@gmail.com",subject=paste(mail,"packages"),
				body=mailBody,
				smtp = list(host.name = "aspmx.l.google.com",port = 25),
     				authenticate = FALSE,
          			send = TRUE)
		cat("Your mail has been sent!\nThank you very much!\n")
	}
}

readinteger <- function(prompt){ 
  n <- readline(prompt=prompt)
  if(n!=1 & n!=2){
    return(readinteger(prompt=prompt))
  }
  
  return(as.integer(n))
}
