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
			 if (requireNamespace("mailR", quietly = TRUE)) {
				print('Succesfully installed!')
			} else {
				return("Couldn't install it. Please solve conflicts and try again")
			}
		}else{
			return('Ok!')
		}
	}
	packages = my_packages()
	mail = readline(prompt="I will need your email, we promise not to use this information for referring data
Type you email:")
	areas = readline(prompt="And maybe you can say me 3 areas where you work.
Example: Physics, Statistics, Networks
")
	mailBody = paste(paste("Areas: ", areas),paste("Packages: ",paste(packages,collapse=", ")),sep="\n")
	cat("This is the mail body\n")
	cat(mailBody)
	askSend = readinteger(prompt="Do you agree with sending it?
1:YES
2:NO
Answer number:")
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
