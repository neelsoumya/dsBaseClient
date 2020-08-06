#' @title Passes a string to a server-side environment
#' @description Passes a string to a server side environment and prints the string returned from the server. 
#' @details This is a function to test writing new functions and the communication between the client and the server. 
#' 
#' Server function called: \code{retStrDS}. 
#' 
#' @param search.filter character string (potentially including \code{*} symbol without spaces) 
#' specifying the string that you want to pass to the server-side.
#' For more information see \strong{Details}. 
#' @return \code{ds.retStr} returns to the client-side a string
#' @author Soumya Banerjee, 2020
#' @examples
#' \dontrun{
#'
#'   ## Version 6
#'   
#'   # connecting to the Opal servers
#' 
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   ds.retStr('hello')
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.coxph.SLMA <- function(search.filter=NULL)
{
  
   datasources <- datashield.connections_find()

   # call the server side function
   cat("On client side: \n")
   # calltext <- call("retStrDS",search.filter=search.filter)
   calltext <- call("coxphSLMADS",search.filter=search.filter)
   
   cat("\n Class of calltext\n")
   cat(class(calltext))
   cat("\n What is in calltext ? \n")
   cat(as.character(calltext))
   cat("\n End of function \n")	

   output <- datashield.aggregate(datasources, calltext)
  
   return(output)
	
}
#ds.retStr

