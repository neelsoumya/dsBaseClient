#' @title Passes a string to a server-side environment
#' @description Passes a string to a server side environment and prints the string returned from the server. 
#' @details This is a function to test writing new functions and the communication between the client and the server. 
#' 
#' Server function called: \code{retStrDS}. 
#' 
#' @param search.filter character string (potentially including \code{*} symbol) specifying the filter 
#' for the object name that you want to find in the enviroment. For more information see \strong{Details}. 
#' @param env.to.search an integer (e.g. in \code{2} or \code{2L} format) specifying the position
#' in the search path of the environment to be explored. \code{1L} is the current active analytic
#' environment on the server-side and is the default value of \code{env.to.search}.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return \code{ds.retStr} returns to the client-side a string
#' @author DataSHIELD Development Team
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
ds.retStr <- function(search.filter=NULL, datasources=NULL)
{
  
   if(is.null(datasources))
   {
   	 datasources <- datashield.connections_find()
   }

   # call the server side function
   cat("On client side: \n")
   calltext <- call("retStrDS",search.filter=transmit.object.final)

   cat("\n Class of calltext\n")
   cat(class(calltext))
   cat("\n What is in calltext ? \n")
   cat(as.character(calltext))
   cat("\n End of function \n")	

   output <- datashield.aggregate(datasources, calltext)
  
   return(output)
	
}
#ds.retStr

