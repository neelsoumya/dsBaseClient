#' @title Performs survival analysis using Cox proportional hazards model
#' @description Passes a formula to a server side environment and returns the summary of Cox proportional hazards model from the server. 
#' @details This is a function peform survival analysis using the Cox proportional hazards model. 
#' 
#' Server function called: \code{coxphSLMADS}. 
#' 
#' @param search.filter character string (potentially including \code{*} symbol without spaces) 
#' specifying the formula that you want to pass to the server-side.
#' For more information see \strong{Details}. 
#' @param dataName character string of name of data frame
#' @return \code{coxphSLMADS} returns to the client-side a summary of the Cox proportional hazards model
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
ds.coxph.SLMA <- function(search.filter=NULL, dataName = NULL)
{
  
   datasources <- datashield.connections_find()

   # call the server side function
   cat("On client side: \n")
   # search.filter=stats::as.formula(search.filter)
   calltext <- call("coxphSLMADS",search.filter=search.filter, dataName)
   # calltext <- call("coxphSLMADS",search.filter=stats::as.formula(search.filter), dataName)
   
   cat("\n Class of calltext\n")
   cat(class(calltext))
   cat("\n What is in calltext ? \n")
   cat(as.character(calltext))
   cat("\n End of function \n")	

   output <- datashield.aggregate(datasources, calltext)
  
   return(output)
	
}
#ds.coxph.SLMA

