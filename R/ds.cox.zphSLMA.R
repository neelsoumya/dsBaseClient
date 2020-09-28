#' @title Tests the proportional hazards assumption for a Cox proportional hazards model
#' @description This function tests the proportional hazards assumption 
#'	for a Cox proportional hazards model.
#' @details This is a function that performs diagnostics on a fitted Cox 
#' 	proportional hazards model. 
#' 
#' Server function called: \code{cox.zphSLMADS}. 
#' 
#' @param fit character string (potentially including \code{*} symbol without spaces) 
#' specifying the name of the fitted server-side Cox proportioanl hazards model
#'	 that has been created using ds.coxphSLMAassign() 
#' For more information see \strong{Details}. 
#' @return \code{cox.zphSLMADS} returns to the client-side the diagnostics of
#' 	the Cox proportional hazards model
#' @author Soumya Banerjee and Tom Bishop, 2020
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
#'                  table = "SURVIVAL.EXPAND_NO_MISSING1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   # make sure that the outcome is numeric 
#'   ds.asNumeric(x.name = "D$cens",
#'             newobj = "EVENT",
#'             datasources = connections)
#'
#'   ds.asNumeric(x.name = "D$survtime",
#'             newobj = "SURVTIME",
#'             datasources = connections)
#'
#'   dsBaseClient::ds.Surv(time='SURVTIME', event='EVENT', objectname='surv_object')
#'
#'   dsBaseClient::ds.coxph.SLMA(formula = 'surv_object ~  D$female', 
#'             dataName = 'D', datasources = connections)
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.cox.zphSLMA <- function(fit = NULL,
                           dataName = NULL,
			   datasources = NULL)
{
   
   # look for DS connections
   # if one not provided then get current
   if(is.null(datasources))
   {
      datasources <- datashield.connections_find()
   }
   
   # if the argument 'dataName' is set, check that the data frame is defined (i.e. exists) on the server site
   if(!(is.null(dataName)))
   {
      # TODO: cannot find function isDefined but is is inds.glmerSLMA
      # defined <- isDefined(datasources, dataName)
   }
   
   # verify that 'fit' was set
   if(is.null(fit))
   {
      stop(" Please provide a valid name for a server-side Cox proportional hazards model that has been fit to data !", call.=FALSE)
   }
   
   
   # call the server side function
   # cat("On client side: \n")
	
   #cat(search.filter)
   #cat("\n")
   calltext <- call("cox.zphSLMADS", fit, dataName)
   # calltext <- call("coxphSLMADS",search.filter=stats::as.formula(search.filter), dataName)
   
   #cat("\n Class of calltext\n")
   #cat(class(calltext))
   #cat("\n What is in calltext ? \n")
   #cat(as.character(calltext))
   #cat("\n End of function \n")	

   # call aggregate function
   output <- datashield.aggregate(datasources, calltext)
  
   # return summary of coxph model
   return(output)
	
}
#ds.cox.zphSLMA

