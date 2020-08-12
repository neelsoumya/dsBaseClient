#' @title Creates a server-side Survival object for use in Cox proportional hazards model
#' @description Passes a formula to a server side environment and returns the summary of 
#' Cox proportional hazards model from the server. 
#' @details This is a function that performs survival analysis using the Cox 
#' proportional hazards model. 
#' 
#' Server function called: \code{coxphSLMADS}. 
#' 
#' @param formula character string (potentially including \code{*} symbol without spaces) 
#' specifying the formula that you want to pass to the server-side.
#' For more information see \strong{Details}. 
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @param dataName character string of name of data frame
#' @return \code{coxphSLMADS} returns to the client-side a summary of 
#' the Cox proportional hazards model
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
#'   dsBaseClient::ds.coxph.SLMA(formula = 'survival::Surv(time = SURVTIME, event = EVENT) ~  D$female', 
#'             dataName = 'D', datasources = connections)
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.Surv <- function(formula = NULL, dataName = NULL, objectname = NULL, datasources = NULL)
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
   
   # ds.assign(toAssign = "survival::Surv(time=SURVTIME,event=EVENT)", newobj = "surv_object", datasources = connections)
   
   # verify that 'formula' was set
   if(is.null(formula))
   {
      stop(" Please provide a valid survival formula!", call.=FALSE)
   }
   
   
   # call the server side function
   cat("On client side: \n")
   search.filter = formula
   cat(search.filter)
   cat("\n")
   # calltext <- call("coxphSLMADS", formula=formula, dataName)
   # calltext <- call("asIntegerDS", formula=formula, dataName) # SurvDS
   calltext <- call("SurvDS", formula=formula, dataName) # SurvDS
   # datashield.assign()
   
   cat("\n Class of calltext\n")
   cat(class(calltext))
   cat("\n What is in calltext ? \n")
   cat(as.character(calltext))
   cat("\n End of function \n")	

   # call aggregate function
   # output <- datashield.aggregate(datasources, calltext)
   output <- DSI::datashield.assign(conns = datasources, value = calltext, symbol = objectname) # 'surv_object') 
   # ds.assign(toAssign = calltext, newobj = 'surv_object', datasources = datasources)
   
   # output <- datashield.assign(conns = datasources, symbol = 'surv_object',
   #                             value = calltext)
   
   #ds.assign(toAssign = 'D$female', newobj = 'E', datasources = connections)
   # ds.assign(toAssign = 'D$female', newobj = 'surv_object', datasources = datasources)
   #ds.assign(toAssign = 'SurvDS(', newobj = 'surv_object', datasources = datasources)
   # return summary of coxph model
   # output <- NULL
   return(output)
	
}
#ds.Surv

