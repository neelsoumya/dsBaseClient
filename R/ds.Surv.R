#' @title Creates a server-side Survival object for use in Cox proportional hazards model
#' @description Creates a server side Survival object and returns it 
#' @details This is a function that performs survival analysis using the Cox 
#' proportional hazards model. 
#' 
#' Server function called: \code{SurvDS}. 
#' 
#' @param time_param character string  
#' specifying the server-side parameter that has the time element for survival analysis
#' For more information see \strong{Details}. 
#' @param event_param character string of name of server side event parameter for
#'    use in survival analysis
#' @param objectname character string of name of new server-side object which will
#'  	store object of class survival::Surv()
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return \code{SurvDS} returns to the client-side a Surv() obejct for use in
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
#'   dsBaseClient::ds.Surv('SURVTIME', 'EVENT', 'surv_object')
#'   dsBaseClient::ds.coxph.SLMA(formula = 'surv_object~D$age+D$female')
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.Surv <- function(time_param = NULL, event_param = NULL, objectname = NULL, datasources = NULL)
{
   
   # look for DS connections
   # if one not provided then get current
   if(is.null(datasources))
   {
      datasources <- datashield.connections_find()
   }
   
   # if the argument 'event_param' is set, check that the data frame is defined (i.e. exists) on the server site
   if(!(is.null(event_param)))
   {
      # TODO: cannot find function isDefined but is is inds.glmerSLMA
      # defined <- isDefined(datasources, event_param)
   }
   
   # ds.assign(toAssign = "survival::Surv(time=SURVTIME,event=EVENT)", newobj = "surv_object", datasources = connections)
   
   # verify that 'time_param' was set
   if(is.null(time_param))
   {
      stop(" Please provide a valid survival time parameter", call.=FALSE)
   }
   
   
   # call the server side function
   cat("On client side: \n")
   cat("\n")
   calltext <- call("SurvDS", time_param, event_param) # SurvDS
   
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

