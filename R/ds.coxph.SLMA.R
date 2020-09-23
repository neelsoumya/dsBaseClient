#' @title Performs survival analysis using Cox proportional hazards model
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
#' @param weights vector of case weights
#' @param init vector of initial values of the iteration.
#' @param ties character string specifying the method for tie handling. The Efron approximation is
#'	used as the default. Other options are 'breslow' and 'exact'.
#' @return \code{coxphSLMADS} returns to the client-side a summary of 
#' the Cox proportional hazards model
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
ds.coxph.SLMA <- function(formula = NULL,
			  dataName = NULL,
			  weights = NULL,
			  init = NULL,
			  ties = 'efron',
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
   
   # verify that 'formula' was set
   if(is.null(formula))
   {
      stop(" Please provide a valid survival formula!", call.=FALSE)
   }
   
   
   # call the server side function
   # cat("On client side: \n")
   #search.filter=stats::as.formula(search.filter)
   formula = stats::as.formula(formula)
   
   ####################################################################	
   # Logic for parsing formula: since this need to be passed
   #     to parser, we need to remove special symbols
   #     On the server-side function (coxphSLMADS) this needs
   #     to be reconstructed
   #     formula as text, then split at pipes to avoid triggering parser
   ####################################################################
   formula <- Reduce(paste, deparse(formula))
   formula <- gsub("survival::Surv(", "sssss", formula, fixed = TRUE)
   formula <- gsub("|", "xxx", formula, fixed = TRUE)
   formula <- gsub("(", "yyy", formula, fixed = TRUE)
   formula <- gsub(")", "zzz", formula, fixed = TRUE)
   formula <- gsub("/", "ppp", formula, fixed = TRUE)
   formula <- gsub(":", "qqq", formula, fixed = TRUE)
   formula <- gsub(",", "rrr", formula, fixed = TRUE)
   formula <- gsub(" ", "",    formula, fixed = TRUE)
   formula <- gsub("=", "lll", formula, fixed = TRUE)
   # "survival::Surv(time=SURVTIME,event=EVENT)~D$female"
   # gets converted to EVENTzzz ~ D$female
   # cat(formula)
	
   # convert to formula otherwise we get parser error
   formula <- stats::as.formula(formula)
   #formula <- strsplit(x = formurand()la, split="|", fixed=TRUE)[[1]]
   
   #cat(search.filter)
   #cat("\n")
   calltext <- call("coxphSLMADS", formula=formula, dataName, weights, init, ties)
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
#ds.coxph.SLMA

