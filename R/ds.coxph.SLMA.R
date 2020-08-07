#' @title Performs survival analysis using Cox proportional hazards model
#' @description Passes a formula to a server side environment and returns the summary of Cox proportional hazards model from the server. 
#' @details This is a function that peforms survival analysis using the Cox proportional hazards model. 
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
#'   dsBaseClient::ds.coxph.SLMA(search.filter = 'survival::Surv(time = SURVTIME, event = EVENT) ~  D$female', dataName = 'D')
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
   search.filter=stats::as.formula(search.filter)
   
   # TODO: fix later
   formula = search.filter
   
   #formula as text, then split at pipes to avoid triggering parser
   formula <- Reduce(paste, deparse(formula))
   formula <- gsub("|", "xxx", formula, fixed = TRUE)
   formula <- gsub("(", "yyy", formula, fixed = TRUE)
   formula <- gsub(")", "zzz", formula, fixed = TRUE)
   formula <- gsub("/", "ppp", formula, fixed = TRUE)
   formula <- gsub(":", "qqq", formula, fixed = TRUE)
   formula <- gsub(",", "rrr", formula, fixed = TRUE)
   formula <- gsub(" ", "", formula, fixed = TRUE)
   formula <- stats::as.formula(formula)
   #formula <- strsplit(x = formurand()la, split="|", fixed=TRUE)[[1]]
   
   # TODO: fix later
   search.filter = formula
   
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

