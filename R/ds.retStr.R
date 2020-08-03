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
#'   ## Version 6, for version 5 see the Wiki
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
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.retStr <- function(search.filter=NULL, env.to.search=1L, datasources=NULL)
{
  
   if(is.null(datasources))
   {
   	 datasources <- datashield.connections_find()
   }


   env.to.search<-1L

	 

#make code compatible with ds.passParser
transmit.object<-search.filter
transmit.object.temp1<-NULL

#set up character replacement
input.string<-"*"
replacement.string<-"_:A:_"
replacement.string.split<-unlist(strsplit(replacement.string,split=""))
length.rs<-length(replacement.string.split)
 
#Search for *s in code and convert to transmittable code
if(!is.null(transmit.object))
{
	transmit.object.split<-unlist(strsplit(transmit.object,split=""))

	length.to<-length(transmit.object.split)

	#first check that replacement character string does not appear in original search.filter code 

	if(length.to>=length.rs)
	{
		for(k in 1:(length.to-length.rs+1))
		{
			original.code.problem<-TRUE
			for(m in 1:length.rs)
			{
			  if(transmit.object.split[k+m-1]!=replacement.string.split[m])
				{
				  original.code.problem<-FALSE
			  }
			}

			if(original.code.problem==TRUE)
			{
	#			return.message<-paste0("Warning: Code replacing wildcard (i.e. '",replacement string,"' appears in your original code -please respecify")
				return.message<-paste0("Warning: Code replacing wildcard (i.e. '",input.string,
				"') is '",replacement.string,"' but this appears in your original search filter string - please respecify")
				return(return.message)
			}
		}
	}

	for(j in 1:length.to)
	{
	add.to<-transmit.object.split[j]
		if(add.to=="*")
		{
		add.to<-"_:A:_"
		}
	transmit.object.temp1<-c(transmit.object.temp1,add.to)
	}
	transmit.object.final<-paste(transmit.object.temp1,collapse="")

	}else{
	transmit.object.final<-NULL
	}


  # call the server side function
  cat("On client side: \n")
  # overwriting transmit.object.final
  # transmit.object.final = "Thisisaninputstringfromtheclient"
  # transmit.object.final = "D$cens"
  # transmit.object.final = "EVENT"
  calltext <- call("retStrDS",search.filter=transmit.object.final, env.to.search)

  cat("\n Class of calltext\n")
  #cat(calltext)
  cat(class(calltext))
  cat("\n What is in calltext ? \n")
  cat(as.character(calltext))
  cat("\n End of function \n")	

  output <- datashield.aggregate(datasources, calltext)
  
  return(output)
	
}
#ds.retStr


