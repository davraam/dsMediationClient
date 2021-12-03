#'
#' @title Expand the dataset and impute nested counterfactual outcomes
#' @description This function is similar to R function \code{neImpute} from the 
#' \code{mmedflex} package.
#' @details The function \code{ds.neImpute} both expands the data along hypothetical
#' exposure values and imputes nested counterfactual outcomes.
#' @param object a string character, the name of an object used to select a method.
#' @param nMed an integer, the number of mediators to be considered jointly. For example,
#' if nMed = 2, not only the second predictor variable, but the two predictor
#' variables declared after the exposure variable are internally coded as mediators.
#' Correct specification of the (number of) mediators can easily be checked in the summary
#' output of the natural effect model object (returned by \code{ds.neModel} function), 
#' which lists the names of the exposure and all mediators. Default value of nMed is set
#' to 1.
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. Default \code{neImpute.data}. 
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return a data frame of class c("data.frame", "expData", "impData") is assigned at
#' the server-side of each study.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
ds.neImpute <- function(object=NULL, nMed=1, newobj=NULL, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # verify that model outcomes are provided
  if(is.null(object)){
    stop(" Please provide the name of the fitted model object!", call.=FALSE)
  }
  
  # check if the model outcome is defined in all studies
  defined <- dsBaseClient:::isDefined(datasources, object)
  
  if(is.null(newobj)){
    newobj <- 'neImpute.data'
  }
  
  calltext <- call('neImputeDS', object, nMed)
  DSI::datashield.assign(datasources, newobj, calltext)
  
}  
  
  