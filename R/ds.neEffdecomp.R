#'
#' @title Linear hypotheses for natural effect models
#' @description This function is similar to R function \code{neEffdecomp} from the 
#' \code{medflex} package.
#' @details The function \code{ds.neEffdecomp} automatically extracts relevant causal
#' parameter estimates from a natural effect model.
#' @param model the name of a fitted natural effect model object. This is the
#' object saved on the server-side by the \code{ds.neModel} function.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return a summary table of the object of class c("neLht", "glht") (see glht). The object
#' additionally inherits from class "neEffdecomp".
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
ds.neEffdecomp <- function(model=NULL, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  # verify that name of the fitted model was set
  if(is.null(model)){
    stop(" Please provide the name of a fitted natural effect model object!", call.=FALSE)
  }
  
  # check if the fitted model is defined in all studies
  defined <- dsBaseClient:::isDefined(datasources, model)
  
  model.name <- model
  
  # COMMENTED THE CODE BELOW BUT CAN BE MODIFIED AND INCLUDED IN A NEXT RELEASE
  # if(!is.null(xRef)){
  #   xRef <- paste0(as.character(xRef), collapse=",")
  # }else{
  #   xRef <- xRef
  # }
  # 
  # # check that the lengths of covNames and covLev vectors are equal
  # if(!(length(covNames) == length(covLev))){
  #   stop("The vectors covNames and covLev should have equal lengths", call.=FALSE)
  # }
  # 
  # if(!is.null(covNames)){
  #   covNames <- paste0(as.character(covNames), collapse=",")
  # }else{
  #   covNames <- covNames
  # }
  # 
  # if(!is.null(covLev)){
  #   covLev <- paste0(as.character(covLev), collapse=",")
  # }else{
  #   covLev <- covLev
  # }
  
  calltext <- call('neEffdecompDS', model)
  out <- DSI::datashield.aggregate(datasources, calltext)
  
  return(out)
  
}   
  
  