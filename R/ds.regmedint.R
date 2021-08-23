#'
#' @title Regression-based causal mediation analysis
#' @description This function is similar to R function \code{regmedint} from the 
#' \code{regmedint} package.
#' @details The function \code{ds.regmedint} is used for regression-based causal mediation
#' analysis as described in Valeri & VanderWeele 2013 and Valeri & VanderWeele 2015.
#' @param data a string character, the name of the data frame containing the 
#' relevant variables.
#' @param yvar a character vector of length 1. Outcome variable name. It should be
#' the time variable for survival outcomes.
#' @param avar a character vector of length 1. Treatment variable name.
#' @param mvar a character vector of length 1. Mediator variable name.
#' @param cvar a character vector of length > 0. Covariate names. Use NULL if
#' there is no covariate. However, this is a highly suspicious situation. Even if
#' avar is randomized, mvar is not. Thus, there should usually be some confounder(s)
#' to account for the common cause structure (confounding) between mvar and yvar. 
#' @param eventvar a character vector of length 1. Only required for survival outcome
#' regression models. Note that the coding is 1 for event and 0 for censoring, 
#' following the R survival package convention.
#' @param a0 a numeric vector of length 1. Reference level of treatment variable that
#' is considered "untreated" or "unexposed".
#' @param a1 a numeric vector of length 1.
#' @param m_cde a numeric vector of length 1. Mediator level at which controlled direct
#' effect is evaluated at.
#' @param c_cond a numeric vector of the same length as cvar. Covariate vector at which 
#' conditional effects are evaluated at.
#' @param mreg a character vector of length 1. Mediator regression type: "linear" or "logistic".
#' @param yreg a character vector of length 1. Outcome regression type: "linear", "logistic", 
#' "loglinear", "poisson", "negbin", "survCox", "survAFT_exp", or "survAFT_weibull".
#' @param interaction a logical vector of length 1. Default to TRUE. Whether to include a 
#' mediator-treatment interaction term in the outcome regression model.
#' @param casecontrol a logical vector of length 1. Default to FALSE. Whether data comes from
#' a case-control study.
#' @param na_omit a logical vector of length 1. Default to FALSE. Whether to use na.omit() function
#' in stats package to remove NAs in columns of interest before fitting the models.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return a summary table of the object of class 'multimed'.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
ds.regmedint <- function(data = NULL, yvar = NULL, avar = NULL, mvar = NULL, cvar = NULL, eventvar = NULL,
                         a0 = 0, a1 = 1, m_cde = 1, c_cond = 0.5, mreg = NULL, yreg = NULL, 
                         interaction = TRUE, casecontrol = FALSE, na_omit = FALSE, datasources = NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  if(is.null(yvar)){
    stop(" Please provide the name of the yvar variable!", call.=FALSE)
  }
  if(is.null(avar)){
    stop(" Please provide the name of the avar of interest!", call.=FALSE)
  }
  if(is.null(mvar)){
    stop(" Please provide the name of the mvar variable!", call.=FALSE)
  }
  if(is.null(data)){
    stop(" Please provide the name of the data frame!", call.=FALSE)
  }
  if(is.null(mreg)){
    stop(" Please provide the mediator regression type!", call.=FALSE)
  }
  if(is.null(yreg)){
    stop(" Please provide the outcome regression type!", call.=FALSE)
  }
  
  # check if the input objects are defined in all studies
  defined.yvar <- dsBaseClient:::isDefined(datasources, paste0(data, "$", yvar))
  defined.avar <- dsBaseClient:::isDefined(datasources, paste0(data, "$", avar))
  defined.mvar <- dsBaseClient:::isDefined(datasources, paste0(data, "$", mvar))
  if(!is.null(cvar)){
    lapply(cvar, function(x){dsBaseClient:::isDefined(datasources, paste0(data, "$", cvar))})
  }  
  if(!is.null(eventvar)){
    defined.eventvar <- dsBaseClient:::isDefined(datasources, paste0(data, "$", eventvar))
  }  
  
  yvar.name <- yvar
  avar.name <- avar
  mvar.name <- mvar
  data.name <- data
  if(!is.null(cvar)){
    cvar.name <- paste0(as.character(cvar), collapse=",")
  }else{
    cvar.name <- cvar
  }
  eventvar.name <- eventvar
  
  calltext <- call('regmedintDS', data.name, yvar.name, avar.name, mvar.name, cvar.name, eventvar.name, 
                   a0, a1, m_cde, c_cond, mreg, yreg, interaction=interaction, casecontrol=casecontrol, 
                   na_omit=na_omit)
  study.summary <- DSI::datashield.aggregate(datasources, calltext)
  
  return(study.summary)
  
}  

