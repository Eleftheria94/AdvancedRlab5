################################################################################
#
#name = "Salvador Marti Roman & Eleftheria Chatzitheodoridou"
#liuid = "salma742 & elech646"
#
# 732A94 Advanced R Programming
# Computer lab 5 
# package.skeleton(name="AdvancedRlab5")
# Deadline: 04 October 23:59
################################################################################


# Modified template from Best Practices for API Packages 
# https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html

require(httr)
require(jsonlite)


#' kolada_api handles the API link
#' 
#' @param path The path of the API url.
#' 
#' @return Returns a list that contains all data for the given API.
#'
#'
kolada_api <- function(path) {
  
  #Add v2 to path
  path = paste("v2/",path,sep="")
  url <- modify_url("http://api.kolada.se/v2/", path = path)
  URLencode(url)
  #Check response format is json
  resp <- GET(url)
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  #Json parser-Deserialization
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  # Check Server Response 
  if (status_code(resp) != 200) {
    stop(
      sprintf(
        "Kolada API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  #Return a useful object
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "kolada_api"
  )
}

#' Get municipalities
#' 
#' @param name The name of the municipality we want to access.
#' 
#' @return Returns a data frame that contains all municipalities.
#' @export
#'
get_municipality = function(name){
  #http://api.kolada.se/v2/municipality?title=lund
  
  stopifnot(is.character(name))
  stopifnot(!grepl("/",name, fixed=TRUE)) # checking if a string is in another string
  response = kolada_api(paste("municipality?title=",name,sep=""))
  return(response$content)
}

#' Get a municipality's organizational units
#' 
#' @param name The name/code id of the municipality's organizational unit we want to access.
#' 
#' @return Returns a data frame that contains all organizational units of a municipality.
#' @export
#'
get_municipality_groups = function(name){
  #http://api.kolada.se/v2/ou?municipality=1280
  
  stopifnot(is.character(name))
  stopifnot(!grepl("/",name, fixed=TRUE))
  
  response = kolada_api(paste("municipality?ou=",name,sep=""))
  return(response$content)
}

#' Get KPI
#'
#' @param name The name of the kpi we want to access.
#' 
#' @return Returns a data frame that contains for a specific KPI with its id and description.
#' @export
#'
get_kpi = function(name){
  #http://api.kolada.se/v2/kpi?title=kvinnofridskränkning
  
  stopifnot(is.character(name))
  stopifnot(!grepl("/",name, fixed=TRUE))
  
  response = kolada_api(paste("kpi?title=",name,sep=""))
  return(response$content)
}

#' Get KPI groups
#' 
#' @param name The name of the group we want to access.
#' 
#' @return Returns a data frame that contains information about a specific kpi group.
#' @export
#'
get_kpi_groups = function(name){
  #http://api.kolada.se/v2/kpi_groups?title=kostnad
  
  stopifnot(is.character(name))
  stopifnot(!grepl("/",name, fixed=TRUE))
  
  response = kolada_api(paste("kpi_groups?title=",name,sep=""))
  return(response$content)
}

#' Get Search Results
#' 
#' @param kpi_list Id of the KPIs as a list.
#' @param municipality_list Id of the municipality.
#' @param year_list The years to get.
#' 
#' @return Returns a data frame that contains data about KPIs in a given municipality by a specific (user-defined) year.
#' @export
#'
get_search_results = function(kpi_list=NULL, municipality_list=NULL,year_list=NULL){
  #http://api.kolada.se/v2/data/kpi/N07402/municipality/1280/year/2011
  
  params = list(kpi=kpi_list,
             municipality=municipality_list,
             year=year_list)
  
  #Filter unused arguments
  params[sapply(params, is.null)] = NULL
  
  #Input checking
  stopifnot(length(params)>1)
  sapply(params, function(x) stopifnot(is.list(x)))
  
  #Translate R term list into api term list
  search_terms = sapply(params, function(x) paste(x,collapse=","))
  
  #Build path from search terms
  path = paste(lapply(names(search_terms), function(x) paste(x,search_terms[x], sep="/"))
        ,collapse = "/")
  
  response = kolada_api(paste("data/", path,sep=""))
  return(response$content)
}

# Examples for testing:
# mun = get_municipality("lund")
 mun_group = get_municipality_groups("skola")
 kpi = get_kpi("kvinnofridskränkning")
 kpi_group = get_kpi_groups("kostnad")
 search_res = get_search_results(kpi_list = list("N00945"), year_list = list(2009,2010))