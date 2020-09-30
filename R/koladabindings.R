library(httr)

# Modified template from Best Practices for API Packages 
# https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html

kolada_api <- function(path) {
  
  #Add v2 to path
  path = paste("v2/",path,sep="")
  url <- modify_url("http://api.kolada.se/v2/", path = path)
  
  #Check response format is json
  resp <- GET(url)
  print(resp)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  #Json parser
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  # Check Response OK
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

get_municipality = function(name){
  stopifnot(is.character(name))
  stopifnot(!grepl("/",name, fixed=TRUE))
  
  name = URLencode(name)
  response = kolada_api(paste("municipality?title=",name,sep=""))
  return(response$content)
}


get_municipality_group = function(name){
  stopifnot(is.character(name))
  stopifnot(!grepl("/",name, fixed=TRUE))
  
  name = URLencode(name)
  response = kolada_api(paste("municipality?ou=",name,sep=""))
  return(response$content)
}

get_kpi = function(name){
  
  stopifnot(is.character(name))
  stopifnot(!grepl("/",name, fixed=TRUE))
  
  name = URLencode(name)
  response = kolada_api(paste("kpi?title=",name,sep=""))
  return(response$content)
}

get_kpi_groups = function(name){
  
  stopifnot(is.character(name))
  stopifnot(!grepl("/",name, fixed=TRUE))
  
  name = URLencode(name)
  response = kolada_api(paste("kpi_groups?title=",name,sep=""))
  return(response$content)
}

get_search_results = function(kpi_list=NULL, municipality_list=NULL,year_list=NULL){
  
  params = list(kpi=kpi_list,
             municipality=municipality_list,
             year=year_list)
  
  #Filter unused arguments
  params[sapply(params, is.null)] = NULL
  
  #Input checking
  sapply(params, function(x) stopifnot(is.list(x)))
  
  #Translate R term list into api term list
  search_terms = sapply(params, function(x) paste(x,collapse=","))
  
  #Build path from search terms
  path = paste(lapply(names(search_terms), function(x) paste(x,search_terms[x], sep="/"))
        ,collapse = "/")
  
  response = kolada_api(paste("data/", path,sep=""))
  return(response$content)
}