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
