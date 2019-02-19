#' Get Creators by Creator Names
#'
#' Use this function to search for specific Marvel creators from Marvel API with creators' names
#'
#'
#'The function askes the user to input the first name and/or last name of a specific Marvel creator, if
#'the related information exists, this function returns information of creators ID, first name, last name, comics,
#'series, events and stories information.Please note that charaters the selected creator built cannot
#'be retrieved in this search.
#'
#' @param firstname The first name of the creator. Default is Null, but if lastname also Null, cause error
#' @param lastname  The last name of the creator. Default is Null. but if firstname also Null, cause error
#'
#' @keywords Marvel, Comics, Creators, First Name, Last Name
#' @export

get_creators <- function(firstname="", lastname=""){
  if (firstname !=""& lastname != "") {
    endpoint <- paste0("https://gateway.marvel.com/v1/public/creators?", "firstName=", tolower(firstname), "&lastName=", tolower(lastname))
  }
  if (firstname =="" & lastname !="") {
    endpoint <- paste0("https://gateway.marvel.com/v1/public/creators?", "lastName=", tolower(lastname))
  }
  if (firstname !=""& lastname ==""){
    endpoint <- paste0("https://gateway.marvel.com/v1/public/creators?", "firstName=", tolower(firstname))
  }
  if (firstname =="" & lastname =="") {
    stop("no valid input, cannot process")
  }
  results <- GET(endpoint, query = query_params)
  if (http_status(results)$category != "Success")  {
    paste("there is an error,", http_status(results)$message)
  } else {
    if (content(results)$data$count == 0) {
      stop("No results")
    } else{
      df1 <- data.frame(fromJSON(content(results, as = "text")))
      df1 <- cbind("Comics ID" = df1$data.results.id, "First Name" = df1$data.results.firstName, "Last Name" = df1$data.results.lastName, "Series" = unnest(df1$data.results.series)$name, "Comics" = unnest(df1$data.results.comics)$name, "Stories" = unnest(df1$data.results.stories)$name, "Events" = unnest(df1$data.results.events)$name)
      print(df1)
    }
  }
}





