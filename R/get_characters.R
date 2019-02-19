#' Get Characters by Character Names
#'
#' Use this function to search for specific Marvel character from Marvel API with character' name
#'
#'
#'The function askes the user to input the name of a specific Marvel character, if the character
#'exists, this function returns information of characters ID, name, description, comics,series,
#'events and stories information.
#'
#' @param name The name of the character. No defalut value, and cannot be left blank or error occurs
#'
#' @keywords Marvel, Comics, Character, Character name
#' @export

get_characters <- function(name){
  endpoint <- paste0("https://gateway.marvel.com/v1/public/characters?", "name=", name)
  results <- GET(endpoint, query = query_params)
  if (http_status(results)$category != "Success")  {
    paste("there is an error,", http_status(results)$message)
  } else {
    if (content(results)$data$count == 0) {
      stop("No results")
    } else{
      df1 <- data.frame(fromJSON(content(results, as = "text")))
      df1 <- cbind("Comics ID" = df1$data.results.id, "Name" = df1$data.results.name, "Description" = df1$data.results.description, "Series" = unnest(df1$data.results.series)$name, "Comics" = unnest(df1$data.results.comics)$name, "Stories" = unnest(df1$data.results.stories)$name, "Events" = unnest(df1$data.results.events)$name)
      print(df1)
    }
  }
}
