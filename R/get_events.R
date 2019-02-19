#' Get Events by Event Names
#'
#' Use this function to search for specific Marvel event from Marvel API with event's name
#'
#'
#'The function askes the user to input the name of a particular Marvel event, if the event
#'exists, this function returns information of events ID, name, description, comics, characters,
#'series, creators and stories information.
#'
#' @param name The name of the event. No defalut value, and cannot be left blank or error occurs
#'
#' @keywords Marvel, Comics, Events, Event name
#' @export
#'

get_events <- function(name){
  endpoint <- paste0("https://gateway.marvel.com/v1/public/events?", "name=", name)
  results <- GET(endpoint, query = query_params)
  if (http_status(results)$category != "Success")  {
    paste("there is an error,", http_status(results)$message)
  } else {
    if (content(results)$data$count == 0) {
      stop("No results")
    } else{
      df1 <- data.frame(fromJSON(content(results, as = "text")))
      df1 <- cbind("Comics ID" = df1$data.results.id, "title" = df1$data.results.title, "Description" = df1$data.results.description, "Series" = unnest(df1$data.results.series)$name, "Characters" = unnest(df1$data.results.characters)$name,"Comics" = unnest(df1$data.results.comics)$name, "Stories" = unnest(df1$data.results.stories)$name, "Creators" = unnest(df1$data.results.creators)$name)
      print(df1)
    }
  }
}
