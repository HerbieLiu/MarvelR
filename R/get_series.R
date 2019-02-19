#' Get Series by Series Titles
#'
#' Use this function to search for specific Marvel series from Marvel API with series' title
#'
#'
#'The function askes the user to input the title of a particular Marvel series, if the series
#'exists, this function returns information of series ID, name, description, comics, creators,
#'series, events and stories information.
#'
#' @param name The title of the series. No defalut value, and cannot be left blank or error occurs
#'
#' @keywords Marvel, Comics, Series, Series title
#' @export
#'

get_series <- function(title){
  endpoint <- paste0("https://gateway.marvel.com/v1/public/series?", "title=", title)
  results <- GET(endpoint, query = query_params)
  if (http_status(results)$category != "Success")  {
    paste("there is an error,", http_status(results)$message)
  } else {
    if (content(results)$data$count == 0) {
      stop("No results")
    } else{
      df1 <- data.frame(fromJSON(content(results, as = "text")))
      df1 <- cbind("Comics ID" = df1$data.results.id, "title" = df1$data.results.title, "Description" = df1$data.results.description, "Events" = unnest(df1$data.results.events)$name, "Characters" = unnest(df1$data.results.characters)$name,"Comics" = unnest(df1$data.results.comics)$name, "Stories" = unnest(df1$data.results.stories)$name, "Creators" = unnest(df1$data.results.creators)$name)
      print(df1)
    }
  }
}

