#' Get Comics by Comics Title
#'
#' Use this function to search for specific comics from Marvel API with comics titles
#'
#'
#'The function askes the user to input the title or keyword of a particular comics, if the related
#'information exists,this function returns information of comics ID, title, description, series,
#'creators, characters, stories and events information. Otherwise, there will be error messages
#'with instructions to correct.
#'
#'
#' @param title The title of the comics. No defalut value, and cannot be left blank or error occurs
#'
#' @keywords Marvel, Comics, Comics Title
#' @export

get_comics <- function(title){
  endpoint <- paste0("https://gateway.marvel.com/v1/public/comics?", "title=", title)
  results <- GET(endpoint, query = query_params)
  if (http_status(results)$category != "Success")  {
    paste("there is an error,", http_status(results)$message)
  } else {
    if (content(results)$data$count == 0) {
      stop("No results")
    } else{
      df1 <- data.frame(fromJSON(content(results, as = "text")))
      df1 <- cbind("Comics ID" = df1$data.results.id, "Title" = df1$data.results.title, "Description" = df1$data.results.description, "Series" = unnest(df1$data.results.series)$name, "Creators" = unnest(df1$data.results.creators)$name, "Characters" = unnest(df1$data.results.characters)$name, "Stories" = unnest(df1$data.results.stories)$name, "Events" = unnest(df1$data.results.events)$name)
      print(df1)
    }
  }
}
