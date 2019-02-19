#' Search creators from Marvel
#'
#' Use this function to search for creators from Marvel API
#'
#' The function retrieves creators of Marvels from the user's input on creators id number
#' and/or the type of information prefered, including comics, stories, series, and events.
#' Please note, characters cannot be retireved using this function.
#'
#'
#' @param id Each creator's unique ID number. The default is Null. Without input,
#'          it will reutrn random creators with the count limit set in tooken_to_query().
#' @param type Type of Information from Marvel, Default is Null, choose one from
#'             comics, events, series and stories.

#' @keywords Marvel, Comics, creators
#' @export

search_creators <- function(id = "", type = ""){
  if (id == "" & type == "") {
    endpoint <- "https://gateway.marvel.com/v1/public/creators"
    results <- GET(endpoint, query = query_params)
    if (http_status(results)$category != "Success") {
      paste("there is an error,", http_status(results)$message)
    } else {
      if (content(results)$data$count == 0) {
        stop("No results")
      } else{
      df <- data.frame(fromJSON(content(results, as = "text")))
      df1 <- cbind("Creators ID" = df$data.results.id, "Creators Name" = df$data.results.fullName, "Description" = df$data.results.description, "Series" = unnest(df$data.results.series)$name, "Comics" = unnest(df$data.results.comics)$name, "Stories" = unnest(df$data.results.stories)$name, "Events" = unnest(df$data.results.events)$name)
      print(df1)
      }
    }
  }
  if (id == "" & type != "") {
    stop("Cannot proceed: missing ID number")
  }
  if (type == "characters"){
    stop("Cannot proceed: the type cannot be creators under characters search")
  }

  if (id !="" & type == ""){
    endpoint <- paste0("https://gateway.marvel.com/v1/public/creators/", id)
    results <- GET(endpoint, query = query_params)
    if (http_status(results)$category != "Success")  {
      paste("there is an error,", http_status(results)$message)
    } else {
      if (content(results)$data$count == 0) {
        stop("No results")
      } else{
      df2 <- data.frame(fromJSON(content(results, as = "text")))
      df3 <- cbind("ID" = df2$data.results.id, "Name" = df2$data.results.fullName, "Description" = df2$data.results.description, "Series" = unnest(df2$data.results.series)$name, "Creators" = unnest(df2$data.results.creators)$name, "Comics" = unnest(df2$data.results.comics)$name, "Stories" = unnest(df2$data.results.stories)$name, "Events" = unnest(df2$data.results.events)$name)
      print(df3)
      }
    }
  }


  if (type == "" | type == "comics" | type =="events" | type =="series" | type == "stories"){
    if (id != "" & type == "comics"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/characters/", id, "/", type)
      results <- GET(endpoint, query = query_params)
      if (http_status(results)$category != "Success")  {
        paste("there is an error,", http_status(results)$message)
      } else {
        if (content(results)$data$count == 0) {
          stop("No results")
        } else{
        df4 <- data.frame(fromJSON(content(results, as = "text")))
        df5 <- df4 %>% select("Comics ID" = data.results.id, "Comics Title"= data.results.title, "Description" = data.results.description)
        print(df5)
        }
      }
    }
    if (id != "" & type == "events"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/characters/", id, "/", type)
      results <- GET(endpoint, query = query_params)
      if (http_status(results)$category != "Success")  {
        paste("there is an error,", http_status(results)$message)
      } else {
        if (content(results)$data$count == 0) {
          stop("No results")
        } else{
        df6 <- data.frame(fromJSON(content(results, as = "text")))
        df7 <- df6 %>% select("Events ID" = data.results.id, "Events Title"= data.results.title, "Description" = data.results.description)
        print(df7)
        }
      }
    }
    if (id != "" & type == "series"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/characters/", id, "/", type)
      results <- GET(endpoint, query = query_params)
      if (http_status(results)$category != "Success")  {
        paste("there is an error,", http_status(results)$message)
      } else {
        if (content(results)$data$count == 0) {
          stop("No results")
        } else{
        df6 <- data.frame(fromJSON(content(results, as = "text")))
        df7 <- df6 %>% select("Series ID" = data.results.id, "Series Title"= data.results.title, "Description" = data.results.description)
        print(df7)
        }
      }
    }
    if (id != "" & type == "stories"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/comics/", id, "/", type)
      results <- GET(endpoint, query = query_params)
      if (http_status(results)$category != "Success")  {
        paste("there is an error,", http_status(results)$message)
      } else {
        if (content(results)$data$count == 0) {
          stop("No results")
        } else{
        df8 <- data.frame(fromJSON(content(results, as = "text")))
        df9 <- df8 %>% select("Stories ID" = data.results.id, "Stories Title"= data.results.title, "Description" = data.results.description)
        print(df9)
        }
      }
    }
  }
  else{
    stop("Type invalid.") }
}

