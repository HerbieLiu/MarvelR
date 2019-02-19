#' Search events from Marvel
#'
#' Use this function to search for events from Marvel API
#'
#' The function retrieves events of Marvels from the user's input on events id number
#' and/or the type of information prefered, including comics, stories, creators, characters,
#' and series.
#'
#'
#' @param id Each event's unique ID number. The default is Null. Without input,
#'           it will reutrn random events with the count limit set in tooken_to_query().
#' @param type Type of Information from Marvel, Default is Null, choose one from
#'             comics, stories, creators, characters, and series.
#' @keywords Marvel, Comics, events
#' @export


search_events <- function(id = "", type = ""){
  if (id == "" & type == "") {
    endpoint <- "https://gateway.marvel.com/v1/public/events"
    results <- GET(endpoint, query = query_params)
    if (http_status(results)$category != "Success") {
      paste("there is an error,", http_status(results)$message)
    } else {
      if (content(results)$data$count == 0) {
        stop("No results")
      } else{
      df <- data.frame(fromJSON(content(results, as = "text")))
      df1 <- cbind("Events ID" = df$data.results.id, "Events Title" = df$data.results.title, "Description" = df$data.results.description, "Series" = unnest(df$data.results.series)$name, "Creators" = unnest(df$data.results.creators)$name, "Characters" = unnest(df$data.results.characters)$name, "Comics" = unnest(df$data.results.comics)$name, "Stories" = unnest(df$data.results.stories)$name)
      print(df1)
      }
    }
  }
  if (id == "" & type != "") {
    stop("Cannot proceed: missing ID number")
  }

  if (id !="" & type == ""){
    endpoint <- paste0("https://gateway.marvel.com/v1/public/events/", id)
    results <- GET(endpoint, query = query_params)
    if (http_status(results)$category != "Success")  {
      paste("there is an error,", http_status(results)$message)
    } else {
      if (content(results)$data$count == 0) {
        stop("No results")
      } else{
      df2 <- data.frame(fromJSON(content(results, as = "text")))
      df3 <- cbind("ID" = df2$data.results.id, "Title" = df2$data.results.title, "Description" = df2$data.results.description, "Series" = unnest(df2$data.results.series)$name, "Creators" = unnest(df2$data.results.creators)$name, "Characters" = unnest(df2$data.results.characters)$name, "Comics" = unnest(df2$data.results.comics)$name, "Stories" = unnest(df2$data.results.stories)$name)
      print(df3)
      }
    }
  }


  if (type == "" | type == "characters" | type == "creators" | type =="series" | type == "stories" | type == "comics"){
    if (id != "" & type == "characters"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/events/", id, "/", type)
      results <- GET(endpoint, query = query_params)
      if (http_status(results)$category != "Success")  {
        paste("there is an error,", http_status(results)$message)
      } else {
        if (content(results)$data$count == 0) {
          stop("No results")
        } else{
        df4 <- data.frame(fromJSON(content(results, as = "text")))
        df5 <- df4 %>% select("Character ID" = data.results.id, "Character Name"= data.results.name, "Description" = data.results.description)
        print(df5)
        }
      }
    }
    if (id != "" & type == "stories"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/events/", id, "/", type)
      results <- GET(endpoint, query = query_params)
      if (http_status(results)$category != "Success")  {
        paste("there is an error,", http_status(results)$message)
      } else {if (content(results)$data$count == 0) {
        stop("No results")
      } else{
        df6 <- data.frame(fromJSON(content(results, as = "text")))
        df7 <- df6 %>% select("Event ID" = data.results.id, "Event Title"= data.results.title, "Description" = data.results.description)
        print(df7)
      }
      }
    }
    if (id != "" & type == "creators"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/events/", id, "/", type)
      results <- GET(endpoint, query = query_params)
      if (http_status(results)$category != "Success")  {
        paste("there is an error,", http_status(results)$message)
      } else {
        if (content(results)$data$count == 0) {
          stop("No results")
        } else{
        df6 <- data.frame(fromJSON(content(results, as = "text")))
        df7 <- df6 %>% select("Creators ID" = data.results.id, "Creators Name"= data.results.fullName, "Description" = data.results.description )
        print(df7)
        }
      }
    }
    if (id != "" & type == "comics"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/events/", id, "/", type)
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
    if (id != "" & type == "series"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/events/", id, "/", type)
      results <- GET(endpoint, query = query_params)
      if (http_status(results)$category != "Success")  {
        paste("there is an error,", http_status(results)$message)
      } else {
        if (content(results)$data$count == 0) {
          stop("No results")
        } else{
        df8 <- data.frame(fromJSON(content(results, as = "text")))
        df9 <- df8 %>% select("Series ID" = data.results.id, "Series Title"= data.results.title, "Description" = data.results.description)
        print(df9)
        }
      }
    }
  }
  else{
    stop("Type invalid.") }
}

