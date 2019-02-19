#' Search stories from Marvel
#'
#' Use this function to search for stories from Marvel API
#'
#' The function retrieves stories of Marvels from the user's input on stories id number
#' and/or the type of information prefered, including comics, events, creators, characters,
#' and series.
#'
#' @param id Each story's unique ID number. The default is Null. Without input,
#'           it will reutrn random stories with the count limit set in tooken_to_query().
#' @param type Type of Information from Marvel, Default is Null, choose one from
#'             comics, events, creators, characters, and series.
#' @keywords Marvel, Comics, stories
#' @export

search_stories <- function(id = "", type = ""){
  if (id == "" & type == "") {
    endpoint <- "https://gateway.marvel.com/v1/public/stories"
    results <- GET(endpoint, query = query_params)
    if (http_status(results)$category != "Success") {
      paste("there is an error,", http_status(results)$message)
    } else {
      if (content(results)$data$count == 0) {
        stop("No results")
      } else{
      df <- data.frame(fromJSON(content(results, as = "text")))
      df1 <- cbind("Stories ID" = df$data.results.id, "Stories Title" = df$data.results.title, "Description" = df$data.results.description, "Series" = unnest(df$data.results.series)$name, "Creators" = unnest(df$data.results.creators)$name, "Characters" = unnest(df$data.results.characters)$name, "Comics" = unnest(df$data.results.comics)$name, "Events" = unnest(df$data.results.events)$name)
      print(df1)
      }
    }
  }
  if (id == "" & type != "") {
    stop("Cannot proceed: missing ID number")
  }

  if (id !="" & type == ""){
    endpoint <- paste0("https://gateway.marvel.com/v1/public/stories/", id)
    results <- GET(endpoint, query = query_params)
    if (http_status(results)$category != "Success")  {
      paste("there is an error,", http_status(results)$message)
    } else {
      if (content(results)$data$count == 0) {
        stop("No results")
      } else{
      df2 <- data.frame(fromJSON(content(results, as = "text")))
      df3 <- cbind("ID" = df2$data.results.id, "Title" = df2$data.results.title, "Description" = df2$data.results.description, "Series" = unnest(df2$data.results.series)$name, "Creators" = unnest(df2$data.results.creators)$name, "Characters" = unnest(df2$data.results.characters)$name, "Comics" = unnest(df2$data.results.comics)$name, "Events" = unnest(df2$data.results.events)$name)
      print(df3)
    }
  }
}

  if (type == "" | type == "characters" | type == "creators" | type =="events" | type == "series" | type == "comics"){
    if (id != "" & type == "characters"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/stories/", id, "/", type)
      results <- GET(endpoint, query = query_params)
      if (http_status(results)$category != "Success")  {
        paste("there is an error,", http_status(results)$message)
      } else {if (content(results)$data$count == 0) {
        stop("No results")
      } else{
        df4 <- data.frame(fromJSON(content(results, as = "text")))
        df5 <- df4 %>% select("Character ID" = data.results.id, "Character Name"= data.results.name, "Description" = data.results.description)
        print(df5)
      }
      }
    }
    if (id != "" & type == "events"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/stories/", id, "/", type)
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
      endpoint<- paste0("https://gateway.marvel.com/v1/public/stories/", id, "/", type)
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
      endpoint<- paste0("https://gateway.marvel.com/v1/public/stories/", id, "/", type)
      results <- GET(endpoint, query = query_params)
      if (http_status(results)$category != "Success")  {
        paste("there is an error,", http_status(results)$message)
      } else {if (content(results)$data$count == 0) {
        stop("No results")
      } else{
        df8 <- data.frame(fromJSON(content(results, as = "text")))
        df9 <- df8 %>% select("Stories ID" = data.results.id, "Stories Title"= data.results.title, "Description" = data.results.description)
        print(df9)
      }
      }
    }
    if (id != "" & type == "series"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/stories/", id, "/", type)
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

