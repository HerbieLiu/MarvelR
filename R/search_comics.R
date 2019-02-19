#' Search comics from Marvel
#'
#' Use this function to search for comics from Marvel API
#'
#' The function retrieves comics of Marvels from the user's input on comics id number
#' and/or the type of information prefered, including creators, stories, events, and characters.
#' Please note, series cannot be tetireved using this function.
#'
#'
#' @param id Each comics' unique ID number. The default is Null. Without input,
#'          it will reutrn random comics with the count limit set in tooken_to_query().
#' @param type Type of Information from Marvel, Default is Null, choose one from
#'             creators, stories, events, and characters.
#' @keywords Marvel, Comics, comics
#' @export

search_comics <- function(id = "", type = ""){
  if (id == "" & type == "") {
    endpoint <- "https://gateway.marvel.com/v1/public/comics"
    results <- GET(endpoint, query = query_params)
    if (http_status(results)$category != "Success") {
      paste("there is an error,", http_status(results)$message)
    } else {
      if (content(results)$data$count == 0) {
        stop("No results")
      } else{
      df <- data.frame(fromJSON(content(results, as = "text")))
      df1 <- cbind("Comics ID" = df$data.results.id, "Comics Title" = df$data.results.title, "Description" = df$data.results.description, "Creators" = unnest(df$data.results.creators)$name, "Characters" = unnest(df$data.results.characters)$name, "Stories" = unnest(df$data.results.stories)$name, "Events" = unnest(df$data.results.events)$name)
      print(df1)
      }
    }
  }
  if (id == "" & type != "") {
    stop("Cannot proceed: missing ID number")
  }
  if (type == "series"){
    stop("Cannot proceed: the type cannot be series under comics search")
  }

  if (id !="" & type == ""){
    endpoint <- paste0("https://gateway.marvel.com/v1/public/comics/", id)
    results <- GET(endpoint, query = query_params)
    if (http_status(results)$category != "Success")  {
      paste("there is an error,", http_status(results)$message)
    } else {
      if (content(results)$data$count == 0) {
        stop("No results")
      } else{
      df2 <- data.frame(fromJSON(content(results, as = "text")))
      df3 <- cbind("ID" = df2$data.results.id, "Title" = df2$data.results.title, "Description" = df2$data.results.description, "Series" = unnest(df2$data.results.series)$name, "Creators" = unnest(df2$data.results.creators)$name, "Characters" = unnest(df2$data.results.characters)$name, "Stories" = unnest(df2$data.results.stories)$name, "Events" = unnest(df2$data.results.events)$name)
      print(df3)
      }
    }
  }


  if (type == "" | type == "characters" | type == "creators" | type =="events" | type == "stories"){
    if (id != "" & type == "characters"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/comics/", id, "/", type)
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
    if (id != "" & type == "events"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/comics/", id, "/", type)
      results <- GET(endpoint, query = query_params)
      if (http_status(results)$category != "Success")  {
        paste("there is an error,", http_status(results)$message)
      } else {
        if (content(results)$data$count == 0) {
          stop("No results")
        } else{
        df6 <- data.frame(fromJSON(content(results, as = "text")))
        df7 <- df6 %>% select("Event ID" = data.results.id, "Event Title"= data.results.title, "Description" = data.results.description)
        print(df7)
        }
      }
    }
    if (id != "" & type == "creators"){
      endpoint<- paste0("https://gateway.marvel.com/v1/public/comics/", id, "/", type)
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

