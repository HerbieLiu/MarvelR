#' Create Token
#'
#' This function automatically transits the Marvels Tokens gained from Marvel website
#' to the required format of the API query.
#'
#' The function asks the user to input their Marvel token and private token, and the
#' function will create and store a query based on the unique tokens for later-on searches
#' and retrievals.
#'
#' @param token User's personal Marvel Comics API public key/token.
#' @param secret_token User's personal Marvel Comics API private key/token.
#' @param limit the limit of returns of the API retrieves, the default and maximum is 100.
#' @keywords Marvel, Comics, Token
#' @export


token_to_query <- function(token, secret_token, limit = 100){
  text <- paste(1, secret_token, token, sep = "" )
  list <- list(ts = 1, apikey = token, hash = digest(text, algo = "md5", serialize = F), limit = limit)
  query_params <<- list}




