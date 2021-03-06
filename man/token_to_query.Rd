% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_token.R
\name{token_to_query}
\alias{token_to_query}
\title{Create Token}
\usage{
token_to_query(token, secret_token, limit = 100)
}
\arguments{
\item{token}{User's personal Marvel Comics API public key/token.}

\item{secret_token}{User's personal Marvel Comics API private key/token.}

\item{limit}{the limit of returns of the API retrieves, the default and maximum is 100.}
}
\description{
This function automatically transits the Marvels Tokens gained from Marvel website
to the required format of the API query.
}
\details{
The function asks the user to input their Marvel token and private token, and the
function will create and store a query based on the unique tokens for later-on searches
and retrievals.
}
\keyword{Comics,}
\keyword{Marvel,}
\keyword{Token}
