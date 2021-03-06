% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_creators.R
\name{get_creators}
\alias{get_creators}
\title{Get Creators by Creator Names}
\usage{
get_creators(firstname = "", lastname = "")
}
\arguments{
\item{firstname}{The first name of the creator. Default is Null, but if lastname also Null, cause error}

\item{lastname}{The last name of the creator. Default is Null. but if firstname also Null, cause error}
}
\description{
Use this function to search for specific Marvel creators from Marvel API with creators' names
}
\details{
The function askes the user to input the first name and/or last name of a specific Marvel creator, if
the related information exists, this function returns information of creators ID, first name, last name, comics,
series, events and stories information.Please note that charaters the selected creator built cannot
be retrieved in this search.
}
\keyword{Comics,}
\keyword{Creators,}
\keyword{First}
\keyword{Last}
\keyword{Marvel,}
\keyword{Name}
\keyword{Name,}
