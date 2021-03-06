% Generated by roxygen2 (4.0.2): do not edit by hand
\name{chancer.character}
\alias{chancer.character}
\title{Get a random character}
\usage{
chancer.character(case = c("all", "lower", "upper"), pool = c("all",
  "alpha", "numeric", "symbol"), user = NULL)
}
\arguments{
\item{case}{select 'uppercase', 'lowercase' or 'all' for both cases}

\item{pool}{select 'alpha' for just letters, 'numeric' for just numbers,
'symbol' for just symbols or 'all' for all}

\item{user}{pass in string to select from your own pool of chars otherwise
leave NULL. If this is not NULL then case and pool do nothing}
}
\description{
Get a random character
}
\examples{
chancer.character('all', 'symbol')
chancer.character(user='i_am_a_test_string')
}

