% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unique_eval.R
\name{unique_eval}
\alias{unique_eval}
\title{unique_eval}
\usage{
unique_eval(data, col_name)
}
\arguments{
\item{data}{Tidy dataframe.}

\item{col_name}{Character string specifying the column whose unique values will be pulled.}
}
\description{
Unique values of column in tidy dataframe
}
\details{
Low-level function not recommended for end-user usage (see base R unique function). Generates vector of unique values from a column of a tidy dataframe using tidy eval principles
}
