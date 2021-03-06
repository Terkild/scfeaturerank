% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{sipk}
\alias{sipk}
\title{Calculate Staining Index per Thousand (SIPK)}
\usage{
sipk(positive, negative, total = NULL, denominator = 1000, ...)
}
\arguments{
\item{positive}{Vector of values for positive cells}

\item{negative}{Vector of values for negative cells}

\item{total}{Total value. If NULL, then total will be calculated as the sum of the positive and negative vectors}

\item{denominator}{Denominator for total value}

\item{...}{Passed on to staining_index()}
}
\description{
SI / (TOTAL/1000)
}
