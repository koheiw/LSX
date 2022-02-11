% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap.R
\name{bootstrap_lss}
\alias{bootstrap_lss}
\title{[experimental] Compute polarity scores of words with different
hyper-parameters}
\usage{
bootstrap_lss(
  x,
  what = c("seeds", "k", "slice"),
  by = 50,
  n = 10,
  size = 100,
  ...
)
}
\arguments{
\item{x}{a fitted textmodel_lss object.}

\item{what}{choose the parameter to resample in bootstrapping.}

\item{by}{the amount of increase in \code{k}; only used for when \code{what = "k"}.}

\item{n}{the number of resampling; only used for when \code{what = "slice"}.}

\item{size}{the number of word vectors to be resampled; only used  when \code{what = "slice"}.}
}
\description{
[experimental] Compute polarity scores of words with different
hyper-parameters
}