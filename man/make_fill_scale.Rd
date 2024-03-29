% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_processing.R
\name{make_fill_scale}
\alias{make_fill_scale}
\title{Portfolio fill scale (tickers only)}
\usage{
make_fill_scale(cols)
}
\arguments{
\item{cols}{Named character vector of colours in hex format. Names correspond
to tickers in the universe, plus "Cash" and "Portfolio"}
}
\value{
ggplot2 fill scale
}
\description{
Portfolio fill scale (tickers only)
}
\examples{
my_cols <- portfolio_colours(7)
fill_scale <- make_fill_scale(my_cols)
}
