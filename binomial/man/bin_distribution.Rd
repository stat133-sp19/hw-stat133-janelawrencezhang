% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binomial.R
\name{bin_distribution}
\alias{bin_distribution}
\title{Distribution of a Binomial Distribution}
\usage{
bin_distribution(trials, prob)
}
\arguments{
\item{trials}{total number of trials}

\item{prob}{the probability of a success in one trial}
}
\value{
a dataframe which each row has the number of successes and its corresponding probability
}
\description{
Calculating all probabilities of successes of a binomial distribution
}
\examples{
bin_distribution(trials = 5, prob=0.5)
##  success   probability
##1    0         0.03125
##2    1         0.15625
##3    2         0.31250
##4    3         0.31250
##5    4         0.15625
##6    5         0.03125
}
