% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppscore.R
\name{ppscore}
\alias{ppscore}
\title{Generate Predictive Power Score (PPS) for "feature predicts target"}
\usage{
ppscore(df, target, feature, task = NA, sample_val = 5000)
}
\arguments{
\item{df}{A data.frame containing the target and feature variables}

\item{target}{The target of the relationship. What we want to explain. The dependent variable.}

\item{feature}{The feature that explains the target.  The independent variable.}

\item{task}{Name of the prediction task, e.g. classification or regression. If the task is not specified, it is infered based on the y column The task determines which model and evaluation score is used for the PPS}

\item{sample_val}{Number of rows for sampling. The sampling decreases the calculation time of the PPS. If NA there will be no sampling. Default is 5000.}
}
\value{
A data.frame containing:
\itemize{
\item The Target Variable
\item The Feature Variable
\item The Task Name
\item The Predictive Power Score
\item The Evaluation metric
\item The Baseline Score (Evaluate Metric vs. a Zero-R Classifer/Median)
\item The Model Score
}
}
\description{
This function calculates the Predictive Power Score for "feature predicts target"
The score always ranges from 0 to 1 and is data-type agnostic:

A score of 0 means that the column x cannot predict the column y better than a naive baseline model.
A score of 1 means that the column x can perfectly predict the column y given the model.
A score between 0 and 1 states the ratio of how much potential predictive power the model achieved compared to the baseline model.
}
\examples{
data(titanic_example)
ppscore(titanic_example, "Survived", "Sex")
}
\keyword{zuh}
