% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_mlr3_predictions.R
\name{get_mlr3_predictions}
\alias{get_mlr3_predictions}
\title{Get predictions after a mlr3 training/tuning with an custom data splitting}
\usage{
get_mlr3_predictions(splits, instance, idx_model = NULL)
}
\arguments{
\item{splits}{(mldesign_splits) \cr Splits that were utilized in mlr3 training/tuning process
via "custom" resampling.}

\item{instance}{(OptimInstance) \cr \code{OptimInstance} object resulting from mlr3 training/tuning process
with \code{$archive$predictions()} function.}

\item{idx_model}{(NULL | numeric) \cr either NULL to select all models or an integer (vector) in the range
1:instance$archive$n_evals}
}
\value{
(data.table) A table of predictions and labels for all observations.
}
\description{
Get predictions after a mlr3 training/tuning with an custom data splitting
}
\details{
See \href{overview.html}{"overview"} vignette.
}
