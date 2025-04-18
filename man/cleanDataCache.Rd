% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanDataCache.R
\name{cleanDataCache}
\alias{cleanDataCache}
\title{Clean the local data cache}
\usage{
cleanDataCache(
  cache = Sys.getenv("PKG_DATA_CACHE", file.path(dirname(tempdir()),
    utils::packageName()))
)
}
\arguments{
\item{cache}{\link{character} path where downloaded files from cloud are cached (See details)}
}
\value{
This function is only used for side effect.
}
\description{
This function is intented to be called by the same name function
in the fairify project.
}
\details{
Raises an error if the deleting failed.
}
