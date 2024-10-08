% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sor_plot_results.R
\name{sor_plot_results}
\alias{sor_plot_results}
\title{Make plots of SOR results}
\usage{
sor_plot_results(vessel = NULL, cruise = NULL, region = NULL, survey = NULL)
}
\arguments{
\item{vessel}{vessel ID number as a numeric vector (e.g. 162 for Alaska Knight. Must provide rds_dir or all of region, vessel, cruise, survey.}

\item{cruise}{Cruise number as a numeric vector (e.g. 202202). Must provide rds_dir or all of region, vessel, cruise, survey.}

\item{region}{Survey region as a 1L character vector (EBS, NBS, GOA, or AI). Must provide rds_dir or all of region, vessel, cruise, survey.}

\item{survey}{Survey name prefix to use in filename (e.g. NBS_2022). Must provide rds_dir or all of region, vessel, cruise, survey.}
}
\value{
Reads in measurement data from _ping.rds files from rds_dir and writes corrected results to _sor.rds files in rds_dir.
}
\description{
Make plots of SOR results
}
