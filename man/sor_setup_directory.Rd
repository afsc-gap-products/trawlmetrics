% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sor_setup_directory.R
\name{sor_setup_directory}
\alias{sor_setup_directory}
\title{Setup directory}
\usage{
sor_setup_directory(
  channel = NULL,
  region,
  cruise,
  cruise_idnum,
  vessel,
  survey,
  haul_types = 3,
  gear_codes = NULL,
  width_range = NULL
)
}
\arguments{
\item{channel}{An RODBC channel. Will prompt user to get connected if NULL.}

\item{region}{Survey region as a 1L character vector (EBS or NBS)}

\item{cruise}{Cruise number as a numeric vector (e.g. 202202)}

\item{cruise_idnum}{Cruise ID number as a numeric vector (e.g. 757)}

\item{vessel}{vessel ID number as a numeric vector (e.g. 162 for Alaska Knight.}

\item{survey}{Survey name prefix to use in file name (e.g. NBS_2022)}

\item{haul_types}{A numeric vector of HAUL_TYPE to use.}

\item{gear_codes}{A numeric vector of GEAR codes to use.}

\item{width_range}{Gate filter for net width values as a 2L numeric vector. If not provided, Defaults to survey standards if not provided c(8,22) for GOA and AI, c(10, 22) for EBS/NBS}
}
\description{
Function to retrieve data for sequential outlier rejection from RACEBASE and split data by haul.
}
