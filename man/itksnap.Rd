% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itksnap.R
\name{itksnap}
\alias{itksnap}
\title{Run ITK-SNAP}
\usage{
itksnap(
  grayscale,
  overlay = NULL,
  segmentation = NULL,
  labels = NULL,
  zoom = NULL,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{grayscale}{filenames or nifti objects to be displayed.}

\item{overlay}{filenames or nifti objects to be set as overlays}

\item{segmentation}{segmentation image using \code{-s} option}

\item{labels}{label descriptions using \code{-s} option}

\item{zoom}{initial zoom in screen pixels/mm}

\item{verbose}{Print out the command executed}

\item{...}{arguments to pass to \code{\link{itksnap_cmd}}}
}
\value{
Return of \code{\link{system}} command
}
\description{
Executes ITK-SNAP
}
\examples{
dims = rep(10, 3)
arr = array(rnorm(prod(dims)), dim = dims)
nim = oro.nifti::nifti(arr)
\dontrun{
itksnap(nim)
}
}
