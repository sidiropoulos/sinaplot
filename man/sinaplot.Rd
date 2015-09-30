\name{sinaplot}
\alias{sinaplot}
\title{sinaplot}
\usage{
sinaplot(x, groups, method = "density", groupwiseScale = TRUE,
  yFraction = 0.02, neighbLimit = 1, adjust = 3/4, xSpread = 0.1,
  labels = NULL, plot = TRUE, main = "", ylab = "", bw = FALSE,
  shape = 16, size = 2, color = NULL)
}
\arguments{
\item{x}{numeric vector of values to be plotted.}

\item{groups}{vector of \code{length(x)}.}

\item{method}{choose the method to spread the samples within the same
neighbourhood along the x-axis. Available methods: "density",
"neighbourhood". See \code{Details}.}

\item{groupwiseScale}{logical. When set to \code{TRUE} x-coordinate widths across all
groups are scaled based on the densiest are in the plot. Default: \code{TRUE}}

\item{yFraction}{binning factor. The range of the values in \code{x} are
binned in windows of length \code{(max(x) - min(x)) * yFraction}. Samples
within the same bin belong to the same "neighbourhood".}

\item{neighbLimit}{if the samples within the same y-axis bin are more than
neighbLimit, the samples's X coordinates will be adjusted.}

\item{adjust}{adjusts the bandwidth of the density kernel when
\code{method = "density"} (see \code{\link[stats]{density}}).}

\item{xSpread}{tuning parameter that adjusts the spread of the samples within
the same neighbourhood along the x-axis when \code{method = "neighbourhood"}.
Accepts values between 0 and 1.}

\item{labels}{optional arguments that controls the labels along the x-axis.
Must be of length \code{unique(groups)}.}

\item{plot}{logical. When \code{TRUE} the sinaplot is produced, otherwise the
function returns the new sample coordinates. Default: \code{TRUE}}

\item{main}{plot title}

\item{ylab}{Y axis title}

\item{bw}{logical. If \code{TRUE} a theme with white background and black
gridlines is used. Default: FALSE}

\item{shape}{single value or vector of \code{length(x)}. Controls the sample
shape.}

\item{size}{single value of vector of \code{length(x)}. Controls the sample
size.}

\item{color}{vector of \code{length(unique(groups))}. Controls the sample
color of each group.}
}
\value{
if \code{plot = FALSE} a list is returned containing a data.frame
with the new sample coordinates and a vector of length x that corresponds to
each sample's group.
}
\description{
The sinaplot is a data visualization chart suitable for plotting
any single variable in a multiclass dataset. It is an enhanced jitter strip
chart, where the width of the jitter is controlled by the density
distribution of the data within each class.
}
\details{
There are two available ways to define the x-axis borders for the
samples to spread within:
\itemize{
 \item{\code{method = "density"}

  A density kernel is estimated along the y-axis for every sample group. The
  borders are then defined by the density curve. Tuning parameter
  \code{adjust} can be used to control the density bandwidth in the same way
  it is used in \code{\link[stats]{density}}. }
 \item{\code{method = "neighbourhood"}:

 The borders are defined by the number of samples that 'live' in the same
 neighbourhood and the parameter \code{xSpread} in the following fashion:

 \code{xBorder = nsamples * xSpread}

  }
}
}
\examples{
x <- c(rnorm(200, 4, 1), rnorm(200, 5, 2), rnorm(200, 6, 1.5))
groups <- c(rep("Cond1", 200), rep("Cond2", 200), rep("Cond3", 200))

sinaplot(x, groups)
sinaplot(x, groups, groupwiseScale = FALSE)
sinaplot(x, groups, groupwiseScale = FALSE, adjust = 1/6)
sinaplot(x, groups, groupwiseScale = FALSE, adjust = 3)

#blood
data("blood", package = "sinaplot")
sinaplot(blood$value, blood$type, method = "neighbourhood")
sinaplot(blood$value, blood$type, method = "neighbourhood", groupwiseScale = FALSE)
}

