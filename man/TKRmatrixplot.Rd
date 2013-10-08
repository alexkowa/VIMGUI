\name{TKRmatrixplot}
\alias{TKRmatrixplot}
\title{Matrix plot}
\usage{
  TKRmatrixplot(x, delimiter = NULL, hscale = NULL,
    vscale = NULL, TKRpar = list(), ...)
}
\arguments{
  \item{x}{a matrix or \code{data.frame}.}

  \item{delimiter}{a character-vector to distinguish
  between variables and imputation-indices for imputed
  variables (therefore, \code{x} needs to have
  \code{\link{colnames}}). If given, it is used to
  determine the corresponding imputation-index for any
  imputed variable (a logical-vector indicating which
  values of the variable have been imputed). If such
  imputation-indices are found, they are used for
  highlighting and the colors are adjusted according to the
  given colors for imputed variables (see \code{col}).}

  \item{\dots}{for \code{matrixplot} and \code{iimagMiss},
  further graphical parameters to be passed to
  \code{\link[graphics]{plot.window}},
  \code{\link[graphics]{title}} and
  \code{\link[graphics]{axis}}.  For \code{TKRmatrixplot},
  further arguments to be passed to \code{matrixplot}.}

  \item{hscale}{horizontal scale factor for plot to be
  embedded in a \emph{Tcl/Tk} window (see
  \sQuote{Details}).  The default value depends on the
  number of variables.}

  \item{vscale}{vertical scale factor for the plot to be
  embedded in a \emph{Tcl/Tk} window (see
  \sQuote{Details}).  The default value depends on the
  number of observations.}

  \item{TKRpar}{a list of graphical parameters to be set
  for the plot to be embedded in a \emph{Tcl/Tk} window
  (see \sQuote{Details} and \code{\link[graphics]{par}}).}
}
\description{
  Create a matrix plot, in which all cells of a data matrix
  are visualized by rectangles.  Available data is coded
  according to a continuous color scheme, while
  missing/imputed data is visualized by a clearly
  distinguishable color.
}
\details{
  In a \emph{matrix plot}, all cells of a data matrix are
  visualized by rectangles.  Available data is coded
  according to a continuous color scheme. To compute the
  colors via interpolation, the variables are first scaled
  to the interval \eqn{$[0,1]$}{[0,1]}. Missing/imputed
  values can then be visualized by a clearly
  distinguishable color. It is thereby possible to use
  colors in the \emph{HCL} or \emph{RGB} color space. A
  simple way of visualizing the magnitude of the available
  data is to apply a greyscale, which has the advantage
  that missing/imputed values can easily be distinguished
  by using a color such as red/orange.  Note that
  \code{-Inf} and \code{Inf} are always assigned the begin
  and end color, respectively, of the continuous color
  scheme.

  Additionally, the observations can be sorted by the
  magnitude of a selected variable.  If \code{interactive}
  is \code{TRUE}, clicking in a column redraws the plot
  with observations sorted by the corresponding variable.
  Clicking anywhere outside the plot region quits the
  interactive session.

  \code{TKRmatrixplot} behaves like \code{matrixplot}, but
  uses \code{\link[tkrplot]{tkrplot}} to embed the plot in
  a \emph{Tcl/Tk} window. This is useful if the number of
  observations and/or variables is large, because
  scrollbars allow to move from one part of the plot to
  another.
}
\note{
  This is a much more powerful extension to the function
  \code{imagmiss} in the former CRAN package \code{dprep}.

  \code{iimagMiss} is deprecated and may be omitted in
  future versions of \code{VIM}.  Use \code{matrixplot}
  instead.
}
\examples{
data(sleep, package = "VIM")
## for missing values
x <- sleep[, -(8:10)]
x[,c(1,2,4,6,7)] <- log10(x[,c(1,2,4,6,7)])
matrixplot(x, sortby = "BrainWgt")

## for imputed values
x_imp <- kNN(sleep[, -(8:10)])
x_imp[,c(1,2,4,6,7)] <- log10(x_imp[,c(1,2,4,6,7)])
matrixplot(x_imp, delimiter = "_imp", sortby = "BrainWgt")
}
\author{
  Andreas Alfons, Matthias Templ, modifications by Bernd
  Prantner
}
\references{
  M. Templ, A. Alfons, P. Filzmoser (2012) Exploring
  incomplete data using visualization tools.  \emph{Journal
  of Advances in Data Analysis and Classification}, Online
  first. DOI: 10.1007/s11634-011-0102-y.
}
\keyword{hplot}

