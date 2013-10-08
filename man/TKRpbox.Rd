\name{TKRpbox}
\alias{TKRpbox}
\title{Parallel boxplots with information about missing/imputed values}
\usage{
  TKRpbox(x, pos = 1, delimiter = NULL, hscale = NULL,
    vscale = 1, TKRpar = list(), ...)
}
\arguments{
  \item{x}{a vector, matrix or \code{data.frame}.}

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

  \item{pos}{a numeric value giving the index of the
  variable of interest. Additional variables in \code{x}
  are used for grouping according to missingness/number of
  imputed missings.}

  \item{\dots}{for \code{pbox}, further arguments and
  graphical parameters to be passed to
  \code{\link[graphics]{boxplot}} and other functions.  For
  \code{TKRpbox}, further arguments to be passed to
  \code{pbox}.}

  \item{hscale}{horizontal scale factor for plot to be
  embedded in a \emph{Tcl/Tk} window (see
  \sQuote{Details}).  The default value depends on the
  number of boxes to be drawn.}

  \item{vscale}{vertical scale factor for the plot to be
  embedded in a \emph{Tcl/Tk} window (see
  \sQuote{Details}).}

  \item{TKRpar}{a list of graphical parameters to be set
  for the plot to be embedded in a \emph{Tcl/Tk} window
  (see \sQuote{Details} and \code{\link[graphics]{par}}).}
}
\value{
  a list as returned by \code{\link[graphics]{boxplot}}.
}
\description{
  Boxplot of one variable of interest plus information
  about missing/imputed values in other variables.
}
\details{
  This plot consists of several boxplots. First, a standard
  boxplot of the variable of interest is produced. Second,
  boxplots grouped by observed and missing/imputed values
  according to \code{selection} are produced for the
  variable of interest.

  Additionally, the frequencies of the missing/imputed
  values can be represented by numbers.  If so, the first
  line corresponds to the observed values of the variable
  of interest and their distribution in the different
  groups, the second line to the missing/imputed values.

  If \code{interactive=TRUE}, clicking in the left margin
  of the plot results in switching to the previous variable
  and clicking in the right margin results in switching to
  the next variable.  Clicking anywhere else on the
  graphics device quits the interactive session.

  \code{TKRpbox} behaves like \code{pbox} with
  \code{selection="none"}, but uses
  \code{\link[tkrplot]{tkrplot}} to embed the plot in a
  \emph{Tcl/Tk} window.  This is useful for drawing a large
  number of parallel boxes, because scrollbars allow to
  move from one part of the plot to another.
}
\note{
  Some of the argument names and positions have changed
  with version 1.3 due to extended functionality and for
  more consistency with other plot functions in \code{VIM}.
  For back compatibility, the arguments \code{names} and
  \code{cex.text} can still be supplied to \code{\dots{}}
  and are handled correctly.  Nevertheless, they are
  deprecated and no longer documented.  Use \code{labels}
  and \code{cex.numbers} instead.
}
\examples{
data(chorizonDL, package = "VIM")
## for missing values
pbox(log(chorizonDL[, c(4,5,8,10,11,16:17,19,25,29,37,38,40)]))

## for imputed values
pbox(kNN(log(chorizonDL[, c(4,8,10,11,17,19,25,29,37,38,40)])),
     delimiter = "_imp")
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
\seealso{
  \code{\link{parcoordMiss}}
}
\keyword{hplot}

