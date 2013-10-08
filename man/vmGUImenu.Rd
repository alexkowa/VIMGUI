\name{vmGUImenu}
\alias{vmGUIenvir}
\alias{vmGUImenu}
\title{GUI for Visualization and Imputation of Missing Values}
\usage{
  vmGUImenu()
}
\description{
  Graphical user interface for visualization and imputation
  of missing values.
}
\details{
  The \emph{Data} menu allows to select a data set from the
  workspace or load data into the workspace from
  \code{RData} files.  Furthermore, it can be used to
  transform variables, which are then appended to the data
  set in use. Commonly used transformations in official
  statistics are available, e.g., the Box-Cox
  transformation and the log-transformation as an important
  special case of the Box-Cox transformation.  In addition,
  several other transformations that are frequently used
  for compositional data are implemented. Background maps
  and coordinates for spatial data can be selected in the
  data menu as well.

  After a data set was chosen, variables can be selected in
  the main menu, along with a method for scaling.  An
  important feature is that the variables will be used in
  the same order as they were selected, which is especially
  useful for parallel coordinate plots.  Variables for
  highlighting are distinguished from the plot variables
  and can be selected separately.  For more than one
  variable chosen for highlighting, it is possible to
  select whether observations with missing values in any or
  in all of these variables should be highlighted.

  A plot method can be selected from the
  \emph{Visualization} menu. Note that plots that are not
  applicable to the selected variables are disabled, for
  example, if only one plot variable is selected,
  multivariate plots cannot be chosen.

  The \emph{Imputation} menu offers robust imputation
  methods to impute variables of the data set.

  The \emph{Diagnostics} menu is similar to the
  \emph{Visualization} menu, but is designed to verify the
  results after the imputation of missing values.

  Last, but not least, the \emph{Options} menu allows to
  set the colors, alpha channel and the delimiter for
  imputed variables to be used in the plots.  In addition,
  it contains an option to embed multivariate plots in
  \code{Tcl/Tk} windows. This is useful if the number of
  observations and/or variables is large, because
  scrollbars allow to move from one part of the plot to
  another.

  Internal information regarding the VIM GUI is stored in
  the environment \code{vmGUIenvir}.
}
\author{
  Andreas Alfons, based on an initial design by Matthias
  Templ, modifications by Bernd Prantner
}
\references{
  M. Templ, A. Alfons, P. Filzmoser (2012) Exploring
  incomplete data using visualization tools.  \emph{Journal
  of Advances in Data Analysis and Classification}, Online
  first. DOI: 10.1007/s11634-011-0102-y.
}
\keyword{hplot}
\keyword{multivariate}

