# ------------------------------------------
# Authors: Andreas Alfons, Matthias Templ
#          and Daniel Schopfhauser
#          Vienna University of Technology
# ------------------------------------------

TKRmatrixplot <- function(x, delimiter = NULL, hscale = NULL, 
                          vscale = NULL, TKRpar = list(), ...) {
  UseMethod("TKRmatrixplot", x)
}

TKRmatrixplot.data.frame <- function(x, delimiter = NULL, hscale = NULL, 
                                     vscale = NULL, TKRpar = list(), ...) {
  TKRmatrixplot_work(x, delimiter, hscale, vscale, TKRpar, ...)
}

TKRmatrixplot.survey.design <- function(x, delimiter = NULL, hscale = NULL, 
                                        vscale = NULL, TKRpar = list(), ...) {
  TKRmatrixplot_work(x$variables, delimiter, hscale, vscale, TKRpar, ...)
}

TKRmatrixplot.default <- function(x, delimiter = NULL, hscale = NULL, 
                                  vscale = NULL, TKRpar = list(), ...) {
  TKRmatrixplot_work(as.data.frame(x), delimiter, hscale, vscale, TKRpar, ...)
}

TKRmatrixplot_work <- function(x, delimiter = NULL, hscale = NULL, 
        vscale = NULL, TKRpar = list(), ...) {
    # initializations and error messages
    if(!(inherits(x, c("data.frame","matrix")))) 
        stop("'x' must be a data.frame or matrix")
	imp_var <- NULL
	## delimiter ##
	if(!is.null(delimiter)) {
		tmp <- grep(delimiter, colnames(x)) # Position of the missing-index
		if(length(tmp) > 0) {
			imp_var <- x[, tmp, drop = FALSE]
			x <- x[, -tmp, drop = FALSE]
		}
	}
    n <- nrow(x)
    p <- ncol(x)
    if(p < 2) stop("'x' must be at least 2-dimensional")
    # prepare data
    if(is.data.frame(x)) x <- data.matrix(x)
    else if(mode(x) != "numeric") mode(x) <- "numeric"
    if(is.null(colnames(x))) colnames(x) <- defaultNames(p)
    # prepare tkrplot
	if(!is.null(imp_var)) x <- cbind(x,imp_var)
	dots <- list(x=x, delimiter = delimiter, ...)
    dots$interactive <- FALSE
    if(existsVm(".sortby")) rmVm(".sortby")  # from prevent previous calls
    dev <- TKRdevice()
    if(length(hscale) == 0) {
        pmax <- 50
        hscale <- 1 + max(p-pmax, 0)/pmax  # horizontal scale factor
    }
    if(length(vscale) == 0) {
#        nmax <- dev$resolution[2]/3
        nmax <- 400
        vscale <- 1 + max(n-nmax, 0)/nmax  # vertical scale factor
    }
    img <- tkrplot(dev$frame, 
        fun=function() {
            if(length(TKRpar)) par(TKRpar)
            if(existsVm(".sortby")) dots$sortby <- getVm(".sortby")
            do.call(matrixplot, dots)
            putVm(".plt", par("plt"))
            putVm(".usr", par("usr"))
        }, 
        hscale=hscale, vscale=vscale)
    # interactive features
    OnLeftClick <- function(x, y) {
        plt <- getVm(".plt")
        usr <- getVm(".usr")
        width <- as.numeric(tclvalue(tkwinfo("reqwidth", img)))
        height <- as.numeric(tclvalue(tkwinfo("reqheight", img)))
        xMin <- plt[1]*width
        xMax <- plt[2]*width
        yMin <- plt[3]*height
        yMax <- plt[4]*height
        rangeX <- diff(usr[1:2])
        rangeY <- diff(usr[3:4])
        x <- as.numeric(x)
        y <- height-as.numeric(y)
        pt <- list(x=usr[1]+(x-xMin)*rangeX/(xMax-xMin), 
            y=usr[3]+(y-yMin)*rangeY/(yMax-yMin))
        if(max(0.5, usr[1]) <= pt$x && pt$x < min(p+0.5, usr[2]) && 
                max(0.5, usr[3]) <= pt$y && pt$y <= min(n+0.5, usr[4])) {
            sortby <- round(pt$x)
            putVm(".sortby", sortby)
            tkrreplot(img)
            svar <- colnames(dots$x)[sortby]  # new sort variable
            cat(gettextf("Matrix plot sorted by variable '%s'.\n", svar))
        }
    } 
    tkbind(img, "<ButtonRelease-1>", OnLeftClick)
    # finish TKRdevice
    finish(dev, img, 
        label="Click in a column to sort by the corresponding variable.")
    invisible()
}
