# --------------------------------------
# Author: Andreas Alfons and Daniel Schopfhauser
#         Vienna University of Techology
# --------------------------------------

TKRmarginmatrix <- function(x, delimiter = NULL, col = c("skyblue","red","red4","orange","orange4"), 
                            alpha = NULL, hscale = NULL, 
                            vscale = NULL, TKRpar = list(), ...) {
  UseMethod("TKRmarginmatrix", x)
}

TKRmarginmatrix.data.frame <- function(x, delimiter = NULL, col = c("skyblue","red","red4","orange","orange4"), 
                                       alpha = NULL, hscale = NULL, 
                                       vscale = NULL, TKRpar = list(), ...) {
  TKRmarginmatrix_work(x, delimiter, col, alpha, hscale, vscale, TKRpar, ...)
}

TKRmarginmatrix.survey.design <- function(x, delimiter = NULL, col = c("skyblue","red","red4","orange","orange4"), 
                                          alpha = NULL, hscale = NULL, 
                                          vscale = NULL, TKRpar = list(), ...) {
  TKRmarginmatrix_work(x$variables, delimiter, col, alpha, hscale, vscale, TKRpar, ...)
}

TKRmarginmatrix.default <- function(x, delimiter = NULL, col = c("skyblue","red","red4","orange","orange4"), 
                                    alpha = NULL, hscale = NULL, 
                                    vscale = NULL, TKRpar = list(), ...) {
  TKRmarginmatrix_work(as.data.frame(x), delimiter, col, alpha, hscale, vscale, TKRpar, ...)
}

TKRmarginmatrix_work <- function(x, delimiter = NULL, col = c("skyblue","red","red4","orange","orange4"), 
        alpha = NULL, hscale = NULL, vscale = NULL, TKRpar = list(), ...) {
    # initializations and error messages
    if(!(inherits(x, c("data.frame","matrix")))) {
        stop("'x' must be a data.frame or matrix")
    }
	imp_var <- NULL
	## delimiter ##
	if(!is.null(delimiter)) {
		tmp <- grep(delimiter, colnames(x)) # Position of the missing-index
		if(length(tmp) > 0) {
			imp_var <- x[, tmp, drop = FALSE]
			x <- x[, -tmp, drop = FALSE]
		}
	}
    p <- ncol(x)
    if(p < 2) stop("'x' must be at least 2-dimensional")
    # prepare data
    if(is.data.frame(x)) x <- data.matrix(x)
    else if(mode(x) != "numeric") mode(x) <- "numeric"
    if(is.null(colnames(x))) colnames(x) <- defaultNames(p)
    # prepare tkrplot
	if(!is.null(imp_var)) x <- cbind(x,imp_var)
    dots <- list(x=x, delimiter = delimiter, ...)
    dev <- TKRdevice()
    pmax <- 4
    if(length(hscale) == 0) {
        hscale <- 1 + max(p-pmax, 0)/pmax  # horizontal scale factor
    }
    if(length(vscale) == 0) {
        vscale <- 1 + max(p-pmax, 0)/pmax  # vertical scale factor
    }
    img <- tkrplot(dev$frame, 
        fun=function() {
            if(length(TKRpar)) par(TKRpar)
            if(!is.null(alpha)) {
                bg <- par("bg")
				if(Sys.info()["sysname"] == "Windows" && bg == "transparent") bg <- windows.options()$canvas
				else if (bg == "transparent") bg <- X11.options()$canvas
                col <- alphablend(col, alpha, bg)
            }
            dots$col <- col
            do.call(marginmatrix, dots)
        }, 
        hscale=hscale, vscale=vscale)
    # finish TKRdevice
    finish(dev, img)
    invisible()
}