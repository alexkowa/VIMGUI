# --------------------------------------
# Author: Andreas Alfons
#         Vienna University of Techology
# --------------------------------------

TKRaggr <- function(x, ..., delimiter = NULL, hscale = NULL, vscale = NULL, TKRpar = list()) {
    # initializations and error messages
	if(is.null(dim(x))) {
        nx <- length(x)
        px <- 1
        if(nx == 0) stop("'x' must have positive length")
    } else {
		imp_var <- NULL
		## delimiter ##
		if(!is.null(delimiter)) {
			tmp <- grep(delimiter, colnames(x)) # Position of the missing-index
			if(length(tmp) > 0) imp_var <- x[, tmp, drop = FALSE]
		}
        nx <- nrow(x)
        px <- ncol(x) - ifelse(is.null(ncol(imp_var)),0,ncol(imp_var))
        if(nx == 0) stop("'x' has no rows")
        else if(px == 0) stop("'x' has no columns")
    }
    tmp <- aggr(x, delimiter = delimiter, plot=FALSE)
    nc <- nrow(tmp$tabcomb)
    # prepare tkrplot
    dots <- list(x=tmp, ...)
    dev <- TKRdevice()
    if(length(hscale) == 0) {
        pmax <- 40
        hscale <- 1 + max(px-pmax, 0)/pmax  # horizontal scale factor
    }
    if(length(vscale) == 0) {
        nmax <- 125
        vscale <- 1 + max(nc-nmax, 0)/nmax  # vertical scale factor
    }
    img <- tkrplot(dev$frame, 
        fun=function() {
            if(length(TKRpar)) par(TKRpar)
            do.call(plot.aggr, dots)
        }, 
        hscale=hscale, vscale=vscale)
    # finish TKRdevice
    finish(dev, img)
    invisible()
}