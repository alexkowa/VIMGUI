# ---------------------------------------
# Author: Andreas Alfons and Daniel Schopfhauser
#         Vienna University of Technology
# ---------------------------------------

TKRpbox <- function(x, pos = 1, delimiter = NULL, hscale = NULL, 
                    vscale = 1, TKRpar = list(), ...) {
  UseMethod("TKRpbox", x)
}

TKRpbox.data.frame <- function(x, pos = 1, delimiter = NULL, hscale = NULL, 
                               vscale = 1, TKRpar = list(), ...) {
  TKRpbox_work(x, pos, delimiter, hscale, vscale, TKRpar, ...)
}

TKRpbox.survey.design <- function(x, pos = 1, delimiter = NULL, hscale = NULL, 
                                  vscale = 1, TKRpar = list(), ...) {
  TKRpbox_work(x$variables, pos, delimiter, hscale, vscale, TKRpar, ...)
}

TKRpbox.default <- function(x, pos = 1, delimiter = NULL, hscale = NULL, 
                            vscale = 1, TKRpar = list(), ...) {
  TKRpbox_work(as.data.frame(x), pos, delimiter, hscale, vscale, TKRpar, ...)
}

TKRpbox_work <- function(x, pos = 1, delimiter = NULL, hscale = NULL, 
        vscale = 1, TKRpar = list(), ...) {
    # initializations and error messages
    if(is.null(dim(x))) {  # vector
        n <- length(x)
        p <- 1
        if(n == 0) stop("'x' must have positive length")
    } else {  # matrix or data.frame
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
        n <- nrow(x)
        p <- ncol(x)
        if(n == 0) stop("'x' has no rows")
        else if(p == 0) stop("'x' has no columns")
        if(is.null(colnames(x))) colnames(x) <- defaultNames(p)
    }
    if(p == 1) pos <- 1
    else {
        if(!is.numeric(pos) || length(pos) != 1) {
            stop("'pos' must be an integer specifying one column of 'x'")
        }
		if(is.null(imp_var)) nNA <- apply(x, 2, countNA)
		else nNA <- countImp(x, delimiter, imp_var)
        # number of highlight variables with missings
        nh <- length(which(nNA[-pos] > 0))
    }
    # prepare tkrplot
	if(!is.null(imp_var)) x <- cbind(x,imp_var) 
    dots <- list(x=x, pos=pos, delimiter = delimiter, ...)
    nmdots <- names(dots)
    dots$selection <- "none"
    dots$interactive <- FALSE
    putVm(".pos", pos)  # we don't need 'highlight' in 'dots'
    putVm(".main", dots$main)
    putVm(".sub", dots$sub)
    putVm(".xlab", dots$xlab)
    putVm(".ylab", dots$ylab)
    putVm(".labels", dots$labels)
    putVm(".at", dots$at)
    dev <- TKRdevice()
    if(length(hscale) == 0) {
        nhmax <- 15
        hscale <- 1 + max(nh-nhmax, 0)/nhmax  # horizontal scale factor
    }
    img <- tkrplot(dev$frame, 
        fun=function() {
            if(length(TKRpar)) par(TKRpar)
            dots$pos <- getVm(".pos")
            dots$main <- getVm(".main")
            dots$sub <- getVm(".sub")
            dots$xlab <- getVm(".xlab")
            dots$ylab <- getVm(".ylab")
            dots$labels <- getVm(".labels")
            dots$at <- getVm(".at")
            do.call(pbox, dots)
            putVm(".plt", par("plt"))
            putVm(".usr", par("usr"))
        }, 
        hscale=hscale, vscale=vscale)
    # interactive features
    labels <- if("labels" %in% nmdots) dots$labels else TRUE
    axes <- if("axes" %in% nmdots) dots$axes else TRUE
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
        if(pt$x < usr[1] || pt$x > usr[2]) {
            pos <- getVm(".pos")
            if(pt$x < usr[1]) pos <- if(pos == 1) p else (pos - 1) %% p
            else pos <- if(pos == p-1) p else (pos + 1) %% p
            putVm(".pos", pos)
            putVm(".main", NULL)
            putVm(".sub", NULL)
            putVm(".xlab", NULL)
            putVm(".ylab", NULL)
            putVm(".labels", if(is.logical(labels)) labels else axes)
            putVm(".at", NULL)
            tkrreplot(img)
        }
    }
    tkbind(img, "<ButtonRelease-1>", OnLeftClick)
    # finish TKRdevice
    finish(dev, img, 
        label=paste("Click in in the left or right margin", 
            "to switch to the previous or next variable."))
    invisible()
}
