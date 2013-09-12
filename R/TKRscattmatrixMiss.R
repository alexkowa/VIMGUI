# --------------------------------------
# Author: Andreas Alfons and Daniel Schopfhauser
#         Vienna University of Techology
# --------------------------------------

TKRscattmatrixMiss <- function(x, delimiter = NULL, highlight = NULL, 
                               selection = c("any","all"), 
                               plotvars = NULL, col = c("skyblue", "red", "orange"), 
                               alpha = NULL, hscale = NULL, 
                               vscale = NULL, TKRpar = list(), ...) {
  UseMethod("TKRscattmatrixMiss", x)
}

TKRscattmatrixMiss.data.frame <- function(x, delimiter = NULL, highlight = NULL, 
                                          selection = c("any","all"), 
                                          plotvars = NULL, col = c("skyblue", "red", "orange"), 
                                          alpha = NULL, hscale = NULL, 
                                          vscale = NULL, TKRpar = list(), ...) {
  TKRscattmatrixMiss_work(x, delimiter, highlight, selection, plotvars, 
                          col, alpha, hscale, vscale, TKRpar, ...)
}

TKRscattmatrixMiss.survey.design <- function(x, delimiter = NULL, highlight = NULL, 
                                             selection = c("any","all"), 
                                             plotvars = NULL, col = c("skyblue", "red", "orange"), 
                                             alpha = NULL, hscale = NULL, 
                                             vscale = NULL, TKRpar = list(), ...) {
  TKRscattmatrixMiss_work(x$variables, delimiter, highlight, selection, plotvars, 
                          col, alpha, hscale, vscale, TKRpar, ...)
}

TKRscattmatrixMiss.default <- function(x, delimiter = NULL, highlight = NULL, 
                                       selection = c("any","all"), 
                                       plotvars = NULL, col = c("skyblue", "red", "orange"), 
                                       alpha = NULL, hscale = NULL, 
                                       vscale = NULL, TKRpar = list(), ...) {
  TKRscattmatrixMiss_work(as.data.frame(x), delimiter, highlight, selection, plotvars, 
                          col, alpha, hscale, vscale, TKRpar, ...)
}

TKRscattmatrixMiss_work <- function(x, delimiter = NULL, highlight = NULL, 
        selection = c("any","all"), 
        plotvars = NULL, col = c("skyblue", "red", "orange"), 
        alpha = NULL, hscale = NULL, 
        vscale = NULL, TKRpar = list(), ...) {
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
    px <- ncol(x)
    cn <- colnames(x)
    if(is.null(cn)) colnames(x) <- defaultNames(px)
    if(length(highlight) > ncol(x)) stop("'highlight' is too long")
    if(length(plotvars) > ncol(x)) stop("'plotvars' is too long")
    z <- if(is.null(plotvars)) x else x[, plotvars, drop=FALSE]
    pz <- ncol(z)
    if(pz < 2) stop("the data to be plotted must be at least 2-dimensional")
    selection <- match.arg(selection)
    # prepare tkrplot
    if(is.null(highlight)) highlight <- cn
    else if(!is.character(highlight)) highlight <- cn[highlight]
    plotvars <- colnames(z)
	if(!is.null(imp_var)) x <- cbind(x,imp_var) 
	dots <- list(x=x, delimiter = delimiter, selection=selection, plotvars=plotvars, ...)
    # 'gap', 'oma' and 'layout' are needed later on
    if(is.null(dots$oma)) {
        # only 'oma' is used by 'pairs' for outer margins
        dots$oma <- rep.int(4, 4)
        if(!is.null(dots$main)) dots$oma[3] <- 6
        if(!is.null(dots$sub)) dots$oma[1] <- 5
    }
    if(is.null(dots$gap)) dots$gap <- 1
    if(is.null(dots$layout)) dots$layout <- "matrix"
    dots$interactive <- FALSE
    putVm(".highlight", highlight)  # we don't need 'highlight' in 'dots'
    dev <- TKRdevice()
    pmax <- 5
    if(length(hscale) == 0) {
        hscale <- 1 + max(pz-pmax, 0)/pmax  # horizontal scale factor
    }
    if(length(vscale) == 0) {
        vscale <- 1 + max(pz-pmax, 0)/pmax  # vertical scale factor
    }
    rf <- if(pz == 2) 5/6 else 2/3  # reduction factor for line height
    img <- tkrplot(dev$frame, 
        fun=function() {
            if(length(TKRpar)) par(TKRpar)
            dots$highlight <- getVm(".highlight")
            if(!is.null(alpha)) {
                bg <- par("bg")
				if(Sys.info()["sysname"] == "Windows" && bg == "transparent") bg <- windows.options()$canvas
				else if (bg == "transparent") bg <- X11.options()$canvas
                col <- alphablend(col, alpha, bg)
            }
            dots$col <- col
            do.call(scattmatrixMiss, dots)
            par(mar=dots$oma*rf, usr=c(0,1,0,1))
            putVm(".plt", par("plt"))
            putVm(".cxy", par("cxy"))
        }, 
        hscale=hscale, vscale=vscale)
    # interactive features
    # retrieve geometry of graphics device
    xcenter <- seq(from=1/(2*pz), by=1/pz, length.out=pz)
    ycenter <- if(dots$layout == "matrix") rev(xcenter) else xcenter
    cxy <- getVm(".cxy")  # cxy[2] gives the line height of the graphics device
    lxy <- 1/(2*pz) - dots$gap*cxy[2]*rf/2  # half side length of the panels
    xleft <- xcenter - lxy
    ybottom <- ycenter - lxy
    xright <- xcenter + lxy
    ytop <- ycenter + lxy
    OnLeftClick <- function(x, y) {
        plt <- getVm(".plt")
        width <- as.numeric(tclvalue(tkwinfo("reqwidth", img)))
        height <- as.numeric(tclvalue(tkwinfo("reqheight", img)))
        xMin <- plt[1]*width
        xMax <- plt[2]*width
        yMin <- plt[3]*height
        yMax <- plt[4]*height
        x <- as.numeric(x)
        y <- height-as.numeric(y)
        pt <- list(x=(x-xMin)/(xMax-xMin), y=(y-yMin)/(yMax-yMin))
        i <- which(pt$y > ybottom & pt$y < ytop)
        j <- which(pt$x > xleft & pt$x < xright)
        if(length(i) && length(j) && i == j) {
            highlight <- getVm(".highlight")
            highlight <- 
                if(plotvars[i] %in% highlight) 
                    setdiff(highlight, plotvars[i]) 
                else c(highlight, plotvars[i])
            putVm(".highlight", highlight)
            tkrreplot(img)
            highlightInfo(highlight, selection)  # print out current selection
        }
    }
    tkbind(img, "<ButtonRelease-1>", OnLeftClick)
    # finish TKRdevice
    finish(dev, img, 
        label=paste("Click in a diagonal panel to add to", 
            "or remove from the highlight selection."))
    highlightInfo(highlight, selection)  # print out current selection
    invisible()
    
}
