# ---------------------------------------
# Author: Andreas Alfons and Daniel Schopfhauser
#         Vienna University of Technology
# ---------------------------------------

TKRparcoordMiss <- function(x, delimiter = NULL,  highlight = NULL,
                            selection = c("any","all"), plotvars = NULL, plotNA = TRUE, 
                            col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                            alpha = NULL, hscale = NULL, vscale = 1, 
                            TKRpar = list(), ...) {
  UseMethod("TKRparcoordMiss", x)
}

TKRparcoordMiss.data.frame <- function(x, delimiter = NULL,  highlight = NULL,
                                       selection = c("any","all"), plotvars = NULL, plotNA = TRUE, 
                                       col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                                       alpha = NULL, hscale = NULL, vscale = 1, 
                                       TKRpar = list(), ...) {
  TKRparcoordMiss_work(x, delimiter,  highlight, selection, plotvars, plotNA, 
                       col, alpha, hscale, vscale, TKRpar, ...)
}

TKRparcoordMiss.survey.design <- function(x, delimiter = NULL,  highlight = NULL,
                                          selection = c("any","all"), plotvars = NULL, plotNA = TRUE, 
                                          col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                                          alpha = NULL, hscale = NULL, vscale = 1, 
                                          TKRpar = list(), ...) {
  TKRparcoordMiss_work(x$variables, delimiter,  highlight, selection, plotvars, plotNA, 
                       col, alpha, hscale, vscale, TKRpar, ...)
}

TKRparcoordMiss.default <- function(x, delimiter = NULL,  highlight = NULL,
                                    selection = c("any","all"), plotvars = NULL, plotNA = TRUE, 
                                    col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                                    alpha = NULL, hscale = NULL, vscale = 1, 
                                    TKRpar = list(), ...) {
  TKRparcoordMiss_work(as.data.frame(x), delimiter,  highlight, selection, plotvars, plotNA, 
                       col, alpha, hscale, vscale, TKRpar, ...)
}

TKRparcoordMiss_work <- function(x, delimiter = NULL,  highlight = NULL,
		selection = c("any","all"), plotvars = NULL, plotNA = TRUE, 
        col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
        alpha = NULL, hscale = NULL, vscale = 1, 
        TKRpar = list(), ...) {
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
    plotNA <- isTRUE(plotNA)
    # prepare data
    if(is.data.frame(z)) z <- data.matrix(z)
    else if(mode(x) != "numeric") mode(x) <- "numeric"
	if(is.null(imp_var)) haveNA <- any(isNA(z, selection="any"))
	else haveNA <- any(isImp(z, pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = selection)[["missh"]])
	# prepare tkrplot
    if(is.null(highlight)) highlight <- cn
    else if(!is.character(highlight)) highlight <- cn[highlight]
    plotvars <- colnames(z)
	if(!is.null(imp_var)) x <- cbind(x,imp_var) 
    dots <- list(x=x, delimiter = delimiter, selection=selection, plotvars=plotvars, ...)
    dots$interactive <- FALSE
    putVm(".highlight", highlight)  # we don't need 'highlight' in 'dots'
    putVm(".plotNA", plotNA)  # we don't need 'plotNA' in 'dots'
    dev <- TKRdevice()
    if(length(hscale) == 0) {
        pmax <- 25
        hscale <- 1 + max(pz-pmax, 0)/pmax  # horizontal scale factor
    }
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
            dots$plotNA <- getVm(".plotNA")
            do.call(parcoordMiss, dots)
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
        if(max(1, usr[1]) <= pt$x && pt$x < min(px, usr[2]) && 
                max(0, usr[3]) <= pt$y && 
                if(haveNA) TRUE else pt$y <= min(1, usr[4])) {
            if(pt$y <= min(1, usr[4])) {
                # variable selected or deselected
                i <- round(pt$x)
                highlight <- getVm(".highlight")
                highlight <- 
                    if(plotvars[i] %in% highlight) 
                        setdiff(highlight, plotvars[i]) 
                    else c(highlight, plotvars[i])
                putVm(".highlight", highlight)
                tkrreplot(img)
                highlightInfo(highlight, selection)  # print out current selection
            } else {
                # toggle separate NA level for missings in plot variables
                plotNA <- !getVm(".plotNA")
                putVm(".plotNA", plotNA)
                tkrreplot(img)
            }
        }
    }
    tkbind(img, "<ButtonRelease-1>", OnLeftClick)
    # finish TKRdevice
    lab <- paste("Click on a coordinate axis to add to", 
        "or remove from the highlight selection.")
    if(haveNA) {
		if(is.null(imp_var)) label <- "missing"
		else label <- "imputed missing"
		lab <- paste(lab, "\n", 
            "Click in the top margin to toggle visualizing ", 
            label, " values in the plot variables.", sep="")
	}
    finish(dev, img, label=lab)
    highlightInfo(highlight, selection)  # print out current selection
    invisible()
}
