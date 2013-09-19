# ---------------------------------------
# Author: Daniel Schopfhauser
#         Vienna University of Technology
# ---------------------------------------

#contains different small helper functions for the GUI

#a efficent RGtk2 based function for clearning a table
clearTable <- function(table, ncol=1){
#   t <- getToolkitWidget(table)
#   model <- gtkTreeViewGetModel(t)
#   #print(model[1,])
#   d <- dim(model)
#   df <- data.frame(matrix(" ",ncol=d[1]))
#   #gtkTreeViewSetModel(t, rGtkDataFrameNew(frame=df))
#   rGtkDataFrameSetFrame(model, df)
  insertTable(table, as.data.frame(matrix(rep(" ", ncol), ncol=ncol)))
}

#a more efficent way to insert new values into a table
#gwidgets create a table structure with invisible columns which contain additional
#information like rownames or colors
insertTable <- function(table, values){
  #print(values)
  if (class(values)!="data.frame"){
    values <- as.data.frame(values)
  }
  t <- getToolkitWidget(table)
  model <- gtkTreeViewGetModel(t)
  #str(model[])
  d <- dim(model)
  df <- data.frame(matrix(model[1,], nrow=dim(values)[1], ncol=d[2], byrow=TRUE), stringsAsFactors = FALSE)
  #print(6 + (ncol(values)-1)*3)
  df[,seq(from=6, to = 6 + (ncol(values)-1)*3, by=3)] <- values
  rGtkDataFrameSetFrame(model, df)
}

# access vmGUIenv
vmGUIenv <- function() {
  get("vmGUIenvir", envir=as.environment("package:VIMGUI"))
}

# put in vmGUIenv
putVm <- function(x, value) {
  assign(x, value, envir=vmGUIenv())
}

# get from vmGUIenv
getVm <- function (x, mode="any") { 
  get(x, envir=vmGUIenv(), mode=mode, inherits=FALSE)
}

# does object exist in vmGUIenv?
existsVm <- function (x, mode="any") { 
  exists(x, envir=vmGUIenv(), mode=mode, inherits=FALSE)
}

#checks per regular expression if string is empty(only consists of whitespaces)
#used to make code more readable
isEmpty <- function(s){
  grepl("^(\\s)*$",s)
}

#splits a string of form (param1 param2 param3) into a vector(c(param1,param2,param3))
cutParam <- function(c){
  out <- strsplit(c,"((\\s)+)|((\\s)*[,;](\\s)*)")
  ret <- unlist(as.vector(sapply(out, FUN = function(s)trim(s))))
}

#tests if a string ends with different formula operators like +:*,^-
endsWithSymbol <- function(c){
  grepl("[+:*,^-](\\s)*$",c)
}

#tests if a string ends with a specific suffix
#suffix can be a vector of different suffixes
endsWithText <- function(string, suffix){
  ret <- sapply(suffix, FUN=function(s)grepl(paste(s,"(\\s)*$", sep=""),string))
  return(TRUE%in%ret)
}

#checks per regular expression if string contains a integer
#used to make code more readable
isNumber <- function(s, integer=TRUE){
  if(integer==TRUE){
    grepl("^(\\s)*[0-9]+(\\s)*$",s)
  }
  else {
    grepl("^(\\s)*[0-9]+(\\.[0-9]+)?(\\s)*$",s)
  }
}

#removes leading/tailing whitespaces
trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

#quick and better readable test if a object is of class survey
is.survey <- function(x){
  return (TRUE %in% (class(x) == "survey.design" || class(x) == "survey.design2"))
}

#returns the names of the variables in a object
#for data.frames these are simple the names of the columns
#for survey objects names of the variables are extracted
getVariableNames <- function(x){
  if (class(x) == "survey.design" || class(x) == "survey.design2"){
    colnames(x$variables)
  }
  else{
    colnames(x)
  }
}

#returns the data types of the variables of a data.frame or survey object
getVariableTypes <- function(x){
  if (class(x) == "survey.design" || class(x) == "survey.design2"){
    sapply(x$variables, class)
  }
  else{
    sapply(x, class)
  }
}

#tests if data is discret
is.categorical <- function(x, nValues=25){
  length(unique(x)) < nValues
}

#uses RGtk2 functionality to verify if a widget is currently in focus
isFocus <- function(x){
  gtkWidgetIsFocus(getToolkitWidget(x))
}

#tests if a string is "empty", i.e. if it only contains whitespaces 
#or is null
is.Empty <- function(x){
  if (is.null(x)){
    return(TRUE)
  }
  return(trim(x) == "")
}

parseVar <- function(x, ...) {
  if(length(x)==0)return("NULL")
  s <- "c("
  for ( i in 1:length(x) ) {
    s <- paste(s, x[i])
    if (i < length(x)) {
      s <- paste(s, ",")
    }
  }
  s <- paste(s, ")")
  return(s)
}

parseVarStr <- function(x, ...) {
  if(length(x)==0)return("NULL")
  s <- "c("
  for ( i in 1:length(x) ) {
    s <- paste(s, "'", x[i], "'", sep="")
    if (i < length(x)) {
      s <- paste(s, ",", sep="")
    }
  }
  s <- paste(s, ")", sep="")
  return(s)
}

#opens a dialog which allows the adjustment of datatypes in a dataframe or survey object
adjustTypesDialog <- function(original){
  dialog.window <- gbasicdialog("Adjust Data Types", width=768, height=512, do.buttons=FALSE)
  lw <- loadingWindow(parent=dialog.window)
  size(dialog.window) <- c(768, 768)
  g <- ggroup(horizontal=FALSE, container=dialog.window)
  gg <- ggroup(use.scrollwindow=TRUE, container=g, expand=TRUE)
  retData <- original
  names <- getVariableNames(original)
  types <- getVariableTypes(original)
  if (is.survey(original)){
    data <- original$variables
  }
  else{
    data <- original
  }
  dialog.layout <- glayout(container=gg, expand=TRUE)
  accept.button <- gbutton("Accept", container=g)
  widgets <- list()
  accepts <- list()
  dialog.layout[1, 1, anchor=c(0,0)] <- glabel('<span size="large"><b><u></u></b></span>', markup=TRUE)
  dialog.layout[1, 2, anchor=c(0,0)] <- glabel('<span size="large"><b><u>Variable:</u></b></span>', markup=TRUE)
  dialog.layout[1, 3, expand=TRUE,anchor=c(0,0)] <- glabel('<span size="large"><b><u>Content:</u></b></span>', markup=TRUE)
  dialog.layout[1, 4, expand=FALSE,anchor=c(0,0)] <- glabel('<span size="large"><b><u>Type:</u></b></span>', markup=TRUE)
  for(i in 1:length(names)){
    a <- gcheckbox("")
    size(a) <- c(20,15)
    svalue(a) <- TRUE
    accepts[i] <- a
    dialog.layout[i+1, 1, expand=FALSE, anchor=c(0,0)] <- a
    dialog.layout[i+1, 2, expand=FALSE, anchor=c(0,0)] <- glabel(paste('<span size="large"><b>',names[i],"</b></span>"), markup=TRUE)
    dialog.layout[i+1, 3, expand=TRUE,anchor=c(0,0)] <- glabel(substr(paste(head(data[,i]), collapse=", "), start=0, stop=75))
    w <- gdroplist(c("numeric", "factor", "logical", "character", "integer", "complex", "ordered"))
    svalue(w) <- types[i]
    dialog.layout[i+1, 4,expand=FALSE] <- w
    widgets[i] <- w
  }
  dispose(lw)
  addHandlerClicked(accept.button, handler=function(h,...){
    nTypes <- sapply(widgets, FUN=function(s)svalue(s))
    newData <- original
    if(is.survey(newData)){
      for (i in 1:length(nTypes)){
        if (nTypes[i] == "numeric"){
          newData$variables[,i] <- as.numeric(original$variables[,i])
        }
        else if (nTypes[i] == "factor"){
          newData$variables[,i] <- as.factor(original$variables[,i])
        }
        else if (nTypes[i] == "character"){
          newData$variables[,i] <- as.character(original$variables[,i])
        }
        else if (nTypes[i] == "logical"){
          newData$variables[,i] <- as.logical(original$variables[,i])
        }
        else if (nTypes[i] == "integer"){
          newData$variables[,i] <- as.integer(original$variables[,i])
        }
        else if (nTypes[i] == "complex"){
          newData$variables[,i] <- as.complex(original$variables[,i])
        }
        else if (nTypes[i] == "ordered"){
          newData$variables[,i] <- as.ordered(original$variables[,i])
        }
      }
    }
    else{
      for (i in 1:length(nTypes)){
        if (nTypes[i] == "numeric"){
          newData[,i] <- as.numeric(original[,i])
        }
        else if (nTypes[i] == "factor"){
          newData[,i] <- as.factor(original[,i])
        }
        else if (nTypes[i] == "character"){
          newData[,i] <- as.character(original[,i])
        }
        else if (nTypes[i] == "logical"){
          newData[,i] <- as.logical(original[,i])
        }
        else if (nTypes[i] == "integer"){
          newData[,i] <- as.integer(original[,i])
        }
        else if (nTypes[i] == "complex"){
          newData[,i] <- as.complex(original[,i])
        }
        else if (nTypes[i] == "ordered"){
          newData[,i] <- as.ordered(original[,i])
        }
      }
    }
    sel <- sapply(accepts, function(s)svalue(s))
    dispose(dialog.window)
    retData <<- newData[which(sel==TRUE)]
  })
  out = visible(dialog.window, set=TRUE)
  return(retData)
}

#small window with loading message
loadingWindow <- function(parent=NULL){
  window <- gwindow("Loading!", height=100, parent=parent)
  glabel('<span size="x-large"><b>Loading, Please Wait!</b></span>', markup=TRUE, container=window)
  return(window)
}


#creates a dataframe for the summary tables in the data and imputation tab
#contains the variable names, classes, number of missings and different summary statistics
createSummaryDataframe <- function(data){
  if(is.survey(data)) data <- data$variables
  #remove _imp variables
  data <- data[,grep("_imp", colnames(data), invert=TRUE)]
  types <- getVariableTypes(data)
  stats <- sapply(data, FUN=function(s){
    if(class(s) %in% c("integer","numeric"))as.character(fivenum(s, na.rm = TRUE))
    else rep("",5)
    })
  sumtable <- as.data.frame(cbind(colnames(data), types, 
                                  sapply(data, FUN=function(s)sum(is.na(s))),
                                  t(stats)))
}

#compares two imputations and findes out which variables are different i.e. where are new
#imputed observations present
#does this by counting the NA
#the variables of both datasets need the be in the same order
compareImputations <- function(old, new){
  old <- old[,grep("_imp", colnames(old), invert=TRUE)]
  new <- new[,grep("_imp", colnames(new), invert=TRUE)]
  n <- sapply(old, function(s)sum(is.na(s))) != sapply(new, function(s)sum(is.na(s)))
  names(n[which(n==TRUE)])
}

#uses gtk code to set the base color (background color of most widgets) to specified color
#R names for colors are valid
setWidgetBgColor <- function(widget, color){
  w <- getToolkitWidget(widget)
  gtkWidgetModifyBase(w, GtkStateType['normal'], as.GdkColor(color))
  gtkWidgetModifyBase(w, GtkStateType['selected'], as.GdkColor(color))
  gtkWidgetModifyBase(w, GtkStateType['active'], as.GdkColor(color))
}