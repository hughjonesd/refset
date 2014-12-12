
# TODO:
# allow for empty indices like with data frames?
# indices evaluated within data frames like subset?
# this creates a view of a particular object. Could you also have
# a "data.viewable" class that, when subsetted, spits out a data.view?
make.data.view <- function(x, data, ..., dyn.idx=TRUE) {
  dots <- match.call(expand.dots=FALSE)$...
  missings <- sapply(dots, function(x) is.symbol(x) && identical(as.character(x), ""))
  # use argument recycling (of TRUE) as a substitute for non-standard eval:
  dots[missings] <- if (dyn.idx) TRUE else lapply(dim(data)[missings], seq, from=1)
  x <- deparse(substitute(x))
  isdfr <- is.data.frame(data)
  data <- substitute(data)
  pfr <- parent.frame()  
  idxval <- if (dyn.idx) {
    eval(substitute(substitute(dots)), pfr) 
  } else if (isdfr) {
    eval(substitute(dots), eval(data, pfr), pfe)
  } else {
    eval(substitute(dots), pfr)    
  }
  
  f <- function(v) {
    str(ls(pfr))
    str(data)
    indices <- if (! dyn.idx) idxval else if (isdfr) 
          eval(idxval, eval(data, pfr), pfr) else  eval(idxval, pfr) 
    if (missing(v)) {
      do.call("[", c(data, indices), envir=pfr)
    } else {
      do.call("<-", list(data, do.call("[<-", c(XXX, indices, 
            list(value=v)), envir=pfr)), envir=pfr)
    }
  }
  
  makeActiveBinding(x, f, parent.frame())
}

g <- function() {
  test <- function() evalq(dfr)
  dfr <- data.frame(a=1:5, b=6:10)
  foo <- "foo"
  make.data.view(ss, dfr, 1:3, 1:2)
  warning(test())  
  ss
}


