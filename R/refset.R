
# TODO/IDEAS:
# Could you also have
# a "reference.frame" class that, when subsetted, spits out a data.view?
# if so, need a way to know what name is being bound to the subset...
# but then what about e.g. a[1:3,] <- b[1:3,] ? Perhaps this isn't sensible.
#
# calculated columns? somewhat like data.table?
# dfr <- data.frame(a=1:4, b=1:4)
# refset(rs, dfr, x=a+2*b)
# this basically works already:
# refset(rs, transform(dfr, x=a+2*b))

#' Create a reference to a subset of an object
#' 
#' Create a refset - a reference to a subset of an object. 
#' When the object changes, the
#' contents of the refset change, and when the refset is changed, the object
#' is changed too.
#' 
#' @param x unquoted name of the refset to create
#' @param data the object to refer to
#' @param ... indices to subset with
#' @param drop passed to \code{\link{Extract}}
#' @param dyn.idx update indices dynamically
#' @param read.only create a read-only refset which throws an error if assigned
#'        to
#' @param env environment in which to create the refset 
#' 
#' @details
#' There are two ways to call \code{refset}. The two-argument form, e.g.
#' \code{refset(myref, mydata[rows,"mycol"])}, creates a reference to the 
#' subset of \code{mydata} passed in the second argument. The three-or-more
#'argument form acts like the \code{\link{subset}} function: the indices in 
#' \code{...} are applied to \code{data}. If \code{data} is a data.frame, then
#' \code{...} is interpreted within it, so you can refer to columns directly: 
#' \code{refset(myref, mydata, a>1 & b<a,)}. Bare column names must be quoted,
#' however.
#' 
#' Empty arguments in \code{...} are allowed and are treated as indexing 
#' the whole dimension, just as in \code{\link{Extract}}.
#' 
#' By default, the indices in subset are updated dynamically. 
#' For example, if you call \code{refset(myref, mydata, x >= 3,)} and then
#' set \code{mydata$x <- 3}, the number of rows in \code{myref} will
#' increase. To turn this behaviour off and make a reference to a "fixed" 
#' subset of your object, use \code{dyn.idx=FALSE}.
#' 
#' 
#' @return \code{refset} returns \code{NULL}, but the \code{x} argument 
#' will be assigned to
#' in the calling environment (or in \code{env}, if it is specified). 
#' \code{x} will have an attribute \code{".refset."}.
#' \code{is.refset} returns \code{TRUE} or \code{FALSE}.
#' 
#' @seealso
#' Refsets are implemented using \code{makeActiveBinding}.
#' 
#' @examples
#' dfr <- data.frame(a=1:4, b=1:4)
#' ss <- dfr[1:2,]
#' refset(rs, dfr[1:2,])
#' dfr$a <- 4:1
#' ss # 1:2
#' rs # 4:3
#' 
#' # same:
#' refset(rs, dfr, 1:2, )
#' 
#' 
#' vec <- 1:10
#' refset(middle, vec[4:6])
#' vec[4:6] <- NA
#' middle 
#' middle <- 4:6 + 100
#' vec
#' 
#' # dynamic versus static indices:
#' dfr <- data.frame(a=rnorm(100), b=rnorm(100))
#' refset(ss, dfr, a>1,)
#' refset(ss.static, dfr, a>1,, dyn.idx=FALSE)
#' nrow(ss) == nrow(ss.static)
#' dfr$a <- dfr$a + 2 * dfr$b
#' 
#' 
#' precious.data <- rnorm(100)
#' refset(big, precious.data, precious.data>1, read.only=TRUE)
#' big
#' \dontrun{
#' big <- big * 2 # throws an error
#' }
#'
#' # Using refset with other functions:
#' # dynamically updated calculated column
#' dfr <- data.frame(a=rnorm(10), b=rnorm(10))
#' refset(rs, transform(dfr, x=a+2*b+rnorm(10)))
#' rs
#' rs # different
#' 
#' # Non-readonly refset with other functions. Works but gives a warning:
#' dontrun{
#' vec <- 1:5
#' refset(ssv, names(vec), read.only=FALSE)
#' ssv <- LETTERS[1:5]
#' vec
#' }
#' 
## the below is not yet true :-( 
## Works nicely with magrittr or dplyr:
# \dontrun{
# a %>% refset(mydata[1:3,1:4])
# }
#
#' @export
refset <- function(x, data, ..., drop=TRUE, dyn.idx=TRUE, read.only=FALSE,
      env=parent.frame()) {
  
  if (missing(...)) {
    ssarg <- as.character(substitute(data)[[1]])
    dots <- as.list(substitute(data)[-1:-2])
    data <- substitute(data)[[2]]
    if ("drop" %in% names(dots)) {
      drop <- eval(dots$drop, env) # should it be in env? think so
      dots$drop <- NULL
    }
    rdata <- eval(data, env)
  } else {
    ssarg <- "["
    dots <- match.call(expand.dots=FALSE)$...
    rdata <- data
    data <- substitute(data)  
  }
  x <- deparse(substitute(x))
  assignargs <- list("["="[<-", "[["="[[<-", "$"="$<-")
  assignarg <- assignargs[[ssarg]]
  if (is.null(assignarg)) {
    if (! missing(read.only) && ! read.only) {
      assignarg <- paste0(ssarg, "<-") 
      warning("Using read.only=FALSE with a non-standard argument of ", 
            sQuote(assignarg), ", assigning to ", sQuote(x), 
            " may cause unexpected behaviour.")
    } else {
      read.only <- TRUE
    }
  }
  missings <- sapply(dots, function(x) is.symbol(x) && identical(
        as.character(x), ""))
  if (length(missings)==0) missings <- logical(0)
  # use argument recycling (of TRUE) as a substitute for non-standard eval:
  dots[missings] <- if (dyn.idx) TRUE else lapply(dim(rdata)[missings], seq, 
        from=1)
  isdfr <- is.data.frame(rdata)
  idxval <- if (dyn.idx) {
    eval(substitute(substitute(dots)), env) 
  } else if (isdfr) {
    lapply(dots, eval, envir=eval(data, env), enclos=env)
  } else {
    lapply(dots, eval, envir=env)    
  }
  
  f <- function(v) {
    if (dyn.idx && isdfr && ssarg != "$") idxval <- lapply(idxval, eval, 
          eval(data, env), env) 
    if (missing(v)) {
      args <- c(data, idxval)
      if (length(idxval) > 1) args <- c(args, drop=drop)
      res <- do.call(ssarg, args, envir=env)
      if (! is.null(res)) attr(res, ".refset.") <- TRUE
      res
    } else {
      if (read.only) stop("Tried to assign to a readonly refset")
      do.call("<-", list(data, do.call(assignarg, c(data, idxval, 
        list(value=v)), envir=env)), envir=env)
    }
  }
  
  makeActiveBinding(x, f, env)
}

#' @export
#' @rdname refset
is.refset <- function(x) isTRUE(attr(x, ".refset.")) && 
      bindingIsActive(substitute(x), parent.frame())

# 
# reference.frame <- function(...) {
# #   cl <- match.call(expand.dots=FALSE)
# #   cl[[1]] <- data.frame
# #   dfr <- eval(cl)
#   dfr <- data.frame(...)
#   class(dfr) <- c("reference.frame", "data.frame")
#   dfr
# }
# 
# as.data.frame.reference.frame <- function(rfr) {
#   class(rfr) <- "data.frame"
#   rfr
# }
# 
# `[.reference.frame` <- function() {
#   
# }
# 
# `[[.reference.frame` <- function() {
#   
# }
# 
# 
# `$.reference.frame` <- function() {
#   
# }
# 
