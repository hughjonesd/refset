
# TODO/IDEAS:

"
Should I make rs %r% dfr or refset(rs, dfr) work without extra commas?
If so, it has to take the real semantics of dfr, not dfr[] ...

make is.refset() work for e.g. parcel$ref ... (how? is.refset has env argument? and
insists on is.name of its first argument?)

way to not set .refset. attribute?

do you need parcel? One possibility: parcel() should be more generally a 
way of passing around activeBindings, which remember their environment.

Note: pryr can do stuff like:
foo %<a-% if (missing(value)) dfr[1,] else dfr[1,] <<- value
 But I think yours is probably easier.
"

# would like this to work but it doesn't at the mo:
# dfr <- data.frame(a=1:2, b=1:2)
# refset(rs, dfr,)
# names(rs) <- c("c", "d")
# similarly with rownames() etc...
#
# Observe:
# > dfr <- data.frame(a=3:4, b=5:6)
# > refset(rs, dfr,,)
#   a b
# 1 3 5
# 2 4 6
# > `length<-`(rs, 1)
# $a
# [1] 3 4
# 
# > rs
#   a b
# 1 3 5
# 2 4 6
# > length(rs) <- 1
# > rs
#   a b
# 1 3 3
# 2 4 4
#
# why this happens:
# the call evaluates to `length<-`(rs, 1)
# this returns a 1-length version of dfr, i.e. just dfr$a
# the result is automatically assigned to rs
# but we haven't really changed the length of dfr
# so we get the repetition
# if you do e.g. names(x)[1] <- "jim"
# then names(x) is called, the first component of the result is replaced by "jim"
# and the result is passed to `names<-` ... then the result is assigned to 
# x. Like so:
#
# tmp <- names(x)[1]
# x <- `names<-`(x, tmp)
#
# at the moment, the first x will give you the relevant names. Fine.
# the result of the `names<-` call will also be what you want.
# but the evaluation will give you a subset.
# arguably behaviour for names is correct, since if you call e.g.
# names(dfr[1:2,]) you don't affect the names of dfr.
# similarly for length:
# vec <- 1:10
# length(vec[1:5]) <- 2 # 1 2 1 2 1 6 7 8 9 10
# even if you do refset(rs, vec,) you are implicitly creating a 
# reference to vec[]... and again, length(vec[]) does not change the length...

#' Create a reference to a subset of an object
#' 
#' Create a refset - a reference to a subset of an object. 
#' When the object changes, the
#' contents of the refset change, and when the refset is changed, the object
#' is changed too.
#' 
#' @param x name of the refset to create, as a bare name or character string
#' @param data the object to refer to
#' @param ... indices to subset with
#' @param drop passed to \code{\link{Extract}}
#' @param dyn.idx update indices dynamically
#' @param read.only create a read-only refset which throws an error if assigned
#'        to
#' @param eval.env environment in which \code{data} and indices will be evaluated
#' @param assign.env environment in which the variable named by \code{x} will be created 
#' 
#' @details
#' There are two ways to call \code{refset}. The two-argument form, e.g.
#' \code{refset(myref, mydata[rows,"column"])}, creates a reference to the 
#' subset of \code{mydata} passed in the second argument. The three-or-more
#'argument form acts like the \code{\link{subset}} function: the indices in 
#' \code{...} are applied to \code{data}. If \code{data} is a data.frame, then 
#' the indices are interpreted within it, so you can refer to columns directly: 
#' \code{refset(myref, mydata, a>1 & b<a,)}. Bare column names must be quoted,
#' however.
#' 
#' Empty arguments in \code{...} are allowed and are treated as indexing 
#' the whole dimension, just as in \code{\link{Extract}}.
#' 
#' By default, the indices in subset are updated dynamically. 
#' For example, if you call \code{refset(myref, mydata, x >= 3,)} and then
#' set \code{mydata$x <- 3}, the number of rows in \code{myref} will probably
#' increase. To turn this behaviour off and make a reference to a "fixed" 
#' subset of your object, use \code{dyn.idx=FALSE}.
#' 
#' \code{\%r\%} is an infix version of the two-argument form.
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
#' @family parcel
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
#' # same:
#' rs %r% dfr[1:2,]
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
      eval.env=parent.frame(), assign.env=parent.frame()) {
  env <- eval.env
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
  if (is.name(substitute(x))) x <- deparse(substitute(x))
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
  
  if (exists(x, where=assign.env, inherits=FALSE)) rm(x, pos=env)
  makeActiveBinding(x, f, assign.env)
}

#' @export
#' @rdname refset
is.refset <- function(x) isTRUE(attr(x, ".refset.")) && 
      bindingIsActive(substitute(x), parent.frame())

#' @export
#' @rdname refset
`%r%` <- refset

#' Wrap an expression and its environment into a parcel.
#' 
#' Refsets (and other active bindings) cannot be passed as function
#' arguments, since doing so makes a copy. \code{wrap} allows you to pass
#' arbitrary expressions between functions. 
#' 
#' @param expr an R expression
#' @param env environment in which \code{expr} is to be evaluated
#' 
#' @return
#' An object of class 'parcel', with components \code{expr} and \code{env}.
#' 
#' @seealso 
#' To evaluate the parcel contents in the environment \code{env}, call 
#' \code{\link{contents}} on the parcel. To change parcel contents use
#' \code{\link{contents<-}}.To rebind the contents to a variable, use
#' \code{\link{unwrap_as}}.
#' 
#' 
#' @examples
#' dfr <- data.frame(a=1:5, b=1:5)
#' rs %r% dfr[1:2,]
#' parcel <- wrap(rs)
#' f <- function (parcel) contents(parcel) <- contents(parcel)*2
#' f(parcel)
#' f(parcel)
#'     
wrap <- function(expr, env=parent.frame()) {
  stopifnot(is.environment(env))
  parcel <- new.env(parent=env) # is parent=env necessary?
  parcel$env <- env
  expr <- match.call()$expr
  parcel$expr <- expr
  class(parcel) <- c("parcel", class(parcel))
  parcel
}

wrapset <- function(data, ..., env=parent.frame()) {
  stopifnot(is.environment(env))
  parcel <- new.env(parent=env) # is parent=env necessary?
  parcel$env <- env
  mc <- match.call(expand.dots=TRUE)
  mc$env <- NULL
  mc$x <- quote(expr)
  mc$eval.env=env
  mc$assign.env=parcel
  mc[[1]] <- quote(refset)
  eval(mc)
  class(parcel) <- c("parcel", class(parcel))
  parcel
}

is.parcel <- function(x) inherits(x, "parcel")

contents <- function(parcel) {
  stopifnot(is.parcel(parcel))
  eval(parcel$expr, parcel$env)
}

`contents<-` <- function(parcel, value) {
  stopifnot(is.parcel(parcel))
  parcel$value <- value
  expr2 <- substitute(expr <- value, parcel)
  rm("value", pos=parcel)
  eval(expr2, parcel$env)
  parcel
}

unwrap_as <- function(x, parcel, env=parent.frame()) {
  x <- deparse(substitute(x))
  f <- function(val) if (missing(val)) contents(parcel) else contents(parcel) <- val
  if (exists(x, where=env, inherits=FALSE)) rm(x, pos=env)
  makeActiveBinding(x, f, env=env) 
}
