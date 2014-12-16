[![Build Status](https://travis-ci.org/hughjonesd/refset.svg?branch=master)](https://travis-ci.org/hughjonesd/refset)

The skinny
----------

### Installation

``` r
library(devtools)
install_github("hughjonesd/refset")
```

### Creating a refset

``` r
library(refset)
employees <- data.frame(
      id=1:4, 
      name=c("James", "Sylvia", "Meng Qi", "Luis"), 
      age=c(28,44,38, 23), 
      gender=factor(c("M", "F", "F", "M")), 
      stringsAsFactors=FALSE)
      
refset(rs, employees[1:2,])
```

### Refsets refer to the original

``` r
rs
```

    ##   id   name age gender
    ## 1  1  James  28      M
    ## 2  2 Sylvia  44      F

``` r
employees$name[1] <- "Jimmy"
rs
```

    ##   id   name age gender
    ## 1  1  Jimmy  28      M
    ## 2  2 Sylvia  44      F

### Refsets change the original

``` r
rs$age <- c(29, 45)
employees$age
```

    ## [1] 29 45 38 23

### Assigning to a new variable breaks the link

``` r
ss <- rs
employees$name[2] <- "Silvia"
rs$name[2]
```

    ## [1] "Silvia"

``` r
ss$name[2]
```

    ## [1] "Sylvia"

### You can have refsets of refsets

``` r
refset(rs2, rs$id)
rs2 
```

    ## [1] 1 2

``` r
rs$id <- rs$id + 1000
rs2
```

    ## [1] 1001 1002

``` r
rs2 <- 101:102
employees$id
```

    ## [1] 101 102   3   4

### Refset size can change dynamically

``` r
# the multi-argument form. Note the empty argument, to select all columns:
refset(rsd, employees, age < 30, , drop=FALSE)
rsd
```

    ##    id  name age gender
    ## 1 101 Jimmy  29      M
    ## 4   4  Luis  23      M

``` r
employees$age <- employees$age + 1
rsd
```

    ##   id name age gender
    ## 4  4 Luis  24      M

### You can refset any subsettable object...

``` r
vec <- 1:10
refset(rs, vec, 4:6)
rs <- rs*10
vec
```

    ##  [1]  1  2  3 40 50 60  7  8  9 10

### ... using any form of subsetting

``` r
lst <- list(a="text", b=42, NA)
refset(rsl, lst$b)
rsl <- "more text"
lst$b
```

    ## [1] "more text"

### The short form

``` r
rs %r% employees[1:3,] # equivalent to refset(rs, employees[1:3,])
```

### To pass a refset into a function, use `wrapset` to create a parcel:

``` r
f <- function(x) {
  cx <- contents(x)
  contents(x)$name <- paste(cx$name, "the", sample(c("Kid", "Terrible", "Silent", 
        "Fair"), nrow(cx), replace=TRUE))
}

parcel <- wrapset(employees[])
f(parcel)
employees
```

    ##    id               name age gender
    ## 1 101   Jimmy the Silent  30      M
    ## 2 102     Silvia the Kid  46      F
    ## 3   3 Meng Qi the Silent  39      F
    ## 4   4      Luis the Fair  24      M

Introduction
------------

Normally, R uses "pass by value". This means that when you run `b <- a` you have two independent copies of the same data. Similarly, the code:

``` r
f <- function(x) {x <- x*2}
a <- 4
f(a)
a
```

    ## [1] 4

does not change the value of `a`, since the function `f` gets passed the contents of `a` rather than the variable `a` itself.

This is fine for most cases, especially for traditional uses of R in which the programmer or statistician passes in a value to a function, and sees the result on the command line. However, in some cases we would like to work with a single object, rather than multiple copies. For example:

-   working on a complex dataset, an analyst may wish to work with part of the dataset, but to have any changes reflected in the whole data frame.
-   if a data frame represents objects in a relational database, changes to the database on disk should be reflected in the data frame.
-   for large datasets, assigning into multiple copies can take up memory.

The refset package allows you to do this, by creating objects that refer to other objects, or subsets of them.

To create a refset, call refset with two arguments:

``` r
dfr <- data.frame(x1=1:5, x2=rnorm(5), alpha=letters[1:5])
refset(rs, dfr[dfr$x1 <= 3, c("x1", "alpha")])
```

The call above creates a new variable `rs` in your environment. (Strictly, it creates a new *binding*, but we needn't worry about that for now.) For comparison, we'll also create a standard subset.

``` r
ss <- dfr[dfr$x1 <= 3, c("x1", "alpha")]
rs
```

    ##   x1 alpha
    ## 1  1     a
    ## 2  2     b
    ## 3  3     c

``` r
ss
```

    ##   x1 alpha
    ## 1  1     a
    ## 2  2     b
    ## 3  3     c

`rs` and `ss` look and behave just the same:

``` r
c(class(rs), class(ss))
```

    ## [1] "data.frame" "data.frame"

``` r
c(mean(rs$x1), mean(ss$x1))
```

    ## [1] 2 2

To see the difference, let's change the data in `dfr`:

``` r
dfr$alpha <- c(NA, letters[23:26])
rs
```

    ##   x1 alpha
    ## 1  1  <NA>
    ## 2  2     w
    ## 3  3     x

``` r
ss
```

    ##   x1 alpha
    ## 1  1     a
    ## 2  2     b
    ## 3  3     c

As is normal, `ss` has not updated to reflect changes in the original data frame. But `rs` has.

The connection also works the other way, if you change `rs`.

``` r
rs$alpha <- LETTERS[1:3]
rs
```

    ##   x1 alpha
    ## 1  1     A
    ## 2  2     B
    ## 3  3     C

``` r
dfr
```

    ##   x1           x2 alpha
    ## 1  1  0.577346854     A
    ## 2  2  2.420008356     B
    ## 3  3  0.509808225     C
    ## 4  4 -0.253336200     y
    ## 5  5 -0.006459907     z

Everything that you do to `rs` will be reflected in the original data, and vice versa. Well, almost everything: remember that `rs` refers to a *subset* of the data. If you can't do it to a subset, you probably can't do it to a refset. For example, changing the `names` of a refset doesn't work, because assigning to the names of a subset of your data doesn't change the original names.

Ways to call refset
-------------------

There are three ways to create a refset. The first you have already seen: call `refset(name, data[indices])` where `name` is the variable name of the variable you want to create, and `data[indices]` is the subset you want to look at. You aren't limited to using data frames. You can refset any object which you can subset, and you can use any of the three standard ways to subset data: `$`, `[[` and `[`.

``` r
vec <- 1:10
refset(rvec, vec[2:3])
mylist <- list(a="some", b="more", c="data")
refset(rls, mylist$b)
refset(rls2, mylist[["c"]])
rvec
```

    ## [1] 2 3

``` r
c(rls, rls2)
```

    ## [1] "more" "data"

However, this won't work:

``` r
myss <- subset(dfr, x1>1)
refset(rs, myss)
```

    ## Error in substitute(data)[[1]]: object of type 'symbol' is not subsettable

You have to specifically write out the subset you want: you can't put it in a variable.

The second way to call `refset` is using the `%r%` infix operator. This is conveniently short, and also makes it clearer that you are assigning to a variable.

``` r
top4 %r% dfr[1:4,]
exists("top4")
```

    ## [1] TRUE

The last way to create a refset is the 3-or-more argument form of the function. This works like the `subset` command in R base: you can refer to data frame columns by name directly.

``` r
refset(large, dfr, x2 > 0,)
large
```

    ##   x1        x2 alpha
    ## 1  1 0.5773469     A
    ## 2  2 2.4200084     B
    ## 3  3 0.5098082     C

Notice that we've included an empty argument. This is just the same as when you call `dfr[dfr$x2 > 0, ]` with an empty argument after the comma: it includes all the columns.

Dynamic indexing
----------------

Refsets don't just sync their data with their "parent". They also update their indices dynamically. For example, suppose we have a database of employees, including hours worked in the past month.

``` r
employees <- data.frame(
      id=1:4, 
      name=c("James", "Sylvia", "Meng Qi", "Luis"), 
      age=c(28,44,38, 23), 
      gender=factor(c("M", "F", "F", "M")),
      hours=c(160, 130, 185, 145),
      pay=c(60000, 50000, 70000, 60000),
      stringsAsFactors=FALSE)
```

We can create a refset of employees who worked overtime:

``` r
overtimers %r% employees[employees$hours > 140,]
overtimers
```

    ##   id    name age gender hours   pay
    ## 1  1   James  28      M   160 60000
    ## 3  3 Meng Qi  38      F   185 70000
    ## 4  4    Luis  23      M   145 60000

When the new monthly data comes in, the set of people in `overtimers` will change:

``` r
employees$hours <- c(135, 150, 70, 145)
overtimers
```

    ##   id   name age gender hours   pay
    ## 2  2 Sylvia  44      F   150 50000
    ## 4  4   Luis  23      M   145 60000

Sometimes you may wish to turn this behaviour off. For example, you may want to look at a particular subset that had a certain characteristic at a point in time. For this, use the argument `dyn.idx=FALSE` to `refset`.

``` r
# people who worked long hours last month:
refset(overtimers_static, employees, hours > 140, , dyn.idx=FALSE)
# give them a holiday...
overtimers_static$hours <- 0
# ... and a pay rise
overtimers_static$pay <- overtimers_static$pay * 1.1 
overtimers_static
```

    ##   id   name age gender hours   pay
    ## 2  2 Sylvia  44      F     0 55000
    ## 4  4   Luis  23      M     0 66000

Without the `dyn.idx=FALSE` argument, the refset would have zero rows after the call setting `hours` to 0.

Delinking from the parent, and using parcels
--------------------------------------------

If you want to break the link to the parent dataset, simply assign your refset to a new variable.

``` r
copy <- overtimers
copy$pay <- copy$pay * 2
employees$pay # still the same :/
```

    ## [1] 60000 55000 70000 66000

Refsets are implemented using an R feature called "active binding", which calls a function when you access or change a variable. Reassigning to a new variable reassigns the contents, rather than the binding.

This causes a problem if you want to pass a reference into functions, rather than passing the value of the refset -- for example, if you would like to change the refset in the body of the function, and have this affect the original data. When you use a refset in a function argument, it binds it to a new value, breaking the link with the parent.

If you are writing your own code, you can avoid this problem by creating a refset which is "wrapped" in a parcel object. Parcels simply contain an expression and an environment in which the expression should be evaluated. For example, they can contain the name of a refset. When the `contents` function is called on a parcel, the expression is reevaluated. Here's how to write a function that changes the name of our employees:

``` r
rs %r% employees[1:3,]
```

``` r
f <- function(x) {
  cx <- contents(x)
  contents(x)$name <- paste(cx$name, "the", sample(c("Kid", "Terrible", "Silent", 
        "Fair"), nrow(cx), replace=TRUE))
}

parcel <- wrapset(employees[])
f(parcel)
employees
```

    ##   id                name age gender hours   pay
    ## 1  1       James the Kid  28      M   135 60000
    ## 2  2 Sylvia the Terrible  44      F     0 55000
    ## 3  3    Meng Qi the Fair  38      F    70 70000
    ## 4  4       Luis the Fair  23      M     0 66000

As the above shows, you can assign to `contents(parcel)` as well as read from it. You can also create a new variable from the parcel by using `unwrap_as`. Another way to write the function above would be:

``` r
f <- function(parcel) {
  unwrap_as(emps, parcel)
  emps$name <- paste(emps$name, "the", sample(c("Kid", "Terrible", "Silent", 
        "Fair"), nrow(emps), replace=TRUE))
}
f(parcel)
employees
```

    ##   id                         name age gender hours   pay
    ## 1  1     James the Kid the Silent  28      M   135 60000
    ## 2  2 Sylvia the Terrible the Fair  44      F     0 55000
    ## 3  3     Meng Qi the Fair the Kid  38      F    70 70000
    ## 4  4       Luis the Fair the Fair  23      M     0 66000

Using parcels is a way to pass references around code. You could also do this using non-standard evaluation ([NSE](http://adv-r.had.co.nz/Computing-on-the-language.html)). Parcels have the nice feature that they store the environment where they should be evaluated.

More information
----------------

For more information, see the help files for `refset` and `wrap`.

The code for refset lives at [github](http://github.com/hughjonesd/refset).
