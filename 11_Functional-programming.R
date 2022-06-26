## -----------------------------------------------------------------------------
m <- matrix(1:6, nrow=3)
sum_of_squares <- function(x) sum(x^2)
apply(m, 1, sum_of_squares)


## -----------------------------------------------------------------------------
apply(m, 1, \(x) sum(x^2))


## -----------------------------------------------------------------------------
apply(m^2, 1, sum)


## -----------------------------------------------------------------------------
apply_if <- function(x, p, f) {
  result <- vector(length = length(x))
  n <- 0
  for (i in seq_along(x)) {
    if (p(x[i])) {
      n <- n + 1
      result[n] <- f(x[i])
    }
  }
  head(result, n)
}


## -----------------------------------------------------------------------------
apply_if(1:8, \(x) x %% 2 == 0, \(x) x^2)


## -----------------------------------------------------------------------------
power <- function(n) function(x) x^n
square <- power(2) # square fixes n to 2, so it compute squares
cube <- power(3)   # cube fixes n to 3, so it compute cubes
square(1:5)
cube(1:5)


## -----------------------------------------------------------------------------
power <- function(n) {
  # inside power we can see n because we get it as an argument
  f <- function (x) {
    # inside f we can see x because it is an argument,
    # but *also* n, since when we are inside f, we are also
    # inside power.
    x^2
  }
  return(f)
}


## -----------------------------------------------------------------------------
first_index <- function(x, p) {
  for (i in seq_along(x)) { # Run through x
    if (p(x[i])) { # return the first index that satisfy p
      return (i)
    }
  }
}


## -----------------------------------------------------------------------------
x <- 1:10
first_index(x, \(x) x %% 2 == 0)
first_index(x, \(x) x %% 2 != 0)


## -----------------------------------------------------------------------------
first_index <- function(x, range) {
  for (i in seq_along(x)) { # Run through x
    if ((range[1] <= x[i]) && (x[i] <= range[2])) { 
      # return the first index where x[i] is in the range
      return (i)
    }
  }
}
first_index(x, c(4, 7))


## -----------------------------------------------------------------------------
first_index <- function(x, p, p_data) {
  for (i in seq_along(x)) { # Run through x
    if (p(x[i], p_data)) { 
      # return the first index where x[i] is in the range
      return (i)
    }
  }
}


## -----------------------------------------------------------------------------
range_pred <- function(x, range) {
  (range[1] <= x) && (x <= range[2])
}
first_index(x, range_pred, c(4, 7))


## -----------------------------------------------------------------------------
first_index <- function(x, p) {
  for (i in seq_along(x)) { # Run through x
    if (p(x[i])) { # return the first index that satisfy p
      return (i)
    }
  }
}


## -----------------------------------------------------------------------------
first_index(x, \(x) (4 <= x) && (x <= 7))


## -----------------------------------------------------------------------------
in_range <- function(from, to) { # A function for creating a predicate
  \(x) (from <= x) && (x <= to) # the predicate function it returns
}

p <- in_range(4, 7)
first_index(x, p)


## -----------------------------------------------------------------------------
repeated <- function() { # we don't need initial data for this
  # We will remember previously seen values here
  seen <- c()
  
  # The predicate goes here, it will check if we have seen a 
  # given value before and update the `seen` 
  # values if we haven't
  function(x) {
    if (x %in% seen) {
      TRUE # We have a repeat!
    } else {
      seen <<- c(seen, x) # append `x` to `seen`
      FALSE # this was the first time we saw x
    }
  }
}


## -----------------------------------------------------------------------------
x <- c(1:4, 1:5) # We see 1 a second time at index 5
first_index(x, repeated())


## -----------------------------------------------------------------------------
is_even <- \(x) x %% 2 == 0
Filter(is_even, 1:8)
Filter(is_even, as.list(1:8))


## -----------------------------------------------------------------------------
square <- \(x) x^2
Map(square, 1:4)
unlist(Map(square, 1:4))


## -----------------------------------------------------------------------------
plus <- \(x, y) x + y
unlist(Map(plus, 0:3, 3:0))


## -----------------------------------------------------------------------------
add_parenthesis <- \(a, b) paste("(", a, ", ", b, ")", sep = "")
Reduce(add_parenthesis, 1:4)


## -----------------------------------------------------------------------------
mysum <- \(x) Reduce(`+`, x)
sum(1:4)
mysum(1:4)


## -----------------------------------------------------------------------------
1:10 |> keep(\(x) x %% 2 == 0) # get the even numbers
1:10 |> discard(\(x) x %% 2 == 0) # remove the even numbers


## -----------------------------------------------------------------------------
1:10 |> keep(~ .x %% 2 == 0) # get the even numbers
1:10 |> discard(~ .x %% 2 == 0) # remove the even numbers


## -----------------------------------------------------------------------------
1:4 |> map(\(x) x^2) # square the numbers


## -----------------------------------------------------------------------------
# get a vector of integers (The values we compute must be integers)
1L:4L |> map_int(\(x) x + 2L) 
# get a vector of nummerics; any number will work here
1:4 |> map_dbl(\(x) x^2) 
# get a vector of logical (boolean) values (we must compute booleans)
1:4 |> map_lgl(\(x) x %% 2 == 0) 


## -----------------------------------------------------------------------------
dfs <- list(
  tibble(x = 1:2, y = 1:2, z = 1:2),
  tibble(x = 3:4, y = 3:4),
  tibble(x = 4:5, z = 4:5)
)

# mapping the identifier to see what map_df does with that
dfs |> map_df(\(df) df) 


## -----------------------------------------------------------------------------
# modifying the data frames
mut_df <- \(df) df |> mutate(w = 2 * x ) # add column w
dfs |> map_df(mut_df) # now add w for all and merge them


## -----------------------------------------------------------------------------
add_parenthesis <- \(a, b) paste("(", a, ", ", b, ")", sep = "")
1:4 |> reduce(add_parenthesis)


## -----------------------------------------------------------------------------
1:4 |> reduce(add_parenthesis, .dir = "backward")


## -----------------------------------------------------------------------------
cached <- function(f) {
  # ensures that we get f as it is when we call cached (see text)
  force(f) 
  table <- list()

  function(n) {
    key <- as.character(n)
    if (key %in% names(table)) {
      print(paste("I have already computed the value for", n))
      table[[key]]

    } else {
      print(paste("Going to compute the value for", n))
      res <- f(n)
      print(paste("That turned out to be", res))
      table[key] <<- res # NB: <<- to update the closure table!
      print_table(table) # see function below
      res
    }
  }
}

# pretty-printing the table
print_table <- function(tbl) {
  print("Current table:")
  for (key in names(tbl)) {
    print(paste(key, "=>", tbl[key]))
  }
}


## -----------------------------------------------------------------------------
factorial <- function(n) {
  if (n == 1) {
    1
  } else {
    n * factorial(n - 1)
  }
}

factorial <- cached(factorial)
factorial(4)
factorial(1)
factorial(2)
factorial(3)
factorial(4)


## -----------------------------------------------------------------------------
fibonacci <- function(n) {
  if (n == 1 || n == 2) {
    1
  } else {
    fibonacci(n-1) + fibonacci(n-2)
  }
}

fibonacci <- cached(fibonacci)
fibonacci(4)
fibonacci(1)
fibonacci(2)
fibonacci(3)
fibonacci(4)


## ---- error=TRUE--------------------------------------------------------------
f <- function(a, b) NULL
f(a = 1, b = 2, c = 3)


## -----------------------------------------------------------------------------
g <- function(a, b, ...) NULL
g(a = 1, b = 2, c = 3)


## -----------------------------------------------------------------------------
tolist <- function(...) list(...)

tolist()
tolist(a = 1)
tolist(a = 1, b = 2)


## -----------------------------------------------------------------------------
time_it <- function(f) {
  force(f)
  function(...) {
    system.time(f(...))
  }
}


## -----------------------------------------------------------------------------
ti_mean <- time_it(mean)
ti_mean(runif(1e6))


## ---- echo=FALSE--------------------------------------------------------------
`%.%` <- function(f,g) function(...) f(g(...))


## -----------------------------------------------------------------------------
uMap <- unlist %.% Map


## -----------------------------------------------------------------------------
plus <- function(x, y) x + y
unlist(Map(plus, 0:3, 3:0))
uMap(plus, 0:3, 3:0)


## -----------------------------------------------------------------------------
error <- function(truth) function(x) x - truth
square <- function(x) x^2

rmse <- function(truth)
  sqrt %.% mean %.% square %.% error(truth)

mu <- 0.4
x <- rnorm(10, mean = 0.4)
rmse(mu)(x)

