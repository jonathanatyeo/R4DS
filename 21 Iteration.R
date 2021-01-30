library(tidyverse)
library(bench)

# 21.2.1 Exercises
 
# 1. Write for loops to:
#   1. Compute the mean of every column in mtcars.
#   2. Determine the type of each column in nycflights13::flights.
#   3. Compute the number of unique values in each column of iris.
#   4. Generate 10 random normals from distributions with means of -10, 0, 10, and 
#      100.
# Think about the output, sequence, and body before you start writing the loop.

output <- vector("double", ncol(mtcars))  # 1. output
for (i in seq_along(mtcars)) {            # 2. sequence
  output[[i]] <- mean(mtcars[[i]])      # 3. body
}
output

output <- vector("character", ncol(nycflights13::flights)) # 1. output
for(i in seq_along(nycflights13::flights)) { #2. sequence
  output[[i]] <- typeof(nycflights13::flights[[i]]) # 3. body
}
output

output <- vector("integer", ncol(iris))  # 1. output
for(i in seq_along(iris)) {    # 2. sequence
  output[[i]] <- length(unique(iris[[i]]))  # 3. body
}
output

mu <- c(-10, 0, 10, 100)
output <- vector("list", length(mu)) # 1. output
for(i in seq_along(mu)) { # 2. sequence
  output[[i]] <- rnorm(10, mean = mu[i]) # 3. body
}
output

# 2. Eliminate the for loop in each of the following examples by taking advantage of
# an existing function that works with vectors:
   
  out <- ""
  for (x in letters) {
    out <- stringr::str_c(out, x)
  }
  
  out <- stc_c(letters, collapse = "") # for loop removed
 
 x <- sample(100)
 sd <- 0
 for (i in seq_along(x)) {
   sd <- sd + (x[i] - mean(x)) ^ 2
 }
 sd <- sqrt(sd / (length(x) - 1))
 
 sd <- sqrt(sum((x - mean(x)) ^ 2) / (length(x) - 1)) # for loop removed
 
 x <- runif(100)
 out <- vector("numeric", length(x))
 out[1] <- x[1]
 for (i in 2:length(x)) {
   out[i] <- out[i - 1] + x[i]
 }
 
 cumsum(x) # for loop removed
 
# 3. Combine your function writing and for loop skills:
# 
#   1. Write a for loop that prints() the lyrics to the children’s song “Alice the 
#  camel”.
# 

humps <- c("five", "four", "three", "two", "one", "no")

for(i in seq_along(humps)) {
  cat(str_c("Alice the camel has ", 
            rep(humps[[i]], 3), 
            if(humps[[i]] == "one") {
              " hump."
              } else {
                " humps."
                },
            "\n"), sep = "")
  
  if(humps[[i]] == "no") {
    cat("‘Cause Alice is a horse, of course!")
    } else {
      cat("So go, Alice, go!\nBoom, boom, boom, boom!\n\n")
    }
}

#   2. Convert the nursery rhyme “ten in the bed” to a function. Generalise it to 
# any number of people in any sleeping structure.
 
in_the_bed <- function(people, structure = "bed") {
  cat(str_c("There",
            if(people == "one") {
              " was "
              } else {
                " were "
                }, 
            people, " in the ", structure, "\nAnd the little one said\n"), sep = "")
  
  if(people == "one") {
    cat(str_c("\"I'm Lonely\"\n"), sep = "")
    } else {
    cat(str_c("\"Roll over, roll over\"\n",
              "So they both rolled over and one fell out\n\n"), sep = "")
    }
}

#   3. Convert the song “99 bottles of beer on the wall” to a function. Generalise 
#   to any number of any vessel containing any liquid on any surface.

bottles <- function(number_of, vessel = "bottle") {
  if(number_of == 1) {
    str_c(number_of, " ", vessel, collapse = "")
  } else if(number_of == 0) {
    str_c("No more ", vessel, "s", collapse = "")
  } else {
    str_c(number_of, " ", vessel, "s", collapse = "")
  }
}

bottles_song <- function(number_of, vessel = "bottle", liquid = "beer") {
  cat(str_c(bottles(number_of, vessel), " of ", liquid, " on the wall, ",
        bottles(number_of, vessel), " of ", liquid, ".\n", collapse = ""))
  
  if(number_of == 0) {
    cat(str_c("We’ve taken them down and passed them around; now we’re drunk and passed 
        out!\n\n", collapse = ""))
  } else {
    cat(str_c("Take one down, pass it around, ", bottles(number_of - 1, vessel), 
              " of ", liquid, " on the wall.\n\n", collapse = ""))
  }
}

for(i in 99:0){
  bottles_song(i)
}

# 4. It’s common to see for loops that don’t preallocate the output and instead 
# increase the length of a vector at each step:

x <- rnorm(10000)

without_preallocate <- function() {
  output <- vector("integer", 0)
  for (i in seq_along(x)) {
    output <- c(output, lengths(x[[i]]))
    }
 output
}

with_preallocate <- function() {
  output <- vector("integer", length(x))
  for (i in seq_along(x)) {
    output[[i]] <- lengths(x[[i]])
  }
  output
}

# How does this affect performance? Design and execute an experiment.

bench::mark(without_preallocate())
# median execution time = 698us, memory allocation = 191MB

bench::mark(with_preallocate())
# median execution time = 13.7us, memory allocation = 63.8KB

# 21.3.5 Exercises
 
# 1. Imagine you have a directory full of CSV files that you want to read in. You 
# have their paths in a vector, 
# files <- dir("data/", pattern = "\\.csv$", full.names = TRUE), 
# and now want to read each one with read_csv(). Write the for loop that will load
# them into a single data frame.

files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)

df_list <- vector("list", length = length(files)) # preallocate

for(i in seq_along(files)) {
  df_list[[i]] <- read_csv(files[[i]])
}

bind_rows(df_list)

# 2. What happens if you use for (nm in names(x)) and x has no names? What if only 
# some of the elements are named? What if the names are not unique?

x <- c(1, 2, 3)

for(nm in names(x)) {
  print(nm)
  print(x[[nm]])
} # x has no names so there is no output

x <- c(a = 1, 2, c = 3)

for(nm in names(x)) {
  print(nm)
  print(x[[nm]])
} # Error message when we loop to the unnamed element

x <- c(a = 1, b = 2, b = 3)

for(nm in names(x)) {
  print(nm)
  print(x[[nm]])
} # x prints the first element with the same name


# 3. Write a function that prints the mean of each numeric column in a data frame, 
# along with its name. For example, show_mean(iris) would print:
  
  show_mean(iris)
#> Sepal.Length: 5.84
#> Sepal.Width:  3.06
#> Petal.Length: 3.76
#> Petal.Width:  1.20

  show_mean <- function(data_frame) {
    max_names_width <- 0
    
    for(nm in names(data_frame)) {
      if(is.numeric(data_frame[[nm]])) {
        max_names_width <- max(max_names_width, str_length(names(data_frame[nm])))
      }
    }
    
    print(max_names_width)
    
    for(nm in names(data_frame)) {
      if(is.numeric(data_frame[[nm]])) {
        cat(
          str_c(
            str_pad(str_c(nm, ":", collapse = ""), 
                    width = max_names_width + 2, side = "right"),
                round(mean(data_frame[[nm]]), 2), "\n", collapse = "")
        )
      }
    }
  }

  show_mean(iris)
  
# (Extra challenge: what function did I use to make sure that the numbers lined up 
# nicely, even though the variable names had different lengths?)

# What does this code do? How does it work?
  
  trans <- list( 
    disp = function(x) x * 0.0163871,
    am = function(x) {
      factor(x, labels = c("auto", "manual"))
    }
  )
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}

# This code converts the variable disp (displacement) in the mtcars dataset from 
# cubic inches to cubic cm and the variable am (Transmission) from a binary integer
# to a factor.
  
  
# 21.4.1 Exercises
 
# 1. Read the documentation for apply(). In the 2d case, what two for loops does it
#  generalise?
# when MARGIN = 1 the apply loops over rows. when MARGIN = 2 apply loops over columns
  
# 2. Adapt col_summary() so that it only applies to numeric columns You might want 
# to start with an is_numeric() function that returns a logical vector that has a 
# TRUE corresponding to each numeric column.
  
  df <- tibble(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
  )
  
  col_summary <- function(df, fun) {
    numeric_cols <- vector("logical", length(df))
    for (i in seq_along(df)) {
      numeric_cols[[i]] <- is.numeric(df[[i]])
    }
    
    col_number <- which(numeric_cols)
    
    out <- vector("double", sum(numeric_cols))
    for (i in seq_along(col_number)) {
      out[i] <- fun(df[[col_number[i]]])
    }
    names(out) <- names(df[col_number])
    out
  }

  
# 21.5.3 Exercises
 
# 1. Write code that uses one of the map functions to:
#   Compute the mean of every column in mtcars.
  
  mtcars %>% 
    map_dbl(mean)
  
#   Determine the type of each column in nycflights13::flights.
  nycflights13::flights %>% 
    map_chr(typeof)

#   Compute the number of unique values in each column of iris.
  iris %>% 
    map_int(~length(unique(.x)))
  
#   Generate 10 random normals from distributions with means of -10, 0, 10, and 100.
  c(-10, 0, 10, 100) %>% 
    map(~rnorm(10, .x))
  
# 2. How can you create a single vector that for each column in a data frame 
# indicates whether or not it’s a factor?
 iris %>% 
   map_lgl(is.factor)
  
# 3. What happens when you use the map functions on vectors that aren’t lists? What
# does map(1:5, runif) do? Why?
# map applies the function to every element in the list. In the example below it 
# applies runif to each element of the vector 1:5 and returns a list with 5 vectors
  map(1:5, runif) 
 
# 4. What does map(-2:2, rnorm, n = 5) do? Why? What does map_dbl(-2:2, rnorm, n = 5)
# do? Why?
  map(-2:2, rnorm, n = 5) 
  # generates a list of length five each with a vector of random samples from a normal distribution

  map_dbl(-2:2, rnorm, n = 5)  
  # generates an error message as map_dbl requires the output of the function to be
  # a double of length 1
  
# 5. Rewrite map(x, function(df) lm(mpg ~ wt, data = df)) to eliminate the anonymous
# function.
  
  x <- split(mtcars, mtcars$cyl)
  
  map(x, ~ lm(mpg ~ wt, data = .))

# 21.9.3 Exercises
 
# 1. Implement your own version of every() using a for loop. Compare it 
# with purrr::every(). What does purrr’s version do that your version doesn’t?

  x <- list(1:5, letters, list(10))
  
  x %>% 
    every(is_vector)

  my_every <- function(.x, .p, ...) {
    for (i in .x)
    {
      if(!func(i, ...)) {
        return(FALSE)
      }
    }
    TRUE
  }
  
# 2. Create an enhanced col_summary() that applies a summary function to every 
# numeric column in a data frame.

  col_summary2 <- function(df, f, ...) {
    map(keep(df, is.numeric), f, ...)
  }
   
# 3. A possible base R equivalent of col_summary() is:
   
  col_sum3 <- function(df, f) {
    is_num <- sapply(df, is.numeric)
    df_num <- df[, is_num]
    
    sapply(df_num, f)
    }

# But it has a number of bugs as illustrated with the following inputs:

  df <- tibble(
    x = 1:3, 
    y = 3:1,
    z = c("a", "b", "c")
    )
  
  # OK
  col_sum3(df, mean)
  
  # Has problems: don't always return numeric vector
  col_sum3(df[1:2], mean)
  col_sum3(df[1], mean)
  col_sum3(df[0], mean)
   
  # What causes the bugs?