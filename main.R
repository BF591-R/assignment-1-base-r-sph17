# ----------------------- Helper Functions to Implement ------------------------

#' Evaluate whether the argument is less than 0
#'
#' Returns TRUE if the numeric argument x is a prime number, otherwise returns
#' FALSE
#'
#' @param x (numeric): the numeric value(s) to test
#'
#' @return logical value or vector indicating whether the numeric argument is less than 0
#' @export
#'
#' @examples
#' less_than_zero(-1)
#' [1] TRUE
#' less_than_zero(10)
#' [1] FALSE
#' less_than_zero(c(-1,0,1,2,3,4))
#' [1] TRUE FALSE FALSE FALSE FALSE FALSE
less_than_zero <- function(x) {
    return(x<0)
}

#' Evaluate whether the argument is between two numbers
#'
#' Returns TRUE if the numeric argument x is contained within the open interval
#' (a, b), otherwise return FALSE.
#'
#' @param x (numeric): the numeric value(s) to test
#' @param a (number): the lower bound
#' @param b (number): the upper bound
#'
#' @return logical value of same type as input (e.g. scalar, vector, or matrix)
#' @export
#'
#' @examples
#' is_between(3,1,5)
#' [1] TRUE
#' is_between(c(1,9,5,2), 1, 5)
#' [1] FALSE FALSE FALSE TRUE
#' is_between(matrix(1:9, nrow=3, byrow=TRUE), 1, 5)
#'       [,1]  [,2]  [,3]
#' [1,] FALSE  TRUE  TRUE
#' [2,]  TRUE FALSE FALSE
#' [3,] FALSE FALSE FALSE
is_between <- function(x, a, b) {
    return(x>a & x<b)
}

#' Return the values of the input vector that are not NA
#'
#' Returns the values of the input vector `x` that are not NA
#'
#' @param x (numeric): numeric vector to remove NA from 
#'
#' @return numeric vector `x` with all `NA` removed
#' @export
#'
#' @examples
#' x <- c(1,2,NA,3)
#' rm_na(x)
#' [1] 1 2 3
rm_na <- function(x) {
    clean_x <- x[!is.na(x)]
    return(clean_x)
}

#' Calculate the median of each row of a matrix
#'
#' Given the matrix x with n rows and m columns, return a numeric vector of
#' length n that contains the median value of each row of x
#'
#' @param x (numeric matrix): matrix to compute median along rows
#'
#' @return (numeric vector) vector containing median row values
#' @export
#'
#' @examples
#' m <- matrix(1:9, nrow=3, byrow=T)
#' row_medians(m)
#' [1] 1 4 7
#' 
row_medians <- function(x) {
    # Median values for each row
    row_medians <- apply(x, MARGIN = 1, FUN = median)
    return(row_medians)
}

#' Evaluate each row of a matrix with a provided function
#'
#' Given the matrix `x` with n rows and m columns, return a numeric vector of
#' length n that contains the returned values from the evaluation of the
#' function `fn` on each row. `fn` should be a function that accepts a vector as
#' input and returns a scalar value
#'
#' @param x (numeric matrix): matrix to evaluate the function along rows
#' @param fn (function) function that accepts a vector as input and returns a scalar
#' @param na.rm (logical) OPTIONAL: a logical evaluating to `TRUE` or `FALSE`
#'   indicating whether `NA` values should be stripped before computing summary
#'
#' @return (numeric vector) vector of the evaluation of `fn` on rows of `x`
#' @export
#'
#' @examples
#' m <- matrix(1:9, nrow=3, byrow=T)
#' summarize_rows(m, min)
#' [1] 1 4 7
#' summarize_rows(m, mean)
#' [1] 2 5 8
summarize_rows <- function(x, fn, na.rm=FALSE) {
    #function that accepts a vector as input and returns a scalar
    fn_on_row <- function(row) {
      return(fn(row, na.rm = na.rm))
    }
    results <- apply(x, MARGIN = 1, fn_on_row)
    return(results)
}

#' Summarize matrix rows into data frame
#'
#' Summarizes the rows of matrix `x`. Returns a data frame with the following
#' columns in order and the corresponding value for each row:
#' 
#'   * mean - arithmetic mean
#'   * stdev - standard deviation
#'   * median - median
#'   * min - minimum value
#'   * max - maximum value
#'   * num_lt_0 - the number of values less than 0
#'   * num_btw_1_and_5 - the number of values between 1 and 5
#'   * num_na - the number of missing (NA) values (ignoring value of na.rm)
#'
#' @param x (numeric matrix): matrix to evaluate the function along rows
#' @param na.rm (logical) OPTIONAL: a logical evaluating to `TRUE` or `FALSE`
#'   indicating whether `NA` values should be stripped before computing summary
#'
#' @return (data frame) data frame containing summarized values for each row of `x`
#' @export
#'
#' @examples
#' m <- matrix(1:9, nrow=3, byrow=T)
#' summarize_matrix(m)
#'   mean stdev median min max num_lt_0 num_btw_1_and_5 num_na
#' 1    2     1      2   1   3        0               3      0
#' 2    5     1      5   4   6        0               1      0
#' 3    8     1      8   7   9        0               0      0
#'
#' m <- matrix(rnorm(1000), nrow=4, byrow=T)
#' summarize_matrix(m)
#'          mean    stdev      median       min      max num_lt_0 num_btw_1_and_5 num_na
#' 1 -0.02220010 1.006901  0.02804177 -3.485147 3.221089      120              61      0
#' 2 -0.01574033 1.026951 -0.04725656 -2.967057 2.571608      112              70      0
#' 3 -0.09040182 1.027559 -0.02774705 -3.026888 2.353087      130              54      0
#' 4  0.09518138 1.030461  0.11294781 -3.409049 2.544992       90              72      0
#' 
#' set.seed(42)
#' m <- matrix(rnorm(1000), nrow = 4, byrow = TRUE)
#' summarize_matrix(m)
#'    mean     stdev      median       min      max num_lt_0 num_btw_1_and_5 num_na
#'  1 -0.02039958 0.9754359 -0.03507168 -2.993090 2.701891      132              41      0
#'  2 -0.03969286 0.9707705 -0.03797064 -2.699930 2.965865      133              34      0
#'  3 -0.06015963 0.9903508 -0.05018184 -3.017933 3.229069      132              35      0
#'  4  0.01695436 1.0744280  0.09870923 -3.371739 3.495304      118              43      0
summarize_matrix <- function(x, na.rm=FALSE) {
  # Check for numeric matrix
  if (!is.matrix(x)) {
    stop("Data must be a numeric matrix!")
  }  
  
  # Summarize each row of the numeric matrix
  data_summary <- data.frame(
    mean = apply(x, MARGIN = 1, function(row) mean(row, na.rm = na.rm)),
    stdev = apply(x, MARGIN = 1, function(row) sd(row, na.rm = na.rm)),
    median = apply(x, MARGIN = 1, function(row) median(row, na.rm = na.rm)),
    min = apply(x, MARGIN = 1, function(row) min(row, na.rm = na.rm)),
    max = apply(x, MARGIN = 1, function(row) max(row, na.rm = na.rm)),
    num_lt_0 = apply(x, MARGIN = 1, function(row) sum(row < 0, na.rm = na.rm)),
    num_btw_1_and_5 = apply(x, MARGIN = 1, function(row) sum(row >= 1 & row <= 5, na.rm = na.rm)),
    num_na = apply(x, MARGIN = 1, function(row) {
      if (na.rm) {
        # If na.rm is TRUE
        sum(is.na(row[!is.na(row)]))
      } else {
        # If na.rm is FALSE
        sum(is.na(row))
      }
    })
  )
  
  return(data_summary)
}

# ------------ Helper Functions Used By Assignment, You May Ignore ------------
sample_normal <- function(n, mean=0, sd=1) {
  return(rnorm(n, mean = mean, sd = sd))
}

sample_normal_w_missing <- function(n, mean=0, sd=1, missing_frac=0.1) {
  # Sample without NAs
  sample<-rnorm(n, mean = mean, sd = sd)
  # Determine number of NAs required
  missing_n <- ceiling(n * missing_frac)
  # Randomly select indices for NA
  missing_index <- sample(seq_len(n),size=missing_n)
  # Replace with NA
  sample[missing_index] <- NA

  return(sample)
}

simulate_gene_expression <- function(num_samples, num_genes) {
  gene_expression <- matrix(
    rnorm(num_samples * num_genes, mean = 10, sd = 3), nrow = num_genes, ncol = num_samples)

  return(gene_expression)

}

simulate_gene_expression_w_missing <- function(num_samples, num_genes, missing_frac=0.1) {
    # Use simulate_gene_expression to sample without NAs
    gene_expression <- simulate_gene_expression(num_samples, num_genes)
    # Determine number of NAs required
    total_n <- num_samples * num_genes
    #print(paste("Total values:", total_n))  # Debug
    missing_n <- ceiling(total_n * missing_frac)
    #print(paste("Missing values to introduce:", missing_n)) # Debug
    # Randomly select indices for NA
    missing_index <- sample(seq_len(total_n), size = missing_n)
    # Replace with NA
    gene_expression[missing_index] <- NA
    
    return(gene_expression)
}
