#' Multiply two square matrices and a vector
#'
#' @param A A square matrix.
#' @param B Another square matrix.
#' @param x A vector.
#' @param slow A boolean indicating whether to multiply using the slower method,
#'   \eqn{(AB)x}, or the faster method, \eqn{A(Bx)} (default).
#'
#' @returns A numeric vector or column/row vector (\eqn{1\times n} or \eqn{n \times 1}
#'   matrix).
#' @examples
#' mult_ABx(matrix(rnorm(25), nrow = 5), matrix(runif(25, 0, 10), nrow = 5), rpois(5, 5))
#' @export

mult_ABx <- function(A, B, x, slow = FALSE) {
  if (slow == FALSE) result <- try(A %*% (B %*% x), silent = TRUE)
  else if (slow == TRUE) result <- try((A %*% B) %*% x, silent = TRUE)
  else stop("Specify multiplication method as slow = TRUE or = FALSE (default).")

  if (!("try-error" %in% class(result))) return(result)

  # only do the below if an error is thrown
  if (!is.matrix(A) || !is.matrix(B)) stop("A and B must both be matrices.")
  if ((dim(A)[1] != dim(A)[2]) || (dim(A)[1] != dim(B)[2]) || (dim(B)[1] != dim(A)[2])) {
    stop("A and B must be square matrices of the same dimensions.")
  }
  if (is.matrix(x)) {
    if (dim(B)[2] != dim(x)[1]) {
      if (dim(B)[2] == dim(x)[2]) {
        x <- t(x)
        message("Note: x was transposed to be conformable with A and B.")
        return(mult_ABx(A, B, x, slow))
      } else stop("Incorrect dimensions of vector x.")
    }
  } else if (is.vector(x) && is.numeric(x)) {
    if (dim(B)[2] != length(x)) stop("Incorrect dimensions of vector x.")
  } else {
    stop("x must be a vector or 1 x n matrix.")
  }
}
