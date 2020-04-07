#' is.prime
#'
#' is.prime check the given number is prime or not
#'
#' @param num integer
#' @return TRUE or FALSE
#' @export
#' @details if the given number is divisible any number other than 1 and itself it return NULL.
#' otherwise TRUE.
#' @examples
#' is.prime(3)
#' #TRUE
#' @examples
#' is.prime(21)
#' #FALSE
is.prime <- function(num) {
  if (num == 2) {
    TRUE
  } else if (any(num %% 2:(num-1) == 0)) {
    FALSE
  } else {
    TRUE
  }
}
