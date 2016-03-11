#' Franke function - http://www.sfu.ca/~ssurjano/franke2d.html
#'
#' @param x First dimension
#' @param y Second dimension
#' @return The franke function evaluated at x,y
#' @export
#' @examples
#' franke(0,1)
franke <- function(x, y) {
  e1 <- -((9 * x - 2)^2)/4 - ((9 * y - 2)^2/4)
  e2 <- -((9 * x + 1)^2)/49 - ((9 * y + 1)/10)
  e3 <- -((9 * x - 7)^2)/4 - ((9 * y - 3)^2/4)
  e4 <- -((9 * x - 4)^2) - ((9 * y - 7)^2)

  return (0.75 * exp(e1) + 0.75 * exp(e2) + 0.5 * exp(e3) - 0.2 * exp(e4))
}
