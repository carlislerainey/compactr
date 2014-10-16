sci_notation <- function(x, digits = 1) {
  if (length(x) > 1) {
    return(append(sci_notation(x[1]), sci_notation(x[-1])))
  }
  if (!x) return(0)
  exponent <- floor(log10(x))
  base <- round(x / 10^exponent, digits)
  as.expression(substitute(base %*% 10^exponent, 
                           list(base = base, exponent = exponent)))
}