"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

resolution <- function(x, zero = TRUE) {
  x <- unique(as.numeric(x))
  if (length(x) == 1) return(1)

  if (zero) {
    x <- unique(c(0, x))
  }
  
  min(diff(sort(x)))
}

# Name ggplot grid object
# Convenience function to name grid objects
# 
# @keyword internal
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}


.pt <- 1 / 0.352777778