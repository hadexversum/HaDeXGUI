.onLoad <- function(libname, pkgname) {
  # to keep icecream from being problematic
  if (length(find.package("icecream", quiet = TRUE)) == 0)
    ic <<- function(x) invisible(identity(x))
}
