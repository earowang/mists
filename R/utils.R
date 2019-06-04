## helpers
# ref: tibble:::cat_line
cat_line <- function(...) {
  cat(paste0(..., "\n"), sep = "")
}

paste_inline <- function(...) {
  paste(..., sep = "\n")
}

comma <- function(...) {
  paste(..., collapse = ", ")
}

ellipsis <- function(...) {
  paste(paste(..., collapse = " "), "...")
}

backticks <- function(x) {
  paste0("`", x, "`")
}

parenthesis <- function(x) {
  paste0("(", x, ")")
}

brackets <- function(x) {
  paste0("[", x, "]")
}

angle_brackets <- function(x) {
  paste0("<", x, ">")
}
