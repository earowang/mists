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

# stolen from purrr
vec_depth <- function(x) {
  if (is_null(x)) {
    0L
  } else if (is_atomic(x)) {
    1L
  } else if (is_list(x)) {
    depths <- map_int(x, vec_depth)
    1L + max(depths, 0L)
  } else {
    abort("`x` must be a vector")
  }
}

# stolen from tibble
justify <- function(x, right = TRUE, space = " ") {
  if (length(x) == 0L) return(character())
  width <- nchar_width(x)
  max_width <- max(width)
  spaces_template <- paste(rep(space, max_width), collapse = "")
  spaces <- map_chr(max_width - width, substr, x = spaces_template, start = 1L)
  if (right) {
    paste0(spaces, x)
  } else {
    paste0(x, spaces)
  }
}

nchar_width <- function(x) {
  nchar(x, type = "width")
}
