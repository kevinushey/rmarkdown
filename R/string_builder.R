string_builder <- function() {
  (function() {
    indent_ <- "  "
    indentSize_ <- 0
    data_ <- character()

    indented_ <- function(data) {
      indent <- paste(character(indentSize_ + 1), collapse = indent_)
      for (i in seq_along(data))
        if (is.list(data[[i]]))
          data[[i]] <- indented_(data[[i]])
        else
          data[[i]] <- paste(indent, data[[i]], sep = "")
      data
    }

    expr_to_list({

      append <- function(...) {
        data_ <<- c(data_, indented_(list(...)))
      }

      appendf <- function(...) {
        data_ <<- c(data_, indented_(sprintf(...)))
      }

      indent <- function() {
        indentSize_ <<- indentSize_ + 1
      }

      unindent <- function() {
        indentSize_ <<- max(0, indentSize_ - 1)
      }

      data <- function() unlist(data_)

    })

  })()
}
