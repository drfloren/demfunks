#' @title Wrap Text
#' 
#' @description This is a general text wrapping function similar to `label_wrap` from `scales`.
#' 
#' @param x The character string/vector of text to be wrapped
#' @param width The max width that should be allowed (number of characters)
#' 
#' @details This follows closely with the `label_wrap` function from the `scales` package, but due to the funciton design is able to be used in a wider variety of contexts. Note that this simply inserts line-breaks at appropriate places to break up the string.
#' 
#' @examples
#' ex_text <- "this is a very long text string"
#' wrap_text(ex_text, width=50)
#' wrap_text(ex_text, width=10)
#' cat(wrap_text(ex_text, width=10))
#' 
#' @author Michael Floren
#' 
#' @seealso [scales::label_wrap()]
#' 
#' @export


wrap_text <- function(x, width){ #wraps entries in x (a string vector) at "width" characters, inserting a \n into them when appropriate
  unlist(lapply(strwrap(x, width = width, simplify = FALSE), 
                paste0, 
                collapse = "\n")
  )
}