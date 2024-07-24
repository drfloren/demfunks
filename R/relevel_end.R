#' @title Relevel Factor to End
#' 
#' @description This function relevels a factor level to the end of the factor, rather than the beginning as in the typical `relevel` command.
#' 
#' @param x The factor vector to be reordered.
#' @param ref The reference level, to be placed at the end
#' @param ... Additional arguments (currently not used).
#' 
#' @details This is a copy of the current relevel command, with an edit at the end to change the referenced factor level to come at the end rather than the beginning.
#' 
#' @examples
#' levels(factor(ChickWeight$Diet))
#' levels(relevel_end(factor(ChickWeight$Diet), "1"))
#' 
#' @author Michael Floren
#' 
#' @seealso [relevel()]
#' 
#' @export


relevel_end <- function (x, ref, ...) 
{
  lev <- levels(x)
  if (length(ref) != 1L) 
    stop("'ref' must be of length one")
  if (is.character(ref)) 
    ref <- match(ref, lev)
  if (is.na(ref)) 
    stop("'ref' must be an existing level")
  nlev <- length(lev)
  if (ref < 1 || ref > nlev) 
    stop(gettextf("ref = %d must be in 1L:%d", ref, nlev), 
         domain = NA)
  factor(x, levels = lev[c(seq_along(lev)[-ref], ref)], exclude = NULL)
}