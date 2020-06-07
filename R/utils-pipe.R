#' Pipe operator support
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @details See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Tibble support
#'
#' @details See \code{tibble::\link[tibble:tibble]{tibble}} for details.
#' @rdname tibble
#' @importFrom tibble tibble
#' @export
NULL

#' Other deps
#'
#' @import stats
#' @import utils
NULL

# variaveis globais
utils::globalVariables(c('::',':','.','<<-','.N','doc_','temp_quebra__','..nm_base','query','ini0'))
