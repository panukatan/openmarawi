#'
#' Get Google Drive identifier given a search pattern
#'
#' @param pattern A pattern to search for
#' @param drive_list A dribble (usually produced from one of the `marawi_ls()`
#' functions) from which to search the `pattern` from.
#'
#' @returns A Google Drive identifier
#'
#' @examples
#' get_drive_id("ARMM", drive_list = marawi_ls())
#'
#' @rdname marawi_utils
#' @export
#'
get_drive_id <- function(pattern,
                         drive_list) {
  ## Check that drive_list is a dribble ----
  if (!googledrive::is_dribble(drive_list))
    stop("The `drive_list` must be a dribble. Please try again.")

  ## Get id ----
  drive_list |>
    dplyr::filter(
      stringr::str_detect(
        string = .data$name,
        pattern = pattern
      )
    ) |>
    dplyr::pull(.data$id)
}
