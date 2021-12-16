################################################################################
#
#'
#' List files and folders within the Open Marawi Google Drive database
#'
#' A convenience wrapper to [googledrive] functions set to specifically access
#' the Open Marawi Google Drive database and list files and folders within.
#'
#' @param id Character vector of the Open Marawi **Google Drive** id. This is
#'   currently set to *1bph1LBRpxwydAvjuggyjmhmq6HhyteY8*
#'
#' @return A tibble of names and ids of files and folders within the the
#'   Open Marawi Google Drive
#'
#' @author Ernest Guevarra
#'
#' @examples
#' marawi_ls()
#'
#' @rdname marawi_ls
#' @export
#'
#
################################################################################

marawi_ls <- function(id = "1bph1LBRpxwydAvjuggyjmhmq6HhyteY8") {
  ## Google Drive deauthorisation
  googledrive::drive_deauth()

  ## List files and folders inside OpenBangsa
  x <- googledrive::drive_ls(googledrive::drive_get(id = id))

  ## Return x
  x
}
