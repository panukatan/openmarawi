#'
#' List files and folders within the Open Marawi Google Drive database
#'
#' A convenience wrapper to [googledrive] functions set to specifically access
#' the Open Marawi Google Drive database and list files and folders within.
#'
#' @param id Character vector of the Open Marawi **Google Drive** id. This is
#'   currently set to *1bph1LBRpxwydAvjuggyjmhmq6HhyteY8* which is the id of
#'   the Open Marawi **Google Drive**. Change this if Open Marawi
#'   **Google Drive** is moved.
#' @param drive_list A dribble from which to search for required Google Drive
#'   directory.
#'
#' @return A dribble of names and ids of files and folders within the
#'   specified Google Drive
#'
#' @author Ernest Guevarra
#'
#' @examples
#' marawi_ls()
#' marawi_ls_armm(drive_list = marawi_ls())
#' marawi_ls_lanao(drive_list = marawi_ls())
#'
#' @rdname marawi_ls
#' @export
#'
marawi_ls <- function(id = "1bph1LBRpxwydAvjuggyjmhmq6HhyteY8") {
  ## Google Drive deauthorisation ----
  googledrive::drive_deauth()

  ## Get dribble of files and folders inside Open Marawi Google Drive ----
  drive_list <- googledrive::as_id(id) |>
    googledrive::drive_ls()

  ## Return drive_list ----
  drive_list
}

#'
#' @rdname marawi_ls
#' @export
#'
marawi_ls_armm <- function(drive_list) {
  ## Google Drive deauthorisation ----
  googledrive::drive_deauth()

  ## Get Google Drive identifier for ARMM ----
  drive_id <- get_drive_id(pattern = "ARMM", drive_list = drive_list)

  ## Get dribble of files and folders inside Open Marawi Google Drive ----
  drive_list <- googledrive::as_id(drive_id) |>
    googledrive::drive_ls()

  ## Return drive_list ----
  drive_list
}


#'
#' @rdname marawi_ls
#' @export
#'
marawi_ls_lanao <- function(drive_list) {
  ## Google Drive deauthorisation ----
  googledrive::drive_deauth()

  ## Get Google Drive identifier for ARMM ----
  drive_id <- get_drive_id(pattern = "Lanao del Sur", drive_list = drive_list)

  ## Get dribble of files and folders inside Open Marawi Google Drive ----
  drive_list <- googledrive::as_id(drive_id) |>
    googledrive::drive_ls()

  ## Return drive_list ----
  drive_list
}


