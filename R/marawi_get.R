################################################################################
#
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
#'
#' @return A dribble of names and ids of files and folders within the the
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

  ## Get dribble of files and folders inside Open Marawi Google Drive
  x <- googledrive::drive_get(id = id) %>%
    googledrive::drive_ls()

  ## Return x
  x
}


################################################################################
#
#'
#' List files and folders within the ARMM directory in the Open Marawi Google
#' Drive database
#'
#' A convenience wrapper to [googledrive] functions set to specifically access
#' the ARMM directory in the Open Marawi Google Drive database and list files
#' and folders within.
#'
#' @param id Character vector of the ARMM directory in the Open Marawi
#'   **Google Drive** id. This can be retrieved from the listing provided by
#'   `marawi_ls`.
#'
#' @return A dribble of names and ids of files and folders within the the
#'   ARMM directory in the Open Marawi Google Drive
#'
#' @author Ernest Guevarra
#'
#' @examples
#' marawi_ls_armm()
#'
#' @rdname marawi_ls
#' @export
#'
#
################################################################################

marawi_ls_armm <- function(id = marawi_ls()$id[marawi_ls()$name == "ARMM"]) {
  ## Google Drive deauthorisation
  googledrive::drive_deauth()

  ## Get dribble of files and folders inside Open Marawi Google Drive
  x <- googledrive::drive_get(id = id) %>%
    googledrive::drive_ls()

  ## Return x
  x
}


################################################################################
#
#'
#' Get ARMM Barangay level data masterlist from the Open Marawi Google Drive
#' Database
#'
#' @param id Character value for the unique identifier for the ARMM Open Marawi
#'   **Google Drive** Masterlist file in **.xlsx** format. Currently set
#'   to the identifier specified when `marawi_ls_armm()` is called. Change this
#'   if Open Marawi **Google Drive** is moved or if name of masterlist file
#'   changes.
#' @param dataset A string value from either `metadata`, `demographics`,
#'   `facilities`, or `conflict` specifying the masterlist data to retrieve. If
#'   not specified, will default to `metadata`
#'
#' @return A tibble of the specified masterlist dataset available in the
#'   ARMM directory of the Open Marawi Database.
#'
#' @author Ernest Guevarra
#'
#' @examples
#' ## Retrieve the metadata dataset of the ARMM directory in the Open Marawi
#' ## database
#' marawi_get_armm(dataset = "metadata")
#'
#' @rdname marawi_get
#' @export
#'
#'
#
################################################################################

marawi_get_armm <- function(id = marawi_ls_armm()$id[stringi::stri_detect(marawi_ls_armm()$name, fixed = "Masterlist")],
                            dataset = c("metadata", "demographics",
                                        "facilities", "conflict")) {
  ## Google Drive deauthorisation
  googledrive::drive_deauth()

  ## Create temp file
  downloaded_file <- tempfile()

  ## Download masterlist file
  googledrive::drive_download(file = id, path = downloaded_file)

  ## Define dataset argument
  dataset <- match.arg(dataset)

  sheet <- switch(
    EXPR = dataset,
    metadata = 1,
    demographics = 2,
    facilities = 3,
    conflict = 4
  )

  ## Read the specified inventory
  x <- readxl::read_xls(path = downloaded_file, sheet = sheet)

  ## Return x
  x
}
