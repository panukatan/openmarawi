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
  x <- googledrive::drive_ls(googledrive::drive_get(id = id))

  ## Return x
  x
}


################################################################################
#
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
  x <- googledrive::drive_ls(googledrive::drive_get(id = id))

  ## Return x
  x
}


################################################################################
#
#'
#' @examples
#' marawi_ls_lanao()
#'
#' @rdname marawi_ls
#' @export
#'
#
################################################################################

marawi_ls_lanao <- function(id = marawi_ls()$id[marawi_ls()$name == "Lanao del Sur"]) {
  ## Google Drive deauthorisation
  googledrive::drive_deauth()

  ## Get dribble of files and folders inside Open Marawi Google Drive
  x <- googledrive::drive_ls(googledrive::drive_get(id = id))

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


################################################################################
#
#'
#' @examples
#' marawi_get_armm_all()
#'
#' @rdname marawi_get
#' @export
#'
#
################################################################################

marawi_get_armm_all <- function(id = marawi_ls_armm()$id[stringi::stri_detect(marawi_ls_armm()$name, fixed = "Masterlist")]) {
  ## Google Drive deauthorisation
  googledrive::drive_deauth()

  ## Create temp file
  downloaded_file <- tempfile()

  ## Download masterlist file
  googledrive::drive_download(file = id, path = downloaded_file)

  ## Read demographics
  x <- readxl::read_xls(path = downloaded_file, sheet = "demographics")

  ## Read facilities
  y <- readxl::read_xls(path = downloaded_file, sheet = "bgyfacilities")

  ## Read conflict
  z <- readxl::read_xls(path = downloaded_file, sheet = "conflict")

  ## Merge
  xyz <- merge(merge(x, y), z)

  ## Return xyz
  xyz
}


################################################################################
#
#'
#' Get Lanao del Sur Barangay level data masterlist from the Open Marawi Google
#' Drive Database
#'
#' @param id Character value for the unique identifier for the Lanao del Sur
#'   Open Marawi **Google Drive** Masterlist file in **.xlsx** format. Currently set
#'   to the identifier specified when `marawi_ls_armm()` is called. Change this
#'   if Open Marawi **Google Drive** is moved or if name of masterlist file
#'   changes.
#' @param dataset A string value from either `metadata`, `demographics`,
#'   `facilities`, `establishment`, `mode_transport`, `conflict`,
#'   `private_schools`, or `health` specifying the masterlist data to retrieve.
#'   If not specified, will default to `metadata`
#'
#' @return A tibble of the specified masterlist dataset available in the
#'   Lanao del Sur directory of the Open Marawi Database.
#'
#' @author Ernest Guevarra
#'
#' @examples
#' ## Retrieve the metadata dataset of the Lanao del Sur directory in the Open
#' ## Marawi database
#' marawi_get_lanao(dataset = "metadata")
#'
#' @rdname marawi_get
#' @export
#'
#'
#
################################################################################

marawi_get_lanao <- function(id = marawi_ls_lanao()$id[stringi::stri_detect(marawi_ls_lanao()$name, fixed = "Masterlist_bgy")],
                            dataset = c("metadata", "demographics",
                                        "facilities", "establishment",
                                        "mode_transport", "conflict",
                                        "private_schools", "health")) {
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
    establishment = 4,
    mode_transport = 5,
    conflict = 6,
    private_schools = 7,
    health = 8
  )

  ## Read the specified inventory
  x <- readxl::read_xlsx(path = downloaded_file, sheet = sheet)

  ## Return x
  x
}


################################################################################
#
#'
#' @examples
#' marawi_get_lanao_all()
#'
#' @rdname marawi_get
#' @export
#'
#
################################################################################

marawi_get_lanao_all <- function(id = marawi_ls_lanao()$id[stringi::stri_detect(marawi_ls_lanao()$name, fixed = "Masterlist_bgy")]) {
  ## Google Drive deauthorisation
  googledrive::drive_deauth()

  ## Create temp file
  downloaded_file <- tempfile()

  ## Download masterlist file
  googledrive::drive_download(file = id, path = downloaded_file)

  ## Read demographics
  x1 <- readxl::read_xlsx(path = downloaded_file, sheet = "demographics")

  ## Read facilities
  x2 <- readxl::read_xlsx(path = downloaded_file, sheet = "bgyfacilities")

  ## Read establishment
  x3 <- readxl::read_xlsx(path = downloaded_file, sheet = "estab")

  ## Read mode transportation
  x4 <- readxl::read_xlsx(path = downloaded_file, sheet = "modetranspo")

  ## Read conflict
  x5 <- readxl::read_xlsx(path = downloaded_file, sheet = "conflict")

  ## Read private schools
  x6 <- readxl::read_xlsx(path = downloaded_file, sheet = "privateschools")

  ## Read health
  x7 <- readxl::read_xlsx(path = downloaded_file, sheet = "health")

  ## Merge
  x <- merge(
    merge(
      merge(
        merge(
          merge(
            merge(x1, x2),
            x3
          ),
          x4
        ),
        x5
      ),
      x6,
    ),
    x7
  )

  ## Return xyz
  x
}
