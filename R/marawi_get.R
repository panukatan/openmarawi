#'
#' Get ARMM Barangay level data masterlist from the Open Marawi Google Drive
#' Database
#'
#' @param dataset A string value from either *"metadata"*, *"demographics"*,
#'   *"bgyfacilities"*, or *"conflict"* specifying the masterlist data to
#'   retrieve. If not specified, will default to *"metadata"*.
#' @param tabular Logical. Should output be flattened into a single data.frame?
#'   Default is TRUE. If FALSE, output is a list of data.frames for each
#'   sheet in the masterlist Excel file.
#'
#' @return A tibble or a list of tibbles of the specified masterlist dataset
#'   available in the ARMM directory of the Open Marawi Database.
#'
#' @author Ernest Guevarra
#'
#' @examples
#' ## Retrieve the metadata dataset of the ARMM directory in the Open Marawi
#' ## database
#' marawi_get_armm(dataset = "metadata")
#' \dontrun{marawi_get_armm_all()}
#'
#' @rdname marawi_get_armm
#' @export
#'
#'
marawi_get_armm <- function(dataset = c("metadata", "demographics",
                                        "bgyfacilities", "conflict")) {
  ## Google Drive deauthorisation ----
  googledrive::drive_deauth()

  ## Get ARMM Google Drive identifier ----
  id <- get_drive_id(
    pattern = "Masterlist",
    drive_list = marawi_ls() |> marawi_ls_armm()
  )

  ## Create temp file ----
  downloaded_file <- tempfile()

  ## Download masterlist file
  googledrive::drive_download(file = id, path = downloaded_file) |>
    googledrive::with_drive_quiet()

  ## Define dataset argument ----
  dataset <- match.arg(dataset)

  ## Read the specified inventory ----
  df <- readxl::read_xls(path = downloaded_file, sheet = dataset)

  ## Return df ----
  df
}


#'
#' @rdname marawi_get_armm
#' @export
#'

marawi_get_armm_all <- function(tabular = TRUE) {
  ## Google Drive deauthorisation ----
  googledrive::drive_deauth()

  ## Get ARMM Google Drive identifier ----
  id <- get_drive_id(
    pattern = "Masterlist",
    drive_list = marawi_ls() |> marawi_ls_armm()
  )

  ## Create temp file ----
  downloaded_file <- tempfile()

  ## Download masterlist file ----
  googledrive::drive_download(file = id, path = downloaded_file) |>
    googledrive::with_drive_quiet()

  ## Read each data sheet and merge ----
  df <- Map(
    f = readxl::read_xls,
    path = downloaded_file,
    sheet = list("demographics", "bgyfacilities", "conflict")
  )

  ## Should datasets be flattened into a single tibble? ----
  if (tabular) {
    df <- Reduce(dplyr::left_join, df)
  }

  ## Return df ----
  df
}


#'
#' Get Lanao del Sur Barangay level data masterlist from the Open Marawi Google
#' Drive Database
#'
#' @param dataset A string value from either *"metadata"*, *"demographics"*,
#'   *"facilities"*, *"establishment"*, *"mode_transport"*, *"conflict"*,
#'   *"private_schools"*, or *"health"* specifying the masterlist data to
#'   retrieve. If not specified, will default to *"metadata"*.
#' @param tabular Logical. Should output be flattened into a single data.frame?
#'   Default is TRUE. If FALSE, output is a list of data.frames for each
#'   sheet in the masterlist Excel file.
#'
#' @return A tibble or a list of tibbles of the specified masterlist dataset
#'   available in the Lanao del Sur directory of the Open Marawi Database.
#'
#' @author Ernest Guevarra
#'
#' @examples
#' ## Retrieve the metadata dataset of the Lanao del Sur directory in the Open
#' ## Marawi database
#' marawi_get_lanao(dataset = "metadata")
#' \dontrun{marawi_get_lanao_all()}
#'
#' @rdname marawi_get_lanao
#' @export
#'

marawi_get_lanao <- function(dataset = c("metadata", "demographics",
                                         "bgyfacilities", "estab",
                                         "modetranspo", "conflict",
                                         "privateschools", "health")) {
  ## Google Drive deauthorisation ----
  googledrive::drive_deauth()

  ## Get Lanao del Sur Google Drive identifier ----
  id <- get_drive_id(
    pattern = "Masterlist_bgy",
    drive_list = marawi_ls() |> marawi_ls_lanao()
  )

  ## Create temp file ----
  downloaded_file <- tempfile()

  ## Download masterlist file ----
  googledrive::drive_download(file = id, path = downloaded_file) |>
    googledrive::with_drive_quiet()

  ## Define dataset argument ----
  dataset <- match.arg(dataset)

  ## Read the specified inventory
  df <- readxl::read_xlsx(path = downloaded_file, sheet = dataset)

  ## Return df ----
  df
}


#'
#' @rdname marawi_get_lanao
#' @export
#'

marawi_get_lanao_all <- function(tabular = TRUE) {
  ## Google Drive deauthorisation ----
  googledrive::drive_deauth()

  ## Get Lanao del Sur Google Drive identifier ----
  id <- get_drive_id(
    pattern = "Masterlist_bgy",
    drive_list = marawi_ls() |> marawi_ls_lanao()
  )

  ## Create temp file ----
  downloaded_file <- tempfile()

  ## Download masterlist file ----
  googledrive::drive_download(file = id, path = downloaded_file) |>
    googledrive::with_drive_quiet()

  ## Read each data sheet and merge ----
  df <- Map(
    f = readxl::read_xlsx,
    path = downloaded_file,
    sheet = list(
      "demographics", "bgyfacilities", "estab", "modetranspo", "conflict",
      "privateschools", "health"
    )
  )

  ## Ensure that PSGC codes are character values ----
  df <- df |>
    lapply(
      FUN = function(x) dplyr::mutate(
        x,
        dplyr::across(
          .cols = dplyr::starts_with("PSGC"),
          .fns = ~as.character(.x)
        )
      )
    )

  ## Should datasets be flattened into a single tibble? ----
  if (tabular) {
    df <- Reduce(dplyr::left_join, df) |>
      suppressMessages()
  }

  ## Return df ----
  df
}



#'
#' Get Marawi Barangay level data masterlist from the Open Marawi Google
#' Drive Database
#'
#' @param dataset A string value from either *"metadata"*, *"demographics"*,
#'   *"movements"*, *"culture"*, *"housing"*, *"facilities"*,
#'   *"establishments"*, *"modetranspo"*, *"conflict"*, *"publicschools"*,
#'   *"privateschools"*, and *"health"* specifying the masterlist data to
#'   retrieve. If not specified, will default to *"metadata"*.
#' @param tabular Logical. Should output be flattened into a single data.frame?
#'   Default is TRUE. If FALSE, output is a list of data.frames for each
#'   sheet in the masterlist Excel file.
#'
#' @return A tibble or a list of tibbles of the specified masterlist dataset
#'   available in the Lanao del Sur directory of the Open Marawi Database.
#'
#' @author Ernest Guevarra
#'
#' @examples
#' ## Retrieve the metadata dataset of the Lanao del Sur directory in the Open
#' ## Marawi database
#' marawi_get_marawi(dataset = "metadata")
#' \dontrun{marawi_get_marawi_all()}
#'
#' @rdname marawi_get_marawi
#' @export
#'

marawi_get_marawi <- function(dataset = c("metadata", "demographics",
                                          "movements", "culture",
                                          "housing", "facilities",
                                          "establishments", "modetranspo",
                                          "conflict", "publicschools",
                                          "privateschools", "health")) {
  ## Google Drive deauthorisation ----
  googledrive::drive_deauth()

  ## Get Marawi Google Drive identifier ----
  id <- marawi_ls() |> marawi_ls_marawi() |>
    (\(x) x[!googledrive::is_native(x), ])() |>
    dplyr::filter(
      stringr::str_detect(string = .data$name, pattern = "Masterlist")
    ) |>
    dplyr::pull(.data$id)

  ## Create temp file ----
  downloaded_file <- tempfile()

  ## Download masterlist file ----
  googledrive::drive_download(file = id, path = downloaded_file) |>
    googledrive::with_drive_quiet()

  ## Define dataset argument ----
  dataset <- match.arg(dataset)

  sheet <- switch(
    EXPR = dataset,
    metadata = 1,
    demographics = 2,
    movements = 3,
    culture = 4,
    housing = 5,
    facilities = 6,
    establishments = 7,
    modetranspo = 8,
    conflict = 9,
    publicschools = 10,
    privateschools = 11,
    health = 12
  )

  ## Read the specified inventory
  df <- readxl::read_xlsx(path = downloaded_file, sheet = sheet)

  ## Return df ----
  df
}


#'
#' @rdname marawi_get_marawi
#' @export
#'
marawi_get_marawi_all <- function(tabular = TRUE) {
  ## Google Drive deauthorisation ----
  googledrive::drive_deauth()

  ## Get Marawi Google Drive identifier ----
  id <- marawi_ls() |> marawi_ls_marawi() |>
    (\(x) x[!googledrive::is_native(x), ])() |>
    dplyr::filter(
      stringr::str_detect(string = .data$name, pattern = "Masterlist")
    ) |>
    dplyr::pull(.data$id)

  ## Create temp file ----
  downloaded_file <- tempfile()

  ## Download masterlist file ----
  googledrive::drive_download(file = id, path = downloaded_file) |>
    googledrive::with_drive_quiet()

  ## Read each data sheet and merge ----
  df <- Map(
    f = readxl::read_xlsx,
    path = downloaded_file,
    sheet = readxl::excel_sheets(downloaded_file) |>
      (\(x)
        x[stringr::str_detect(string = x, pattern = "metadata", negate = TRUE)]
      )()
  )

  ## Ensure that PSGC codes are character values ----
  df <- df |>
    lapply(
      FUN = function(x) dplyr::mutate(
        x,
        dplyr::across(
          .cols = dplyr::starts_with("PSGC"),
          .fns = ~as.character(.x)
        )
      )
    )

  ## Should datasets be flattened into a single tibble? ----
  if (tabular) {
    df <- Reduce(dplyr::left_join, df) |>
      suppressMessages()
  }

  ## Return df ----
  df
}
