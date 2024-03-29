#'
#' List Marawi maps
#'
#' @param id Character value of the Open Marawi **Google Drive** id for the
#'   Marawi geospatial directory. This is currently set to
#'   *1Fj5sK2tKo0_v8EnEbN8m1pMfaSFADmYL* which is the id of the current id
#'   of the Open Marawi **Google Drive** for the Marawi geospatial directory.
#'   Change this if Open Marawi **Google Drive** is moved.
#'
#' @returns A dribble of all the geospatial files for Marawi found in the
#'   Open Marawi **Google Drive**.
#'
#' @examples
#' marawi_maps_ls()
#'
#' @rdname marawi_maps_ls
#' @export
#'

marawi_maps_ls <- function(id = "1Fj5sK2tKo0_v8EnEbN8m1pMfaSFADmYL") {
  ## Google Drive deauthorisation ----
  googledrive::drive_deauth()

  ## Get Marawi Google Drive identifier ----
  dsn <- googledrive::as_id(id) |>
    googledrive::drive_ls()

  ## First pass list of files and folders ----
  df1 <- lapply(
    X = googledrive::as_id(dsn$id),
    FUN = googledrive::drive_ls
  ) |>
    (\(x) { names(x) <- dsn$name; x })() |>
    dplyr::bind_rows(.id = "dsn")

  ## Second pass list of files and folders ----
  df2 <- df1 |>
    dplyr::filter(googledrive::is_folder(df1))

  df2 <- df2 |>
    dplyr::pull(.data$id) |>
    lapply(FUN = googledrive::drive_ls) |>
    (\(x) { names(x) <- df2$name; x })() |>
    dplyr::bind_rows(.id = "dsn")

  ## Third pass list of files ----
  df3 <- df2 |>
    dplyr::filter(googledrive::is_folder(df2))

  df3 <- df3 |>
    dplyr::pull(.data$id) |>
    lapply(FUN = googledrive::drive_ls) |>
    (\(x) { names(x) <- df3$name; x })() |>
    dplyr::bind_rows(.id = "dsn")

  ## Bring dfs together ----
  df <- dplyr::full_join(df1, df2, by = c("name" = "dsn")) |>
    dplyr::mutate(
      dsn = ifelse(
        is.na(.data$name.y), .data$dsn, file.path(.data$dsn, .data$name)
      ),
      name = ifelse(is.na(.data$name.y), .data$name, .data$name.y),
      id = ifelse(is.na(.data$name.y), .data$id.x, .data$id.y),
      drive_resource = ifelse(
        is.na(.data$name.y), .data$drive_resource.x, .data$drive_resource.y
      )
    ) |>
    dplyr::select(.data$dsn, .data$name, .data$id, .data$drive_resource) |>
    dplyr::full_join(df3, by = c("name" = "dsn")) |>
    dplyr::mutate(
      dsn = ifelse(
        is.na(.data$name.y), .data$dsn, file.path(.data$dsn, .data$name)
      ),
      name = ifelse(is.na(.data$name.y), .data$name, .data$name.y),
      id = ifelse(is.na(.data$name.y), .data$id.x, .data$id.y),
      drive_resource = ifelse(
        is.na(.data$name.y), .data$drive_resource.x, .data$drive_resource.y),
      layer = stringr::str_remove_all(
        string = .data$name,
        pattern = paste0(".", tools::file_ext(.data$name)) |>
          paste(collapse = "|")
      )
    ) |>
    dplyr::select(
      .data$dsn, .data$layer, .data$name, .data$id, .data$drive_resource
    ) |>
    googledrive::as_dribble()

  ## Return df ----
  df
}
