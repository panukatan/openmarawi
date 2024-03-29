#'
#' Get maps data from Open Marawi Google Drive
#'
#' @param dsn Data source name of map to retrieve. This should be one of the
#'   data source names listed in `marawi_maps_list`
#' @param layer Layer name of map to retrieve. If NULL (default), all layers
#'   available within `dsn` will be retrieved.
#'
#' @returns An sf object of specified map
#'
#' @examples
#' ##marawi_maps_get(dsn = "Hazards/Faultlines")
#'
#' @rdname marawi_maps_get
#' @export
#'

marawi_maps_get <- function(dsn, layer = NULL) {
  if (is.null(layer))
    map_sublist <- openmarawi::marawi_maps_list |>
      dplyr::filter(.data$dsn == !!dsn)
  else
    map_sublist <- openmarawi::marawi_maps_list |>
      dplyr::filter(.data$dsn == !!dsn, .data$layer == !!layer)

  Map(
    f = googledrive::drive_download,
    file = as.list(googledrive::as_id(map_sublist$id)),
    path = file.path(tempdir(), map_sublist$name) |> as.list(),
    overwrite = TRUE
  )

  ## Detect SHP ----
  shp_file <- map_sublist$name |>
    tools::file_ext() |>
    stringr::str_detect("shp|SHP") |>
    any()

  ## Read map file ----
  if (shp_file) {
      map_data <- sf::st_read(dsn = tempdir(), layer = map_sublist$layer[1])
    }

  ## Return map data ----
  map_data
}
