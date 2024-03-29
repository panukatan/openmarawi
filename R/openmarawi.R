#'
#' An Interface to the Open Marawi Database
#'
#' The citizens of Marawi have a right to the data and maps about their home
#' city. When problems are complex, helping people find useful maps (access)
#' can aid them both in finding themselves in the map (understanding) and making
#' the map by themselves (ownership). Open data and useful maps can help empower
#' citizens in mapmaking, placemaking, and decision-making because it can help
#' citizens and interested parties in understanding the issues spatially. It is
#' practical in deliberating, deciding, and delivering the rehabilitation of
#' Marawi City.
#'
#' @name openmarawi
#' @docType package
#' @keywords internal
#' @importFrom googledrive drive_deauth drive_ls as_id is_dribble
#' @importFrom googlesheets4 gs4_deauth read_sheet
#' @importFrom dplyr filter pull
#' @importFrom readxl read_xls
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#'
"_PACKAGE"
