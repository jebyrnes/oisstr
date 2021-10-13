#' Download NOAA OISST Data
#'
#' @param start Start date. Must be formatted with \code{as.Date()}
#' @param end End date. Must be formatted with \code{as.Date()}
#' @param latitude_ext Vector of two coordinates with latitude extent.
#' @param longitude_ext Vector of two coordinates with longitude extent
#' @param url URL of ERDDAP server. Defaults to https://coastwatch.pfeg.noaa.gov/erddap
#' @param dataset Dataset used on ERDDAP server. Defaults to ncdcOisst21Agg_LonPM180 See https://coastwatch.pfeg.noaa.gov/erddap/search/index.html?page=1&itemsPerPage=1000&searchFor=OISST for more.
#'
#' @return A data frame of latitude, longitude, date, and temperature
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' temp <- OISST_sub_dl(
#'   start = as.Date("1982-01-01"),
#'   end = as.Date("1989-12-31"),
#'   latitude_ext = c(-40, -35),
#'   longitude_ext = c(15, 21)
#' )
#'
#'   ggplot(data = temp,
#'     aes(x = lon, y = lat)) +
#'     geom_tile(aes(fill = temp)) +
#'     scale_fill_viridis_c() +
#'     coord_quickmap(expand = F) +
#'     labs(x = NULL, y = NULL, fill = "SST (°C)") +
#'     theme(legend.position = "bottom")
#' }
OISST_sub_dl <- function(start,
                         end,
                         latitude_ext,
                         longitude_ext,
                         url = "https://coastwatch.pfeg.noaa.gov/erddap/",
                         dataset = "ncdcOisst21Agg_LonPM180") {
  OISST_dat <- rerddap::griddap(
    x = dataset,
    url = url,
    time = c(start, end),
    zlev = c(0, 0),
    latitude = latitude_ext,
    longitude = longitude_ext,
    fields = "sst"
  )$data %>%
    dplyr::mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>%
    dplyr::rename(t = time, temp = sst) %>%
    dplyr::select(lon, lat, t, temp) %>%
    stats::na.omit()
}
