#' read a fars file
#'
#' This funtion reads the file located at \code{filename}.
#'
#' @param filename the path to the file
#' @return a data frame
#' @importFrom readr read_csv
#' @note stops if \code{filename} does not exist
#' @examples \dontrun{data <- fars_read(make_filename("2013")}
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' generate a filename
#'
#' generate a string in the form of 'accident_<year>.csv.bz2'
#'
#' @param year a string or integer. It is converted to an integer.
#' @return string of the generaed filename
#' @examples make_filename("2017")
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' months and years in the fars data
#'
#' read and print the months and years from fars data for a given list of years
#'
#' @param years a list of years
#' @return a data frame with the month and year
#' @importFrom tidyr %>%
#' @importFrom dplyr mutate
#' @note warns when a year is not found in the fars data
#' @examples \dontrun{d <- fars_read_years(years = c("2015","2016")) }
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' read and summarize fars data
#'
#' read fars data and summize annual data for a given list of years
#'
#' @param years a list of years
#' @return a data frame with year and count
#' @importFrom tidyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @note warns when a year is not found in the fars data
#' @examples \dontrun{d <- fars_summarize_years(years = c("2015","2016")) }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' plot a map of accidents
#'
#' plot a map of accidents for the given state and year
#'
#' @param state.num a number code for a state
#' @param year a list of years
#' @return a data frame with year and count
#' @importFrom graphics points
#' @importFrom maps map
#' @note warns when the year is not found in the fars data
#' @note stops when the state is not found in the fars data
#' @note warns when no accidents found
#' @examples \dontrun{d <- fars_map_states(state.num = 9, years = "2015") }
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
