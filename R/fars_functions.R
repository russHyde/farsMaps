#' Reads 'Fatality Analysis Reporting System' data from a file
#' 
#' Given a path to a file that contains the FARS dataset, this function will
#' read that dataset in and return a tibble containing the data. Throws an
#' error if the filepath does not exist.
#' 
#' @param        filename      The path to a .csv file that contains the FARS
#'   dataset.
#'
#' @return       A tibble containing the FARS dataset.
#' 
#' @importFrom   dplyr         tbl_df
#' @importFrom   readr         read_csv
#' 
#' @examples
#' \dontrun{fars_read("some_file.csv")}
#' 
fars_read <- function(filename) {
        if (!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Converts a year into the filename that contains FARS data for that year.
#' 
#' The FARS data is stored in a range of .bz2 files with a consistent format.
#' This function takes a year (as a string or an integer, like '1984') and
#' returns the .bz2 file corresponding to that year
#' 
#' @param        year          A vector of years, provided as integers (1984) 
#'   or strings ("1984", "2001"). Do not provide R 'Date' objects or shortened
#'   suffixes (eg 84) for a year.
#'
#' @return       A vector of filenames.
#' 
#' @examples
#' make_filename(1984:1987)
#' make_filename(year = c("1984", "1985"))
#' 
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Obtains the 'Fatality Analysis Reporting System' data for a set of years
#' from the whole set of FARS data.
#' 
#' Takes a vector of years, imports the data for each year from the FARS
#' dataset and returns a list of tibbles containing the data for each year.
#' 
#' @param        years         A vector of years, provided as a character
#'   vector or as an integer vector. Provide the years in 4-digit format
#'   to ensure the correct file can be found for that year. If any of the years
#'   is absent from the FARS dataset, a NULL value is returned for that year.
#'
#' @return       A list of tibble data-frames containing the FARS dataset for
#'   each year in the input set. The entry is NULL for any year that is absent
#'   from the FARS dataset.
#'
#' @importFrom   dplyr         mutate   select
#' @importFrom   magrittr      %>%
#' 
#' @export
#' 
#' @examples
#' fars_read_years(c(1984, 1988))
#' 
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

#' Summarises the FARS data for a set of years
#' 
#' Given a vector of years, this function will obtain all FARS data for each
#' year and combine it into a summarised dataset. The count of fatailities in 
#' each month in the selected years is returned. No data is returned for years
#' that are absent from the available data.
#' 
#' @param        years         A vector of years, provided as integers or 
#'   strings. Only those entries that are present in the FARS data provide any
#'   valid returned data.
#' 
#' @return       A tibble containing the count of fatalities in the FARS data
#'   for each month and year. If all of the years are absent from the FARS data
#'   a NULL value is returned.
#'
#' @importFrom   magrittr      %>%
#' @importFrom   dplyr         bind_rows   group_by   summarize
#' @importFrom   tidyr         spread
#'
#' @examples
#' fars_summarize_years(1984:1987)
#' 
#' @export
#' 
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Generates a map of the fatalities occurring in a given state in a given year
#' 
#' Provided a state (as an integer) and a year, this obtains all fatality data
#' for that state and plots out all the positions of the fatalities for that
#' state on a map.
#' 
#' @param        state.num     An integer corresponding to a required state.
#'   The function throws an error if no fatality information is available for
#'   the requested state in the FARS data for the requested year.
#'
#' @param        year          A year, provided as a string or an integer. If
#'   the year is absent from the FARS data, this throws an error.
#' 
#' @return       NULL return value. The function plots a graph as a side-effect
#'   If no accidents were observed in the state/year, no map is plotted.
#' 
#' @importFrom   dplyr         filter
#' @importFrom   maps          map
#' @importFrom   graphics      points
#' 
#' @export
#' 
#' @examples
#' fars_map_state(10, 2013)
#' 
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if (!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if (nrow(data.sub) == 0L) {
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
