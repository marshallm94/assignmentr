#' Read a CSV file into a tibble (tidyverse dataframe)
#'
#' This function reads a CSV file into R as a tibble.
#'
#' @param filename The aboslute or relative path to the filename to read into
#'    R (should be a CSV file) .
#' 
#' @return The CSV data in tibble (tidyverse version of data frames) format.
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' 
#' @note If the file doesn't exist, a message will be displayed saying so.
#'
#' @examples
#' \dontrun{df <- fars_read("path/to/some/random/file.csv")}
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Creates a descriptive filename based on the input year.
#'
#' @param year The year that you would like in the filename.
#' 
#' @return A character object that represents the filename for car fatality data.  
#'
#' @examples
#' \dontrun{filename_2018 <- make_filename(2018)}
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        filename <- sprintf("accident_%d.csv.bz2", year)
	filepath <- system.file('extdata', filename, package='assignmentr')
	return(filepath)
}

#' Collects the MONTH and year associated with a fatal car incident for each year
#'    provided.
#'
#' @param years A vector of years which have an associated file in the current
#'    working directory.
#' 
#' @return A list of data frames, each with two columns; MONTH and year. Each
#' row in a data frame represent a fatal car accident in that month and year. 
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' 
#' @note If one of the years specified doesn't have an associated file, and error
#'    will be thrown.
#' 
#' @examples
#' \dontrun{years <- fars_read_years(c(2013, 2014, 2015))}
#' 
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate_(dat, year = ~year) %>% 
                                dplyr::select_(~MONTH, ~year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Creates a table summarizing the count of fatal car incidents in a given month
#' and year combination.
#'
#' @param years a vector of years for which the number of fatal car incidents
#'     should be displayed, grouped by Month.
#' 
#' @return a data frame.
#' 
#' @importFrom dplyr bind_rows 
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize 
#' @importFrom tidyr spread 
#' 
#' @note If one of the years specified doesn't have an associated file, and error
#'    will be thrown.
#' 
#' @examples
#' \dontrun{summary_df <- fars_summarize_years(c(2013, 2014, 2015))}
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by_(~year, ~MONTH) %>% 
                dplyr::summarize_(n = ~n()) %>%
                tidyr::spread_(key_col = 'year',
			       value_col = 'n')
}

#' Creates a geographical plot showing the accidents for a given year in a given
#'    state. Each accident is plotted as a single dot.
#'
#' @param state.num The state number which you would like displayed.
#' @inheritParams make_filename
#'
#' @return Displays a plot (no object returned).
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom graphics points 
#'
#' @note If the state.num argument doesn't correspond to a state within the 
#'    specified data frame's STATE column, and error will be thrown.
#' @note If there are no accidents to plot for the specified state.num/year combination
#'   a message will display saying so. 
#' 
#' @examples
#' \dontrun{fars_map_state(42, 2015)}
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~STATE == state.num)
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
