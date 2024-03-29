require(tidyverse)
context('Assignment Functions')

test_that("fars_read reads valid data", {
		  filename <- make_filename(2013)
		  test_data <- readr::read_csv(filename, progress = FALSE)
		  expect_equal(test_data, fars_read(filename))
})


test_that("fars_read throws error for invalid data", {
		  test_file <- "jason_bournes_data.csv"
		  expect_error(fars_read(test_file))
})

test_that("make_filename creates correct filename", {
		  test_name <- "accident_2013.csv.bz2"
		  test_file <- system.file('extdata',
					   test_name,
					   package='assignmentr')
		  expect_equal(test_file, make_filename(2013))
})
	
test_that("fars_read_years returns list of dataframe", {
		  df_2013 <- read_csv(make_filename(2013)) %>%
			  mutate(year = 2013) %>%
			  select(MONTH, year)
		  df_2014 <- read_csv(make_filename(2014)) %>%
			  mutate(year = 2014) %>%
			  select(MONTH, year)
		  df_2015 <- read_csv(make_filename(2015)) %>%
			  mutate(year = 2015) %>%
			  select(MONTH, year)

		  correct_years <- list(df_2013, df_2014, df_2015)
		  test_years <- fars_read_years(c(2013,	2014, 2015))

		  expect_equal(correct_years, test_years)
})

test_that("fars_summarize_years returns correct data", {
		  df_2013 <- read_csv(make_filename(2013)) %>%
			  mutate(year = 2013) %>%
			  select(MONTH, year)
		  df_2014 <- read_csv(make_filename(2014)) %>%
			  mutate(year = 2014) %>%
			  select(MONTH, year)
		  df_2015 <- read_csv(make_filename(2015)) %>%
			  mutate(year = 2015) %>%
			  select(MONTH, year)

		  correct_data <- list(df_2013, df_2014, df_2015) %>%
			  bind_rows() %>%
			  group_by(year, MONTH) %>%
			  summarize(n = n()) %>%
			  spread(year, n)

		  years <- c(2013, 2014, 2015)
		  expect_equal(correct_data, fars_summarize_years(years))
})
