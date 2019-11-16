library(tidyverse)
context('Assignment Functions')

# copy files in data directory so fars_read_years() has
# access to the files needed
for (year in c(2013, 2014, 2015)) {
	file_name <- paste("../../data/", make_filename(year), sep = "")
	file.copy(file_name, make_filename(year))
}

test_that("fars_read reads valid data", {
		  test_file <- "accident_2013.csv.bz2"
		  test_data <- readr::read_csv(test_file, progress = FALSE)
		  expect_equal(test_data, fars_read(test_file))
})


test_that("fars_read throughs error for invalid data", {
		  test_file <- "jason_bournes_data.csv"
		  expect_error(fars_read(test_file))
})

test_that("make_filename creates correct filename", {
		  test_name <- "accident_2013.csv.bz2"
		  expect_equal(test_name, make_filename(2013))
})
	
test_that("fars_read_years returns list of dataframe", {
		  df_2013 <- read_csv("accident_2013.csv.bz2") %>%
			  mutate(year = 2013) %>%
			  select(MONTH, year)
		  df_2014 <- read_csv("accident_2014.csv.bz2") %>%
			  mutate(year = 2014) %>%
			  select(MONTH, year)
		  df_2015 <- read_csv("accident_2015.csv.bz2") %>%
			  mutate(year = 2015) %>%
			  select(MONTH, year)

		  correct_years <- list(df_2013, df_2014, df_2015)
		  test_years <- fars_read_years(c(2013,	2014, 2015))

		  expect_equal(correct_years, test_years)
})

test_that("fars_summarize_years returns correct data", {
		  df_2013 <- read_csv("accident_2013.csv.bz2") %>%
			  mutate(year = 2013) %>%
			  select(MONTH, year)
		  df_2014 <- read_csv("accident_2014.csv.bz2") %>%
			  mutate(year = 2014) %>%
			  select(MONTH, year)
		  df_2015 <- read_csv("accident_2015.csv.bz2") %>%
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

# removing data files
for (year in c(2013, 2014, 2015)) {
	file.remove(make_filename(year))
}
