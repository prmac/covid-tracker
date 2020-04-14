library(httr)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(zoo)
library(plotly)


covid_stats <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
populations <- read_csv("https://raw.githubusercontent.com/datasets/population/master/data/population.csv")


format_covid_stats <- function(x, country = "United Kingdom") {
	dplyr::filter(x, 
		`Country/Region` %in% country,
		is.na(`Province/State`)) %>%
	dplyr::select(
		Country = `Country/Region`, 
		dplyr::matches("\\d")) %>%
	dplyr::mutate(Country = stringr::str_replace(Country, "US", "United States")) %>%
	reshape2::melt(variable.name = "Date", value.name = "Deaths") %>%
	dplyr::group_by(Country) %>%
	dplyr::mutate(
		`New Deaths` = Deaths - lag(Deaths, 1, default = 0),
		`Rolling average deaths` = rollapply(`New Deaths`, 7, mean, fill = 0, align = "right")
		)
}

format_population_stats <- function(x, country = "United Kingdom") {
	dplyr::filter(x,
		`Country Name` %in% country,
		Year == max(Year)) %>%
	dplyr::select(Country = `Country Name`, Population = Value)
}

format_plot_data <- function(covid_data = covid_stats, 
	population_data = populations, countries = "United Kingdom") {

	x <- format_covid_stats(covid_data, countries)
	y <- format_population_stats(population_data, countries)
	dplyr::left_join(x, y) %>%
	dplyr::mutate(
		`Rolling average deaths/million population` = `Rolling average deaths`/Population * 1000000) %>%
	dplyr::filter(`Rolling average deaths/million population` > 1) %>%
	dplyr::mutate(Day = 1:n()) %>%
	dplyr::ungroup()
}


plotting_data <- format_plot_data( 
	countries = c("United Kingdom", "Italy", "Spain", "France", "US", "United States", "Ireland", "Germany"))

p <- ggplot(plotting_data, aes(x = Day, y = `Rolling average deaths/million population`, group = Country)) +
	geom_line(aes(colour = Country)) +
	scale_colour_brewer(palette = "Set2") + 
	theme_classic()

ply <- ggplotly(p)

htmlwidgets::saveWidget(ply, "index.html", selfcontained = FALSE)


