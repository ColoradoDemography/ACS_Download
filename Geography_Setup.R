#Download the following files with the appropriate year and unzip into the data folder:
#From 'http://www2.census.gov/geo/tiger/GENZ2015/shp/', download
#'cb_2015_us_county_500k.zip','cb_2015_us_state_500k.zip', 'carto_bg_15.zip',
#'carto_pl_15.zip', 'carto_tr_15.zip'      

#From 'http://www2.census.gov/geo/tiger/TIGER2015/COUNTY/', download
#'tl_2015_us_county.zip', 'tl_2015_us_state.zip', 'tiger_bg_15.zip',
#'tiger_pl_15.zip', 'tiger_tr_15.zip'

co_commute = get_acs(
  geography = "county",
  variables = commute_vars,
  endyear = 2010,
  output = "tidy",
  state = "CO",
  geometry = TRUE
)

co_commute %>%
  filter(variable %in% commute_filter) %>%
  ggplot(aes(x = NAME, y = estimate, fill = variable)) +
    geom_col() +
    xlab("County") + ylab("county") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))