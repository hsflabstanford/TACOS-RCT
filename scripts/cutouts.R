
### Cutouts

# how many NAs in location? Check how many matches we got
(nrow(dat) - sum(is.na(dat$location))) - sum(!is.na(dat$median_income))
# if you wish to investigate whether this match worked, do the following:
problem_locations <- dat |> filter(is.na(median_income)) |> select(record_id, county_name, state, location, median_income) |> filter(!is.na(county_name)); problem_locations
# (one person put location down incorrectly; there's no Dallas VA)
# here's a spot check to see if we got these right or 
# if there are other data processing issues
# Set seed for reproducibility
set.seed(123)

# Select 10 random rows from dat
random_rows <- dat |> 
  filter(!is.na(median_income)) |> 
  sample_n(10) |> 
  select(record_id, location, median_income)

# For each random row, find the matching row in income_dat
for (i in 1:nrow(random_rows)) {
  loc <- random_rows$location[i]
  
  # Find the matching row in income_dat
  income_match <- income_dat |> 
    filter(location == loc) |> 
    select(NAME, estimate, location)
  
  # Print the comparison
  cat("Row", i, "- Record ID:", random_rows$record_id[i], "\n")
  cat("Location:", random_rows$location[i], "\n")
  cat("Combined dataset median_income:", random_rows$median_income[i], "\n")
  cat("Income dataset estimate:", income_match$estimate, "\n")
  cat("Match:", random_rows$median_income[i] == income_match$estimate, "\n\n")
}
rm(random_rows)
# Let's check some examples to make sure cleaning worked as expected
election_dat |> 
  select(county_name, state_name, location) |>
  filter(grepl("Baltimore|Fairfax|Alexandria|Richmond", county_name))
# Check join success with a summary
join_summary <- dat |>
  summarize(
    total_rows = n(),
    rows_with_income = sum(!is.na(median_income)),
    rows_with_election = sum(!is.na(votes_gop)),
    pct_with_income = mean(!is.na(median_income)) * 100,
    pct_with_election = mean(!is.na(votes_gop)) * 100
  ); join_summary

library(VIM)
aggr(dat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(dat), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
