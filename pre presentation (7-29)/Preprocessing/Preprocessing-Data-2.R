# PURPOSE: Compress CSVs to RDS files

year <- seq(2015, 2022, 1)

# Read in CSVs and write as compressed RDS files
for(i in 1:8)
{
  temp_csv <- read_csv(glue("data/statcast_data_batters_", year[i]))
  write_rds(temp_csv, glue("data/statcast_data_", year[i], ".rds"), compress = "gz")
}