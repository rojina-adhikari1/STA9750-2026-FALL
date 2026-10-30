library(readr)
library(dplyr)

TEST_RESULTS <- read_csv("https://michael-weylandt.com/STA9750/schools_test_results.csv")
SCHOOL_INFO  <- read_csv("https://michael-weylandt.com/STA9750/schools_covariates.csv")

glimpse(TEST_RESULTS)
glimpse(SCHOOL_INFO)

## Question 5
# Find the column(s) that mention "district"
district_cols <- grep("district", names(SCHOOL_INFO), ignore.case = TRUE, value = TRUE)
print(district_cols)
