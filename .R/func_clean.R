library(tidyverse)
library(lubridate)

clean_data <- function(data){
  ##formats dates, gift region and amount
  parsed_data <- data %>%
    mutate(Gf_Date = parse_date(Gf_Date, format = "%m/%d/%Y")) %>%
    mutate(
      g_year = lubridate::year(Gf_Date),
      g_month = lubridate::month(Gf_Date),
      g_date = lubridate::day(Gf_Date)
    ) %>%
    mutate(Gf_Gift_code = as.factor(Gf_Gift_code)) %>%
    mutate(Gf_Amount = parse_number(Gf_Amount))
  ##removes misc Business Force labels
  final_df <- parsed_data %>%
    mutate(Gf_Gift_code = as.factor(case_when(
      Gf_Gift_code == "Business Force - New Jersey" ~ "Other",
      Gf_Gift_code == "Business Force - Los Angeles" ~ "Other",
      Gf_Gift_code == "Other - Foreign" ~ "Other",
      Gf_Gift_code == "KC" ~ "Other",
      .default = Gf_Gift_code
    )))
  final_df
}

