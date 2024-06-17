library(readr)
library(tidyverse)
library(janitor)

df_pt <- read_delim("C:/Users/asg2195/OneDrive - cumc.columbia.edu/Research/data/early_mobilization/Report 8F - Physical Therapy.txt",
                                         delim = "|", escape_double = FALSE, col_types = cols(PAT_ENC_CSN_ID = col_character(),
                                                                                              MRN = col_character(), FLOWSHEET_NAME = col_character(),
                                                                                              MEASURE_VALUE = col_number(), RECORDED_TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S")),
                                         trim_ws = TRUE) %>%
     clean_names()
