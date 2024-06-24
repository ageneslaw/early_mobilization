library(readr)
library(tidyverse)
library(ggplot2)
library(janitor)

df_icd <- read_delim("C:/Users/asg2195/OneDrive - cumc.columbia.edu/Research/data/early_mobilization/Report 3 - ICD-10-CM Diagnosis Codes.txt",
                     delim = "|", escape_double = FALSE, col_types = cols(PAT_ENC_CSN_ID = col_character(),
                                                                          DX_DATE = col_date(format = "%Y-%m-%d %H:%M:%S")),
                     trim_ws = TRUE) %>%
     clean_names() %>%
     rename(enc_id = pat_enc_csn_id)

df_icd <- df_icd %>%
     separate_longer_delim(cols = icd10, delim = ',')

# Merge with PICU dates
df_picu_startstop_round <- df_picu_startstop %>%
     mutate(icu_start_date = floor_date(icu_start_date, unit = 'day'),
            icu_stop_date = floor_date(icu_stop_date, unit = 'day'),
            icu_first_24 = icu_start_date + days(1))

by = join_by(enc_id, between(dx_date, icu_start_date, icu_first_24, bounds = '[]'))
df_icd_picu <- inner_join(df_icd, df_picu_startstop_round, relationship = 'many-to-many')

df_icd_picu <- df_icd_picu %>%
     group_by(enc_id,
              icd10) %>%
     filter(dx_date == min(dx_date)) %>%
     slice(1) %>%
     ungroup() %>%
     arrange(enc_id, dx_date, primary_dx_yn)

df_icd_counts <- df_icd %>%
     add_count(icd10) %>%
     arrange(desc(n)) %>%
     distinct(icd10, .keep_all = TRUE) %>%
     slice_head(n=20)

df_icd_problem_primary <- df_icd %>%
     filter(dx_type == 'Hospital Problem List' & primary_dx_yn == 'Y') %>%
     add_count(icd10) %>%
     arrange(desc(n)) %>%
     distinct(icd10, .keep_all = TRUE) %>%
     slice_head(n=20)

df_icd_any_primary <- df_icd %>%
     filter(primary_dx_yn == 'Y') %>%
     add_count(icd10) %>%
     arrange(desc(n)) %>%
     distinct(icd10, .keep_all = TRUE) %>%
     slice_head(n=20)

df_icd %>% ggplot(aes(y = ))
