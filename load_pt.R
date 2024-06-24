library(readr)
library(tidyverse)
library(janitor)

df_pt <- read_delim(paste0(data_path, '/Report 8F - Physical Therapy.txt'),
                    delim = "|",
                    escape_double = FALSE,
                    col_types = cols(PAT_ENC_CSN_ID = col_character(),
                                     MRN = col_character(),
                                     FLOWSHEET_NAME = col_character(),
                                     MEASURE_VALUE = col_number(),
                                     RECORDED_TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S")),
                    trim_ws = TRUE) %>%
     clean_names() %>%
     select(mrn, enc_id = pat_enc_csn_id, everything()) %>%
     mutate(across(where(is.character), str_to_lower)) %>%
     mutate(across(where(is.character), ~ str_remove_all(.x, '^r nyc '))) %>%
     mutate(across(where(is.character), ~ str_remove_all(.x, '^r rhb '))) %>%
     mutate(across(where(is.character), ~ str_remove_all(.x, '^rhb ')))

# Print all flowsheet names
df_pt %>% distinct(flowsheet_name) %>% print(n=Inf)

# There is a kind of variable called "PT received on" which is where the PT
# entered the date they supposedly provided PT. However, they always log PT
# when provided and there are multiple instances of the incorrect date being
# logged (such as a date in the future). Because of this, we will only use the
# date at the time PT was logged, rather than what the therapist manually entered.
# If we do need to use it, the date is recorded as the number of days after
# 12/31/1840 (unclear why). To convert to a date, you can use:
#     mutate(pt_received_on = days(pt_received_on) + ymd('1840-12-31'))

# The variable "IP PT TIME CALCULATION" appears to be the total of all other
# types of PT being provided. The other units specify the PT being provided.

# Relevant types of PT rows...
relevant_pt <- c('therapeutic activity time entry',
                 'ip pt time calculation',
                 # 'pt received on',
                 'pt re-evaluation time entry',
                 'gait training time entry',
                 'neuromuscular re-education time entry',
                 'self care/home management (adls) time entry',
                 'therapeutic exercise time entry',
                 'therapeutic activity comments',
                 'therapeutic exercise comments',
                 # 'orthotic management training time entry',
                  'gait training comments',
                 # 'cognitive function direct pt contact',
                 'manual therapy time entry',
                 'orthotic/prosthetic management and/or training time entry',
                 # 'canalith repositioning time entry',
                 'wheelchair management time entry',
                 'pt eval treat tolerance',
                 # 'group therapy time entry',
                 'prosthetic training time entry',
                 'therapeutic massage time entry'
                 )
relevant_pt_str <- str_flatten(relevant_pt, collapse = '|')

# Keep only relevant kinds of PT
df_pt <- df_pt %>% filter(str_detect(flowsheet_name, relevant_pt_str))

# Make sure all types are numeric
df_pt <- df_pt %>% mutate(measure_value = as.numeric(measure_value))

# Remove any 0-time units
df_pt <- df_pt %>% filter(measure_value > 0)


# Convert to wide format
df_pt_wide <- df_pt %>%
     pivot_wider(id_cols = c('mrn', 'enc_id', 'recorded_time'),
                 names_from = flowsheet_name,
                 values_from = measure_value) %>%
     clean_names()

# Just keep a version with total time
df_pt_total <- df_pt_wide %>%
     select(mrn, enc_id, recorded_time, pt_time = ip_pt_time_calculation)

# Update the PICU start/stop so that it has a timestamp and intervals
# for 24, 48, and 72 hours
df_temp <- df_picu_startstop %>%
     mutate(t24 = icu_start_date + hours(24),
            int0_24 = interval(icu_start_date, t24),
            t48 = icu_start_date + hours(48),
            int24_48 = interval(t24 + minutes(1), t48),
            t72 = icu_start_date + hours(72),
            int48_72 = interval(t48 + minutes(1), t72),
            int0_72 = interval(icu_start_date, t72)
            )

# Create an inequality join so we make sure to only match PT that occurred after
# the PICU start
pt_join_by <- join_by(mrn, enc_id, y$recorded_time > x$icu_start_date)
df_pt_picu <- left_join(df_temp, df_pt_total, by = pt_join_by, relationship = "many-to-many")

# Determine whether patients had PT in each of the intervals
df_early_pt <- df_pt_picu %>%
     mutate(early_pt = if_else(recorded_time %within% int0_72, TRUE, FALSE)) %>%
     group_by(mrn, enc_id, icu_start_date) %>%
     filter(early_pt == max(early_pt)) %>%
     ungroup() %>%
     distinct(mrn, enc_id, icu_start_date, early_pt)

tabyl(df_early_pt, early_pt)

# What if we only look at the first PICU hospitalization?
df_early_pt_firstpicu <- df_pt_picu %>%
     mutate(early_pt = if_else(recorded_time %within% int0_72, TRUE, FALSE)) %>%
     group_by(mrn, enc_id) %>%
     filter(icu_start_date == min(icu_start_date)) %>%
     filter(early_pt == max(early_pt)) %>%
     ungroup() %>%
     distinct(mrn, enc_id, icu_start_date, early_pt)

tabyl(df_early_pt_firstpicu, early_pt)

# Now try to sum all the active types of PT
df_pt_active <- df_pt_wide %>%
     mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
     mutate(pt_active_time = gait_training_time_entry + therapeutic_exercise_time_entry + wheelchair_management_time_entry +
                 neuromuscular_re_education_time_entry + therapeutic_exercise_comments + gait_training_comments +
                 orthotic_prosthetic_management_and_or_training_time_entry + prosthetic_training_time_entry) %>%
     ungroup() %>%
     select(mrn, enc_id, recorded_time, pt_active_time) %>%
     filter(pt_active_time > 0)

# Do the same kind of inequality join as before, ensuring the PT is after PICU start
df_pt_active_picu <- left_join(df_temp, df_pt_active, by = pt_join_by, relationship = "many-to-many")

# Determine whether patients had PT in each of the intervals
df_early_pt_active <- df_pt_active_picu %>%
     mutate(early_pt_active = if_else(recorded_time %within% int0_72, TRUE, FALSE)) %>%
     group_by(mrn, enc_id, icu_start_date) %>%
     filter(early_pt_active == max(early_pt_active)) %>%
     ungroup() %>%
     distinct(mrn, enc_id, icu_start_date, early_pt_active)

tabyl(df_early_pt_active, early_pt_active)
