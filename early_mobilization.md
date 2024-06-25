# Early mobilization
Andrew Geneslaw

- [*Early mobilization retrospective
  analysis*](#early-mobilization-retrospective-analysis)
- [*Initial setup*](#initial-setup)
  - [*Loading libraries and data
    paths*](#loading-libraries-and-data-paths)
  - [*Loading patients, encounters and PICU start/stop
    times*](#loading-patients-encounters-and-picu-startstop-times)
  - [*Invasive mechanical ventilation
    (IMV)*](#invasive-mechanical-ventilation-imv)
  - [*Physical therapy*](#physical-therapy)
    - [*Amount of PT*](#amount-of-pt)

# *Early mobilization retrospective analysis*

*This project will assess the delivery of physical therapy to critically
ill children at the Morgan Stanley Children’s Hospital in New York City.
We are interested in whether physical therapy is being delivered within
72 hours of admission to the PICU. Physical therapy within 72 hours will
be referred to from here out as “Early mobilization” or EM.*

# *Initial setup*

## *Loading libraries and data paths*

``` r
library(chonyepicdata)
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(gtsummary)
library(kableExtra)

options(scipen = 3)

# Load configuration files. You may need to edit the file (located in a config folder) with your own filepath.
# Alternately you can just send in the correct filename.
load_config(yml_path = 'config/config.yml', useglobal = TRUE)
```

    <environment: R_GlobalEnv>

``` r
data_path <- data_path_chony
```

## *Loading patients, encounters and PICU start/stop times*

*A patient has a medical record number (MRN) which is persistent across
time.*

*A unique patient MRN can have multiple encounters. An encounter is
defined by the variable* *, which has been renamed* *for easier typing
throughout this project. A patient encounter is a distinct encounter
with the CHONY hospital system. An encounter can be inpatient,
outpatient, ED, virtual, or not even involve patient contact (such as an
attempt to call a patient that fails). For the purposes of this project,
all encounters are an inpatient hospitalization. All encounters will
have a start and stop date.*

*During a given hospital encounter (“admission”), a patient may move
between multiple inpatient locations. These involve floor locations,
PICUs, and operative areas. It also includes “virtual” areas, such as
changing locations to a radiology area during a portable x-ray. The
patient does not physically move in these circumstances and so we will
mostly ignore them.*

*Because a patient can move multiple times between floor and PICU
locations, a given hospital encounter might have multiple PICU
admissions. We will break hospital encounters into individual PICU
encounters, and each get a unique ID.*

``` r
# load all encounters
df_encounters <- load_encounters(paste0(data_path, fname_encounter))

# Remove any encounters that were for less than 24 hours
df_encounters <- df_encounters %>% 
     filter((hospital_discharge_date - hospital_admission_date)/ddays(1) >= 1)

enc_valid <- df_encounters %>% distinct(enc_id) %>% pull()

# Now get a set of all PICU start/stop datetimes for these encounters
df_picu_startstop <- get_picu_intervals(paste0(data_path, fname_adt)) %>%
     arrange(mrn, enc_id, icu_start_date)

# Remove encounters that were less than 24 hours
df_picu_startstop <- df_picu_startstop %>% 
     filter(enc_id %in% enc_valid)

# Remove PICU episodes that were less than 24 hours
df_picu_startstop <- df_picu_startstop %>% 
     filter((icu_stop_date - icu_start_date)/ddays(1) >= 1)

n_mrn <- distinct(df_encounters, mrn) %>% nrow()
n_encounters <- distinct(df_encounters, enc_id) %>% nrow()
n_picu <- distinct(df_picu_startstop, mrn, icu_start_date) %>% nrow() 
```

|         | *PICU* | *Encounters* | *Patients* |
|---------|--------|--------------|------------|
| ***n*** | *3026* | *3117*       | *2498*     |

*Since we are interested primarily in mobilization within the first 72
hours of ICU hospitalization, we should determine how many patients
stayed for this long.*

``` r
df_picu_stats <- df_picu_startstop %>% 
     mutate(los_picu = (icu_stop_date - icu_start_date)/ddays(1),
            los_picu_3days = if_else(los_picu >= 3, TRUE, FALSE))

df_encounter_stats <- df_encounters %>% 
     mutate(los_hosp = (hospital_discharge_date - hospital_admission_date)/ddays(1),
            los_hosp_3days = if_else(los_hosp >=3, TRUE, FALSE))

df_picu_stats %>% ggplot(aes(x = los_picu)) + 
     geom_histogram() + 
     scale_x_log10(n.breaks = 8) + 
     labs(x = 'Days (log10 axis)',
          y = 'N',
          title = 'PICU LOS')
```

![](early_mobilization_files/figure-commonmark/72%20hour%20LOS-1.png)

``` r
df_encounter_stats %>% ggplot(aes(x = los_hosp)) + 
     geom_histogram() + 
     scale_x_log10(n.breaks = 8) + 
     labs(x = 'Days (log10 axis)',
          y = 'N',
          title = 'Hospital LOS')
```

![](early_mobilization_files/figure-commonmark/72%20hour%20LOS-2.png)

``` r
kable(tabyl(df_picu_stats, los_picu_3days),
      format = "markdown", 
      align = "c",
      caption = "PICU admissions with PICU LOS at least 72 hours")
```

| los_picu_3days |  n   |  percent  |
|:--------------:|:----:|:---------:|
|     FALSE      | 1460 | 0.4824851 |
|      TRUE      | 1566 | 0.5175149 |

PICU admissions with PICU LOS at least 72 hours

``` r
kable(tabyl(df_encounter_stats, los_hosp_3days), 
      format = "markdown", 
      align = "c",
      caption = "Encounters with hospital LOS at least 72 hours")
```

| los_hosp_3days |  n   |  percent  |
|:--------------:|:----:|:---------:|
|     FALSE      | 651  | 0.2088547 |
|      TRUE      | 2466 | 0.7911453 |

Encounters with hospital LOS at least 72 hours

``` r
# df_stats <- left_join(df_picu_stats, df_encounter_stats)
```

## *Invasive mechanical ventilation (IMV)*

*Understanding the numbers of patients receiving IMV, and the timing of
IMV, is essential to the study.*

*First we will load data. This takes time, so we will separate it from
other processing so it can be cached.*

``` r
# Get ventilator data and just limit to patient encounters we are interested in
df_vent <- load_resp_support(paste0(data_path, fname_imv))
df_vent_wide <- clean_resp_support(df_vent)
df_resp_episodes <- classify_resp_support(df_vent_wide)
```

    [1] "Cleaning and arranging data by levels of support..."
    [1] "Finding changed levels of support..."
    [1] "Finding start/stop times of changed support levels..."
    [1] "Cleaning episodes shorter than [min_ep_duration] hours and stitching adjacent ones together.."

*Now, we will get some counts.*

``` r
# Only keep IMV data, and relabel each episode number
df_vent_episodes <- df_resp_episodes %>% filter(current_support == 'imv') %>%
     rename(vent_time_start = support_time_start,
            vent_time_stop = support_time_stop,
            vent_episode = support_episode) 

# Only keep vent episodes for at least 24 hours
df_vent_episodes <- df_vent_episodes %>% 
     filter(timediff >= hours(24)) %>% 
     group_by(enc_id) %>%
     arrange(vent_episode) %>%
     mutate(vent_episode = row_number()) %>%
     ungroup()

# Join based on encounter ID, and whether the intervals for (vent start, vent stop) and (icu start, icu stop) have any overlap. This ensures we include patients intubated in the ED or a procedural area
by <- join_by(enc_id, 
              overlaps(x$vent_time_start, x$vent_time_stop,
                       y$icu_start_date, y$icu_stop_date))

df_vent_episodes <- inner_join(df_vent_episodes, df_picu_startstop, by) %>%
     inner_join(df_encounters, by = c('mrn', 'enc_id')) %>% 
     relocate(mrn, enc_id)

# How many ventilator episodes, encounters, and among unique patients?
n_vent_ep <- df_vent_episodes %>% nrow()
n_vent_picu <- df_vent_episodes %>% distinct(icu_start_date) %>% nrow()
n_vent_mrn <- df_vent_episodes %>% distinct(mrn) %>% nrow()

display_array <- c(n_vent_ep, n_vent_picu, n_vent_mrn)
names(display_array) <- c('Episodes of IMV', 'PICU stays with IMV', 'Unique patients requiring IMV')

# display_table <- tibble(`Episodes of IMV` = n_vent_ep, 
       # `PICU stays with IMV` = n_vent_picu,
       # `Unique patients requiring IMV` = n_vent_mrn)

kable(display_array, 
      format = "markdown", 
      align = "c",
      caption = NULL,
      col.names = 'N')
```

|                               |  N  |
|:------------------------------|:---:|
| Episodes of IMV               | 836 |
| PICU stays with IMV           | 723 |
| Unique patients requiring IMV | 576 |

## *Physical therapy*

*Physical therapy is logged in flowsheets. For the most part, the
physical therapists log the total amount of time they worked. Some
physical therapists also log the specific activities they performed
(such as gait training, wheel chair training, etc). This may or may not
be consistent and will need some QAing.*

``` r
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
```

*There is a kind of variable called “PT received on” which is where the
PT entered the date they supposedly provided PT. However, they always
log PT when provided and there are multiple instances of the incorrect
date being logged (such as a date in the future). Because of this, we
will only use the date at the time PT was logged, rather than what the
therapist manually entered. If we do need to use it, the date is
recorded as the number of days after 12/31/1840 (unclear why). To
convert this to a date, you can use:*

``` r
mutate(pt_received_on = days(pt_received_on) + ymd('1840-12-31'))
```

*There are also many kinds flowsheet rows being documented on. Not all
are really relevant to our purposes. They are:*

``` r
# Print all flowsheet names
df_pt %>% distinct(flowsheet_name) %>% print(n=Inf) %>% 
     kable(format = 'markdown')
```

    # A tibble: 21 x 1
       flowsheet_name                                           
       <chr>                                                    
     1 therapeutic activity time entry                          
     2 ip pt time calculation                                   
     3 pt received on                                           
     4 pt re-evaluation time entry                              
     5 gait training time entry                                 
     6 neuromuscular re-education time entry                    
     7 self care/home management (adls) time entry              
     8 therapeutic exercise time entry                          
     9 therapeutic activity comments                            
    10 therapeutic exercise comments                            
    11 orthotic management training time entry                  
    12 gait training comments                                   
    13 cognitive function direct pt contact                     
    14 manual therapy time entry                                
    15 orthotic/prosthetic management and/or training time entry
    16 canalith repositioning time entry                        
    17 wheelchair management time entry                         
    18 pt eval treat tolerance                                  
    19 group therapy time entry                                 
    20 prosthetic training time entry                           
    21 therapeutic massage time entry                           

| flowsheet_name                                            |
|:----------------------------------------------------------|
| therapeutic activity time entry                           |
| ip pt time calculation                                    |
| pt received on                                            |
| pt re-evaluation time entry                               |
| gait training time entry                                  |
| neuromuscular re-education time entry                     |
| self care/home management (adls) time entry               |
| therapeutic exercise time entry                           |
| therapeutic activity comments                             |
| therapeutic exercise comments                             |
| orthotic management training time entry                   |
| gait training comments                                    |
| cognitive function direct pt contact                      |
| manual therapy time entry                                 |
| orthotic/prosthetic management and/or training time entry |
| canalith repositioning time entry                         |
| wheelchair management time entry                          |
| pt eval treat tolerance                                   |
| group therapy time entry                                  |
| prosthetic training time entry                            |
| therapeutic massage time entry                            |

*Most of these differentiate the amount of time spent on a given task.
It’s not always clear which of these are active versus passive in terms
of participation of the patient. Some also seem irrelevant. We will
dictate all of the relevant ones as:*

- *therapeutic activity time entry*

- *ip pt time calculation*

- *pt re-evaluation time entry*

- *gait training time entry*

- *neuromuscular re-education time entry*

- *self care/home management (adls) time entry*

- *therapeutic exercise time entry*

- *therapeutic activity comments*

- *therapeutic exercise comments*

- *gait training comments*

- *manual therapy time entry*

- *orthotic/prosthetic management and/or training time entry*

- *wheelchair management time entry*

- *pt eval treat tolerance*

- *prosthetic training time entry*

- *therapeutic massage time entry*

*(It might seem odd that we are keeping “comments.” The reason is that
in a subsequentstep, we enforce that only numeric values are retained.
Often the amount of time spent is erroneously entered as a “comment” so
we will keep these.)*

*In the next step, we save all of these types of PT as a wide format
data frame. Now we have access to each type of PT in case we need it for
later.*

*The variable `ip_pt_time_calculation` appears to be the total of all
other types of PT being provided. The other units specify the PT being
provided, which all sum to this variable. Therefore
`ip_pt_time_calculation` is the most important variable here and the one
we will use as the basis for how much PT is provided.*

``` r
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
     select(mrn, enc_id, recorded_time, pt_time = ip_pt_time_calculation) %>% 
     distinct()
```

### *Amount of PT*

*First we will describe, in general, how much PT is being delivered. We
need to make sure that we only sum up PT that was delivered after the
start of a PICU stay. To do this we take the `df_picu_startstop` data
frame, which contains ICU admit and discharge times. Create a few
windows of time spanning 0-24 hours, 24-48 hours, and 48-72 hours. Also
create an interval that spans 0-72 hours.*

``` r
# Update the PICU start/stop so that it has a timestamp and intervals
# for 24, 48, and 72 hours
df_temp <- df_picu_startstop %>%
     mutate(t24 = icu_start_date + hours(24),
            t48 = icu_start_date + hours(48),
            t72 = icu_start_date + hours(72),
            int0_72 = interval(icu_start_date, t72),
            int0_24 = interval(icu_start_date, t24),
            int24_48 = interval(t24, t48),
            int48_72 = interval(t48, t72)
            )
# Label the 1st hospitalization
df_temp <- df_temp %>% 
     group_by(mrn, enc_id) %>% 
     arrange(mrn, enc_id, icu_start_date) %>% 
     mutate(icu_stay = row_number()) %>% 
     ungroup()

# Create an inequality join so we make sure to only match PT that occurred after
# the PICU start
pt_join_by <- join_by(mrn, enc_id, x$icu_start_date < y$recorded_time)
df_pt_picu <- left_join(df_temp, df_pt_total, by = pt_join_by, relationship = "many-to-many") %>% 
     relocate(mrn, enc_id, icu_stay, icu_start_date, icu_stop_date, recorded_time, pt_time)

# Only keep 1st hospitalization
# df_pt_picu <- df_pt_picu %>% filter(icu_stay == 1)

# Determine whether each episode of PT fell in a given interval
df_pt_timing <- df_pt_picu %>%
     mutate(early_pt = if_else(recorded_time %within% int0_72, TRUE, FALSE, FALSE),
            any_pt = if_else(recorded_time > icu_start_date, TRUE, FALSE, FALSE),
            icu_pt = if_else(recorded_time %within% interval(icu_start_date, icu_stop_date), TRUE, FALSE, FALSE),
            pt0_24 = if_else(recorded_time  %within% int0_24, TRUE, FALSE, FALSE),
            pt24_48 = if_else(recorded_time %within% int24_48, TRUE, FALSE, FALSE),
            pt48_72 = if_else(recorded_time %within% int48_72, TRUE, FALSE, FALSE))

# Get counts of PT in each interval.
df_pt_counts <- df_pt_timing %>%  
     group_by(mrn, enc_id, icu_start_date) %>% 
     summarize(n_early_pt = sum(early_pt), 
               n_icu_pt = sum(icu_pt), 
               n_any_pt = sum(any_pt), 
               n_pt0_24 = sum(pt0_24), 
               n_pt24_48 = sum(pt24_48), 
               n_pt48_72 = sum(pt48_72),
               n_pt_each = sum(if_else(pt0_24 & pt24_48 & pt48_72, TRUE, FALSE)),
               .groups = 'drop') %>% 
     mutate(across(starts_with('n_'), ~ if_else(.x > 0, TRUE, FALSE)))

# Get aggregate time of PT delivered in each interval. Need to do this separately because they all have different denominators
df_early_pt_timesum <- df_pt_timing %>% 
     filter(early_pt) %>% 
     group_by(mrn, enc_id, icu_start_date) %>% 
     summarize(early_pt_time = sum(pt_time))

df_icu_pt_timesum <- df_pt_timing %>% 
     filter(icu_pt) %>% 
     group_by(mrn, enc_id, icu_start_date) %>% 
     summarize(icu_pt_time = sum(pt_time))

df_any_pt_timesum <- df_pt_timing %>% 
     filter(any_pt) %>% 
     group_by(mrn, enc_id, icu_start_date) %>% 
     summarize(any_pt_time = sum(pt_time))

# Bind the 3 times together. 
df_pt_timesums <- full_join(df_early_pt_timesum, df_icu_pt_timesum) %>% 
     full_join(df_any_pt_timesum)
```

Now let’s display some findings. First, display counts of ICU stays
involving PT at varying time points. Note that that the sum of PT in the
0-24, 24-48, and 42-72 hour ranges can sum to more than the number of
patients who received early PT (0-72 hours). This is because a patient
could receive PT multiple times in the early range, e.g. at 0-24 and
24-48 hours. They would be counted in each range, but only once total
for the 0-72 hour range.

``` r
# Now display tables of each.
df_pt_counts %>% 
     tbl_summary(include = starts_with('n_'),
                 label = list(n_early_pt ~ 'Early PT (within 72 hours)',
                              n_icu_pt ~ 'PT any time in the ICU',
                              n_any_pt ~ 'PT any time in the hospital',
                              n_pt0_24 ~ 'PT within 0-24 hours',
                              n_pt24_48 ~ 'PT within 24-48 hours',
                              n_pt48_72 ~ 'PT within 48-72 hours',
                              n_pt_each ~ 'PT days 0, 1, and 2'))
```

<div>

<div id="zgtlutoeun" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zgtlutoeun table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#zgtlutoeun thead, #zgtlutoeun tbody, #zgtlutoeun tfoot, #zgtlutoeun tr, #zgtlutoeun td, #zgtlutoeun th {
  border-style: none;
}
&#10;#zgtlutoeun p {
  margin: 0;
  padding: 0;
}
&#10;#zgtlutoeun .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#zgtlutoeun .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#zgtlutoeun .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#zgtlutoeun .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#zgtlutoeun .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#zgtlutoeun .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zgtlutoeun .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#zgtlutoeun .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#zgtlutoeun .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#zgtlutoeun .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#zgtlutoeun .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#zgtlutoeun .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#zgtlutoeun .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#zgtlutoeun .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#zgtlutoeun .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#zgtlutoeun .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#zgtlutoeun .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#zgtlutoeun .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#zgtlutoeun .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zgtlutoeun .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#zgtlutoeun .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#zgtlutoeun .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#zgtlutoeun .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zgtlutoeun .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#zgtlutoeun .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#zgtlutoeun .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zgtlutoeun .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zgtlutoeun .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#zgtlutoeun .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#zgtlutoeun .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#zgtlutoeun .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zgtlutoeun .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#zgtlutoeun .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zgtlutoeun .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#zgtlutoeun .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zgtlutoeun .gt_left {
  text-align: left;
}
&#10;#zgtlutoeun .gt_center {
  text-align: center;
}
&#10;#zgtlutoeun .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#zgtlutoeun .gt_font_normal {
  font-weight: normal;
}
&#10;#zgtlutoeun .gt_font_bold {
  font-weight: bold;
}
&#10;#zgtlutoeun .gt_font_italic {
  font-style: italic;
}
&#10;#zgtlutoeun .gt_super {
  font-size: 65%;
}
&#10;#zgtlutoeun .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#zgtlutoeun .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#zgtlutoeun .gt_indent_1 {
  text-indent: 5px;
}
&#10;#zgtlutoeun .gt_indent_2 {
  text-indent: 10px;
}
&#10;#zgtlutoeun .gt_indent_3 {
  text-indent: 15px;
}
&#10;#zgtlutoeun .gt_indent_4 {
  text-indent: 20px;
}
&#10;#zgtlutoeun .gt_indent_5 {
  text-indent: 25px;
}
</style>

| **Characteristic**                                                                                                         | **N = 3,026**<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> |
|----------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|
| Early PT (within 72 hours)                                                                                                 | 1,485 (49%)                                                                                                                       |
| PT any time in the ICU                                                                                                     | 1,877 (62%)                                                                                                                       |
| PT any time in the hospital                                                                                                | 2,370 (78%)                                                                                                                       |
| PT within 0-24 hours                                                                                                       | 590 (19%)                                                                                                                         |
| PT within 24-48 hours                                                                                                      | 852 (28%)                                                                                                                         |
| PT within 48-72 hours                                                                                                      | 892 (29%)                                                                                                                         |
| PT days 0, 1, and 2                                                                                                        | 0 (0%)                                                                                                                            |
| <span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%) |                                                                                                                                   |

</div>

</div>

Next, display the amount of PT (in hours) that was received.

``` r
df_pt_timesums %>% 
     tbl_summary(include = ends_with('_time'),
                 label = list(early_pt_time ~ 'Duration of PT within first 72 hours, minutes',
                              icu_pt_time ~ 'Duration of PT within ICU, minutes',
                              any_pt_time ~ 'Duration of PT within the hospital, minutes'),
                 missing = 'no',
                 statistic = list(all_continuous() ~ '{median} [{p25}-{p75}] - {N_nonmiss} patients'))
```

<div>

<div id="qhyjfxeget" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#qhyjfxeget table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#qhyjfxeget thead, #qhyjfxeget tbody, #qhyjfxeget tfoot, #qhyjfxeget tr, #qhyjfxeget td, #qhyjfxeget th {
  border-style: none;
}
&#10;#qhyjfxeget p {
  margin: 0;
  padding: 0;
}
&#10;#qhyjfxeget .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#qhyjfxeget .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#qhyjfxeget .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#qhyjfxeget .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#qhyjfxeget .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#qhyjfxeget .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qhyjfxeget .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#qhyjfxeget .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#qhyjfxeget .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#qhyjfxeget .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#qhyjfxeget .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#qhyjfxeget .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#qhyjfxeget .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#qhyjfxeget .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#qhyjfxeget .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#qhyjfxeget .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#qhyjfxeget .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#qhyjfxeget .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#qhyjfxeget .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qhyjfxeget .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#qhyjfxeget .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#qhyjfxeget .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#qhyjfxeget .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qhyjfxeget .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#qhyjfxeget .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#qhyjfxeget .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qhyjfxeget .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qhyjfxeget .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#qhyjfxeget .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#qhyjfxeget .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#qhyjfxeget .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qhyjfxeget .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#qhyjfxeget .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qhyjfxeget .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#qhyjfxeget .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qhyjfxeget .gt_left {
  text-align: left;
}
&#10;#qhyjfxeget .gt_center {
  text-align: center;
}
&#10;#qhyjfxeget .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#qhyjfxeget .gt_font_normal {
  font-weight: normal;
}
&#10;#qhyjfxeget .gt_font_bold {
  font-weight: bold;
}
&#10;#qhyjfxeget .gt_font_italic {
  font-style: italic;
}
&#10;#qhyjfxeget .gt_super {
  font-size: 65%;
}
&#10;#qhyjfxeget .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#qhyjfxeget .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#qhyjfxeget .gt_indent_1 {
  text-indent: 5px;
}
&#10;#qhyjfxeget .gt_indent_2 {
  text-indent: 10px;
}
&#10;#qhyjfxeget .gt_indent_3 {
  text-indent: 15px;
}
&#10;#qhyjfxeget .gt_indent_4 {
  text-indent: 20px;
}
&#10;#qhyjfxeget .gt_indent_5 {
  text-indent: 25px;
}
</style>

| **Characteristic**                                                                                                                                   | **N = 2,370**<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> |
|------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|
| Duration of PT within first 72 hours, minutes                                                                                                        | 34 \[25-70\] - 1,485 patients                                                                                                     |
| Duration of PT within ICU, minutes                                                                                                                   | 55 \[25-132\] - 1,876 patients                                                                                                    |
| Duration of PT within the hospital, minutes                                                                                                          | 119 \[48-299\] - 2,367 patients                                                                                                   |
| <span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Median \[25%-75%\] - N patients |                                                                                                                                   |

</div>

</div>

Not used

``` r
# tabyl(df_early_pt_firstpicu, early_pt)
# 
# # Now try to sum all the active types of PT
# df_pt_active <- df_pt_wide %>%
#      mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
#      mutate(pt_active_time = gait_training_time_entry + therapeutic_exercise_time_entry + wheelchair_management_time_entry +
#                  neuromuscular_re_education_time_entry + therapeutic_exercise_comments + gait_training_comments +
#                  orthotic_prosthetic_management_and_or_training_time_entry + prosthetic_training_time_entry) %>%
#      ungroup() %>%
#      select(mrn, enc_id, recorded_time, pt_active_time) %>%
#      filter(pt_active_time > 0)
# 
# # Do the same kind of inequality join as before, ensuring the PT is after PICU start
# df_pt_active_picu <- left_join(df_temp, df_pt_active, by = pt_join_by, relationship = "many-to-many")
# 
# # Determine whether patients had PT in each of the intervals
# df_early_pt_active <- df_pt_active_picu %>%
#      mutate(early_pt_active = if_else(recorded_time %within% int0_72, TRUE, FALSE)) %>%
#      group_by(mrn, enc_id, icu_start_date) %>%
#      filter(early_pt_active == max(early_pt_active)) %>%
#      ungroup() %>%
#      distinct(mrn, enc_id, icu_start_date, early_pt_active)
# 
# tabyl(df_early_pt_active, early_pt_active)
```
