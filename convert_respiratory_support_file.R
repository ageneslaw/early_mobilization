library(readr)

df_temp <- read_delim("C:/Users/Andy/OneDrive - Columbia University Irving Medical Center/Research/data/early_mobilization/Report 8E - Mechanical Ventilation.txt",
                      delim = "|",
                      escape_double = FALSE,
                      col_types = cols(FSD_ID = col_number(),
                                       PAT_ENC_CSN_ID = col_character(),
                                       MRN = col_character(),
                                       FLOWSHEET_GROUP = col_character(),
                                       DISPLAY_NAME = col_character(),
                                       FLOWSHEET_NAME = col_character(),
                                       MEASURE_VALUE = col_character(),
                                       UNITS = col_character(),
                                       RECORDED_TIME = col_character()),
                      trim_ws = TRUE)

df_temp <- df_temp %>%
     select(PAT_ENC_CSN_ID,
            MEASUREMENT_NAME = DISPLAY_NAME ,
            FLOWSHEET_MEASURE_NAME = FLOWSHEET_NAME,
            MEASURE_VALUE,
            RECORDED_TIME)

write_delim(df_temp, paste0(data_path, '/Report 8E - Mechanical Ventilation_converted.txt'), delim = '|')
