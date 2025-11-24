study_df_ihsn %>%
  mutate(topic=case_when(
    grepl('Population|Housing|Censuses',series_info) ~ 'Population and Housing Censuses',
    grepl('lfs',series_info) ~ 'Labor Force Surveys',
    grepl('dhs|DHS|mics|MICS|whs|hea',series_info) ~ 'Health Surveys',
    grepl('ies',series_info) ~ 'Income/Expenditure Surveys',
    grepl('sems',series_info) ~ 'Socio/Economic Monitoring Surveys',
    grepl('lsms', series_info) ~ 'Living Standards Measurement Study',
    grepl('ag', series_info) ~ 'Agriculture Survey or Census',
    grepl('en', series_info) ~ 'Enterprise Survey',
    grepl('oth',series_info) ~ 'Other Household Surveys',
    grepl('Opinion', series_info) ~ 'Opinion Survey',
    TRUE ~ 'Other'
  ), 
  DHS = case_when(
    #grepl('Population|Housing|Censuses',series_info) ~ 'Population and Housing Censuses',
    #grepl('lfs',series_info) ~ 'Labor Force Surveys',
    grepl('dhs|DHS',series_info) ~ 1,
    #grepl('ies',series_info) ~ 'Income/Expenditure Surveys',
    #grepl('sems',series_info) ~ 'Socio/Economic Monitoring Surveys',
    #grepl('lsms', series_info) ~ 'Living Standards Measurement Study',
    #grepl('ag', series_info) ~ 'Agriculture Survey or Census',
    #grepl('en', series_info) ~ 'Enterprise Survey',
    #grepl('oth',series_info) ~ 'Other Household Surveys',
    #grepl('Opinion', series_info) ~ 'Opinion Survey',
    TRUE ~ 0
  )) |> 
  filter(DHS == 1) |> 
  select(nation, year_end) |> 
  unique() |> 
  arrange(-year_end) |> 
  filter(year_end > 2016)





