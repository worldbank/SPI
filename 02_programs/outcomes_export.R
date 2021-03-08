#GDP
outcomes_df1 <- wbstats::wb_data(
  indicator=c('NY.GDP.PCAP.KD','SP.POP.TOTL'),
  start_date=2019,
  end_date=2019,
  return_wide = T
) %>%
  mutate(log_gdp=log10(NY.GDP.PCAP.KD)) %>%
  left_join(select(SPI,iso3c,date, region,SPI.INDEX)) %>%
  left_join(wbstats::wb_countries()) %>%
  select(country, iso3c, region, SPI.INDEX, SP.POP.TOTL,log_gdp, NY.GDP.PCAP.KD)

outcomes_df2 <- wbstats::wb_data(
  indicator=c('HD.HCI.OVRL'),
  start_date=2018,
  end_date=2018,
  return_wide = T
) %>%
  mutate(date=2019) %>%
  left_join(select(SPI,iso3c,date, region,SPI.INDEX)) %>%
  select(country, iso3c, SPI.INDEX, HD.HCI.OVRL)

outcomes_df3 <- wbstats::wb_data(
  indicator=c('GE.EST'),
  start_date=2019,
  end_date=2019,
  return_wide = T
) %>%
  left_join(select(SPI,iso3c,date, region,SPI.INDEX)) %>%
  select(country, iso3c, SPI.INDEX, GE.EST)

outcomes_df <- outcomes_df1 %>%
  left_join(outcomes_df2) %>%
  left_join(outcomes_df3)

write_excel_csv(outcomes_df, file=paste0(here(),"SPI_outcomes.csv"))