library(RAQSAPI)
library("keyring") 
library(dplyr)
library(dbplyr)
# https://aqs.epa.gov/aqsweb/documents/data_mart_welcome

# Your user ID is your email address: luca1.mainini@mail.polimi.it 
# Your key is: copperheron34

## inserisci key dopo aver runnato la prossima riga
keyring::key_set(service = "AQSDatamart", username = "luca1.mainini@mail.polimi.it")

datamartAPI_user <- "luca1.mainini@mail.polimi.it" 
server <- "AQSDatamart"
aqs_credentials(username = datamartAPI_user,
                key = key_get(service = server,
                              username = datamartAPI_user
                )
)

# returns a tibble of all benzene annualy
# North Carolina = 37
## parameter = 45201 #benzene

#extract US STATES
library("tidycensus")
data(fips_codes)
us_codes <- unique(fips_codes$state_code)
  
data_extr <- data.frame()
for (state in us_codes[us_codes>=22]){
  print(paste("---- downloading state",state))
  temp <- aqs_quarterlysummary_by_state(parameter = "88101",
                                          bdate = as.Date("20150101",
                                                          format="%Y%m%d"
                                          ),
                                          edate = as.Date("20221231",
                                                          format = "%Y%m%d"
                                          ),
                                          stateFIPS = state
  )
  data <- temp %>% select(-actual_days_gt_std, -tribal_code, -tribal_land, -estimated_days_gt_std  # removing null columns 
  )%>% filter (pollutant_standard == 'PM25 24-hour 2012'
  ) %>% filter (datum == "WGS84" & quarterly_criteria_met=='Y' & poc=="2"
  ) %>% group_by(state_code,year,quarter
  ) %>% summarise( media = mean(arithmetic_mean), min = min(minimum_value), max = max(maximum_value)) 
  
  data_extr <- rbind(data_extr,data)
} 
View(data_extr)
 
## --------PROVA - by step --------
# direttamente per semestre parameter = "88101" : PM 2.5, stato 37
temp <- aqs_quarterlysummary_by_state(parameter = "88101",
                                      bdate = as.Date("20160101",
                                                      format = "%Y%m%d"),
                                      edate = as.Date("20161231",
                                                      format = "%Y%m%d"),
                                      stateFIPS = "37"
)

data <- temp %>% select(-actual_days_gt_std, -tribal_code, -tribal_land, -estimated_days_gt_std  # removing null columns 
                        )%>% filter (pollutant_standard == 'PM25 24-hour 2012'
                        ) %>% filter (datum == "WGS84" & quarterly_criteria_met=='Y' & poc=="2"
                                      ) %>% group_by(state_code,quarter
                                                     ) %>% summarise( media = mean(arithmetic_mean), min = min(minimum_value), max = max(maximum_value)) 

res1 <- data %>% filter (pollutant_standard == 'PM25 24-hour 2012'
) %>% filter (datum == "WGS84" & quarterly_criteria_met=='Y' & site_number=="0030")

                