library(RAQSAPI)
library("keyring") 
keyring::key_set(service = "AQSDatamart", username = "luca1.mainini@mail.polimi.it")

## credenziali
# Your user ID is your email address: luca1.mainini@mail.polimi.it 
# Your key is: copperheron34 

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
data_extr <- aqs_annualsummary_by_state(parameter = "45201",
bdate = as.Date("19950515",
                format="%Y%m%d"
),
edate = as.Date("19950515",
                format = "%Y%m%d"
),
stateFIPS = "37"
)

# direttamente per semestre
temp <- aqs_quarterlysummary_by_state(parameter = "88101",
                                      bdate = as.Date("20160101",
                                                      format = "%Y%m%d"),
                                      edate = as.Date("20170228",
                                                      format = "%Y%m%d"),
                                      stateFIPS = "37"
)

                