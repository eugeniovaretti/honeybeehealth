


###############################################################################.
####                                 GAM                                   ####
###############################################################################.

df <- read.csv("data/new_data/data_bystate_temp_perc.csv")
df_st <- df[df$state != "hawaii" & df$state != "other states",]

df_coord <- read.csv("data/state_coords_lon_lat.csv")
#remove hawaii:
df_coord <- df_coord[df_coord$state != "hawaii",]
#remove Na (for "other states"):
df_coord <- na.omit(df_coord)

df_new <- merge(df_st, df_coord, by = "state") 

df_new <- df_new %>% arrange(state, year, months)

#remove column "state"
df_new <- df_new[,-1]

t <- rep(1:29, 44)
df_new$t <- t #numerical variable for time (needed if we want to smooth the time)

df_new$year <- factor(df_new$year)
df_new$months <- factor(df_new$months)

#transform colony_lost_pct in values between 0 and 1:
df_binary <- df_new
#df_binary$colony_lost_pct <- df_binary$colony_lost_pct/100
cols <- c("colony_lost_pct", "Varroa.mites", "Other.pests.parasites", "Disesases", "Pesticides", "Other", "Unknown")
df_binary[cols] <- lapply(df_binary[cols], function(x) x/100)

gam_binary <- gam(colony_lost_pct ~ year + months + year:months + s(colony_max, bs='cr')
                  + s(I(lon * lat), bs = 'cr') + s(Varroa.mites, bs="cr") + s(Disesases, bs="cr")
                  + s(Pesticides, bs="cr") + + s(Unknown, bs="cr") + s(Other, bs="cr"),
                  family=binomial(link="logit"), data=df_binary) 

summary(gam_binary)
(r_2_squared <- summary(gam_binary)$r.sq)
summary(gam_binary)$s.pv

fun <- function(x) {(x - min(x)) / (max(x) - min(x))}
w <- fun(df_binary["colony_max"])
gam_thinplate_w <- gam(colony_lost_pct ~ year + months + year:months + 
                         + s(lon,lat, by=unlist(w), bs="tp"), family=binomial(link="logit"), data=df_binary) 
#weight the function depending on dimension of the state (supposed proportional to colony max)

gam_thinplate <- gam(colony_lost_pct ~ year + months + year:months + 
                       + s(lon,lat), data=df_binary) 
summary(gam_thinplate)


df_abs <- df_new
#convert stressors to absolute values:
df_abs$Varroa.mites <- df_binary$Varroa.mites*df_binary$colony_max
cols <- c("Varroa.mites", "Other.pests.parasites", "Disesases", "Pesticides", "Other", "Unknown")
df_abs[cols] <- sapply(df_binary[cols], '*', df_binary$colony_max)

gam_tensprod <- gam(colony_lost ~  colony_n + colony_added + colony_reno + s(Varroa.mites, bs="cr") + s(Disesases, bs="cr")
                    + s(Pesticides, bs="cr") + s(Unknown, bs="cr") + s(Other, bs="cr") + te(t,lon,lat), data=df_abs) #family=poisson
summary(gam_tensprod)

gam_tensprod_temp <- gam(colony_lost ~  colony_n + colony_added + colony_reno + s(Varroa.mites, bs="cr") +
                           + s(Pesticides, bs="cr") + te(t,lon,lat) + s(AverageTemperature, bs="cr")
                         + s(Precipitation, bs="cr"), data=df_abs) #family=poisson
summary(gam_tensprod_temp)

gam_tens_spacetime <- gam(colony_lost ~ colony_n + te(lon,lat,t, bs=c("tp","cr"), d=c(2,1), k=10), data=df_abs)
summary(gam_tens_spacetime)

## TRY WITH SHIFT

