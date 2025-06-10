source("model/run_systematic.R")
load("cached/data.manager_global_2025-04-14.Rdata")
load("data_manager/remainder_countries.Rdata")

LOCATIONS = DATA.MANAGER$incidence$LOCATIONS
income.table = (readxl::read_xlsx("data_manager/data/income/CLASS.xlsx",col_names = T))
income.table = income.table[1:218,c(1,4)]
names(income.table) = c("location","income")
income.table$location[income.table$location=="Bahamas, The"] = "Bahamas"
income.table$location[income.table$location=="Bolivia"] = "Bolivia (Plurinational State of)"
income.table$location[income.table$location=="Congo, Rep."] = "Congo"
income.table$location[income.table$location=="Congo, Dem. Rep."] = "Democratic Republic of the Congo"
income.table$location[income.table$location=="Côte d’Ivoire"] = "Côte d'Ivoire"
income.table$location[income.table$location=="Korea, Dem. People's Rep."] = "Democratic People's Republic of Korea"
income.table$location[income.table$location=="Korea, Rep."] = "Republic of Korea"
income.table$location[income.table$location=="Egypt, Arab Rep."] = "Egypt"
income.table$location[income.table$location=="Gambia, The"] = "Gambia"
income.table$location[income.table$location=="Iran, Islamic Rep."] = "Iran (Islamic Republic of)"
income.table$location[income.table$location=="Kyrgyz Republic"] = "Kyrgyzstan"
income.table$location[income.table$location=="Lao PDR"] = "Lao People's Democratic Republic"
income.table$location[income.table$location=="Moldova"] = "Republic of Moldova"
income.table$location[income.table$location=="St. Kitts and Nevis"] = "Saint Kitts and Nevis"
income.table$location[income.table$location=="St. Lucia"] = "Saint Lucia"
income.table$location[income.table$location=="Slovak Republic"] = "Slovakia"
income.table$location[income.table$location=="Venezuela, RB"] = "Venezuela (Bolivarian Republic of)"
income.table$location[income.table$location=="Vietnam"] = "Viet Nam"
income.table$location[income.table$location=="Yemen, Rep."] = "Yemen"

income.table$income[income.table$location=="Venezuela (Bolivarian Republic of)"] = "Upper middle income"

LOCATIONS.INCOME = unlist(sapply(LOCATIONS,function(x){
    income.table$income[income.table$location==x]
}))

LOCATIONS.INCOME = c(LOCATIONS.INCOME,"Sao Tome and Principe" = "Lower middle income")
# setdiff(LOCATIONS,names(LOCATIONS.INCOME))

LOW.INCOME = names(LOCATIONS.INCOME)[LOCATIONS.INCOME=="Low income"]
LOWER.MIDDLE.INCOME = names(LOCATIONS.INCOME)[LOCATIONS.INCOME=="Lower middle income"]
UPPER.MIDDLE.INCOME = names(LOCATIONS.INCOME)[LOCATIONS.INCOME=="Upper middle income"]
HIGH.INCOME = names(LOCATIONS.INCOME)[LOCATIONS.INCOME=="High income"]

LOW.REMAINDER = LOW.INCOME[LOW.INCOME %in% REMAINDER.COUNTRIES.UNAIDS]
LOWER.MIDDLE.REMAINDER = LOWER.MIDDLE.INCOME[LOWER.MIDDLE.INCOME %in% REMAINDER.COUNTRIES.UNAIDS]
UPPER.MIDDLE.REMAINDER = UPPER.MIDDLE.INCOME[UPPER.MIDDLE.INCOME %in% REMAINDER.COUNTRIES.UNAIDS]
HIGH.REMAINDER = HIGH.INCOME[HIGH.INCOME %in% REMAINDER.COUNTRIES.UNAIDS]

LOCATIONS.INCOME = list("full" = LOCATIONS.INCOME,
                        "low.remainder" = LOW.REMAINDER,
                        "lower.middle.remainder" = LOWER.MIDDLE.REMAINDER,
                        "upper.middle.remainder" = UPPER.MIDDLE.REMAINDER,
                        "high.remainder" = HIGH.REMAINDER,
                        "low.all" = LOW.INCOME,
                        "lower.middle.all" = LOWER.MIDDLE.INCOME,
                        "upper.middle.all" = UPPER.MIDDLE.INCOME,
                        "high.all" = HIGH.INCOME)

save(LOCATIONS.INCOME,file="data_manager/locations_income.Rdata")
