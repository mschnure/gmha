sorted.countries = sort(DATA.MANAGER$prevalence$year.location["2022",],decreasing = T)
sorted.countries = data.frame("prevalence" = sorted.countries,
                              "percent" = round(sorted.countries/sum(sorted.countries),3))
sorted.countries$cumulative.percent = cumsum(sorted.countries$percent)
head(sorted.countries,20)


sorted.countries.2 = sort(DATA.MANAGER$prevalence$year.location["2023",],decreasing = T)
sorted.countries.2 = data.frame("prevalence" = sorted.countries.2,
                              "percent" = round(sorted.countries.2/39900000,3))
sorted.countries.2$cumulative.percent = cumsum(sorted.countries.2$percent)
head(sorted.countries.2,20)

sorted.countries.2[rownames(sorted.countries.2) %in% c("Cambodia","Chile","France","Netherlands"),]
