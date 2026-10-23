## NEXTGEN: South Africa 15-24 cascade calibration targets from the national household surveys (SABSSM V 2017, SABSSM VI 2022)
## Data: data_manager/data/sabssm_youth_cascade/sabssm_15-24_cascade_south_africa.csv
##
## Stored with the SAME definitions as the UNAIDS 95-95-95 cascade data already in the data manager:
##   awareness   = aware of status / all PLHIV                      (survey 1st 90/95)
##   engagement  = on ART / aware                                    (survey 2nd 90/95)
##   suppression = (VLS <1000 among on ART) x (on ART / aware)       (survey 3rd 90/95 x 2nd; denominator = aware,
##                 lowers = lower x engagement lower, uppers = upper x engagement upper - same as UNAIDS in
##                 read.surveillance.data)
## Survey definitions: aware = tested HIV+ and reported knowing status OR ARVs detected in blood; on ART = ARVs
## detected in blood (2022: self-report where no ARV result); VLS = <1000 copies/mL.
##
## 2017 (Marinda et al. 2020, Table 1): published 95% CIs, total and by sex.
## 2022 (SABSSM VI summary report, p23): point estimates only, total only (no sex-specific youth cascade published).
##   95% CIs derived on the logit scale from n with a design effect of 3.15, taken from the published 2022 15-24 VLS
##   estimate (70.1%, 63.3-76.1, n=614). n's: aware = 614 (15-24 PLHIV with VL result); on ART = 614 x 0.731;
##   VLS = 614 x 0.731 x 0.874.
##
## METHOD (10/23/26): "ratio" (default) - youth targets = survey youth-to-adult ratio x UNAIDS 15+ value, same year
##   (and sex), so the survey's definitions cancel out and the targets are on the UNAIDS scale used for 15+:
##     aware(15-24)          = UNAIDS aware(15+)          x survey aware(15-24) / survey aware(15+)
##     on ART/aware(15-24)   = UNAIDS on ART/aware(15+)   x survey ratio
##     VLS/on ART(15-24)     = UNAIDS VLS/on ART(15+)     x survey ratio   (then x on ART/aware, as above)
##   bounds: the survey 15-24 bounds scaled by the same factor (keeps the survey's relative uncertainty); capped at 1.
##   Adult survey values: data_manager/data/sabssm_youth_cascade/sabssm_15plus_cascade_south_africa.csv
##   (2022 adult by sex is given, but there is no 2022 youth by sex, so 2022 is total only).
## "direct" - the survey 15-24 values as they are (what the 10/22 calibration used).
##
## Adds age "15-24" to the awareness/engagement/suppression (+ .lowers/.uppers) year.age.location and
## year.age.sex.location arrays, for South Africa only (NA elsewhere, so other locations' likelihoods are unchanged).

add.age.to.array = function(arr, new.age, new.years = NULL){
    dn = dimnames(arr)
    if(!is.null(new.years))
        dn$year = as.character(sort(as.numeric(union(dn$year, new.years))))
    if(!(new.age %in% dn$age))
        dn$age = c(dn$age, new.age)
    rv = array(NA_real_, dim = sapply(dn, length), dimnames = dn)
    old.dn = dimnames(arr)
    # copy old values in
    idx = lapply(names(dn), function(d) match(old.dn[[d]], dn[[d]]))
    rv = do.call(`[<-`, c(list(rv), idx, list(value = arr)))
    rv
}

add.sabssm.youth.cascade = function(dm,
                                    file = "data_manager/data/sabssm_youth_cascade/sabssm_15-24_cascade_south_africa.csv",
                                    adult.file = "data_manager/data/sabssm_youth_cascade/sabssm_15plus_cascade_south_africa.csv",
                                    method = c("ratio","direct"),
                                    location = "South Africa",
                                    age = "15-24"){
    method = match.arg(method)
    df = read.csv(file, stringsAsFactors = F); df$year = as.character(df$year)
    adult = read.csv(adult.file, stringsAsFactors = F); adult$year = as.character(adult$year)
    years = unique(df$year)

    survey = function(d, yr, sx, ind, col = "estimate"){
        x = d[d$year==yr & d$sex==sx & d$indicator==ind, col]
        if(length(x)==0 || is.na(x)) NA_real_ else x/100
    }
    # UNAIDS 15+ on the indicator scale (read before any 15-24 values are added)
    unaids = function(d, yr, sx){
        get = function(type){
            a = if(sx=="all") dm[[type]]$year.age.location else dm[[type]]$year.age.sex.location
            if(!(yr %in% dimnames(a)$year)) return(NA_real_)
            if(sx=="all") a[yr, "15+", location] else a[yr, "15+", sx, location]
        }
        switch(d, aware = get("awareness"), art_among_aware = get("engagement"),
               vls_among_art = get("suppression")/get("engagement")) # suppression is stored among aware
    }
    target = function(yr, sx, ind, col){
        y = survey(df, yr, sx, ind, col)
        if(method == "direct") return(y)
        pmin(1, y * unaids(ind, yr, sx) / survey(adult, yr, sx, ind))
    }

    # table of the targets that go in (indicator scale), for checking
    tab = expand.grid(year = years, sex = c("all","male","female"), indicator = c("aware","art_among_aware","vls_among_art"),
                      stringsAsFactors = F)
    for(col in c("estimate","lower","upper"))
        tab[[col]] = mapply(target, tab$year, tab$sex, tab$indicator, col)
    tab$unaids.15plus = mapply(unaids, tab$indicator, tab$year, tab$sex)
    tab$survey.15.24 = mapply(function(yr, sx, ind) survey(df, yr, sx, ind), tab$year, tab$sex, tab$indicator)
    tab$survey.15plus = mapply(function(yr, sx, ind) survey(adult, yr, sx, ind), tab$year, tab$sex, tab$indicator)
    tab = tab[!is.na(tab$estimate), ]

    data.types = c("awareness","engagement","suppression")
    suffixes = c("" , ".lowers", ".uppers")
    cols = c("estimate","lower","upper")
    for(d in data.types){
        for(s in seq_along(suffixes)){
            name = paste0(d, suffixes[s])
            col = cols[s]
            dm[[name]]$year.age.location = add.age.to.array(dm[[name]]$year.age.location, age, years)
            dm[[name]]$year.age.sex.location = add.age.to.array(dm[[name]]$year.age.sex.location, age, years)
            for(yr in years){
                for(sx in c("all","male","female")){
                    val = switch(d,
                                 awareness = target(yr, sx, "aware", col),
                                 engagement = target(yr, sx, "art_among_aware", col),
                                 suppression = target(yr, sx, "vls_among_art", col) * target(yr, sx, "art_among_aware", col))
                    if(sx=="all")
                        dm[[name]]$year.age.location[yr, age, location] = val
                    else
                        dm[[name]]$year.age.sex.location[yr, age, sx, location] = val
                }
            }
            if(!(age %in% dm[[name]]$AGES)){
                dm[[name]]$AGES = c(dm[[name]]$AGES, age)
                dm[[name]]$AGE.LOWERS = c(dm[[name]]$AGE.LOWERS, 15)
                dm[[name]]$AGE.UPPERS = c(dm[[name]]$AGE.UPPERS, 25)
            }
        }
    }
    print(paste0("SABSSM 15-24 targets added (method = ", method, "), indicator scale:"))
    print(tab, digits = 3, row.names = F)
    attr(dm, "sabssm.youth.cascade") = TRUE
    attr(dm, "sabssm.youth.cascade.method") = method
    attr(dm, "sabssm.youth.cascade.targets") = tab
    dm
}
