
# incidence and prevalence have the same points
incidence.total.points = c(1990:2023)
incidence.age.points = rep(1990:2023,7)
incidence.age.sex.points = rep(c(2005,2010,2015,2017),2)
incidence.all.points = c(incidence.total.points,incidence.age.points,incidence.age.sex.points)
incidence.pre.2010 = length(incidence.all.points[incidence.all.points<2010])
incidence.2010.to.2018 = length(incidence.all.points[incidence.all.points>=2010 & incidence.all.points<2018])
incidence.2018.and.later = length(incidence.all.points[incidence.all.points>=2018])

# awareness and engagement have the same points
awareness.total.points = 2015:2023
awareness.age.sex.points = rep(c(2015:2023),2)
awareness.all.points = c(awareness.total.points,awareness.age.sex.points)
awareness.pre.2010 = length(awareness.all.points[awareness.all.points<2010])
awareness.2010.to.2018 = length(awareness.all.points[awareness.all.points>=2010 & awareness.all.points<2018])
awareness.2018.and.later = length(awareness.all.points[awareness.all.points>=2018])

# suppression has its own # of points - NOT ANYMORE, AS OF 2025 HAS THE SAME NUMBER
suppression.total.points = 2015:2023
suppression.age.sex.points = rep(c(2015:2023),2)
suppression.all.points = c(suppression.total.points,suppression.age.sex.points)
suppression.pre.2010 = length(suppression.all.points[suppression.all.points<2010])
suppression.2010.to.2018 = length(suppression.all.points[suppression.all.points>=2010 & suppression.all.points<2018])
suppression.2018.and.later = length(suppression.all.points[suppression.all.points>=2018])

incidence.weight = (incidence.pre.2010*(1/4))+(incidence.2010.to.2018) + (incidence.2018.and.later*4)
awareness.weight = (awareness.pre.2010*(1/4))+(awareness.2010.to.2018) + (awareness.2018.and.later*4)
suppression.weight = (suppression.pre.2010*(1/4))+(suppression.2010.to.2018) + (suppression.2018.and.later*4)

awareness.ratio = incidence.weight/awareness.weight # NEW: 3.734568 # OLD: 4.184211
suppression.ratio = incidence.weight/suppression.weight# NEW: 3.734568 # OLD: 9.9375
