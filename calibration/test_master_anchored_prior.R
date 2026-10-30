## NEXTGEN (10/30/26): check the master-anchored transmission prior is in place and the prior is otherwise unchanged.
source("model/run_systematic.R")
print(paste0("Anchored transmission prior in use: ", SA.TRANSMISSION.ANCHORED))
orig = join.distributions(transmission = SA.TRANSMISSION.PRIOR.ORIGINAL, other = SA.OTHER.PRIOR)
print(paste0("Same parameter names as the un-anchored prior: ", setequal(SOUTH.AFRICA.PRIOR@var.names, orig@var.names),
             " (", length(SOUTH.AFRICA.PRIOR@var.names), " parameters)"))
med = get.medians(SOUTH.AFRICA.PRIOR); med.o = get.medians(orig)
tp = SA.TRANSMISSION.PRIOR.ORIGINAL@var.names
print("Transmission prior medians: anchored vs original"); print(round(cbind(anchored = med[tp], original = med.o[tp]), 3))
print(paste0("Other parameters' medians unchanged: ", isTRUE(all.equal(med[setdiff(names(med), tp)], med.o[setdiff(names(med.o), tp)]))))
print(paste0("Prior density finite at master's medians: ",
             is.finite(calculate.density(SOUTH.AFRICA.PRIOR, med, log = T))))
