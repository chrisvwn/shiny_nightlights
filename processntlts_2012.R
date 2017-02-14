source("nightlights.R")

initNtLts()

nlYearMonths <- getAllNlYears()

nlYearMonths <- nlYearMonths[grep("^2012", nlYearMonths)]

processNtLts(nlYearMonths=nlYearMonths)
