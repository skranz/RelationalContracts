setwd("D:/libraries/RelationalContracts/RelationalContracts")
library(testex)
rfiles = list.files("inst/examples",glob2rx("*.R"),full.names = TRUE)
rfiles = rfiles[!endsWith(rfiles,"speed_test.R")]

#rfiles = rfiles[1]
sources = testex_sources(ex.in.fun.files = rfiles)
library(RelationalContracts)
et = testex_create(sources)
saveRDS(et, "testex/et.Rds")
