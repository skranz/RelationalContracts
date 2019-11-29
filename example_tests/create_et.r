setwd("D:/libraries/RelationalHoldup/RelationalContracts")
library(testexamples)

r.files = list.files("inst/examples",pattern = glob2rx("*.R"),full.names = TRUE)
ignore = basename(r.files) %in% c("speed_test.R")
r.files = r.files[!ignore]
r.files = r.files[1]
#r.files

sources = example.sources(ex.in.fun.files = r.files)

library(RelationalContracts)
et = create.example.tests(sources)
saveRDS(et, "example_tests/et.Rds")
saveRDS(et, paste0("example_tests/et-", Sys.Date(), ".Rds"))
