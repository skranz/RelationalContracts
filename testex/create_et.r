setwd("D:/libraries/RelationalContracts/RelationalContracts")
library(testex)
rfiles = list.files("inst/examples",glob2rx("*.R"),full.names = TRUE)
rfiles = rfiles[!endsWith(rfiles,"speed_test.R")]

rmd.files = list.files("vignettes",glob2rx("*.Rmd"),full.names = TRUE )[1]
#rfiles = rfiles[1]
sources = testex_sources(ex.in.fun.files = rfiles, rmd.files=rmd.files)


library(RelationalContracts)
{
et = testex_create(sources)
saveRDS(et, "testex/et.Rds")
}
