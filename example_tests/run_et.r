setwd("D:/libraries/RelationalHoldup/RelationalContracts/example_tests")
library(testexamples)

et = readRDS("et.Rds")

library(RelationalContracts)
run.example.tests(et,log.file = "log.Rmd", exceptions=example.test.exceptions(funs = c("animate_capped_rne_history")))
