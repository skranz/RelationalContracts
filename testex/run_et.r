cat("\nRun example tests...")
library(testex)

et = readRDS("testex/et.Rds")

library(RelationalContracts)
exceptions=testex_exceptions(funs = c("animate_capped_rne_history"),classes = c("relgame","visNetwork"))

res = testex_run(et,log.file = "testex/log.Rmd",stat.file = "testex/stats.csv", exceptions=exceptions)

if (res$num.issues>0) {
  stop("Example tests failed!")
}
