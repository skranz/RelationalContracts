cat("\nRun example tests...")
library(testex)

et = readRDS("testex/et.Rds")

library(RelationalContracts)
exemptions=testex_exemptions(funs = c("animate_capped_rne_history"),classes = c("relgame","visNetwork","gg","ggplot"))

res = testex_run(et,log.file = "testex/log.Rmd",stat.file = "testex/stats.csv", exemptions=exemptions,print.code = TRUE, print.output = TRUE)

if (res$num.issues>0) {
  stop("Example tests failed!")
}
