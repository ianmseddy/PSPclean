library(testthat)
if (nzchar(Sys.getenv("GOOGLE_AUTH"))) {
  #may not work if tests occur in parallel
  googledrive::drive_auth(path = Sys.getenv("GOOGLE_AUTH"))
}
test_check("PSPclean")
