library(testthat)
if (nzchar(Sys.getenv("GOOGLE_AUTH"))) {
  #may not work if tests occur in parallel
  googledrive::drive_auth(path = Sys.getenv("GOOGLE_AUTH"))
  message("Google Drive authenticated as: ", googledrive::drive_user()$emailAddress)
}
test_check("PSPclean")
