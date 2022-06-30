test_that("module ui works", {
  ui <- mod_page_start_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_page_start_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

