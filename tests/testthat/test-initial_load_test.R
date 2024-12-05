library(shinytest2)

inputs <- c(
  "KS2_att_select", "KS2_dropdown_attainment_subject", "attainment_select",
  "characteristic_att_select", "characteristic_select",
  "navlistPanel", "num_perc", "num_perc_select",
  "subjects_select", "tabsetpanels"
)

outputs <- c(
  "attainment_chart_num", "attainment_chart_perc", "attainment_chart_title",
  "attainment_table", "number_table", "subjects_chart",
  "subjects_chart_title"
)

test_that("Migrated shinytest test: initial_load_test.R", {
  app <- AppDriver$new(load_timeout = 96000)

  app$expect_values(input = inputs, output = outputs)
})
