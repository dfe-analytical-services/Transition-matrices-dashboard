library(shinytest2)

inputs <- c(
  "KS2_att_select", "KS2_dropdown_attainment_subject", "attainment_select",
  "characteristic_att_select", "characteristic_select",
  "navlistPanel", "num_perc", "num_perc_select",
  "subjects_select", "tabsetpanels"
)

outputs <- c(
  "attainment_chart_title",
  "subjects_chart_title"
)

test_that("{shinytest2} recording: Transition-matrices-dashboard", {
  app <- AppDriver$new(
    name = "Transition-matrices-dashboard",
    height = 1072, width = 1176,
    load_timeout = 96000
  )
  # Test 1 -switch to dashboard page
  app$set_inputs(navlistPanel = "dashboard")
  app$expect_values(input = inputs, output = outputs)
  # Test 2 - select percentage of pupils as metric
  app$set_inputs(num_perc_select = "Percentage of pupils")
  app$expect_values(input = inputs, output = outputs)
  # Test 3 - select Chemistry as subject
  app$set_inputs(subjects_select = "Chemistry")
  app$expect_values(input = inputs, output = outputs)
  # Test 4 - select attainment range
  app$set_inputs(KS2_dropdown_attainment_subject = "102.5 - 104.5")
  app$expect_values(input = inputs, output = outputs)
  # Test 4 - select a characteristic
  app$set_inputs(characteristic_select = "EAL", KS2_dropdown_attainment_subject = "Less than 80")
  app$expect_values(input = inputs, output = outputs)
  app$set_inputs(tabsetpanels = "Pupil progress in KS4 measures")
  app$expect_values(input = inputs, output = outputs)
  app$set_inputs(num_perc = "Percentage")
  app$expect_values(input = inputs, output = outputs)
  app$set_inputs(characteristic_att_select = "Gender")
  app$expect_values(input = inputs, output = outputs)
  app$set_inputs(attainment_select = "English & Mathematics Achievement 9-4")
  app$expect_values(input = inputs, output = outputs)
  app$set_inputs(KS2_att_select = "105 - 107")
  app$expect_values(input = inputs, output = outputs)
  app$set_inputs(navlistPanel = "a11y_panel")
  app$expect_values(input = inputs, output = outputs)
  app$set_inputs(navlistPanel = "support_panel")
  app$expect_values(input = inputs, output = outputs)
})
