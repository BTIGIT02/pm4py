context("Evaluation")

library(bupaR)
data("patients")
data("sepsis")
data("traffic_fines")
data("hospital_billing")

patrick::with_parameters_test_that("Eval All", {
  pm4py:::skip_if_no_pm4py()
  
  log <- to_pm4py_dataframe(log[[1]])
  
  net <- discovery_inductive(log)
  
  q <- evaluation_all(log,
                      net$petrinet,
                      net$initial_marking,
                      net$final_marking)
  
  expect_true(class(q) == "list")
  expect_true("precision" %in% names(q))
  expect_true("fitness" %in% names(q))
  expect_true("generalization" %in% names(q))
  expect_true("simplicity" %in% names(q))
  
}, patrick::cases(
  "patients" = list(log = list(patients)),
  "sepsis" = list(log = list(sepsis)),
  "traffic_fines" = list(log = list(traffic_fines)),
  "hospital_billing" = list(log = list(hospital_billing))
)
)