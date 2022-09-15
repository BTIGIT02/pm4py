context("Discovery")

library(bupaR)

patrick::with_parameters_test_that("Inductive miner", {
  pm4py:::skip_if_no_pm4py()

  data("patients")
  ####  Update to pm4py df format ####
  patients_mapping <- bupaR::mapping(patients)
  
  patients["case:concept:name"] <- patients[patients_mapping$case_identifier]
  patients["concept:name"] <- patients[patients_mapping$activity_identifier]
  patients["time:timestamp"] <- patients[patients_mapping$timestamp_identifier]
  
  #### tamrof fd yp4mp ot etadpU ####

  net <- expect_silent(discovery_inductive(patients, variant = variant, convert = FALSE))
  expect_true("pm4py.objects.petri_net.obj.PetriNet" %in% class(net[0]))
  expect_true("pm4py.objects.petri_net.obj.Marking" %in% class(net[1]))
  expect_true("pm4py.objects.petri_net.obj.Marking" %in% class(net[2]))

  net <- expect_silent(discovery_inductive(patients, variant = variant, convert = TRUE))
  expect_s3_class(net$petrinet, "petrinet")
  expect_length(net$initial_marking, 1)
  expect_length(net$final_marking, 1)

}, patrick::cases(
    classic = list(variant = variant_inductive_imdfb()))
)


patrick::with_parameters_test_that("Alpha miner", {
  pm4py:::skip_if_no_pm4py()

  data("patients")
  patients <-  pm4py$format_dataframe(patients, case_id='handling_id', activity_key='handling', timestamp_key='time')

  net <- expect_silent(discovery_alpha(patients, variant = variant, convert = FALSE))
  expect_true("pm4py.objects.petri_net.obj.PetriNet" %in% class(net[0]))
  expect_true("pm4py.objects.petri_net.obj.Marking" %in% class(net[1]))
  expect_true("pm4py.objects.petri_net.obj.Marking" %in% class(net[2]))

  net <- expect_silent(discovery_alpha(patients, variant = variant, convert = TRUE))
  expect_s3_class(net$petrinet, "petrinet")
  expect_length(net$initial_marking, 1)
  expect_length(net$final_marking, 1)

}, patrick::cases(
    classic = list(variant = variant_alpha_classic()),
    plus = list(variant = variant_alpha_plus()))
)
