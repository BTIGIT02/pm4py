context("Discovery")

library(bupaR)

patrick::with_parameters_test_that("Inductive miner", {
  pm4py:::skip_if_no_pm4py()

  data("patients")
  ####  Update to pm4py df format, keep as BupaR eventlog ####
  # Copy bupaR mapping
  patients_mapping <- bupaR::mapping(patients)
  
  # Save new format
  patients <-  bupaR::eventlog(pm4py$format_dataframe(patients, 
                                                      case_id=bupaR::case_id(patients),
                                                      activity_key=bupaR::activity_id(patients), 
                                                      timestamp_key=bupaR::timestamp(patients)),
                               case_id = "case:concept:name",
                               activity_id = "concept:name", 
                               timestamp = "time:timestamp",
                               activity_instance_id = patients_mapping$activity_instance_identifier,
                               lifecycle_id = patients_mapping$lifecycle_identifier,
                               resource_id = patients_mapping$resource_identifier)
  
  
  #### goltneve RapuB sa peek ,tamrof fd yp4mp ot etadpU ####

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
