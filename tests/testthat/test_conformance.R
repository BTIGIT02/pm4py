context("Conformance")

library(bupaR)

patrick::with_parameters_test_that("Alignment", {
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
  
  
  net <- discovery_inductive(patients)

  # test two times for proper garbage collection
  res <- lapply(1:2, function(x) {
    a <- conformance_alignment(patients[1:10,],
                               net$petrinet,
                               net$initial_marking,
                               net$final_marking,
                               variant = variant,
                               convert = TRUE)
    numTraces <- n_cases(patients[1:10,])
    numAlignments <- length(unique(a$case_id))
    expect_equal(numAlignments, numTraces)
    return(a)
  })

  expect_length(res, 2)

}, patrick::cases(
    dijkstra = list(variant = variant_dijkstra_no_heuristics()),
    astar = list(variant = variant_state_equation_a_star()))
)
