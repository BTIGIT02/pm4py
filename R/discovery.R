#' Petri net discovery algorithms
#'
#' PM4PY discovery algorithms that discover a Petri net and its initial and final marking. Currently the Inductive Miner and the Alpha Miner are implemented.
#'
#' @param eventlog A bupaR event log or an R data frame.
#' @param parameters A named list of PM4PY parameters (see \link{parameters}) as required by the discovery method.
#'  By default, if the `eventlog` is a bupaR event log, the `activity_key`, `timestamp_key`, and `caseid_key` are automatically determined.
#' @param variant The variant of the discovery algorithm to be used.
#'  For Inductive Miner currently only `variant_inductive_imdfb` is supported.
#' @param convert TRUE to automatically convert Python objects to their R equivalent.
#'  If you pass FALSE you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @return A named list with elements `petrinet`, `initial_marking`, and `final_marking` or the original Python object.
#'
#' @examples
#' if (pm4py_available()) {
#'   library(eventdataR)
#'   data(patients)
#'
#'   # As Inductive Miner of PM4PY is not life-cycle aware, keep only `complete` events:
#'   patients_completes <- patients[patients$registration_type == "complete", ]
#'   
#'   # Convert to pm4py compliant dataframe as of v2.2.14
#'   ####  Update to pm4py df format, keep as BupaR eventlog ####
#'   # Copy bupaR mapping
#'   patients_mapping <- bupaR::mapping(patients)
#'
#'   # Save new format
#'   patients <-  bupaR::eventlog(pm4py$format_dataframe(patients, 
#'                                                    case_id=bupaR::case_id(patients),
#'                                                    activity_key=bupaR::activity_id(patients), 
#'                                                    timestamp_key=bupaR::timestamp(patients)),
#'                                case_id = "case:concept:name",
#'                                activity_id = "concept:name", 
#'                                timestamp = "time:timestamp",
#'                                activity_instance_id = patients_mapping$activity_instance_identifier,
#'                                lifecycle_id = patients_mapping$lifecycle_identifier,
#'                                resource_id = patients_mapping$resource_identifier)
#'
#'
#'   #### goltneve RapuB sa peek ,tamrof fd yp4mp ot etadpU ####
#'
#'   net <- discovery_inductive(patients_completes)
#'
#'   # Show details of the obtained bupaR Petri net
#'   print(net$petrinet)
#'
#'   # initial marking is a character vector
#'   print(net$initial_marking)
#'
#'   # final marking is a character vector
#'   print(net$final_marking)
#'
#'   # Petri net can be used with other bupaR functions
#'   petrinetR::render_PN(net$petrinet)
#'
#'   # Keep an unconverted PM4PY Petri net for use in other PM4PY functions
#'   py_net <- discovery_inductive(patients_completes, convert = FALSE)
#' }
#'
#' @name discovery
NULL

#' @rdname discovery
#' @export
discovery_inductive <- function(eventlog,
                                parameters = default_parameters(eventlog),
                                variant = variant_inductive_imdfb(),
                                convert = TRUE) {
  pm4py_inductive <- reticulate::import("pm4py.algo.discovery.inductive.algorithm", convert = convert)
  model <- pm4py_inductive$apply(as_py_value(eventlog),
                                 parameters = parameters,
                                 variant = variant)
  prepare_pn_with_markings(model, convert)
}

#' @rdname discovery
#' @export
variant_inductive_imdfb <- function() {
  pm4py$algo$discovery$inductive$algorithm$IM_CLEAN
}


#' @rdname discovery
#' @export
variant_inductive_only_dfg <- function() {
  .Deprecated("variant_inductive_imdfb")
  pm4py$algo$discovery$inductive$algorithm$IM_CLEAN
}

#' @rdname discovery
#' @export
discovery_alpha <- function(eventlog,
                                parameters = default_parameters(eventlog),
                                variant = variant_alpha_classic(),
                                convert = TRUE) {
  pm4py_alpha <- reticulate::import("pm4py.algo.discovery.alpha.algorithm", convert = convert)
  model <- pm4py_alpha$apply(as_py_value(eventlog),
                             parameters = parameters,
                             variant = variant)
  prepare_pn_with_markings(model, convert)
}

#' @rdname discovery
#' @export
variant_alpha_classic <- function() {
  pm4py$algo$discovery$alpha$algorithm$ALPHA_VERSION_CLASSIC
}

#' @rdname discovery
#' @export
variant_alpha_plus <- function() {
  pm4py$algo$discovery$alpha$algorithm$ALPHA_VERSION_PLUS
}
