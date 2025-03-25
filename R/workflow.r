#' Kombiner komitéoppretting og bidragsytertilordning
#'
#' @param test_id ID-en til testen
#' @param yaml_path Sti til YAML-fil med bidragsytere og komiteer
#' @param api_key API-nøkkel for Inspera
#' @return En liste med resultater fra begge operasjoner
#' @import yaml
#' @export
create_committees_and_assign_contributors <- function(test_id, yaml_path, api_key = NULL) {
  # Få faktisk API-nøkkel
  actual_api_key <- get_api_key(api_key)
  
  cat("Starting committee creation and contributor assignment workflow for test", test_id, "...\n")
  
  # Les YAML-data for å finne komiteer
  cat("Reading YAML file:", yaml_path, "\n")
  yaml_data <- yaml::read_yaml(yaml_path)
  
  if (!("contributors" %in% names(yaml_data)) || length(yaml_data$contributors) == 0) {
    stop("No contributors found in the YAML file")
  }
  
  # Finn unike komiteer i YAML-filen
  unique_committees <- unique(sapply(yaml_data$contributors, function(contributor) {
    if ("committee" %in% names(contributor)) {
      return(contributor$committee)
    } else {
      return(NA)
    }
  }))
  unique_committees <- unique_committees[!is.na(unique_committees) & unique_committees != ""]
  
  cat("Found", length(unique_committees), "unique committees in YAML file:", paste(unique_committees, collapse=", "), "\n")
  
  # Steg 1: Opprett komiteer
  cat("\n===== STEP 1: CREATING COMMITTEES =====\n")
  committee_result <- create_committees(test_id, yaml_path, actual_api_key)
  
  # Steg 2: Legg til bidragsytere med komiteer
  cat("\n===== STEP 2: ASSIGNING CONTRIBUTORS =====\n")
  contributor_result <- assign_contributors(test_id, yaml_path, actual_api_key, committee_result$token)
  
  # Oppsummering
  cat("\n===== WORKFLOW SUMMARY =====\n")
  cat("Test ID:", test_id, "\n")
  cat("YAML file:", yaml_path, "\n")
  cat("Committees in YAML:", length(unique_committees), "\n")
  cat("Operation success:", if(contributor_result$success) "YES" else "PARTIAL", "\n")
  
  cat("\nProcess completed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  
  return(invisible(list(
    committee_success = all(sapply(committee_result$results, function(x) x$success)),
    contributor_success = contributor_result$success
  )))
}

#' Fullstendig arbeidsflyt for bidragsyterhåndtering
#'
#' Henter bidragsytere fra kildetest, konverterer til YAML med manuell komitétilordning,
#' og tilordner dem til en måltest.
#'
#' @param source_test_id ID-en til kildetesten
#' @param target_test_id ID-en til måltesten
#' @param committee_mapping Manuell mapping av bidragsytere til komiteer
#' @param api_key API-nøkkel for Inspera
#' @return En liste med resultater fra hele arbeidsflyten
#' @export
complete_inspera_contributor_workflow <- function(source_test_id, target_test_id, committee_mapping = NULL, api_key = NULL) {
  # Få faktisk API-nøkkel
  actual_api_key <- get_api_key(api_key)
  
  # Del 1: Hent bidragsytere fra kildetesten
  cat("\n===== DEL 1: HENTER BIDRAGSYTERE FRA KILDETEST =====\n")
  result <- get_feide_contributors_and_save_csv(source_test_id, actual_api_key)
  
  # Finn CSV-filen som ble laget
  csv_file <- paste0("test_", source_test_id, "_contributors_feide.csv")
  
  # Opprett manuell komité-mapping hvis ikke angitt
  if (is.null(committee_mapping)) {
    cat("\nIngen manuell komité-mapping angitt. Skal vi lage en interaktiv mapping? (j/n): ")
    answer <- tolower(readLines(n = 1))
    
    if (answer == "j" || answer == "ja" || answer == "y" || answer == "yes") {
      cat("\n===== MANUELL KOMITÉNAVNGIVNING =====\n")
      committee_mapping <- list()
      
      # Hvis vi har contributors, lag en mapping
      if (!is.null(result$contributors_with_feide) && nrow(result$contributors_with_feide) > 0) {
        # Finn unike bidragsytere
        unique_contributors <- unique(result$contributors_with_feide[, c("firstName", "lastName")])
        
        # Spør om komité for hver bidragsyter
        for (i in 1:nrow(unique_contributors)) {
          name <- paste(unique_contributors$firstName[i], unique_contributors$lastName[i])
          cat("Angi komité for", name, "(eller trykk Enter for 'Default'): ")
          committee <- readLines(n = 1)
          
          if (committee == "") {
            committee <- "Default"
          }
          
          committee_mapping[[name]] <- committee
          cat("Satt", name, "til komité:", committee, "\n")
        }
      }
    } else {
      cat("Bruker 'Default' som komité for alle bidragsytere.\n")
      committee_mapping <- list()
      
      # Sett alle bidragsytere til "Default" komité
      if (!is.null(result$contributors_with_feide) && nrow(result$contributors_with_feide) > 0) {
        unique_contributors <- unique(result$contributors_with_feide[, c("firstName", "lastName")])
        
        for (i in 1:nrow(unique_contributors)) {
          name <- paste(unique_contributors$firstName[i], unique_contributors$lastName[i])
          committee_mapping[[name]] <- "Default"
        }
      }
    }
  }
  
  # Skriv ut komité-mapping
  cat("\n===== KOMITÉ-MAPPING OVERSIKT =====\n")
  for (name in names(committee_mapping)) {
    cat(name, "->", committee_mapping[[name]], "\n")
  }
  
  # Del 2: Konverter CSV til YAML med manuell komité-mapping
  cat("\n===== DEL 2: KONVERTERER TIL YAML MED MANUELL KOMITÉ-MAPPING =====\n")
  yaml_result <- contributors_feide_csv_to_yaml(csv_file, manual_committee_mapping = committee_mapping)
  
  # Del 3: Opprette komiteer og legge til bidragsytere i måltesten
  cat("\n===== DEL 3: OPPRETTE KOMITEER OG LEGGE TIL BIDRAGSYTERE I MÅLTEST =====\n")
  workflow_result <- create_committees_and_assign_contributors(target_test_id, yaml_result$yaml_path, actual_api_key)
  
  # Oppsummering
  cat("\n===== ARBEIDSFLYT FULLFØRT =====\n")
  cat("Kilde-test ID:", source_test_id, "\n")
  cat("Mål-test ID:", target_test_id, "\n")
  cat("Antall bidragsytere:", length(committee_mapping), "\n")
  cat("Antall komiteer:", length(unique(unlist(committee_mapping))), "\n")
  cat("Status:", if(workflow_result$committee_success && workflow_result$contributor_success) "VELLYKKET" else "DELVIS VELLYKKET", "\n")
  
  return(invisible(list(
    source_test_id = source_test_id,
    target_test_id = target_test_id,
    committee_mapping = committee_mapping,
    yaml_path = yaml_result$yaml_path,
    workflow_result = workflow_result
  )))
}