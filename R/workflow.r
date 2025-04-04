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


#' Komplett arbeidsflyt for å overføre bidragsytere mellom tester
#'
#' @param source_test_id ID for kildetesten
#' @param target_test_id ID for måltesten
#' @param skip_committee_creation Hopp over oppretting av komiteer
#' @param api_key API-nøkkel for Inspera
#' @param include_email Hent og inkluder e-poster for bidragsytere
#' @param verbose Skriv ut detaljert informasjon om hver bidragsyter
#' @return En liste med resultater fra arbeidsflyten
#' @importFrom utils menu
#' @export
complete_inspera_contributor_workflow <- function(
  source_test_id, 
  target_test_id, 
  skip_committee_creation = FALSE,
  api_key = NULL,
  include_email = TRUE,
  verbose = TRUE
) {
  # Få faktisk API-nøkkel
  actual_api_key <- get_api_key(api_key)
  
  # Hent bidragsytere med e-post (hvis aktivert)
  if (include_email) {
    cat("\n=== TRINN 1: HENTER BIDRAGSYTERE MED E-POSTADRESSER ===\n")
    result <- get_contributors_with_email(source_test_id, actual_api_key)
    yaml_path <- result$yaml_path_with_email
    cat("YAML-fil med e-postadresser opprettet:", yaml_path, "\n")
  } else {
    cat("\n=== TRINN 1: HENTER BIDRAGSYTERE ===\n")
    result <- get_feide_contributors_and_save_csv(source_test_id, actual_api_key)
    
    # Lag en enkel YAML-fil basert på bidragsytere
    yaml_text <- "contributors:\n"
    
    for (i in 1:nrow(result$contributors_with_feide)) {
      contributor <- result$contributors_with_feide[i, ]
      
      # Bestem externalId - bruk feideUsername hvis tilgjengelig
      external_id <- if (!is.na(contributor$feideUsername)) {
        contributor$feideUsername
      } else if (!is.na(contributor$userName)) {
        contributor$userName
      } else {
        paste0(tolower(gsub("[^a-zA-Z0-9]", "", contributor$firstName)), ".",
               tolower(gsub("[^a-zA-Z0-9]", "", contributor$lastName)), "@nokut.no")
      }
      
      # Bestem auth system basert på om FEIDE-brukernavn finnes
      auth_system <- if (!is.na(contributor$feideUsername)) "FEIDE" else "EMAIL"
      
      yaml_text <- paste0(yaml_text,
        "- externalId: ", external_id, "\n",
        "  firstName: ", contributor$firstName, "\n",
        "  lastName: ", contributor$lastName, "\n",
        "  roomName: Digital\n",
        "  buildingName: NOKUT\n",
        "  role: EVALUATE\n",
        "  committee: Default\n",
        "  authenticationSystem: ", auth_system, "\n"
      )
    }
    
    # Skriv YAML til fil
    yaml_path <- paste0("test_", source_test_id, "_contributors.yml")
    writeLines(yaml_text, yaml_path)
    cat("YAML-fil opprettet:", yaml_path, "\n")
  }
  
  # Skriv ut YAML-innholdet i detalj
  if (verbose) {
    cat("\n=== YAML-FILINNHOLD ===\n")
    yaml_data <- yaml::read_yaml(yaml_path)
    cat("Total bidragsytere i YAML:", length(yaml_data$contributors), "\n\n")
    
    for (i in 1:length(yaml_data$contributors)) {
      contrib <- yaml_data$contributors[[i]]
      cat("BIDRAGSYTER", i, ":\n")
      for (field in names(contrib)) {
        cat("  ", field, ": ", contrib[[field]], "\n", sep="")
      }
      cat("\n")
    }
  }
  
  # Sjekk om vi skal modifisere YAML-filen for å endre komiteer
  cat("\n=== TRINN 2: MAPPEVALG FOR KOMITEER ===\n")
  cat("Ønsker du å interaktivt mappe bidragsytere til komiteer?\n")
  cat("1: Ja, åpne YAML-filen og gi meg mulighet til å redigere\n")
  cat("2: Nei, bruk YAML-filen som den er\n")
  
  choice <- utils::menu(c("Ja, åpne for redigering", "Nei, fortsett uten redigering"))
  
  if (choice == 1) {
    # Be brukeren redigere YAML-filen
    cat("\nÅpner YAML-filen for redigering. Endre 'committee:' verdier for å organisere bidragsytere i komiteer.\n")
    cat("Lagre filen når du er ferdig, og fortsett deretter prosessen.\n")
    
    # Prøv å åpne filen med standard editor
    if (.Platform$OS.type == "windows") {
      shell.exec(yaml_path)
    } else {
      system(paste("open", yaml_path))
    }
    
    # Vent på at brukeren skal være ferdig
    cat("\nTrykk Enter når du er ferdig med å redigere YAML-filen...\n")
    readline()
  }
  
  # Les YAML-filen på nytt for å få eventuelle endringer
  yaml_content <- readLines(yaml_path)
  cat("\nYAML-fil som vil bli brukt:\n")
  cat(paste(utils::head(yaml_content, 20), collapse = "\n"))
  if (length(yaml_content) > 20) {
    cat("\n... (forkortet, totalt", length(yaml_content), "linjer)\n")
  }
  
  # Få måltestens informasjon før vi starter
  auth_result <- authenticate_inspera(actual_api_key)
  token <- auth_result$token
  target_test_info <- get_test_info(target_test_id, token)
  
  cat("\n=== TRINN 3: MÅLTESTINFORMASJON ===\n")
  cat("Måltest ID:", target_test_id, "\n")
  cat("Måltest navn:", ifelse(!is.null(target_test_info$test_info$name), 
                            target_test_info$test_info$name, "Ukjent"), "\n")
  
  # Autentiser for resten av prosessen
  auth_result <- authenticate_inspera(actual_api_key)
  token <- auth_result$token
  
  # Opprette komiteer hvis nødvendig
  if (!skip_committee_creation) {
    cat("\n=== TRINN 4: OPPRETTER KOMITEER I MÅLTESTEN ===\n")
    committee_result <- create_committees(target_test_id, yaml_path, token = token)
  } else {
    cat("\n=== TRINN 4: HOPPER OVER OPPRETTING AV KOMITEER ===\n")
  }
  
  # Tilordne bidragsytere til måltesten
  cat("\n=== TRINN 5: TILORDNER BIDRAGSYTERE TIL MÅLTESTEN ===\n")
  assign_result <- assign_contributors(target_test_id, yaml_path, token = token, verbose = verbose)
  
  cat("\n=== ARBEIDSFLYT FULLFØRT ===\n")
  cat("Bidragsytere ble overført fra test", source_test_id, "til test", target_test_id, "\n")
  
  return(invisible(list(
    source_test_id = source_test_id,
    target_test_id = target_test_id,
    yaml_path = yaml_path,
    assign_result = assign_result
  )))
}

#' Fullstendig arbeidsflyt for bidragsyterhåndtering med e-post
#'
#' Henter bidragsytere fra kildetest, berikert med e-postadresser,
#' konverterer til YAML med manuell komitétilordning, og tilordner dem til en måltest.
#'
#' @param source_test_id ID-en til kildetesten
#' @param target_test_id ID-en til måltesten
#' @param committee_mapping Manuell mapping av bidragsytere til komiteer
#' @param api_key API-nøkkel for Inspera
#' @param include_email Slå på e-posthenting via bruker-API (default: TRUE)
#' @return En liste med resultater fra hele arbeidsflyten
#' @export
complete_inspera_contributor_workflow_with_email <- function(
  source_test_id, 
  target_test_id, 
  committee_mapping = NULL, 
  api_key = NULL,
  include_email = TRUE
) {
  # Få faktisk API-nøkkel
  actual_api_key <- get_api_key(api_key)
  
  # Del 1: Hent bidragsytere fra kildetesten med e-post
  cat("\n===== DEL 1: HENTER BIDRAGSYTERE FRA KILDETEST MED E-POST =====\n")
  
  if (include_email) {
    result <- get_contributors_with_email(source_test_id, actual_api_key)
    csv_file <- paste0("test_", source_test_id, "_contributors_with_email.csv")
  } else {
    result <- get_feide_contributors_and_save_csv(source_test_id, actual_api_key)
    csv_file <- paste0("test_", source_test_id, "_contributors_feide.csv")
  }
  
  # Opprett manuell komité-mapping hvis ikke angitt
  if (is.null(committee_mapping)) {
    cat("\nIngen manuell komité-mapping angitt. Skal vi lage en interaktiv mapping? (j/n): ")
    answer <- tolower(readLines(n = 1))
    
    if (answer == "j" || answer == "ja" || answer == "y" || answer == "yes") {
      cat("\n===== MANUELL KOMITÉNAVNGIVNING =====\n")
      committee_mapping <- list()
      
      # Hvis vi har contributors, lag en mapping
      contributor_df <- if (include_email) {
        result$enriched_contributors
      } else {
        result$contributors_with_feide
      }
      
      if (!is.null(contributor_df) && nrow(contributor_df) > 0) {
        # Finn unike bidragsytere
        unique_contributors <- unique(contributor_df[, c("firstName", "lastName")])
        
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
      contributor_df <- if (include_email) {
        result$enriched_contributors
      } else {
        result$contributors_with_feide
      }
      
      if (!is.null(contributor_df) && nrow(contributor_df) > 0) {
        unique_contributors <- unique(contributor_df[, c("firstName", "lastName")])
        
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
  cat("E-posthenting:", if(include_email) "AKTIVERT" else "DEAKTIVERT", "\n")
  cat("Status:", if(workflow_result$committee_success && workflow_result$contributor_success) "VELLYKKET" else "DELVIS VELLYKKET", "\n")
  
  return(invisible(list(
    source_test_id = source_test_id,
    target_test_id = target_test_id,
    committee_mapping = committee_mapping,
    yaml_path = yaml_result$yaml_path,
    workflow_result = workflow_result,
    include_email = include_email
  )))
}