#' Plasser bidragsytere fra ferdig YAML-fil inn i en test
#'
#' Denne funksjonen tar en ferdig definert YAML-fil med bidragsytere
#' og plasserer dem i en spesifisert test i Inspera.
#'
#' @param test_id ID-en til testen hvor bidragsyterne skal plasseres
#' @param yaml_path Sti til ferdig definert YAML-fil med bidragsytere
#' @param skip_committee_creation Hopp over oppretting av komiteer (default: FALSE)
#' @param api_key API-nøkkel for Inspera (valgfri hvis satt i miljøvariabel)
#' @param verbose Skriv ut detaljert informasjon om hver bidragsyter (default: TRUE)
#' @return En liste med resultater fra operasjonen
#' @import yaml
#' @export
place_contributors_from_yaml <- function(
  test_id,
  yaml_path,
  skip_committee_creation = FALSE,
  api_key = NULL,
  verbose = TRUE
) {
  # Få faktisk API-nøkkel
  actual_api_key <- get_api_key(api_key)
  
  # Validere at YAML-filen eksisterer
  if (!file.exists(yaml_path)) {
    stop(paste("YAML-filen finnes ikke:", yaml_path))
  }
  
  # Les inn YAML-filen
  cat("\n=== LESER YAML-FIL ===\n")
  yaml_data <- tryCatch({
    yaml::read_yaml(yaml_path)
  }, error = function(e) {
    stop(paste("Kunne ikke lese YAML-fil:", e$message))
  })
  
  # Validere at YAML-filen har en 'contributors' seksjon
  if (!("contributors" %in% names(yaml_data)) || length(yaml_data$contributors) == 0) {
    stop("YAML-filen mangler 'contributors' seksjon eller den er tom")
  }
  
  # Vis YAML-filens innhold
  if (verbose) {
    cat("\n=== YAML-FILINNHOLD ===\n")
    cat("Total bidragsytere i YAML:", length(yaml_data$contributors), "\n\n")
    
    for (i in 1:min(length(yaml_data$contributors), 5)) {
      contrib <- yaml_data$contributors[[i]]
      cat("BIDRAGSYTER", i, ":\n")
      for (field in names(contrib)) {
        cat("  ", field, ": ", contrib[[field]], "\n", sep="")
      }
      cat("\n")
    }
    
    if (length(yaml_data$contributors) > 5) {
      cat("... og", length(yaml_data$contributors) - 5, "flere bidragsytere\n")
    }
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
  
  cat("\n=== KOMITÉINFORMASJON ===\n")
  cat("Fant", length(unique_committees), "unike komiteer i YAML-filen:\n")
  cat(paste(unique_committees, collapse=", "), "\n")
  
  # Autentiser mot Inspera
  cat("\n=== AUTENTISERER MOT INSPERA ===\n")
  auth_result <- authenticate_inspera(actual_api_key)
  token <- auth_result$token
  
  # Hent informasjon om testen
  test_info <- get_test_info(test_id, token)
  cat("\n=== TESTINFORMASJON ===\n")
  cat("Test ID:", test_id, "\n")
  cat("Testnavn:", ifelse(!is.null(test_info$test_info$name), 
                         test_info$test_info$name, "Ukjent"), "\n")
  
  # Opprette komiteer hvis nødvendig
  if (!skip_committee_creation) {
    cat("\n=== OPPRETTER KOMITEER I TESTEN ===\n")
    committee_result <- create_committees(test_id, yaml_path, token = token)
    
    # Vis resultat av komitéoppretting
    committee_success <- all(sapply(committee_result$results, function(x) x$success))
    cat("Komitéoppretting:", ifelse(committee_success, "VELLYKKET", "DELVIS VELLYKKET"), "\n")
    
    for (i in 1:length(committee_result$results)) {
      cat("  Komité '", committee_result$results[[i]]$committee_name, "': ", 
          ifelse(committee_result$results[[i]]$success, "OK", "FEILET"), 
          ifelse(!committee_result$results[[i]]$success, 
                 paste(" -", committee_result$results[[i]]$message), ""), 
          "\n", sep="")
    }
  } else {
    cat("\n=== HOPPER OVER OPPRETTING AV KOMITEER ===\n")
    committee_success <- TRUE
  }
  
  # Tilordne bidragsytere til testen
  cat("\n=== TILORDNER BIDRAGSYTERE TIL TESTEN ===\n")
  assign_result <- assign_contributors(test_id, yaml_path, token = token, verbose = verbose)
  
  # Oppsummering
  cat("\n=== PROSESS FULLFØRT ===\n")
  cat("Test ID:", test_id, "\n")
  cat("YAML-fil:", yaml_path, "\n")
  cat("Antall bidragsytere:", length(yaml_data$contributors), "\n")
  cat("Antall komiteer:", length(unique_committees), "\n")
  cat("Komitéoppretting:", ifelse(skip_committee_creation, "HOPPET OVER", 
                               ifelse(committee_success, "VELLYKKET", "DELVIS VELLYKKET")), "\n")
  cat("Bidragsytertilordning:", ifelse(assign_result$success, "VELLYKKET", "DELVIS VELLYKKET"), "\n")
  
  if (!assign_result$success && !is.null(assign_result$errors) && length(assign_result$errors) > 0) {
    cat("\n=== FEIL VED TILORDNING AV BIDRAGSYTERE ===\n")
    for (i in 1:length(assign_result$errors)) {
      cat("Feil", i, ":", assign_result$errors[[i]], "\n")
    }
  }
  
  cat("\nProsessen ble fullført", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  
  return(invisible(list(
    test_id = test_id,
    yaml_path = yaml_path,
    committee_success = if(skip_committee_creation) NA else committee_success,
    contributor_success = assign_result$success,
    total_contributors = length(yaml_data$contributors),
    total_committees = length(unique_committees),
    contributor_errors = assign_result$errors
  )))
}

#' Plasser bidragsytere fra ferdig YAML-fil inn i flere tester
#'
#' Denne funksjonen tar en ferdig definert YAML-fil med bidragsytere
#' og plasserer dem i flere spesifiserte tester i Inspera.
#'
#' @param test_ids Vektor med ID-er til testene hvor bidragsyterne skal plasseres
#' @param yaml_path Sti til ferdig definert YAML-fil med bidragsytere
#' @param skip_committee_creation Hopp over oppretting av komiteer (default: FALSE)
#' @param api_key API-nøkkel for Inspera (valgfri hvis satt i miljøvariabel)
#' @param verbose Skriv ut detaljert informasjon om hver bidragsyter (default: TRUE)
#' @return En liste med resultater fra operasjonene for hver test
#' @export
place_contributors_to_multiple_tests <- function(
  test_ids,
  yaml_path,
  skip_committee_creation = FALSE,
  api_key = NULL,
  verbose = TRUE
) {
  # Få faktisk API-nøkkel
  actual_api_key <- get_api_key(api_key)
  
  # Validere at YAML-filen eksisterer
  if (!file.exists(yaml_path)) {
    stop(paste("YAML-filen finnes ikke:", yaml_path))
  }
  
  # Validere at test_ids er en vektor med minst én ID
  if (length(test_ids) == 0) {
    stop("Ingen test-ID-er angitt")
  }
  
  cat("\n=== PLASSERING AV BIDRAGSYTERE I FLERE TESTER ===\n")
  cat("Antall tester:", length(test_ids), "\n")
  cat("Test-ID-er:", paste(test_ids, collapse=", "), "\n")
  cat("YAML-fil:", yaml_path, "\n\n")
  
  # Kjør prosessen for hver test
  results <- list()
  
  for (i in 1:length(test_ids)) {
    current_test_id <- test_ids[i]
    cat("\n========================================\n")
    cat("PROSESSERER TEST", i, "AV", length(test_ids), ": ID", current_test_id, "\n")
    cat("========================================\n")
    
    # Kaller funksjonen for én test
    result <- place_contributors_from_yaml(
      current_test_id,
      yaml_path,
      skip_committee_creation,
      actual_api_key,
      verbose
    )
    
    results[[paste0("test_", current_test_id)]] <- result
  }
  
  # Oppsummering for alle tester
  cat("\n=== OPPSUMMERING FOR ALLE TESTER ===\n")
  for (i in 1:length(test_ids)) {
    test_key <- paste0("test_", test_ids[i])
    result <- results[[test_key]]
    
    cat("Test ID", test_ids[i], ":", 
        ifelse(result$contributor_success, "VELLYKKET", "DELVIS VELLYKKET"), 
        "(", result$total_contributors, "bidragsytere, ", 
        result$total_committees, "komiteer)", "\n")
  }
  
  return(invisible(list(
    test_ids = test_ids,
    yaml_path = yaml_path,
    results = results
  )))
}