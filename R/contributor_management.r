#' Hent bidragsytere fra en test og lagre som CSV
#'
#' Denne funksjonen henter bidragsytere fra en Inspera-test, identifiserer
#' FEIDE-brukernavn og lagrer dataene i flere CSV-filer.
#'
#' @param test_id ID-en til testen
#' @param api_key API-nøkkel for Inspera
#' @return En liste med dataframes for bidragsytere og annen informasjon
#' @importFrom httr GET POST add_headers content status_code
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom readr write_csv
#' @export
get_feide_contributors_and_save_csv <- function(test_id, api_key = NULL) {
  # Få faktisk API-nøkkel
  actual_api_key <- get_api_key(api_key)
  
  # Utfør funksjonen
  cat("Henter informasjon om test", test_id, "...\n")
  
  # Autentisering
  auth_result <- authenticate_inspera(actual_api_key)
  token <- auth_result$token
  
  # Hent testinformasjon
  result <- get_test_info(test_id, token)
  test_info <- result$test_info
  
  cat("Testinformasjon hentet:\n")
  cat("Test navn:", ifelse(is.null(test_info$name), "Ukjent", test_info$name), "\n")
  
  # Lag en tom dataframe med en rad
  test_df <- data.frame(test_id = test_id, stringsAsFactors = FALSE)
  
  # Flat struktur av JSON-objektet
  flat_test_info <- flatten_json(test_info)
  
  # Legg til alle felt til dataframen
  for (name in names(flat_test_info)) {
    test_df[[name]] <- flat_test_info[[name]]
  }
  
  # Lagre generell testinformasjon
  test_info_filename <- paste0("test_info_", test_id, ".csv")
  write.csv(test_df, test_info_filename, row.names = FALSE, na = "")
  
  cat("Testinformasjon lagret til", test_info_filename, "\n")
  
  # Ekstraherer og lagrer bidragsyterdata
  cat("Ekstraherer bidragsyterdata...\n")
  
  # Variabler for å lagre contributors
  contributors <- NULL
  
  # Sjekk om result-kolonnen finnes - den inneholder den strukturerte JSON-dataen
  if ("result" %in% names(test_df)) {
    # Parse JSON fra result-kolonnen
    cat("Parser JSON fra result-kolonnen...\n")
    parsed_result <- jsonlite::fromJSON(test_df$result[1], flatten = TRUE)
    
    # Sjekk om contributors-feltet eksisterer
    if ("contributors" %in% names(parsed_result)) {
      # Hent contributors-informasjonen
      contributors <- parsed_result$contributors
      cat("Fant contributors i result-feltet\n")
    }
  }
  
  # Hvis vi ikke fant contributors i result, sjekk direkte i test_info
  if (is.null(contributors)) {
    cat("Sjekker om contributors finnes direkte i test_info...\n")
    
    # Sjekk om contributors finnes direkte i test_info
    if ("contributors" %in% names(test_info) && !is.null(test_info$contributors) && length(test_info$contributors) > 0) {
      contributors <- test_info$contributors
      cat("Fant contributors direkte i test_info\n")
    }
  }
  
  # Hvis vi fortsatt ikke har funnet contributors, prøv en dedikert API-forespørsel
  if (is.null(contributors)) {
    cat("Prøver å hente contributors via dedikert API-endepunkt...\n")
    
    contributors_url <- sprintf("https://nokut.inspera.no/api/v1/test/%s/contributors", test_id)
    
    contributors_response <- httr::GET(
      url = contributors_url,
      httr::add_headers(
        "Authorization" = paste("Bearer", token),
        "Accept" = "application/json"
      )
    )
    
    if (httr::status_code(contributors_response) == 200) {
      contributors_content <- httr::content(contributors_response, "text", encoding = "UTF-8")
      
      tryCatch({
        contributors_data <- jsonlite::fromJSON(contributors_content)
        
        if (length(contributors_data) > 0) {
          contributors <- contributors_data
          cat("Fant contributors via dedikert API-endepunkt\n")
        }
      }, error = function(e) {
        cat("Feil ved parsing av contributors-data:", e$message, "\n")
      })
    }
  }
  
  # Sjekk om vi fant contributors
  if (is.null(contributors) || length(contributors) == 0) {
    cat("Ingen contributors funnet i testen.\n")
    return(list(test_info = test_df))
  }
  
  # Konverter til dataframe hvis det ikke allerede er det
  if (!is.data.frame(contributors)) {
    tryCatch({
      contributors <- as.data.frame(contributors)
      cat("Konverterte contributors til dataframe.\n")
    }, error = function(e) {
      cat("Kunne ikke konvertere contributors til dataframe:", e$message, "\n")
      return(list(test_info = test_df))
    })
  }
  
  cat("Fant", nrow(contributors), "contributors i testen.\n")
  
  # Hvis roles er en liste, konverter til kommaseparert streng
  if ("roles" %in% names(contributors) && is.list(contributors$roles)) {
    contributors$roles <- sapply(contributors$roles, function(x) {
      if (length(x) == 0) return(NA)
      paste(x, collapse = ", ")
    })
  }
  
  # Finn og legg til FEIDE-identifikatorer
  cat("\nIdentifiserer korrekte FEIDE-brukernavn for bidragsytere...\n")
  
  # Opprett dataframe for å lagre bidragsytere med FEIDE-info
  contributors_with_feide <- contributors
  
  # Legg til kolonner for FEIDE-informasjon
  contributors_with_feide$feideUsername <- NA_character_
  contributors_with_feide$feideSource <- NA_character_
  
  # Først, gruppér bidragsytere etter navn for å identifisere duplikater
  contributors_grouped <- list()
  
  for (i in 1:nrow(contributors)) {
    contrib <- contributors[i, ]
    
    # Bruk navn som nøkkel for gruppering
    name_key <- paste(contrib$firstName, contrib$lastName)
    
    if (!(name_key %in% names(contributors_grouped))) {
      contributors_grouped[[name_key]] <- list()
    }
    
    contributors_grouped[[name_key]][[length(contributors_grouped[[name_key]]) + 1]] <- contrib
  }
  
  # Finn FEIDE-brukernavn for hver gruppe og legg det til i bidragsyterne
  for (name_key in names(contributors_grouped)) {
    feide_info <- find_feide_username(contributors_grouped[[name_key]])
    
    # Finn indeksene for denne gruppen i den originale dataframen
    indices <- which(paste(contributors_with_feide$firstName, contributors_with_feide$lastName) == name_key)
    
    # Oppdater FEIDE-informasjonen for alle bidragsytere i denne gruppen
    contributors_with_feide$feideUsername[indices] <- feide_info$username
    contributors_with_feide$feideSource[indices] <- feide_info$source
  }
  
  # Legg til test_id i datasettet
  contributors_with_feide$test_id <- test_id
  
  # Lag en ny dataframe for detaljert visning av externalUserIds
  external_ids_detail <- data.frame(
    userId = character(),
    firstName = character(),
    lastName = character(),
    userName = character(),
    authType = character(),
    externalUserId = character(),
    feideUsername = character(),
    test_id = character(),
    stringsAsFactors = FALSE
  )
  
  # Prosesser externalUserIds for hver bidragsyter
  for (i in 1:nrow(contributors)) {
    contrib <- contributors[i, ]
    
    # Hent grunnleggende brukerinformasjon
    user_id <- as.character(ifelse("userId" %in% names(contrib), contrib$userId, NA))
    first_name <- ifelse("firstName" %in% names(contrib), contrib$firstName, NA)
    last_name <- ifelse("lastName" %in% names(contrib), contrib$lastName, NA)
    user_name <- ifelse("userName" %in% names(contrib), contrib$userName, NA)
    feide_username <- contributors_with_feide$feideUsername[i]
    
    # Behandle externalUserIds
    if ("externalUserIds" %in% names(contrib) && 
        is.list(contrib$externalUserIds) && 
        length(contrib$externalUserIds) > 0) {
      
      for (j in seq_along(contrib$externalUserIds)) {
        ext_id <- contrib$externalUserIds[[j]]
        
        if (is.list(ext_id) && 
            "authType" %in% names(ext_id) && 
            "externalUserId" %in% names(ext_id)) {
          
          # Legg til en rad i dataframen
          external_ids_detail <- rbind(external_ids_detail, data.frame(
            userId = user_id,
            firstName = first_name,
            lastName = last_name,
            userName = user_name,
            authType = ext_id$authType,
            externalUserId = ext_id$externalUserId,
            feideUsername = feide_username,
            test_id = test_id,
            stringsAsFactors = FALSE
          ))
        }
      }
    } else {
      # Hvis det ikke finnes externalUserIds, legg til en rad med bare brukerinformasjon
      external_ids_detail <- rbind(external_ids_detail, data.frame(
        userId = user_id,
        firstName = first_name,
        lastName = last_name,
        userName = user_name,
        authType = NA,
        externalUserId = NA,
        feideUsername = feide_username,
        test_id = test_id,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Lagre bidragsytere med FEIDE-informasjon
  feide_contributors_filename <- paste0("test_", test_id, "_contributors_feide.csv")
  readr::write_csv(contributors_with_feide, feide_contributors_filename)
  cat("Bidragsytere med FEIDE-informasjon lagret til", feide_contributors_filename, "\n")
  
  # Lagre detaljert externalUserIds-informasjon
  external_ids_filename <- paste0("test_", test_id, "_external_ids_detail.csv")
  readr::write_csv(external_ids_detail, external_ids_filename)
  cat("Detaljert externalUserIds-informasjon lagret til", external_ids_filename, "\n")
  
  # Lagre en enkel CSV med bare det essensielle for å legge til bidragsytere
  essential_info <- data.frame(
    firstName = contributors_with_feide$firstName,
    lastName = contributors_with_feide$lastName,
    feideUsername = contributors_with_feide$feideUsername,
    stringsAsFactors = FALSE
  )
  
  essential_info_filename <- paste0("test_", test_id, "_essential_info.csv")
  readr::write_csv(essential_info, essential_info_filename)
  cat("Essensiell bidragsyterinformasjon lagret til", essential_info_filename, "\n")
  
  # Skriv ut en oversikt
  cat("\n==== BIDRAGSYTER OVERSIKT ====\n")
  cat("Test ID:", test_id, "\n")
  cat("Test Navn:", ifelse(!is.null(test_info$name), test_info$name, "Ukjent"), "\n")
  cat("Totalt antall bidragsytere:", nrow(contributors), "\n")
  cat("Antall med identifisert FEIDE-brukernavn:", sum(!is.na(contributors_with_feide$feideUsername)), "\n")
  
  cat("\n==== KORREKTE FEIDE-BRUKERNAVN FOR OVERFØRING ====\n")
  
  # Lag en dataframe med unike bidragsytere (basert på navn) og deres FEIDE-brukernavn
  unique_contributors <- unique(contributors_with_feide[, c("firstName", "lastName", "feideUsername", "feideSource")])
  
  for (i in 1:nrow(unique_contributors)) {
    cat(paste0(i, ". ", unique_contributors$firstName[i], " ", unique_contributors$lastName[i], ": "))
    
    if (!is.na(unique_contributors$feideUsername[i])) {
      cat(unique_contributors$feideUsername[i], " (", unique_contributors$feideSource[i], ")\n")
    } else {
      cat("FEIDE-brukernavn ikke funnet!\n")
    }
  }
  
  # Returner resultatene
  return(list(
    test_info = test_df,
    contributors = contributors,
    contributors_with_feide = contributors_with_feide,
    external_ids_detail = external_ids_detail,
    essential_info = essential_info,
    token = token
  ))
}

#' Finn FEIDE-brukernavn fra en liste med bidragsytere
#'
#' @param contributors_list Liste med bidragsytere
#' @return En liste med brukernavn og kilde for identifikasjonen
#' @export
find_feide_username <- function(contributors_list) {
  # Først, sjekk om noen av bidragsyterne har FEIDE i externalUserIds
  for (contrib in contributors_list) {
    if ("externalUserIds" %in% names(contrib) && 
        is.list(contrib$externalUserIds) && 
        length(contrib$externalUserIds) > 0) {
      
      for (ext_id in contrib$externalUserIds) {
        if (is.list(ext_id) && 
            "authType" %in% names(ext_id) && 
            !is.null(ext_id$authType) && 
            toupper(ext_id$authType) == "FEIDE") {
          
          # Returner FEIDE-brukernavnet direkte fra userName-feltet hvis tilgjengelig
          if ("userName" %in% names(contrib) && !is.null(contrib$userName) && contrib$userName != "") {
            return(list(username = contrib$userName, source = "userName fra FEIDE-autentisert bruker"))
          }
        }
      }
    }
  }
  
  # Deretter, sjekk etter brukernavn som ikke inneholder @ (typisk FEIDE-format)
  for (contrib in contributors_list) {
    if ("userName" %in% names(contrib) && 
        !is.null(contrib$userName) && 
        contrib$userName != "" && 
        !grepl("@", contrib$userName)) {
      
      # Kontroller om det har INSPERA som authType - da er det sannsynligvis ikke FEIDE
      has_inspera_auth <- FALSE
      
      if ("externalUserIds" %in% names(contrib) && 
          is.list(contrib$externalUserIds) && 
          length(contrib$externalUserIds) > 0) {
        
        for (ext_id in contrib$externalUserIds) {
          if (is.list(ext_id) && 
              "authType" %in% names(ext_id) && 
              !is.null(ext_id$authType) && 
              toupper(ext_id$authType) == "INSPERA") {
            
            has_inspera_auth <- TRUE
            break
          }
        }
      }
      
      if (!has_inspera_auth) {
        return(list(username = contrib$userName, source = "userName uten @-tegn"))
      }
    }
  }
  
  # Som siste utvei, returner det første brukernavnet vi finner
  for (contrib in contributors_list) {
    if ("userName" %in% names(contrib) && 
        !is.null(contrib$userName) && 
        contrib$userName != "") {
      
      # Hvis det er en e-post, ta brukerdelen før @
      if (grepl("@", contrib$userName)) {
        username_part <- sub("@.*$", "", contrib$userName)
        return(list(username = username_part, source = "e-post prefix fra userName"))
      } else {
        return(list(username = contrib$userName, source = "userName (fallback)"))
      }
    }
  }
  
  # Hvis vi ikke fant noe brukernavn, returner NULL
  return(list(username = NA_character_, source = "ikke funnet"))
}

#' Opprett komiteer i en test
#'
#' @param test_id ID-en til testen
#' @param yaml_path Sti til YAML-fil med komiténavn
#' @param api_key API-nøkkel for Inspera
#' @return En liste med resultater for hver komité
#' @importFrom httr POST GET add_headers content status_code
#' @importFrom yaml read_yaml
#' @importFrom jsonlite fromJSON
#' @export
create_committees <- function(test_id, yaml_path, api_key = NULL) {
  # Få faktisk API-nøkkel
  actual_api_key <- get_api_key(api_key)
  
  cat("Starting committee creation for test", test_id, "...\n")
  
  # Read YAML file
  cat("Reading YAML file:", yaml_path, "\n")
  yaml_data <- yaml::read_yaml(yaml_path)
  
  if (!("contributors" %in% names(yaml_data)) || length(yaml_data$contributors) == 0) {
    stop("No contributors found in the YAML file")
  }
  
  # Get unique committees from contributors
  committees <- unique(sapply(yaml_data$contributors, function(x) {
    if ("committee" %in% names(x)) x$committee else "Default"
  }))
  committees <- committees[!is.na(committees) & committees != ""] # Fjern tomme verdier
  
  cat("Found", length(committees), "unique committees in YAML file:", paste(committees, collapse=", "), "\n")
  
  # Authentication
  auth_result <- authenticate_inspera(actual_api_key)
  token <- auth_result$token
  
  # Check existing committees
  test_info <- get_test_info(test_id, token)
  test_data <- test_info$test_info
  
  existing_committees <- c()
  
  if (!is.null(test_data$committees) && length(test_data$committees) > 0) {
    cat("\nExisting committees found:\n")
    print(test_data$committees)
    
    if (is.data.frame(test_data$committees) && "name" %in% names(test_data$committees)) {
      existing_committees <- test_data$committees$name
    } else if (is.list(test_data$committees)) {
      existing_committees <- sapply(test_data$committees, function(x) {
        if (is.list(x) && "name" %in% names(x)) return(x$name)
        else return(NULL)
      })
      existing_committees <- unlist(existing_committees)
    }
    
    cat("Existing committee names:", paste(existing_committees, collapse=", "), "\n")
  } else {
    cat("\nNo existing committees found\n")
  }
  
  # Create committees
  results <- list()
  
  for (committee_name in committees) {
    if (committee_name %in% existing_committees) {
      cat("Committee", committee_name, "already exists, skipping...\n")
      results[[committee_name]] <- list(
        name = committee_name,
        status = "already exists",
        success = TRUE
      )
      next
    }
    
    # Use the fallback method directly since it works despite the "Wrong committee type" error
    cat("\nCreating committee", committee_name, "for test", test_id, "...\n")
    
    # Payload with committees wrapper - this works despite the error
    committee_payload <- list(
      committees = list(list(
        name = committee_name,
        type = "GRADING"  # Changed from EVALUATE to GRADING
      ))
    )
    
    committee_url <- sprintf("https://nokut.inspera.no/api/v1/test/%s/committees", test_id)
    
    committee_response <- httr::POST(
      url = committee_url,
      httr::add_headers(
        "Authorization" = paste("Bearer", token),
        "Content-Type" = "application/json",
        "Accept" = "application/json"
      ),
      body = committee_payload,
      encode = "json"
    )
    
    # Check response
    status <- httr::status_code(committee_response)
    content <- httr::content(committee_response, "text", encoding = "UTF-8")
    
    cat("Status:", status, "\n")
    cat("Response:", content, "\n")
    
    results[[committee_name]] <- list(
      name = committee_name,
      status = status,
      success = (status == 200 || status == 201 || status == 204),
      response = content
    )
    
    if (status == 200 || status == 201 || status == 204) {
      cat("Committee", committee_name, "created successfully!\n")
    } else {
      cat("Failed to create committee", committee_name, "\n")
    }
  }
  
  # Wait for committee creation to take effect
  cat("\nWaiting 5 seconds for committees to be processed...\n")
  Sys.sleep(5)
  
  # Summary
  cat("\nCommittee creation summary:\n")
  successful <- sum(sapply(results, function(x) x$success))
  failed <- length(results) - successful
  
  cat("Successful:", successful, "\n")
  cat("Failed:", failed, "\n")
  cat("Total:", length(results), "\n")
  
  if (failed > 0) {
    cat("\nFailed committees:\n")
    for (name in names(results)) {
      if (!results[[name]]$success) {
        cat(name, "-", results[[name]]$status, "-", results[[name]]$response, "\n")
      }
    }
  }
  
  return(invisible(list(
    token = token,
    results = results
  )))
}

#' Tilordne bidragsytere til en test med komiteer
#'
#' @param test_id ID-en til testen
#' @param yaml_path Sti til YAML-fil med bidragsytere
#' @param api_key API-nøkkel for Inspera
#' @param token Tilgangstoken (hvis ikke oppgitt, blir det hentet automatisk)
#' @return En liste med resultater fra tilordningen
#' @importFrom httr POST GET add_headers content status_code
#' @importFrom yaml read_yaml
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_dfr
#' @export
assign_contributors <- function(test_id, yaml_path, api_key = NULL, token = NULL) {
  # Få faktisk API-nøkkel hvis token ikke er angitt
  if (is.null(token)) {
    actual_api_key <- get_api_key(api_key)
  }
  
  cat("Starting contributor assignment for test", test_id, "...\n")
  
  # Read and process YAML
  cat("Reading YAML file:", yaml_path, "\n")
  yaml_data <- yaml::read_yaml(yaml_path)
  
  if (!("contributors" %in% names(yaml_data)) || length(yaml_data$contributors) == 0) {
    stop("No contributors found in the YAML file")
  }
  
  # Convert YAML data to dataframe
  contributors_df <- purrr::map_dfr(yaml_data$contributors, function(contributor) {
    data.frame(
      externalId = contributor$externalId,
      firstName = contributor$firstName,
      lastName = contributor$lastName,
      email = contributor$email,
      roomName = if ("roomName" %in% names(contributor)) contributor$roomName else "Digital",
      buildingName = if ("buildingName" %in% names(contributor)) contributor$buildingName else "NOKUT",
      committee = if ("committee" %in% names(contributor)) contributor$committee else NA,
      role = tolower(if ("role" %in% names(contributor)) contributor$role else "evaluate"),
      authenticationSystem = if ("authenticationSystem" %in% names(contributor)) {
        contributor$authenticationSystem
      } else {
        "FEIDE"  # Default to FEIDE
      },
      stringsAsFactors = FALSE
    )
  })
  
  cat("Found", nrow(contributors_df), "contributors in YAML file\n")
  
  # Authenticate if token not provided
  if (is.null(token)) {
    auth_result <- authenticate_inspera(actual_api_key)
    token <- auth_result$token
  } else {
    cat("Using provided authentication token\n")
  }
  
  # Group contributors by authentication system
  auth_systems <- unique(contributors_df$authenticationSystem)
  
  # Process each authentication system
  for (auth_system in auth_systems) {
    cat("\nProcessing contributors with authentication system:", auth_system, "\n")
    
    # Filter contributors for this auth system
    system_contributors <- contributors_df[contributors_df$authenticationSystem == auth_system, ]
    cat("Number of contributors with auth system", auth_system, ":", nrow(system_contributors), "\n")
    
    # Create payload with committee assignments
    contributor_payload <- list(
      authenticationSystem = auth_system,
      contributors = lapply(1:nrow(system_contributors), function(i) {
        contributor <- system_contributors[i, ]
        
        # Always include 'evaluate' role, and add 'monitor' if that role is specified
        roles_list <- list("evaluate")  # Always include evaluate role
        if (tolower(contributor$role) == "monitor") {
          roles_list <- c(roles_list, "monitor")  # Add monitor role if specified
        }
        
        # Build basic contributor data
        result <- list(
          externalId = as.character(contributor$externalId),
          firstName = contributor$firstName,
          lastName = contributor$lastName,
          email = contributor$email,
          roomName = contributor$roomName,
          buildingName = contributor$buildingName,
          sendNotification = FALSE,
          roles = roles_list
        )
        
        # Add committee if specified
        if (!is.na(contributor$committee) && contributor$committee != "") {
          result$committeesName <- list(contributor$committee)
        }
        
        return(result)
      })
    )
    
    # Send the API request
    url <- sprintf("https://nokut.inspera.no/api/v1/test/%s/contributors", test_id)
    
    cat("\nSending contributor data to Inspera API:", url, "\n")
    cat("Number of contributors being sent:", length(contributor_payload$contributors), "\n")
    
    # Example contributor for logging
    if (length(contributor_payload$contributors) > 0) {
      cat("Example contributor (first in list):\n")
      example <- contributor_payload$contributors[[1]]
      cat("  Name:", example$firstName, example$lastName, "\n")
      cat("  ExternalId:", example$externalId, "\n")
      cat("  Email:", example$email, "\n")
      cat("  Roles:", paste(unlist(example$roles), collapse=", "), "\n")
      if ("committeesName" %in% names(example)) {
        cat("  Committee:", example$committeesName[[1]], "\n")
      } else {
        cat("  Committee: None\n")
      }
    }
    
    response <- httr::POST(
      url = url,
      httr::add_headers(
        "Authorization" = paste("Bearer", token),
        "Content-Type" = "application/json",
        "Accept" = "application/json"
      ),
      body = contributor_payload,
      encode = "json"
    )
    
    # Print results
    cat("\nAPI Response:\n")
    cat("Status:", httr::status_code(response), "\n")
    
    # Get content
    content <- httr::content(response, as = "text")
    cat("Content:", content, "\n")
    
    # Parse the response
    if (httr::status_code(response) == 200) {
      tryCatch({
        parsed_content <- jsonlite::fromJSON(content)
        
        if (!is.null(parsed_content$errors)) {
          cat("\nWarning: API returned errors with status 200:\n")
          print(parsed_content$errors)
        } else {
          cat("\nSuccessfully added contributors with auth system", auth_system, "\n")
        }
      }, error = function(e) {
        if (content == "{\"success\":true}") {
          cat("\nSuccessfully added contributors with auth system", auth_system, "\n")
        } else {
          cat("\nFailed to parse response JSON:", e$message, "\n")
        }
      })
    } else {
      cat("\nFailed to add contributors with auth system", auth_system, "\n")
    }
  }
  
  # Wait for contributor assignment to take effect
  cat("\nWaiting 5 seconds for contributor assignment to be processed...\n")
  Sys.sleep(5)
  
  # Verify results
  cat("\nVerifying contributor assignment...\n")
  
  # Get test details to check contributors
  test_info <- get_test_info(test_id, token)
  verify_data <- test_info$test_info
  
  if (!is.null(verify_data$contributors) && length(verify_data$contributors) > 0) {
    cat("\nContributors assigned to test:\n")
    
    # Display contributor information
    contributor_info <- verify_data$contributors
    display_cols <- intersect(c("userId", "firstName", "lastName", "userName", "roles"), names(contributor_info))
    print(contributor_info[display_cols])
    cat("\nTotal contributors:", nrow(contributor_info), "\n")
    
    # Check committee assignments if available
    if ("committees" %in% names(contributor_info)) {
      cat("\nCommittee assignments:\n")
      for (i in 1:nrow(contributor_info)) {
        name <- paste(contributor_info$firstName[i], contributor_info$lastName[i])
        committees <- contributor_info$committees[[i]]
        
        if (length(committees) > 0) {
          cat("  -", name, "->", paste(committees, collapse=", "), "\n")
        } else {
          cat("  -", name, "-> No committee\n")
        }
      }
    }
    
    # Check if all contributors were added
    contributor_names <- paste(contributor_info$firstName, contributor_info$lastName)
    yaml_names <- paste(contributors_df$firstName, contributors_df$lastName)
    
    missing_names <- yaml_names[!yaml_names %in% contributor_names]
    if (length(missing_names) > 0) {
      cat("\nWARNING: The following contributors from YAML were not added:\n")
      for (name in missing_names) {
        cat("  -", name, "\n")
      }
    } else {
      cat("\nAll contributors were successfully added!\n")
    }
  } else {
    cat("\nNo contributors found in test\n")
  }
  
  cat("\nProcess completed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Return results
  invisible(list(
    status = httr::status_code(response),
    success = (httr::status_code(response) == 200)
  ))
}