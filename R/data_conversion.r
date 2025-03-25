#' Konverter contributors CSV til YAML med komiténavngivning
#'
#' Denne funksjonen konverterer en CSV-fil med bidragsytere til YAML-format
#' og lar deg angi komitétilordninger manuelt.
#'
#' @param contributors_csv Sti til CSV-filen med bidragsytere
#' @param output_dir Mappe for utdatafiler (hvis NULL, brukes samme mappe som CSV-filen)
#' @param default_committee Standard komiténavn for bidragsytere uten komitétilordning
#' @param manual_committee_mapping Liste med manuell mapping av bidragsytere til komiteer
#' @return En liste med YAML-struktur og sti til utdatafilen
#' @importFrom readr read_csv write_csv
#' @importFrom yaml read_yaml
#' @export
contributors_feide_csv_to_yaml <- function(contributors_csv, output_dir = NULL, default_committee = "Default", manual_committee_mapping = NULL) {
  # Les inn CSV-filen
  cat("Leser contributors-data fra", contributors_csv, "...\n")
  contributors_df <- readr::read_csv(contributors_csv)
  
  # Hent test_id fra datasettet hvis tilgjengelig
  test_id <- NULL
  if ("test_id" %in% names(contributors_df)) {
    test_id <- contributors_df$test_id[1]
    cat("Fant test_id:", test_id, "\n")
  }
  
  # Sjekk at vi har nødvendige kolonner
  required_columns <- c("firstName", "lastName")
  missing_columns <- required_columns[!required_columns %in% names(contributors_df)]
  
  if (length(missing_columns) > 0) {
    cat("Advarsel: Følgende påkrevde kolonner mangler:", paste(missing_columns, collapse=", "), "\n")
    return(NULL)
  }
  
  # Sjekk om vi har FEIDE-brukernavn
  has_feide_username <- "feideUsername" %in% names(contributors_df)
  
  if (!has_feide_username) {
    cat("Advarsel: Kolonnen 'feideUsername' mangler. Vil bruke alternative kilder for externalId.\n")
  } else {
    cat("Fant 'feideUsername'-kolonne. Vil bruke denne som externalId for FEIDE-autentisering.\n")
    cat("FEIDE-brukernavn funnet for", sum(!is.na(contributors_df$feideUsername)), "av", nrow(contributors_df), "bidragsytere.\n")
  }
  
  # Opprett contributors-liste for YAML
  contributors_list <- list()
  
  cat("Behandler", nrow(contributors_df), "contributors...\n")
  
  # Grupper etter navn for å unngå duplikater
  unique_names <- unique(contributors_df[, c("firstName", "lastName")])
  
  for (i in 1:nrow(unique_names)) {
    first_name <- unique_names$firstName[i]
    last_name <- unique_names$lastName[i]
    
    # Finn alle rader for denne brukeren
    user_rows <- which(contributors_df$firstName == first_name & contributors_df$lastName == last_name)
    user_data <- contributors_df[user_rows[1], ]  # Ta den første raden for denne brukeren
    
    # Hent e-post
    email <- if ("email" %in% names(user_data) && !is.na(user_data$email)) {
      user_data$email
    } else if ("userName" %in% names(user_data) && !is.na(user_data$userName) && grepl("@", user_data$userName)) {
      user_data$userName
    } else {
      # Generer en e-post basert på navn
      paste0(tolower(gsub("[^a-zA-Z0-9]", "", first_name)), ".",
             tolower(gsub("[^a-zA-Z0-9]", "", last_name)), "@nokut.no")
    }
    
    # Bestem externalId basert på FEIDE-brukernavn
    externalId <- NA
    auth_system <- "EMAIL"  # Standard
    
    if (has_feide_username && !is.na(user_data$feideUsername)) {
      externalId <- user_data$feideUsername
      auth_system <- "FEIDE"
    } else if ("userName" %in% names(user_data) && !is.na(user_data$userName) && !grepl("@", user_data$userName)) {
      # Hvis userName ikke inneholder @, det kan være et FEIDE-brukernavn
      externalId <- user_data$userName
      auth_system <- "FEIDE"
    } else if ("email" %in% names(user_data) && !is.na(user_data$email)) {
      # Bruk e-post hvis ingenting annet er tilgjengelig
      externalId <- user_data$email
      auth_system <- "EMAIL"
    } else {
      # Fallback til generert e-post
      externalId <- email
      auth_system <- "EMAIL"
    }
    
    # Finn rolle
    role <- "EVALUATE"  # Standard rolle
    
    if ("roles" %in% names(user_data)) {
      roles_text <- user_data$roles
      if (!is.na(roles_text)) {
        is_evaluator <- grepl("evaluat|ar_evaluate", tolower(roles_text))
        role <- if (is_evaluator) "EVALUATE" else "MONITOR"
      }
    }
    
    # Finn komité
    committee <- default_committee
    
    # Sjekk om det finnes en manuell mapping for denne brukeren
    user_key <- paste(first_name, last_name)
    if (!is.null(manual_committee_mapping) && user_key %in% names(manual_committee_mapping)) {
      committee <- manual_committee_mapping[[user_key]]
      cat("Bruker manuell komité-mapping for", user_key, ":", committee, "\n")
    } else if ("committeesName" %in% names(user_data) && !is.na(user_data$committeesName)) {
      if (is.list(user_data$committeesName) && length(user_data$committeesName) > 0) {
        committee <- user_data$committeesName[[1]]
      } else if (is.character(user_data$committeesName)) {
        committee <- user_data$committeesName
      }
    }
    
    # Finn rommet
    roomName <- if ("roomName" %in% names(user_data) && !is.na(user_data$roomName)) {
      user_data$roomName
    } else {
      "Digital"
    }
    
    # Opprett bidragsyterobjekt
    contributor <- list(
      email = email,
      externalId = externalId,
      firstName = first_name,
      lastName = last_name,
      roomName = roomName,
      buildingName = "NOKUT",
      role = role,
      committee = committee,
      exact_tests = 1.0,  # Standard antall tester
      authenticationSystem = auth_system  # Legg til autentiseringssystem
    )
    
    contributors_list[[length(contributors_list) + 1]] <- contributor
  }
  
  # Opprett YAML-struktur
  yaml_structure <- list(contributors = contributors_list)
  
  # Bestem utdatafilen
  if (is.null(output_dir)) {
    output_dir <- dirname(contributors_csv)
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  yaml_path <- if (!is.null(test_id)) {
    file.path(output_dir, paste0("test_", test_id, "_contributors_feide.yml"))
  } else {
    file.path(output_dir, paste0(basename(tools::file_path_sans_ext(contributors_csv)), "_feide.yml"))
  }
  
  # Skriv YAML til fil
  yaml_text <- "contributors:\n"
  for (contributor in yaml_structure$contributors) {
    yaml_text <- paste0(yaml_text, "- email: ", contributor$email, "\n",
                       "  externalId: ", contributor$externalId, "\n",
                       "  firstName: ", contributor$firstName, "\n",
                       "  lastName: ", contributor$lastName, "\n",
                       "  roomName: ", contributor$roomName, "\n",
                       "  buildingName: ", contributor$buildingName, "\n",
                       "  role: ", contributor$role, "\n",
                       "  committee: ", contributor$committee, "\n",
                       "  exact_tests: ", format(contributor$exact_tests, nsmall = 1), "\n",
                       "  authenticationSystem: ", contributor$authenticationSystem, "\n")
  }
  
  writeLines(yaml_text, yaml_path)
  
  cat("YAML-fil opprettet:", yaml_path, "\n")
  
  # Vis et utdrag av YAML-filen
  cat("\nUtdrag av YAML-filen (første", min(3, length(contributors_list)), "bidragsytere):\n")
  
  preview_count <- min(3, length(contributors_list))
  preview_yaml <- "contributors:\n"
  
  for (i in 1:preview_count) {
    contributor <- contributors_list[[i]]
    preview_yaml <- paste0(preview_yaml, "- email: ", contributor$email, "\n",
                          "  externalId: ", contributor$externalId, "\n",
                          "  firstName: ", contributor$firstName, "\n",
                          "  lastName: ", contributor$lastName, "\n",
                          "  roomName: ", contributor$roomName, "\n",
                          "  buildingName: ", contributor$buildingName, "\n",
                          "  role: ", contributor$role, "\n",
                          "  committee: ", contributor$committee, "\n",
                          "  exact_tests: ", format(contributor$exact_tests, nsmall = 1), "\n",
                          "  authenticationSystem: ", contributor$authenticationSystem, "\n")
  }
  
  cat(preview_yaml)
  
  # Lag også en forenklet CSV for manuell import
  simple_csv <- data.frame(
    firstName = sapply(contributors_list, function(x) x$firstName),
    lastName = sapply(contributors_list, function(x) x$lastName),
    email = sapply(contributors_list, function(x) x$email),
    externalId = sapply(contributors_list, function(x) x$externalId),
    authenticationSystem = sapply(contributors_list, function(x) x$authenticationSystem),
    committee = sapply(contributors_list, function(x) x$committee),
    role = sapply(contributors_list, function(x) x$role),
    stringsAsFactors = FALSE
  )
  
  csv_path <- if (!is.null(test_id)) {
    file.path(output_dir, paste0("test_", test_id, "_contributors_import.csv"))
  } else {
    file.path(output_dir, paste0(basename(tools::file_path_sans_ext(contributors_csv)), "_import.csv"))
  }
  
  readr::write_csv(simple_csv, csv_path)
  cat("Enkel importfil opprettet:", csv_path, "\n")
  
  return(list(
    yaml_structure = yaml_structure,
    yaml_path = yaml_path
  ))
}