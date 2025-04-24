#' Bestill og last ned fil med brukerdata fra Inspera
#'
#' @param user_type Type brukere som skal hentes. API-et støtter kun "admin", "student" eller "all".
#' @param token Tilgangstoken (hvis ikke oppgitt, blir det hentet automatisk)
#' @param api_key API-nøkkel for Inspera (brukes kun hvis token ikke er oppgitt)
#' @param format Filformat for eksporten (default: "csv")
#' @param wait_time Hvor lenge skal vi vente mellom sjekkene på eksportstatus (sekunder)
#' @param max_attempts Maksimalt antall forsøk på å sjekke status
#' @param save_path Sti til hvor filen skal lagres (valgfri)
#' @param debug Om detaljert debug-informasjon skal skrives ut
#' @return En data.frame med brukerdata
#' @importFrom httr POST GET add_headers content status_code
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils read.csv
#' @export
get_users <- function(user_type = "admin", token = NULL, api_key = NULL, format = "csv", 
                      wait_time = NULL, max_attempts = NULL, save_path = NULL, 
                      debug = TRUE) {
  
  # Valider brukertypen - API støtter kun "admin" og "student"
  valid_types <- c("admin", "student", "all")
  user_type_lower <- tolower(user_type)
  
  if (!user_type_lower %in% valid_types) {
    stop(paste("Ugyldig brukertype:", user_type, "- API-et støtter kun brukertyper:", paste(valid_types, collapse = ", ")))
  }
  
  # Sett standardverdier for wait_time og max_attempts basert på brukertype
  if (is.null(wait_time)) {
    wait_time <- switch(user_type_lower,
                        "admin" = 5,       # Raskere for admin
                        "student" = 10,    # Lengre for studenter
                        "all" = 15)        # Lengst for alle
  }
  
  if (is.null(max_attempts)) {
    max_attempts <- switch(user_type_lower,
                         "admin" = 20,     # Færre forsøk for admin (ca 1.5 min)
                         "student" = 60,   # Flere forsøk for studenter (10 min)
                         "all" = 120)      # Flest forsøk for alle (30 min)
  }
  
  # Angi total maksimal ventetid
  total_wait_time <- wait_time * max_attempts
  cat("Maksimal ventetid:", total_wait_time, "sekunder (ca", round(total_wait_time/60, 1), "minutter)\n")
  
  # Autentiser hvis token ikke er oppgitt
  if (is.null(token)) {
    auth_result <- authenticate_inspera(api_key)
    token <- auth_result$token
  }
  
  # API-endepunkt for fileksport
  export_url <- "https://nokut.inspera.no/api/v1/fileExport/order"
  
  # Lag forespørselsparametere
  export_params <- list(
    resourceType = "AllUsers",
    parameters = list(),
    expiryPeriod = 24  # Timer filen er tilgjengelig
  )
  
  # Legg til userType hvis spesifisert og det ikke er "all"
  if (user_type_lower != "all") {
    export_params$parameters$userType <- user_type_lower
  }
  
  if (debug) {
    cat("API URL:", export_url, "\n")
    cat("Request body:", jsonlite::toJSON(export_params, auto_unbox = TRUE), "\n")
  }
  
  cat("Bestiller eksport av", ifelse(user_type_lower == "all", "alle brukere", paste(user_type, "brukere")), "...\n")
  
  # Send bestilling på eksport
  export_response <- httr::POST(
    url = export_url,
    body = export_params,
    encode = "json",
    httr::add_headers(
      "Authorization" = paste("Bearer", token),
      "Accept" = "application/json",
      "Content-Type" = "application/json"
    )
  )
  
  if (debug) {
    cat("Response status:", httr::status_code(export_response), "\n")
    cat("Response body:", httr::content(export_response, "text", encoding = "UTF-8"), "\n")
  }
  
  if (httr::status_code(export_response) != 200) {
    cat("Kunne ikke bestille eksport. Status:", httr::status_code(export_response), "\n")
    response_text <- httr::content(export_response, "text", encoding = "UTF-8")
    cat("Respons:", response_text, "\n")
    stop("Eksportbestilling feilet")
  }
  
  # Hent jobbID fra responsen
  order_content <- httr::content(export_response)
  job_id <- order_content$jobId
  
  if (is.null(job_id)) {
    stop("Ingen jobb-ID mottatt fra API")
  }
  
  cat("Eksport bestilt med jobb-ID:", job_id, "\n")
  
  # Hent callback URL fra responsen eller bygg den selv
  if (!is.null(order_content$callbackUrl)) {
    status_url <- order_content$callbackUrl
  } else {
    # Fallback til å bygge URL selv
    status_url <- sprintf("https://nokut.inspera.no/api/v1/fileExport/status/%s", job_id)
  }
  
  cat("Venter på at eksporten skal bli ferdig...\n")
  
  if (debug) {
    cat("Status URL:", status_url, "\n")
  }
  
  # Vent til eksporten er ferdig
  attempts <- 0
  export_ready <- FALSE
  download_url <- NULL
  
  while (!export_ready && attempts < max_attempts) {
    attempts <- attempts + 1
    
    Sys.sleep(wait_time)
    
    status_response <- httr::GET(
      url = status_url,
      httr::add_headers(
        "Authorization" = paste("Bearer", token),
        "Accept" = "application/json"
      )
    )
    
    if (debug) {
      cat("Status response code:", httr::status_code(status_response), "\n")
      if (attempts < 3 || attempts > max_attempts - 3 || attempts %% 10 == 0) { 
        # Vis full respons for første, siste og hver 10. forespørsel
        cat("Status response:", httr::content(status_response, "text", encoding = "UTF-8"), "\n")
      }
    }
    
    if (httr::status_code(status_response) != 200) {
      cat("Kunne ikke sjekke eksportstatus. Status:", httr::status_code(status_response), "\n")
      next
    }
    
    status_content <- httr::content(status_response)
    current_status <- status_content$status
    
    # Vis progress om tilgjengelig
    progress_text <- ""
    if (!is.null(status_content$progress)) {
      progress_text <- paste(" - Fremdrift:", status_content$progress, "%")
    }
    
    cat("Eksportstatus (forsøk", attempts, "av", max_attempts, "):", current_status, progress_text, "\n")
    
    # Sjekk om jobben er ferdig og om vi har en nedlastingslenke
    if (tolower(current_status) == "success" && !is.null(status_content$exportInfo) && 
        !is.null(status_content$exportInfo$signedResponseUrl)) {
      export_ready <- TRUE
      download_url <- status_content$exportInfo$signedResponseUrl
      cat("Eksport ferdig! Laster ned fil...\n")
      break
    } else if (tolower(current_status) %in% c("error", "failed")) {
      stop("Eksportjobben feilet")
    }
  }
  
  if (!export_ready) {
    # Gi en mer informativ feilmelding med forslag til løsning
    stop(paste0("Tidsavbrudd ved venting på eksport etter ", attempts, " forsøk (", 
                round(attempts * wait_time / 60, 1), " minutter). ",
                "Prøv å øke wait_time og/eller max_attempts parameterene, f.eks.: \n",
                "get_users('", user_type, "', wait_time = ", wait_time * 2, ", max_attempts = ", max_attempts * 2, ")"))
  }
  
  # Last ned filen
  if (is.null(download_url)) {
    stop("Ingen nedlastings-URL mottatt")
  }
  
  if (debug) {
    cat("Download URL:", download_url, "\n")
  }
  
  download_response <- httr::GET(
    url = download_url
  )
  
  if (debug) {
    cat("Download response status:", httr::status_code(download_response), "\n")
  }
  
  if (httr::status_code(download_response) != 200) {
    cat("Kunne ikke laste ned fil. Status:", httr::status_code(download_response), "\n")
    stop("Nedlasting feilet")
  }
  
  # Behandle den nedlastede filen
  downloaded_content <- httr::content(download_response, "raw")
  
  # Sjekk filformatet fra URL
  file_ext <- "json"  # Standard fra responsen ser ut til å være json
  if (grepl("\\.csv$", download_url, ignore.case = TRUE)) {
    file_ext <- "csv"
  } else if (grepl("\\.xlsx$", download_url, ignore.case = TRUE)) {
    file_ext <- "xlsx"
  }
  
  # Lagre til temp-fil først
  temp_file <- tempfile(fileext = paste0(".", file_ext))
  writeBin(downloaded_content, temp_file)
  
  # Lag filprefix basert på user_type
  # Erstatt mellomrom med understrek og konverter til små bokstaver for filnavn
  file_prefix <- tolower(gsub(" ", "_", user_type))
  if (tolower(user_type) == "all") {
    file_prefix <- "all_users"
  } else {
    file_prefix <- paste0(file_prefix, "_users")
  }
  
  # Lagre rådataene hvis ønsket
  if (!is.null(save_path)) {
    raw_file <- file.path(save_path, paste0(file_prefix, "_raw_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", file_ext))
    file.copy(temp_file, raw_file, overwrite = TRUE)
    cat("Rådata lagret til", raw_file, "\n")
  }
  
  if (file_ext == "json") {
    # Parse JSON-filen
    json_data <- jsonlite::fromJSON(temp_file)
    
    # Vis struktur av JSON for debugging
    if (debug) {
      cat("JSON struktur:\n")
      print(str(json_data))
    }
    
    # Forsøk å konvertere JSON til dataframe
    if (format == "csv" || format == "data.frame") {
      # Metode 1: Prøv å bruke flatten for å flate ut nestede elementer
      tryCatch({
        if (is.data.frame(json_data)) {
          user_data <- json_data
          cat("JSON allerede i dataframe-format\n")
        } else if (is.list(json_data) && length(json_data) > 0) {
          if (is.data.frame(json_data[[1]])) {
            # Hvis første element er en dataframe, prøv å slå sammen alle
            user_data <- do.call(rbind, json_data)
            cat("Konvertert fra JSON-liste av dataframes\n")
          } else if (is.list(json_data[[1]])) {
            # Hvis første element er en liste, prøv å konvertere hver til dataframe
            # Først sjekk om alle lister har samme struktur/lengde
            list_lengths <- sapply(json_data, function(x) length(x))
            if (length(unique(list_lengths)) == 1) {
              # Konverter til dataframe
              user_data <- as.data.frame(do.call(rbind, lapply(json_data, function(x) {
                unlist(x)
              })), stringsAsFactors = FALSE)
              cat("Konvertert fra JSON-liste av lister med samme struktur\n")
            } else {
              # Ulike strukturer - returner rå JSON
              cat("Kompleks JSON-struktur - returnerer rå JSON\n")
              user_data <- json_data
            }
          } else {
            # Annen type liste - gjør beste forsøk
            user_data <- as.data.frame(json_data, stringsAsFactors = FALSE)
            cat("Konvertert fra generell JSON-liste\n")
          }
        } else {
          # Ikke gjenkjent struktur - returner rå JSON
          user_data <- json_data
          cat("Ukjent JSON-struktur - returnerer rå JSON\n")
        }
      }, error = function(e) {
        cat("Feil ved konvertering av JSON:", e$message, "\n")
        user_data <- json_data  # Returner rå JSON ved feil
      })
    } else {
      # Returner rå JSON-data
      user_data <- json_data
    }
  } else if (file_ext == "csv") {
    # Les CSV-filen
    user_data <- utils::read.csv(temp_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  } else if (file_ext == "xlsx" && requireNamespace("readxl", quietly = TRUE)) {
    # Les XLSX-filen hvis readxl er installert
    user_data <- readxl::read_excel(temp_file)
  } else {
    # For andre formater eller XLSX uten readxl, returner filstien
    user_data <- temp_file
    cat("Filformat", file_ext, "krever manuell behandling, returnerer filsti.\n")
  }
  
  # Lagre konverterte data hvis ønsket og mulig
  if (!is.null(save_path) && is.data.frame(user_data)) {
    output_file <- file.path(save_path, paste0(file_prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
    utils::write.csv(user_data, output_file, row.names = FALSE)
    cat("Behandlet data lagret til", output_file, "\n")
  }
  
  # Rydd opp
  unlink(temp_file)
  
  # Rapporter antall brukere
  if (is.data.frame(user_data)) {
    cat("Hentet data for", nrow(user_data), ifelse(user_type_lower == "all", "brukere", paste(user_type, "brukere")), "\n")
  } else {
    cat("Hentet", ifelse(user_type_lower == "all", "brukerdata", paste(user_type, "brukerdata")), "(ikke dataframe-format)\n")
  }
  
  return(user_data)
}

#' Bestill og last ned fil med administratordata fra Inspera
#'
#' @param token Tilgangstoken (hvis ikke oppgitt, blir det hentet automatisk)
#' @param api_key API-nøkkel for Inspera (brukes kun hvis token ikke er oppgitt)
#' @param format Filformat for eksporten (default: "csv")
#' @param wait_time Hvor lenge skal vi vente mellom sjekkene på eksportstatus (sekunder)
#' @param max_attempts Maksimalt antall forsøk på å sjekke status
#' @param save_path Sti til hvor filen skal lagres (valgfri)
#' @param debug Om detaljert debug-informasjon skal skrives ut
#' @return En data.frame med administratordata
#' @importFrom httr POST GET add_headers content status_code
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils read.csv
#' @export
get_admin_users <- function(token = NULL, api_key = NULL, format = "csv", 
                            wait_time = NULL, max_attempts = NULL, save_path = NULL, 
                            debug = TRUE) {
  # Kall den generiske funksjonen med user_type = "admin"
  get_users(user_type = "admin", token = token, api_key = api_key, 
            format = format, wait_time = wait_time, max_attempts = max_attempts, 
            save_path = save_path, debug = debug)
}

#' Bestill og last ned fil med studentdata fra Inspera
#'
#' @param token Tilgangstoken (hvis ikke oppgitt, blir det hentet automatisk)
#' @param api_key API-nøkkel for Inspera (brukes kun hvis token ikke er oppgitt)
#' @param format Filformat for eksporten (default: "csv")
#' @param wait_time Hvor lenge skal vi vente mellom sjekkene på eksportstatus (sekunder)
#' @param max_attempts Maksimalt antall forsøk på å sjekke status
#' @param save_path Sti til hvor filen skal lagres (valgfri)
#' @param debug Om detaljert debug-informasjon skal skrives ut
#' @return En data.frame med studentdata
#' @importFrom httr POST GET add_headers content status_code
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils read.csv
#' @export
get_student_users <- function(token = NULL, api_key = NULL, format = "csv", 
                              wait_time = NULL, max_attempts = NULL, save_path = NULL, 
                              debug = TRUE) {
  # Kall den generiske funksjonen med user_type = "student"
  get_users(user_type = "student", token = token, api_key = api_key, 
            format = format, wait_time = wait_time, max_attempts = max_attempts, 
            save_path = save_path, debug = debug)
}

#' Bestill og last ned fil med alle brukerdata fra Inspera
#'
#' @param token Tilgangstoken (hvis ikke oppgitt, blir det hentet automatisk)
#' @param api_key API-nøkkel for Inspera (brukes kun hvis token ikke er oppgitt)
#' @param format Filformat for eksporten (default: "csv")
#' @param wait_time Hvor lenge skal vi vente mellom sjekkene på eksportstatus (sekunder)
#' @param max_attempts Maksimalt antall forsøk på å sjekke status
#' @param save_path Sti til hvor filen skal lagres (valgfri)
#' @param debug Om detaljert debug-informasjon skal skrives ut
#' @return En data.frame med alle brukerdata
#' @importFrom httr POST GET add_headers content status_code
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils read.csv
#' @export
get_all_users <- function(token = NULL, api_key = NULL, format = "csv", 
                          wait_time = NULL, max_attempts = NULL, save_path = NULL, 
                          debug = TRUE) {
  # Kall den generiske funksjonen med user_type = "all"
  get_users(user_type = "all", token = token, api_key = api_key, 
            format = format, wait_time = wait_time, max_attempts = max_attempts, 
            save_path = save_path, debug = debug)
}