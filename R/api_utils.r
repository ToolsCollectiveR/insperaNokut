#' Hent API-nøkkel fra parametere eller miljøvariabler
#'
#' @param api_key Direkte angitt API-nøkkel (valgfri)
#' @return Den faktiske API-nøkkelen som skal brukes
#' @export
get_api_key <- function(api_key = NULL) {
  # Prioriter direkte angitt API-nøkkel
  if (!is.null(api_key) && api_key != "") {
    return(api_key)
  }
  
  # Deretter sjekk miljøvariabel
  env_api_key <- Sys.getenv("INSPERA_API_KEY", "")
  if (env_api_key != "") {
    return(env_api_key)
  }
  
  # Til slutt, sjekk .Renviron fil
  if (file.exists("~/.Renviron")) {
    readRenviron("~/.Renviron")
    env_api_key <- Sys.getenv("INSPERA_API_KEY", "")
    if (env_api_key != "") {
      return(env_api_key)
    }
  }
  
  # Hvis ingen API-nøkkel ble funnet
  stop("Ingen API-nøkkel spesifisert. Bruk api_key parameter eller sett INSPERA_API_KEY miljøvariabel.")
}

#' Konfigurer Inspera API-nøkkel
#'
#' Setter API-nøkkelen som en miljøvariabel for gjeldende R-sesjon
#'
#' @param api_key Din Inspera API-nøkkel
#' @param permanent Hvis TRUE, lagrer også nøkkelen i .Renviron for fremtidig bruk
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Sett for gjeldende sesjon
#' configure_api_key("din_api_nøkkel_her")
#'
#' # Lagre permanent
#' configure_api_key("din_api_nøkkel_her", permanent = TRUE)
#' }
configure_api_key <- function(api_key, permanent = FALSE) {
  if (missing(api_key) || is.null(api_key) || api_key == "") {
    stop("API-nøkkel må angis")
  }
  
  # Sett for gjeldende sesjon
  Sys.setenv(INSPERA_API_KEY = api_key)
  
  if (permanent) {
    # Lagre i .Renviron
    renviron_path <- path.expand("~/.Renviron")
    
    if (file.exists(renviron_path)) {
      # Les eksisterende fil
      renviron_lines <- readLines(renviron_path)
      
      # Finn og erstatt eksisterende INSPERA_API_KEY-linje
      key_line_index <- grep("^INSPERA_API_KEY=", renviron_lines)
      
      if (length(key_line_index) > 0) {
        # Erstatt eksisterende linje
        renviron_lines[key_line_index] <- paste0("INSPERA_API_KEY=", api_key)
      } else {
        # Legg til ny linje
        renviron_lines <- c(renviron_lines, paste0("INSPERA_API_KEY=", api_key))
      }
      
      # Skriv tilbake til fil
      writeLines(renviron_lines, renviron_path)
    } else {
      # Opprett ny fil
      writeLines(paste0("INSPERA_API_KEY=", api_key), renviron_path)
    }
    
    cat("API-nøkkel lagret permanent i", renviron_path, "\n")
    cat("Start R på nytt for at endringen skal tre i kraft i nye sesjoner.\n")
  } else {
    cat("API-nøkkel satt for gjeldende sesjon.\n")
  }
  
  invisible(TRUE)
}
#' Autentiser mot Inspera API
#'
#' @param api_key API-nøkkel for Inspera
#' @return En liste som inneholder tilgangstoken og annen autentiseringsinformasjon
#' @importFrom httr POST add_headers content status_code
#' @export
authenticate_inspera <- function(api_key = NULL) {
  # Få faktisk API-nøkkel
  actual_api_key <- get_api_key(api_key)
  
  auth_req <- "https://nokut.inspera.no/api/authenticate/token/"
  auth_params <- list(
    client_id = "nokut",
    grant_type = "authorization_code"
  )
  
  cat("Autentiserer mot Inspera API...\n")
  
  auth_response <- httr::POST(
    url = auth_req,
    body = auth_params,
    encode = "form",
    httr::add_headers(code = actual_api_key)
  )
  
  if (httr::status_code(auth_response) != 200) {
    cat("Autentisering feilet. Status:", httr::status_code(auth_response), "\n")
    cat("Respons:", httr::content(auth_response, "text"), "\n")
    stop("Autentisering feilet")
  }
  
  token <- httr::content(auth_response)$access_token
  cat("Autentisering vellykket\n")
  
  return(list(token = token))
}

#' Hent informasjon om en test fra Inspera API
#'
#' @param test_id ID-en til testen
#' @param token Tilgangstoken (hvis ikke oppgitt, blir det hentet automatisk)
#' @param api_key API-nøkkel for Inspera (brukes kun hvis token ikke er oppgitt)
#' @return En liste med testinformasjon
#' @importFrom httr GET add_headers content status_code
#' @importFrom jsonlite fromJSON
#' @export
get_test_info <- function(test_id, token = NULL, api_key = NULL) {
  if (is.null(token)) {
    auth_result <- authenticate_inspera(api_key)
    token <- auth_result$token
  }
  
  test_url <- sprintf("https://nokut.inspera.no/api/v1/test/%s", test_id)
  
  test_response <- httr::GET(
    url = test_url,
    httr::add_headers(
      "Authorization" = paste("Bearer", token),
      "Accept" = "application/json"
    )
  )
  
  status <- httr::status_code(test_response)
  
  if (status != 200) {
    cat("Kunne ikke hente testinformasjon. Status:", status, "\n")
    cat("Respons:", httr::content(test_response, "text"), "\n")
    return(NULL)
  }
  
  test_data <- httr::content(test_response, "text", encoding = "UTF-8")
  test_info <- jsonlite::fromJSON(test_data)
  
  return(list(
    test_info = test_info,
    token = token
  ))
}

#' Rekursiv funksjon for å flate ut JSON-objekter
#'
#' @param json_obj JSON-objekt som skal flates ut
#' @param prefix Prefiks for nøkler (brukes for nesting)
#' @return En liste med flatet struktur
#' @importFrom jsonlite toJSON
#' @export
flatten_json <- function(json_obj, prefix = "") {
  result <- list()
  
  # Itererer gjennom alle felt i JSON-objektet
  for (name in names(json_obj)) {
    value <- json_obj[[name]]
    new_prefix <- ifelse(prefix == "", name, paste0(prefix, "_", name))
    
    if (is.null(value)) {
      # Håndter NULL-verdier
      result[[new_prefix]] <- NA
    } else if (is.list(value) && !is.data.frame(value)) {
      if (length(value) == 0) {
        # Tom liste
        result[[new_prefix]] <- NA
      } else if (all(sapply(value, function(x) is.atomic(x) && length(x) == 1))) {
        # Liste med enkle verdier
        result[[new_prefix]] <- paste(unlist(value), collapse = ", ")
      } else if (all(sapply(value, is.list))) {
        # Liste med objekter - konverter til JSON-streng
        result[[new_prefix]] <- jsonlite::toJSON(value, auto_unbox = TRUE)
      } else {
        # Rekursivt flate ut underliggende objekter
        nested_result <- flatten_json(value, new_prefix)
        result <- c(result, nested_result)
      }
    } else if (is.data.frame(value)) {
      # Dataframe - konverter til JSON-streng
      result[[new_prefix]] <- jsonlite::toJSON(value, auto_unbox = TRUE)
    } else if (length(value) > 1) {
      # Vektorer med flere verdier
      result[[new_prefix]] <- paste(value, collapse = ", ")
    } else {
      # Enkle verdier
      result[[new_prefix]] <- value
    }
  }
  
  return(result)
}