#' Hent eksterne ID-er for alle administratorer
#'
#' Denne funksjonen henter eksterne ID-er for alle administratorer i Inspera.
#' Den bruker get_admin_users() for å hente listen over administratorer, og deretter
#' gjør den individuelle API-kall for å hente detaljert informasjon for hver administrator.
#'
#' @param save_path Sti til hvor resultatet skal lagres som CSV (valgfritt)
#' @param max_users Maksimalt antall brukere å hente (NULL for alle)
#' @param sleep_time Pause mellom API-kall i sekunder (for å unngå overbelastning)
#' @param token Tilgangstoken (hvis NULL, blir det hentet automatisk)
#' @return En dataframe med administratorer og deres eksterne ID-er
#' @export
get_admin_external_ids <- function(save_path = NULL, max_users = NULL, sleep_time = 0.2, token = NULL) {
  # 1. Autentiser hvis token ikke er gitt
  if (is.null(token)) {
    token <- authenticate_inspera()$token
  }
  
  # 2. Hent administratorer
  cat("Henter liste over alle administratorer...\n")
  admins <- get_admin_users()
  
  # 3. Hent bruker-ID-er fra riktig kolonne
  user_id_col <- "admins.userId"
  
  # Sjekk at kolonnen eksisterer
  if (!user_id_col %in% colnames(admins)) {
    stop(paste0("Kolonnen '", user_id_col, "' eksisterer ikke i resultatet fra get_admin_users()."))
  }
  
  user_ids <- unique(admins[[user_id_col]])
  user_ids <- user_ids[!is.na(user_ids)]
  
  cat("Fant", length(user_ids), "unike administrator-ID-er.\n")
  
  # 4. Begrens antall brukere hvis spesifisert
  if (!is.null(max_users) && length(user_ids) > max_users) {
    user_ids <- user_ids[1:max_users]
    cat("Begrenset til", max_users, "administratorer for testing.\n")
  }
  
  # 5. Opprett resultat dataframe
  result <- data.frame(
    admin_id = character(),
    username = character(),
    firstName = character(),
    lastName = character(),
    email = character(),
    auth_type = character(),
    external_id = character(),
    stringsAsFactors = FALSE
  )
  
  # 6. Hent detaljert informasjon for hver ID
  cat("Henter detaljert informasjon for hver administrator...\n")
  
  # Opprett progresjonsindikator
  pb <- NULL
  if (requireNamespace("utils", quietly = TRUE)) {
    pb <- utils::txtProgressBar(min = 0, max = length(user_ids), style = 3)
  }
  
  for (i in seq_along(user_ids)) {
    admin_id <- user_ids[i]
    
    # Vis fremgang
    if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, i)
    }
    
    if (i %% 20 == 0 || i == 1 || i == length(user_ids)) {
      cat("\nBehandler administrator", i, "av", length(user_ids), "\n")
    }
    
    # Bruk tryCatch for å håndtere eventuelle feil
    tryCatch({
      # API-kall for brukerdetaljer
      user_url <- paste0("https://nokut.inspera.no/api/v1/users/", admin_id)
      user_response <- httr::GET(
        url = user_url,
        httr::add_headers(
          "Authorization" = paste("Bearer", token),
          "Accept" = "application/json"
        )
      )
      
      # Håndter svar
      if (httr::status_code(user_response) == 200) {
        user_data <- httr::content(user_response)
        
        # Grunnleggende brukerinfo - bruk tom streng istedenfor NA når data mangler
        username <- ifelse(!is.null(user_data$username) && !is.na(user_data$username), user_data$username, "")
        firstName <- ifelse(!is.null(user_data$firstName) && !is.na(user_data$firstName), user_data$firstName, "")
        lastName <- ifelse(!is.null(user_data$lastName) && !is.na(user_data$lastName), user_data$lastName, "")
        email <- ifelse(!is.null(user_data$email) && !is.na(user_data$email), user_data$email, "")
        
        # Sjekk om brukeren har eksterne ID-er
        has_external_ids <- FALSE
        
        if (!is.null(user_data$externalIds) && length(user_data$externalIds) > 0) {
          has_external_ids <- TRUE
          
          # For hver ekstern ID
          for (ext_id in user_data$externalIds) {
            if (is.list(ext_id) && !is.null(ext_id$authType) && !is.null(ext_id$externalId)) {
              # Lag ny rad
              new_row <- data.frame(
                admin_id = as.character(admin_id),
                username = as.character(username),
                firstName = as.character(firstName),
                lastName = as.character(lastName),
                email = as.character(email),
                auth_type = as.character(ext_id$authType),
                external_id = as.character(ext_id$externalId),
                stringsAsFactors = FALSE
              )
              
              # Legg til rad i resultatet
              result <- rbind(result, new_row)
            }
          }
        }
        
        # Hvis ingen eksterne ID-er ble funnet, legg til en rad med tom streng for auth_type og external_id
        if (!has_external_ids) {
          new_row <- data.frame(
            admin_id = as.character(admin_id),
            username = as.character(username),
            firstName = as.character(firstName),
            lastName = as.character(lastName),
            email = as.character(email),
            auth_type = NA_character_,
            external_id = NA_character_,
            stringsAsFactors = FALSE
          )
          
          result <- rbind(result, new_row)
        }
        
      } else {
        # Håndter feil status
        cat("\nFeil ved henting av bruker", admin_id, "- Status:", httr::status_code(user_response), "\n")
        
        new_row <- data.frame(
          admin_id = as.character(admin_id),
          username = NA_character_,
          firstName = NA_character_,
          lastName = NA_character_,
          email = NA_character_,
          auth_type = "ERROR",
          external_id = paste("API Error:", httr::status_code(user_response)),
          stringsAsFactors = FALSE
        )
        
        result <- rbind(result, new_row)
      }
    }, error = function(e) {
      # Håndter eventuelle feil under API-kall eller databehandling
      cat("\nFeil ved behandling av bruker", admin_id, ":", conditionMessage(e), "\n")
      
      # Logg bruker-ID som forårsaket feil
      new_row <- data.frame(
        admin_id = as.character(admin_id),
        username = NA_character_,
        firstName = NA_character_,
        lastName = NA_character_,
        email = NA_character_,
        auth_type = "PROCESSING_ERROR",
        external_id = paste("Error:", conditionMessage(e)),
        stringsAsFactors = FALSE
      )
      
      result <<- rbind(result, new_row)  # Merk: Bruker <<- for å endre result i ytre miljø
    })
    
    # Lagre resultatet regelmessig hvis save_path er oppgitt
    if (!is.null(save_path) && (i %% 50 == 0 || i == length(user_ids))) {
      if (!dir.exists(save_path)) {
        dir.create(save_path, recursive = TRUE)
      }
      temp_output_file <- file.path(save_path, paste0("admin_external_ids_temp_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
      tryCatch({
        utils::write.csv(result, temp_output_file, row.names = FALSE)
        cat("\nMidlertidig resultat lagret til", temp_output_file, "\n")
      }, error = function(e) {
        cat("\nFeil ved lagring av midlertidig resultat:", conditionMessage(e), "\n")
      })
    }
    
    # Pause mellom API-kall
    Sys.sleep(sleep_time)
  }
  
  # Lukk progresjonsindikator
  if (!is.null(pb)) {
    close(pb)
  }
  
  # 7. Lagre resultatet til en CSV-fil
  if (!is.null(save_path) && nrow(result) > 0) {
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
    output_file <- file.path(save_path, paste0("admin_external_ids_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
    tryCatch({
      utils::write.csv(result, output_file, row.names = FALSE)
      cat("\nResultatet lagret til", output_file, "\n")
    }, error = function(e) {
      cat("\nFeil ved lagring av resultat:", conditionMessage(e), "\n")
    })
  }
  
  # 8. Vis sammendrag
  cat("\nHentet eksterne ID-er for", length(unique(result$admin_id)), "administratorer\n")
  cat("Antall rader i resultatet:", nrow(result), "\n")
  
  # Vis fordeling av auth_type
  auth_counts <- table(result$auth_type, useNA = "ifany")
  cat("\nAntall brukere per auth_type:\n")
  for (i in 1:length(auth_counts)) {
    auth_name <- names(auth_counts)[i]
    auth_name <- ifelse(is.na(auth_name), "Ingen auth_type", auth_name)
    cat("- ", auth_name, ": ", auth_counts[i], "\n", sep = "")
  }
  
  return(result)
}