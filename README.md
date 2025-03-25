# insperaNokut

Verktøy for administrering av bidragsytere i Inspera for NOKUT.

## Installasjon

Du kan installere pakken fra GitHub med:

```r
# Installer devtools hvis du ikke har det
install.packages("devtools")

# Installer insperaNokut-pakken
devtools::install_github("nokut/insperaNokut")

```

# Bruk
## Hent birdagsytere fra en test
```r
library(insperaNokut)

# Hent bidragsytere fra en test og finn FEIDE-brukernavn
result <- get_feide_contributors_and_save_csv("<testid>")
```

## Konverter til YAML med manuell komitè-mapping
```r
# Definer manuell komité-mapping
committee_mapping <- list(
  "Mathias Meier" = "1ab",
  "Karl Johan Skeidsvoll" = "2ab",
  "Maria Louise Karlsen" = "3ab",
  "Marte Pedersen" = "1ab"
)

# Konverter CSV til YAML med komité-mapping
yaml_result <- contributors_feide_csv_to_yaml(
  "test_330584412_contributors_feide.csv",
  manual_committee_mapping = committee_mapping
)
```

## Opprett komiteer og legg til bidragsytere
```r
# Opprett komiteer og legg til bidragsytere i en test
workflow_result <- create_committees_and_assign_contributors(
  "<testid>",
  yaml_result$yaml_path
)
```

## Komplett arbeidsflyt
```r
# Gjør alt i ett trinn med interaktiv komité-mapping
complete_inspera_contributor_workflow(
  source_test_id = "<test_id>",
  target_test_id = "<test_id>"
)

# Eller med forhåndsdefinert komité-mapping
committee_mapping <- list(
  "navn navnesen1" = "1ab",
  "navn navnesen2" = "2ab",
  "navn navnesen3" = "3ab",
  "navn navnesen4" = "1ab"
)
```

# Funksjoner:
Hovedfunksjoner i denne pakken:

get_feide_contributors_and_save_csv() - Henter bidragsytere fra en test og lagrer som CSV
contributors_feide_csv_to_yaml() - Konverterer bidragsyter-CSV til YAML med støtte for manuell komité-mapping
create_committees() - Oppretter komiteer i en test basert på YAML-fil
assign_contributors() - Tilordner bidragsytere til en test med komitétilknytning
create_committees_and_assign_contributors() - Kombinert funksjon som oppretter komiteer og tilordner bidragsytere
complete_inspera_contributor_workflow() - Fullstendig arbeidsflyt med støtte for interaktiv komité-mapping

complete_inspera_contributor_workflow(
  source_test_id = "330584412",
  target_test_id = "337279121",
  committee_mapping = committee_mapping
)
```


