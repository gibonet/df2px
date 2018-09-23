
# Aggiungi virgolette
add_quotes <- function(x){
  paste0("\"", x, "\"")
}

# Riduci a una stringa separata da virgole
riduci_virgole <- function(x){
  paste0(x, collapse = ",")
}


#' Convert a data cube from data frame to a px file
#' 
#' @param .cube a data cube in data frame format
#' @param vars character vector with the names of the categorical variables of the data cube
#' @param measure_vars character vector with the names of the columns that contain the numeric 
#' estimates
#' @param measure_name character string with the name to be given to the 'measure' column
#' 
#' @examples 
#' data(c09C)
#' str(c09C)
#' 
#' # The categorical variables of the data cube are the first 4 columns,
#' # while the columns from 5 to 10 contain the numeric estimates
#' c09C_px <- to_px(.cube = c09C, 
#'                  vars = colnames(c09C)[1:4], 
#'                  measure_vars = colnames(c09C)[5:10],
#'                  measure_name = "misura")
#'                  
#' head(c09C_px, n = 20)
#' # c09_px is a character vector where every element is the equivalent of a
#' # row of a px file. It can be exported in a px file for example with 
#' # writeLines
#' \dontrun{
#' writeLines(c09C_px, con = "c09C_px.px")
#' }
#'                  
#' @export
to_px <- function(.cube, vars, measure_vars, measure_name){
  livelli_variabili <- lapply(.cube[ , rev(vars)], function(x) as.character(unique(x)))
  
  # 1. Creazione STUB
  s01 <- add_quotes(vars)
  s02 <- riduci_virgole(s01)
  STUB <- paste0("STUB=", s02, ";")
  
  # 2. Creazione HEADING
  h01 <- add_quotes(measure_name)
  HEADING <- paste0("HEADING=", h01, ";")
  
  # 3. Creazione VALUES
  v01 <- lapply(livelli_variabili, add_quotes)
  v02 <- lapply(v01, riduci_virgole)
  
  v03 <- paste0("VALUES(", add_quotes(measure_name), ")=", 
                riduci_virgole(add_quotes(measure_vars)), ";")
  
  v04 <- lapply(rev(vars), add_quotes)
  
  VALUES <- paste0("VALUES(", v04, ")=", v02, ";")
  VALUES <- c(v03, VALUES)
  
  # 4. Creazione DATA
  DATA <- .cube[ , measure_vars, drop = FALSE]
  DATA <- Reduce(paste, DATA)
  DATA <- gsub("NA", "\"..\"", DATA)
  DATA <- c("DATA=", DATA, ";")
  
  # 5. Assemblamento 1-4. più le keywords "fisse" (per il momento)
  #    in un vettore character che in pratica costituisce il file
  #    .px
  keywords01 <- c("CHARSET=\"ANSI\";", "AXIS-VERSION=\"2000\";", "CREATION-DATE=\"20170320 17:03\";",
                  "DECIMALS=11;", "SHOWDECIMALS=0;", "MATRIX=\"file000\";", "SUBJECT-CODE=\"xx\";",
                  "SUBJECT-AREA=\"unknown\";", "TITLE=\"Title unknown\";", "CONTENTS=\"unknown\";",
                  "UNITS=\"unknown\";")
  keywords02 <- c("LAST-UPDATED=\"20170320 17:08:55\";",
                  "INFO=\"File generated using R\";")
  res <- c(
    keywords01,
    STUB,
    HEADING,
    VALUES,
    keywords02,
    DATA
  )
  
  res
}
