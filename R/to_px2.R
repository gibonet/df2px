
# Come to_px ma con l'aggiunta di un paio di DATASYMBOL...
# Argomenti:
# .cube: il data frame con il cubo di dati in formato semi-tidy (con la misura in colonna)
# vars: vettore character con i nomi delle variabili che ci sono in riga
# measure_var: vettore character con i nomi delle modalità della variabile in colonna
#             che contiene le misure (i nomi delle misure, insomma)
# measure_name: nome che si vuole dare alla variabile delle misure ("misura" 
#               può andare bene)
# symbols = c("X", "..."): due stringhe che corrispondono a DATASYMBOL2 e DATASYMBOL3
#           dei dati del cubo px...

#' @rdname to_px
#' @inheritParams to_px
#' @param .symbols character vector with the 'special' symbols for 2 and 3 dots in the px file
#' @param TITLE title of the data cube (default: "Title unknown")
#' @param PRECISION character vector with in the form c("measure_name", "measure") if a certain measure has to be shown with decimals values (default = NULL). Experimental
#' @param info name of the column with information on how missing values have to be treated (default: "info")
#' @param SHOWDECIMALS default number of decimals to show (default = 0)
#' 
#' @examples 
#' # to_px2 considers 2 types of 'special' data (missing or other). This information
#' # is retrieved from the 'info' column of the data cube.
#' table(c09C$info)
#' c09C_px2 <- to_px2(.cube = c09C, 
#'                  vars = colnames(c09C)[1:4], 
#'                  measure_vars = colnames(c09C)[5:10],
#'                  measure_name = "misura")
#'                  
#' head(c09C_px2, n = 50)
#' 
#' @export
to_px2 <- function(.cube, vars, measure_vars, measure_name = NULL, 
                   .symbols = c("X", "...", "()"), TITLE = "Title unknown",
                   PRECISION = NULL, SHOWDECIMALS = 0, info = "info"){
  livelli_variabili <- lapply(.cube[ , rev(vars)], function(x) as.character(unique(x)))
  
  # 1. Creazione STUB
  s01 <- add_quotes(vars)
  s02 <- riduci_virgole(s01)
  STUB <- paste0("STUB=", s02, ";")
  
  # 2. Creazione HEADING
  if(is.null(measure_name)){
    HEADING <- NULL
  }else{
    h01 <- add_quotes(measure_name)
    HEADING <- paste0("HEADING=", h01, ";")
  }
  
  # 3. Creazione VALUES
  v01 <- lapply(livelli_variabili, add_quotes)
  v02 <- lapply(v01, riduci_virgole)
  
  if(is.null(measure_name)){
    v03 <- NULL
  }else{
    v03 <- paste0("VALUES(", add_quotes(measure_name), ")=", 
                  riduci_virgole(add_quotes(measure_vars)), ";")
  }
  
  v04 <- lapply(rev(vars), add_quotes)
  
  VALUES <- paste0("VALUES(", v04, ")=", v02, ";")
  VALUES <- c(v03, VALUES)
  
  if(is.null(PRECISION)){
    PRECISION <- NULL
  }else{
    # PRECISION = c("misura", "ETP")
    PRECISION <- paste(add_quotes(PRECISION), collapse = ", ")
    PRECISION <- paste0("PRECISION(", PRECISION, ")=4;")
  }
  
  
  # 4. Creazione DATA
  DATA <- .cube[ , measure_vars, drop = FALSE]
  info <- .cube[[info]]  # colonna 'info' del cubo in formato semi-tidy
  k_X <- info == .symbols[1]
  k_dots <- info == .symbols[2]
  DATA[k_X, ] <- "\"..\""
  DATA[k_dots, ] <- "\"...\""
  # DATA[] <- lapply(DATA, round)
  DATA <- Reduce(paste, DATA)
  # DATA <- gsub("NA", "\"..\"", DATA)
  DATA <- c("DATA=", DATA, ";")
  
  # 4.1 DATASYMBOL2 e DATASYMBOL3
  DATASYMBOL2 <- paste0("DATASYMBOL2=", add_quotes(.symbols[1]), ";")
  DATASYMBOL3 <- paste0("DATASYMBOL3=", add_quotes(.symbols[2]), ";")
  
  # 4.2 Eventuali DATANOTECELL e DATASYMBOL5
  k_par <- which(info == .symbols[3])
  if(length(k_par) == 0L){
    DATANOTECELLS <- NULL
    DATASYMBOL5 <- NULL
  }else{
    DATANOTECELLS_data <- .cube[k_par, c(vars, measure_name), drop = FALSE]
    DATANOTECELLS <- vector(mode = "character", length = length(k_par))
    
    # Provo con una DATANOTECELL
    DATANOTECELLS_data[] <- lapply(DATANOTECELLS_data, as.character)
    for(i in seq_along(DATANOTECELLS)){
      DATANOTECELLS[i] <- paste0("DATANOTECELL(", 
                                 riduci_virgole(add_quotes(DATANOTECELLS_data[i, ])),
                                 ")=", add_quotes(.symbols[3]), ";")
    }
    
    DATASYMBOL5 <- paste0("DATASYMBOL5=", add_quotes(.symbols[3]), ";")
  }
  
  
  # 5. Assemblamento 1-4. più le keywords "fisse" (per il momento)
  #    in un vettore character che in pratica costituisce il file
  #    .px
  creation_date <- gsub(pattern = "-", replacement = "", x = Sys.time())
  creation_date <- substr(creation_date, 1, 14)
  
  last_updated <- gsub(pattern = "-", replacement = "", x = Sys.time())
  
  TITLE <- add_quotes(TITLE)
  TITLE <- paste0(TITLE, ";")
  keywords01 <- c("CHARSET=\"ANSI\";", "AXIS-VERSION=\"2000\";", 
                  paste0("CREATION-DATE=\"", creation_date, "\";"),
                  "DECIMALS=11;", paste0("SHOWDECIMALS=", SHOWDECIMALS, ";"),
                  "MATRIX=\"file000\";",
                  "SUBJECT-CODE=\"xx\";", 
                  "SUBJECT-AREA=\"unknown\";",
                  paste0("TITLE=", TITLE), 
                  "CONTENTS=\"unknown\";",
                  "UNITS=\"unknown\";")
  keywords02 <- c(paste0("LAST-UPDATED=\"", last_updated, "\";"),
                  "INFO=\"File generated using R\";")
  res <- c(
    keywords01,
    STUB,
    HEADING,
    VALUES,
    PRECISION,
    keywords02,
    DATASYMBOL2,
    DATASYMBOL3,
    DATASYMBOL5,
    DATANOTECELLS,
    DATA
  )
  
  res
}
