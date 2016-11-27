getcellrowcolumn <- function(cells){
    ## Utility function that transforms a (vector of) Excel reference into a
    ## dataframe with asociated column and row numbers.
    cells <- toupper(cells)
    ## check if cells have correct formatting
    u <- regexpr("[A-Z]+[1-9][0-9]*", cells)
    if (!identical(attr(u, "match.length"), stringr::str_length(cells))){
        stop("Incorrect cell specification")
    }

    columns <- stringr::str_extract(cells, "[A-Z]+")
    rows <- stringr::str_extract(cells, "[1-9][0-9]*")
    columnschar <- stringr::str_extract_all(columns, ".")
    letters2number <- function(columns) {
        sum(stringr::str_locate(paste(LETTERS, collapse = ""), columns)[,1] *
              c(rep(26, length(columns)-1), 1))
    }
    colnumbers <- sapply(columnschar,FUN = letters2number)
    data.frame(column = colnumbers, row = as.integer(rows))
}
## -----------------------------------------------------------------------------
##
##                         Funciones para combinar celdas
##
## -----------------------------------------------------------------------------
##
## Primera función auxiliar
##
numberunicos=function(x){
    ## devuelve un vector de misma longitud que x, que contiene en la posición i
    ## el número del valor único correspondiente 
    ## ejemplo x = c("a", "a", "b", "b", "c", "c")
    ## numberunicos = c(1, 1, 2, 2, 3, 3)
    cumsum(c(1, x[-1] != x[c(1:(length(x)-1))]))
}
##

mergeCellsTabla <- function(wb, sheetName,
                            df,
                            columnas,
                            startRow = 1, startCol = 1,
                            transpose = FALSE,
                            colNames = TRUE){
  ## En el workbook wb, combina celdas, columna por columna, en las columnas
  ## recogidas en el vector "columnas". columnas es un vector de
  ## posiciones. Consiste en unir, columna por columna, celdas contiguas que
  ## contienen el mismo valor.  Se impone además una jerarquía: se aplica las
  ## combinaciones verticales sobre la primera columna de columnas, a
  ## continuación se aplica las combinaciones verticales sobre la segunda
  ## columna, pero sólo puede combinar celdas que corresponden a valores
  ## iguales de la primera columnas. Y así sucesivamente, ver la vignette para
  ## más información.
  ## startRow y startCol indican la celda en el workbook
  ## donde se escribirá (en otra instrucción fuera de esta función) la esquina
  ## superior de la tabla df.
  ## En el caso en que queremos escribir el dataframe transpuesto
  if (transpose) {
    startRow.tmp <- startCol
    startCol <- startRow
    startRow <- startRow.tmp
  }
  paramerge <- list()
  for (l in 1:length(columnas)){
    paramerge[[l]] <-
      data.frame(numerounicos = numberunicos(df[, columnas[l]]),
                 fila =  1:nrow(df))
    if (l == 1) {
      paramerge[[l]] <- paramerge[[l]]  %>%
        dplyr::group_by(numerounicos) %>%
        dplyr::mutate(numero = n())
    } else {
      paramerge[[l]] <- paramerge[[l]] %>%
        dplyr::mutate(numerounicos = numerounicos +
                        (get("numerounicos", paramerge[[l-1]]) - 1)) %>%
        dplyr::group_by(numerounicos) %>%
        dplyr::mutate(numero = n())
    }
    for (j in unique((paramerge[[l]] %>%
                        dplyr::filter(numero > 1))$numerounicos)){
      argcols <- startCol - 1 + columnas[l]
      argrows <- (paramerge[[l]] %>%
                    dplyr::filter(numerounicos == j))$fila +
                                                    (startRow -1) + colNames
      if (transpose) {
        argrows.tmp <- argcols
        argcols <- argrows
        argrows <- argrows.tmp
      }
      openxlsx::mergeCells(wb, sheetName,
                           cols = argcols,
                           rows = argrows)
    }
  }
  vhcenterStyle <- openxlsx::createStyle(valign = "center",
                                         halign = "center")
  argrows <- startRow - 1 + colNames + 1:nrow(df)
  argcols <- startCol - 1 + columnas
  if (transpose) {
    argrows.tmp <- argcols
    argcols <- argrows
    argrows <- argrows.tmp
  }
  openxlsx::addStyle(wb, sheetName,
                     style = vhcenterStyle,
                     rows = argrows,
                     cols = argcols,
                     gridExpand = TRUE)
}
## -----------------------------------------------------------------------------
##
profilingcolumna <- function(x){
    t <- gsub(pattern = "[a-z]", replacement = "a", x)
    t <- gsub(pattern = "[A-Z]", replacement = "A", t)
    t <- gsub(pattern = "[0-9]", replacement = "d", t)
    t <- gsub(pattern = " ", replacement = "[SPC]", t)
    t
}

freqprofiling <- function(x,maxrow){
  tt <- ifelse(x == "", "\"\"", x)
  tabla <- table(profilingcolumna(tt), useNA = "always")
  ## buscamos ordenarlos por orden decreciente pero poniendo el NA al final.
  orden <- order(tabla, decreasing = TRUE)
  indNA <- which(orden == dim(tabla))
  tabla <- tabla[c(orden[-indNA], orden[indNA])]
  rownames(tabla)[is.na(rownames(tabla))] <- "NA"
  celda_dd <- paste(rownames(tabla), " (", tabla, ")", sep = "")
  tablavalores <- table(tt)[order(table(tt), decreasing = TRUE)]
    celda_vv <- paste(names(tablavalores), " (", tablavalores, ")",  sep = "")
  c(celda_dd[1:maxrow],
    length(unique(tt)),
    celda_vv[1:5])
}
##
extractinside <- function(string, left.border,
                          right.border){
  ## devuelve una lista de misma longitud que string, que contiene las partes
  ## comprendidas entre left.border y right.border en cada elemento de string.
  if (right.border == "$") {
    rightpattern <- "$"
  } else {
    rightpattern <- paste0("(?=",  right.border, ")")
  }
  if (left.border == "^"){
    leftpattern <- "^"
  } else {
    leftpattern <- paste0("(?<=", left.border, ")")
  }
  pattern <- paste0(leftpattern, "(.*?)+", rightpattern)
  rx <- gregexpr(pattern , text = string, perl = TRUE,
                 ignore.case = TRUE)
  f <- function(position, length, string){
    substr(string, start = position,
          stop = position + length)
  }
  g <- function(rx1, string){
    mapply(FUN = f,rx1, attr(rx1, "match.length")-1, string = string)
  }
  mapply(g, rx, string, SIMPLIFY = FALSE)
}
##
getsubconsultas <- function(sqlselect){
  ## devuelve una lista: el  elemento i de la lista es un vector que contiene las
  ## subconsultas (deben ser enmarcadas por "()") contenidas en sqlselect[i].
    closestpar <- stringr::str_extract_all(sqlselect, "\\([^()]+\\)")
    listasubconsultas <- closestpar
    while (max(sapply(closestpar, FUN = length)) > 0) {
        sqlselect <- stringr::str_replace_all(sqlselect, "\\([^()]+\\)",
                                              "subconsulta")
        closestpar <- stringr::str_extract_all(sqlselect, "\\([^()]+\\)")
        listasubconsultas <- mapply(FUN = c, listasubconsultas, closestpar)
    }
  listasubconsultas <- mapply(FUN = function(x,y) c(x, y),
                               listasubconsultas,
                               sqlselect,
                              SIMPLIFY = FALSE)
    lapply(listasubconsultas, function(v) gsub("\\(|\\)", "", v))
}
## las palabras que servirán de right.border para from y para join
reservedwordsfrom <- c("WHERE ",
                   "GROUP BY ",
                   "HAVING ",
                   "ORDER BY ",
                   "LIMIT ",
                   "PROCEDURE ",
                   "INTO ",
                   "FOR UPDATE ",
                   "LOCK IN ")
reservedwordsjoin <- c("ON ",
                   "USING ")
tableexpression <- function(sqlselect){
  ## sqlselect: vector de strings,
  ## devuelve una lista: el elemento i es el vector de tablas contenidas en la
  ## consulta en sqlselect[i]
  left.border <- "from"
  right.border <- paste0("(", paste(reservedwordsfrom,
                                   collapse = "|"),
                         ")")
  strings1 <- extractinside(sqlselect, left.border, right.border)
  ##
  strings2 <- extractinside(sqlselect, left.border, "$")
  combina12 <- function(string1, string2){
    ## combina string1 y string2: si string1 es "", pone string2.
    if (identical(string1,  "")) {
      stringoutput <- string2
    } else {
      stringoutput <- string1
    }
  }
  stringsfrom <- mapply(combina12, strings1, strings2, SIMPLIFY = FALSE)
  stringsjoin1 <- sapply(stringsfrom, extractinside, left.border = "^",
                         right.border = "join")
  if (!is.list(stringsjoin1)) stringsjoin1 <- as.list(stringsjoin1)
  stringsjoin1 <- mapply(combina12, stringsjoin1, stringsfrom, SIMPLIFY = FALSE)
  left.border = "join "
  right.border <- paste0("(", paste(reservedwordsjoin,
                                    collapse = "|"), ")")
  stringsjoin2 <- sapply(stringsfrom, extractinside, left.border = " join ",
         right.border = right.border)
  if (!is.list(stringsjoin2)) stringsjoin2 <- as.list(stringsjoin2)
  stringsfromjoin <- mapply(FUN = function(x, y) c(x, y),
                            stringsjoin1, stringsjoin2, SIMPLIFY = FALSE)
  stringsfromjoin <- lapply(stringsfromjoin,
                            stringr::str_trim)
  ## queda separar las tablas que van con comas...
  tablas <- lapply(stringsfromjoin,
         function(string, pattern) {
           unlist(stringr::str_split(string, pattern))
         },
         pattern = ",") %>%
    lapply(stringr::str_trim)
  
  tablassinalias <- function(tablastring){
    splitspace <- stringr::str_split(tablastring, " ")
    tablasalias <- sapply(splitspace, '[', 2)
    tablaspropias <- sapply(splitspace, '[', 1)
    setdiff(tablaspropias, c(tablasalias, "", "subconsulta"))
  }
  lapply(tablas, tablassinalias)
}
