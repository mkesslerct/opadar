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
                            startRow = 1, startCol = 1){
    ## En el workbook wb, combina celdas, columna por columna, en las columnas
    ## recogidas en la lista  "columnas". Los elementos de la lista columnas son
    ## vectores de posiciones. En un vector aparecen las posiciones de las
    ## columnas que tienen la misma estructura de combinación.
    ## consiste en unir, columna por columna, celdas contiguas que contienen el
    ## mismo valor en la primera columna del vector "columnas"
    ## Nota: es la primera columna de "columnas[[l]]" que define qué celdas se
    ## combinan, las demás columnas en "columnas" simplemente heredan de estas
    ## combinaciones. Hay además una jerarquía, ver la vignette
    ## StartRow y StartCol indican  la celda en el workbook donde se escribirá
    ## (en otra instrucción fuera de esta función) la esquina superior de la
    ## tabla df.
    paramerge <- list()
    for (l in 1:length(columnas)){
        columnasv <- columnas[[l]]
        paramerge[[l]] <-
            data.frame(numerounicos = numberunicos(df[, columnasv[1]]),
                       fila = (startRow -1) + 1 + 1:nrow(df))
        if (l == 1) {
            paramerge.df <- paramerge[[l]]  %>%
              dplyr::group_by(numerounicos) %>%
              dplyr::mutate(numero = n())
        } else {
            paramerge.df <- paramerge[[l]] %>%
              dplyr::mutate(numerounicos = numerounicos +
                       (get("numerounicos", paramerge[[l-1]]) - 1)) %>%
              dplyr::group_by(numerounicos) %>%
              dplyr::mutate(numero = n())
        }
        for (j in unique((paramerge.df %>%
                            dplyr::filter(numero > 1))$numerounicos)){
            for (columna in columnasv) {
                openxlsx::mergeCells(wb, sheetName,
                                     cols = startCol - 1 + columna,
                                     rows = (paramerge.df %>%
                                               dplyr::filter(numerounicos == j))$fila)
            }
        }
        vhcenterStyle <- openxlsx::createStyle(valign = "center",
                                               halign = "center")
        openxlsx::addStyle(wb, sheetName,
                           style = vhcenterStyle,
                           rows = startRow - 1 + 2:(nrow(df)+1),
                           cols = startCol - 1 + columnasv,
                           gridExpand = TRUE)
    }
}
