#' Searches for a file in a local data repository
#'
#' Searches for \code{file} in the sequence of data repositories
#' specified as strings in the variable \code{datapath}. Returns the path to file as
#' a string tipically for use in input functions (read.table, etc...)
#'
#' \code{datafile} searches for \code{file}
#' first in the first component of the vector \code{datapath}. If it finds it, it returns the
#' concatenation of \code{datapath[1]} and \code{file}.  If it does not, it looks for
#' \code{file} in the second component of \code{datapath}, returns the concatenation of
#' \code{datapath[2]} and \code{file}. If \code{literal = TRUE},
#' it just returns \code{file} without checking whether \code{file} exists in the current directory. If \code{file} is not found in any of the directories
#' specified in \code{datapath} and \code{literal = FALSE}, a message error is thrown
#' and the execution is stopped.
#'
#' The path to the datarepositories in \code{datapath} should end with "/".
#' For example \code{datapath <- "~/OPADA/data/"}.
#'
#' @param file String. The name of the file to search for in the data
#' repositories.
#' @param literal A logical scalar. Should the file be searched in the data
#'  repositories or just locally? If TRUE, just returns \code{file}.
#' @return The output is a string that contains the path to file in one of
#' the data repository specified in \code{datapath}. If \code{literal = TRUE},
#' it just returns \code{file}.
#' @examples
#' ## defines two data repositories
#' datapath <- c("~/OPADA/data/", "./data/)
#'
#' \dontrun
#' {
#' datafile("mydata.txt")
#' }
#' ## In the case when "mydata.txt" is not found in "~OPADA/data/" but is located
#' ## in "./data/":
#' ## > "./data/mydata.txt
#'
#' ## In the case when "mydata.txt" is not to be found in any of the datadirectories
#' ## specified in \code{datapath}
#' ## Error in datafile("brout") :
#' ## Unable to find file  brout in any of the folders specified by datapath.
#'
#' ## With \code{literal = TRUE}, returns the argument \code{file} without checking
#' ## whether it exists.
#' datafile("mydata.txt", literal = TRUE)

#' @importFrom dplyr '%>%'
#' @export
datafile <- function(file, literal = FALSE){
  if (literal) {
    file
  } else {
    if (exists("datapath")) {
      pathfile <- paste(datapath, file, sep = "")
      if (sum(file.exists(pathfile)) == 0)
        stop(paste("Unable to find file ", file,
                   "in any of the folders specified by datapath."))
      file <- pathfile[file.exists(pathfile)][1]
      file
    } else {
      stop(
        "literal is set to FALSE, but the object datapath does not exist")
    }
  }
}

#' Provides information about rows matching for dplyr join verbs
#'
#' Provides verbose information about rows matching when joining dataframes
#' using dplyr join verbs. It prints a summary of the number of rows in \code{x} that
#' have no match in \code{y}, rows in \code{x} that have only one match in
#' \code{y} and rows in \code{x} that have more than one match in \code{y}.
#'
#' \code{info_join} admits the same arguments as the join verbs
#' (\code{\link[dplyr]{left_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}},
#'  \code{\link[dplyr]{anti_join}} and
#' \code{\link[dplyr]{semi_join}}) in \code{dplyr}, and prints a summary of rows matching: the
#' number of rows in \code{x} that have no match in \code{y}, rows in \code{x}
#' that have only one match in \code{y} and rows in \code{x} that have more than
#' one match in \code{y}, and symmetrically for the rows in \code{y} with no,
#' one, or more than one match in \code{x}.
#'
#' It is useful to anticipate the results of the join action, in particular to
#' control the dimension of the output dataframe. It allows, for example, to prevent unvoluntary
#' mistakes upon joining \code{x} and \code{y}, when the user may be unaware of
#' duplicated rows matching in \code{y} which results in more rows than in
#' $\code{x}$.
#'
#' @param x, y tbls to join.
#' @param ... Any argument admitted by the join verbs of \code{dplyr}.
#'
#' @examples
#' ##  Picking the second example from the Two-table-verbs vignette in
#' ## dplyr.
#' library("dplyr")
#' df1 <- data_frame(x = c(1, 2), y = 2:1)
#' df2 <- data_frame(x = c(1, 3), a = 10, b = "a")
#' info_join(df1, df2)
#'
#' @importFrom dplyr '%>%'
#' @export
info_join <- function(x, y, ...){
    xt <- x
    yt <- y
    names(xt) <- paste("V", 1:ncol(x), sep = "")
    names(yt) <- paste("V", 1:ncol(y), sep = "")
    grp_cols <- names(xt)
    # Convert character vector to list of symbols
    dots <- lapply(grp_cols, as.symbol)
    xc <- xt %>%
      dplyr::mutate(fila.i_j.x = 1:nrow(xt)) %>%
      dplyr::group_by_(.dots = dots) %>%
      dplyr::mutate(id.i_j.x= fila.i_j.x[1],
                    freq.i_j.x = n(),
                    position.i_j.x = 1:n()) %>%
      dplyr::ungroup()
    grp_cols <- names(yt)
    # Convert character vector to list of symbols
    dots <- lapply(grp_cols, as.symbol)
    yc <- yt %>%
      dplyr::mutate(fila.i_j.y = 1:nrow(yt)) %>%
      dplyr::group_by_(.dots = dots) %>%
      dplyr::mutate(id.i_j.y = fila.i_j.y[1],
                    freq.i_j.y = n(),
                    position.i_j.y = 1:n()) %>%
      dplyr::ungroup()
    names(xc)[1:ncol(x)] <- names(x)
    names(yc)[1:ncol(y)] <- names(y)
    xcyc <- dplyr::full_join(xc, yc, ...)
    OneMapstoZero <- nrow(xcyc %>%
                            dplyr::filter(is.na(id.i_j.y)))
    dfjoinx <- xcyc %>%
      dplyr::filter(!is.na(id.i_j.x)) %>%
      dplyr::group_by(id.i_j.x, position.i_j.x) %>%
      dplyr::mutate(matchy = sum(!is.na(id.i_j.y)))
    OneMapstoOne <- length(unique((dfjoinx %>%
                                     dplyr::filter(matchy == 1))$fila.i_j.x))
    UniqueOneMapstoOne <- length(unique((dfjoinx %>%
                                           dplyr::filter(matchy == 1))$id.i_j.x))
    OneMapstoMore <- length(unique((dfjoinx %>%
                                     dplyr::filter(matchy > 1))$fila.i_j.x))
    UniqueOneMapstoMore <- length(unique((dfjoinx %>%
                                            dplyr::filter(matchy > 1))$id.i_j.x))
    ZeroMapstoOne <- nrow(xcyc %>%
                            dplyr::filter(is.na(id.i_j.x)))
    dfjoiny <- xcyc %>%
      dplyr::filter(!is.na(id.i_j.y)) %>%
      dplyr::group_by(id.i_j.y, position.i_j.y) %>%
      dplyr::mutate(matchx = sum(!is.na(id.i_j.x)))
    OneMapstoOney <- length(unique((dfjoiny %>%
                                     dplyr::filter(matchx == 1))$fila.i_j.y))
    UniqueOneMapstoOney <-
        length(unique((dfjoiny %>%
                         dplyr::filter(matchx == 1))$id.i_j.y))
    MoreMapstoOne <- length(unique((dfjoiny %>%
                                      dplyr::filter(matchx > 1))$fila.i_j.y))
    UniqueMoreMapstoOne <-
        length(unique((dfjoiny %>%
                         dplyr::filter(matchx > 1))$id.i_j.y))
    cat(" Info Join:\n")
    cat("--------------------------------------------------------------------------------\n")
    cat(paste0(" x: ", nrow(x), " x ", ncol(x)), "\t")
    cat(paste0(" y: ", nrow(y), " x ", ncol(y)), "\n")
    cat("--------------------------------------------------------------------------------\n")
    cat(paste0(" Rows in x with no match in y:             ", OneMapstoZero), "\n")
    cat(paste0(" Rows in x with only one match in y:       ",
               OneMapstoOne, " (total)\t",
               UniqueOneMapstoOne,
               " (unique),\n"))
    cat(paste0(" Rows in x with more than one match in y:  ", OneMapstoMore,
               " (total)\t", UniqueOneMapstoMore, " (unique),\n"))
        cat(paste0(" Rows in y with no match in x:             ",
                   ZeroMapstoOne),
            "\n")
    cat(paste0(" Rows in y with only one match in x:       ",
               OneMapstoOney, " (total)\t",
               UniqueOneMapstoOney,
               " (unique),\n"))
    cat(paste0(" Rows in y with more than one match in x:  ", MoreMapstoOne,
               " (total)\t", UniqueMoreMapstoOne, " (unique),\n"))
    cat("--------------------------------------------------------------------------------\n")
    cat("left_:", nrow(xcyc %>% dplyr::filter(!is.na(id.i_j.x))), "x", ncol(xcyc) - 8, "\t")
    cat("inner_:", nrow(xcyc %>% dplyr::filter(!is.na(id.i_j.x), !is.na(id.i_j.y))),
        "x", ncol(xcyc) - 8, "\t")
    cat("full_:", nrow(xcyc), "x", ncol(xcyc) - 8, "\n")
    cat("anti_:", nrow(xcyc %>% dplyr::filter(!is.na(id.i_j.x), is.na(id.i_j.y))), "x", ncol(x), "\t")
    cat("semi_:", nrow(xcyc %>% dplyr::filter(!is.na(id.i_j.x), !is.na(id.i_j.y))), "x", ncol(x), "\t")
    cat("right_:", nrow(xcyc %>% dplyr::filter(!is.na(id.i_j.y))), "x", ncol(xcyc) -8, "\n")
}

#' Performs a diff between two dataframes
#'
#' For two dataframes \code{df1} and \code{df2} with a unique identifier
#' (\code{key}) and the same columns, performs a
#' diff: it returns (invisibly) a dataframe with the rows in both dataframes which
#' present some differences, and optionally, write a xlsx file (using the
#' \code{openxlsx} package) with the three sheets. The first sheet contains the rows
#' with highlighted  differences, the second sheet contains the rows of
#' \code{df1} that have no match in \code{df2}, while the third sheet contains
#' the rows of \code{df2} with no match in \code{df1}. The differences may be
#' associated to exact matches or  a relative error greater than \code{umbral}
#' for numerical columns.
#'
#' @param df1, df2 dataframes. They should have the same columns, even if they
#' appear in a different order (\code{rbind(df1, df2)} should make sense)
#' @param key String vector. A vector of columns names which serve as a unique identifier.
#' @param file String. The name of an xlsx file where the differences between
#' \code{df1} and \code{df2} are written.
#' @param numcol string vector. A vector of columns names which are numeric and
#'   for which relative error should be checked. If the relative difference
#'   (w.r.t the value of first dataframe) is greater than umbral, it will be
#'   highlighted. Default NULL. 
#' @param umbral numeric. A value that serves as a threshold for the relative
#'   difference in \code{numcol}. It should be a number between 0 and 1. The
#'   default is 0.05 (5%).
#' @return To access the dataframe that contains the differences, the output of
#' \code{diffdataframe} should be assigned to a variable.
#' 
#' @examples
#' library("dplyr")
#' ## consider two dataframes
#' df1 <- data_frame(x = paste("id", c(1, 1, 2)),
#'                   year = c("2001", "2002", "2002"),
#'                   y = letters[1:3])
#' df2 <- data_frame(x = paste("id", 1:2),
#'                   year = rep("2002", 2),
#'                   y = c("a", "c"))
#' ## writes to a xlsx file but does not produce a dataframe:
#'  diffdataframe(df1, df2, key = c("x", "year"), file = "diff-df1-vs-df2.xlsx")
#' ## write the resulting dataframe to an object for further use:
#' diff_df1_df2 <- diffdataframe(df1, df2, key = c("x", "year"))
#' df1$z <- rep(100, 3)
#' df2$z <- c(95, 106, 100)
#' diffdataframe(df1, df2, key = c("x", "year"), numcol = "z", file = "diff-df1-vs-df2.xlsx")
#' @importFrom dplyr '%>%'
#' @export
diffdataframe <- function(df1, df2, key, file = NULL, numcol = NULL,
                          umbral = 0.05){
  if (length(dplyr::union(dplyr::setdiff(names(df1), names(df2)),
                  dplyr::setdiff(names(df2), names(df1)))) > 0){
    stop("names of df1 and df2 present differences")
  }
  if (sum(duplicated(df1[key])) > 0){
    stop("key is not a unique key of df1")
  } else {
      if (sum(duplicated(df2[key])) > 0){
        stop("key is not a unique key of df2")
      }
  }
  hacambiado <- function(x){
      rep(!duplicated(x)[2], 2)
  }
  haydiferenciarelativa <- function(x, umbral){
    if (x[1] == 0) {
      if (tail(x, 1) != 0) {
        x1 <- c(1, 1 + tail(x, 1)-x[1])
      } else {
        x1 <- c(1, 1)
      }
    } else {
      x1 <- x
    }
    rep(abs((head(x1, 1) - tail(x1, 1))/x1[1]) > umbral, 2)
  }
  df1$origen <- "1"
  df2$origen <- "2"
  df12 <- rbind(df1, df2)
  if (is.character(numcol) && sum(sapply(as.list(df12[numcol]), FUN = function(x) !is.numeric(x))) >0){
    stop("Hay columnas no númericas en la especificación de numcol")
  }
  browser()
  arrg_cols <- c(key, "origen")
  grp_cols <- key
  # Convert character vector to list of symbols
  dotsarrg <- lapply(arrg_cols, as.symbol)
  dotsgrp <- lapply(grp_cols, as.symbol)
  cambios <- df12 %>%
    dplyr::arrange_(.dots = dotsarrg) %>%
    dplyr::select(-origen)
  ## Primero creamos un df con una columna que indique si el id tiene fila
  ## duplicado o no (quitando las columnas declaradas como númericas en numcol)
  ndup.df <- cambios[setdiff(names(cambios),numcol)] %>%
    distinct %>% 
    dplyr::group_by_(.dots = dotsgrp) %>%
    mutate(ndup = n()) %>%
    ungroup() %>% 
    select_(.dots = lapply(c(key, "ndup"), as.symbol)) %>%
    distinct
  ## Ahora creamos un df con una columna que indique si las columnas númericas
  ## presentan, id por por id, una diferencia relativa superior a 0.05.
  if (!is.null(numcol)){
    numdiff <- cambios[c(key , numcol)] %>%
      dplyr::group_by_(.dots = dotsgrp) %>%
      mutate_each_(funs(haydiferenciarelativa(., umbral)[1]),
                   vars = numcol) %>%
      ungroup() %>% 
      select_(numcol) %>% 
      rowSums
  } else {
    numdiff <- 0
  }
  num.df <- data.frame(cambios[key], numdiff) %>% distinct
  ## finalmente cambios contiene el df que presente registros id con ndup > 1 o
  ## bien numdiff > 0. 
  cambios <- cambios %>% left_join(ndup.df) %>%
    left_join(num.df) %>%
    filter(ndup > 1 | numdiff > 0)
  ## ---------------------------------------------------------------------------
  ## --    Write to xlsx file
  ## ---------------------------------------------------------------------------
  if (!is.null(file)){
    wb <- openxlsx::createWorkbook()
    options("openxlsx.borderColour" = "#4F80BD")
    options("openxlsx.borderStyle" = "thin")
    openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Arial Narrow")
    openxlsx::addWorksheet(wb, sheetName = "Diff results")
    openxlsx::addWorksheet(wb, sheetName = "Rows that disappear from df1")
    openxlsx::addWorksheet(wb, sheetName = "Rows that appear in df2")
    if (nrow(cambios) > 1){
      colores <- cambios  %>%
        group_by_(.dots = dotsgrp) %>% 
        dplyr::mutate_each_(dplyr::funs(hacambiado(.)),
                            vars = setdiff(names(df1), c(numcol, "origen")))
      if (!is.null(numcol)){
        colores <- colores %>%
          dplyr::mutate_each_(dplyr::funs(haydiferenciarelativa(., umbral = umbral)),
                              vars = numcol)
      }
      colores <- colores %>% ungroup
      colores[key] <- FALSE
      openxlsx::writeData(wb, "Diff results",
                          cambios[setdiff(names(df1), "origen")])
      prevStyle <- openxlsx::createStyle(fontColour = "#FFFFFF",
                                         bgFill = "#FFC7CE")
      actuStyle <- openxlsx::createStyle(fontColour = "#FFFFFF",
                                         bgFill = "red")
      for (j in 1:(ncol(cambios) - 2)){
#          openxlsx::addStyle(wb, 1, style = prevStyle, rows = 1 + which(colores[,j] == TRUE),
#                   cols = rep(j, sum(colores[,j] == TRUE)))
        openxlsx::addStyle(wb, 1,
                           style = actuStyle,
                           rows = 1 + which(colores[,j] == TRUE),
                           cols = rep(j, sum(colores[,j] == TRUE)))
      }
    }
    df1notin2 <- df1 %>%
      dplyr::anti_join(df2, by = key) %>%
      dplyr::mutate(origen = NULL)
    openxlsx::writeData(wb, 2, df1notin2)
    df2notin1 <- df2 %>%
      dplyr::anti_join(df1, by = key) %>%
      dplyr::mutate(origen = NULL)
    openxlsx::writeData(wb, 3, df2notin1)
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  }
  invisible(cambios)
}
#' Returns the final letter of a DNI or NIE 8 characters string.
#'
#' The function implements the algorithm described in
#' \url{http://www.interior.gob.es/web/servicios-al-ciudadano/dni/calculo-del-digito-de-control-del-nif-nie}
#' to derive the final letter of a 8 characters. The first character in the
#' string may be either a number or "X", "Y" or "Z". The remaining 7 characters
#' must be digits. If \code{dni} does not match this format, the function
#' returns \code{NA}.
#'
#' @param dni String vector.
#' @return the final letter of the DNI / NIE. The function is vectorized, so
#' that it admits a vector of strings and returns a vector. If \code{dni} does
#' not match the right format for a DNI or NIE, it returns \code{NA}.
#' @examples
#' letraNIFE(c("12345678", "1234", "A1234567", "Y1234567", "X1234567"))
#'
#' @importFrom dplyr '%>%'
#' @export
## Cálculo letra NIF NIE
## Algoritmo explicado en
## http://www.interior.gob.es/web/servicios-al-ciudadano/dni/calculo-del-digito-de-control-del-nif-nie
letraNIFE <-function(dni){
    ## Controlamos que el formato de DNI o NIE es correcto
    formatovalido <-  grepl("[XYZ0-9][0-9]{7}", dni)
    tipo <-
        ifelse(formatovalido,
               ifelse(grepl((d1 <- substr(dni, 1, 1)),
                                     pattern = "[0-9]"),
                      "DNI",
                      "NIE"),
               "ERROR")
    vletra <- 0:2
    names(vletra) = c("X", "Y", "Z")
    letra <- rep(NA, length(dni))
    letra[tipo == "DNI"] <-
        letrasfinales$LETRA[1 + as.numeric(dni[tipo == "DNI"])%% 23]
    letra[tipo == "NIE"] <-
        letrasfinales$LETRA[1 +
                              as.numeric(paste(
                                  vletra[d1[tipo == "NIE"]],
                                  substr(dni[tipo == "NIE"], 2, 8),
                                  sep =""))
              %% 23]
    letra
}
#' Una función para escribir un dataframe a un fichero Excel.
#'
#' Combina diferentes funciones de \code{openxlsx} para, dado  un workbook, crear
#' una hoja, escribir un título, escribir un dataframe en el workbook en un
#' único paso. Permite también la combinación (merge) de celdas verticales
#' consecutivas que contienen el mismo valor, para mejorar la presentación de
#' tablas.
#' Combina bien con  el "pipe operator" de \code{magrittr}, tal como
#' queda ilustrado en la vignette.
#'
#' @param wb El workbook que será modificado.
#' @param sheetName String. El nombre de la hoja donde se debe escribir el
#' dataframe \code{x}. Si no existe en el workbook, será creada.
#' @param x dataframe. El dataframe que se escribirá en el workbook.
#' @param titulo String. El texto que se coloca encima del dataframe en el
#' workbook. Si no se desea ningún título, es suficiente con no especificarlo,
#' el defecto de este argumento siendo \code{NULL}.
#' @param titleStyle El estilo que se desea usar para \code{titulo}. Debe haber
#' sido creado con \code{openxlsx::createStyle}. Si no se especifica, se usa un
#' estilo con \code{fontsize = 12}, \code{textDecoration = c("bold", "italic")},
#' \code{valign = "center"} y \code{halign = "center"}.
#' @param headerStyle El estilo que se desea usar para la cabecera del dataframe
#' \code{x}. Debe haber sido creado con \code{openxlsx::createStyle}. Si no se
#' especifica, se usa un estilo con color de fuente blanco y fondo azul, además
#' de \code{textDecoration = "Bold"}, \code{valign = "center"} y \code{halign =
#' "center"}.
#' @param areaTitulo String. La especificación formato Excel del área
#' rectangular que debe ocupar el título en la hoja. Se combinarán todas las
#' celdas del área. Por defecto es "A1:C1", es decir las tres primeras celdas de
#' la primera fila.
#' @param upperleftCell String. La especificación formato Excel de la celda
#' superior izquierda del área donde se escribirá el dataframe \code{x}. Por
#' defecto, es dos filas más abajo que la esquina izquierda inferior del título.
#' @param spanColumns String. Permite combinar celdas verticales consecutivas si
#' contienen el mismo valor, para mejorar el aspecto de tablas por ejemplo. Es
#' un vector construido a partir de los nombres de columnas de \code{x} en las
#' que hay que combinar celdas verticales consecutivas iguales. Si \code{x} es
#' un dataframe con columnas \code{profesor},\code{sexo}, \code{asignatura}, por
#' ejemplo, si \code{spanColumn = c("profesor;sexo", "asignatura")}, quiere
#' decir que \code{profesor} y \code{sexo} tienen la misma estructura de
#' combinación, determinada por \code{profesor} (\code{sexo} "hereda" de las
#' combinaciones verticales determinadas por \code{profesor}. \code{asignatura}
#' tiene su propio estructura de combinación vertical, pero está supedita a las
#' combinaciones de \code{profesor}. Ver la vignette para más explicaciones y
#' ejemplos.
#' @param transpose Logical. Indica si se quiere escribir el dataframe
#'   transpuesto.
#' @param colNames String o Logical. Si es FALSE, no se escriben el nombre de
#'   las columnas del dataframe. Si es un vector de caracteres de la misma
#'   longitud que el número de columnas de \code{x}, se usan este vector para escribir.
#' @return Un workbook que es invisible. 
#' @examples
#' library("magrittr")
#' library("openxlsx")
#' opada <- data.frame(Nombre = c("Álvaro", "Antonio", "Mari Carmen",
#'                                "Mathieu"),
#'                    Sexo = c("Hombre", "Hombre", "Mujer", "Hombre"),
#'                      Edad = c(25, 25, 23, 25),
#'                      Nacionalidad = c("720", "720", "720", "250")) 
#'
#'## Escribimos en un fichero
#' \dontrun{
#' createWorkbook() %>%
#'      escribirTablaDatos(createWorkbook(),
#'                         sheetName = "La OPADA",
#'                         x = opada,
#'                         titulo = "Miembros de la OPADA",
#'                         areaTitulo = "A2:D2",
#'                         withFilter = FALSE) %>%
#'      saveWorkbook("ejemplo.xlsx", overwrite = TRUE)
#' }
#' ## Para más ejemplos, ver la vignette
#' @importFrom dplyr '%>%'
#' @export
escribirTablaDatos <- function(wb,
                               sheetName,
                               x,
                               titulo = NULL,
                               titleStyle = "defecto",
                               headerStyle = "defecto",
                               areaTitulo = "A1:C1",
                               upperleftCell = NULL,
                               spanColumns = NULL,
                               transpose = FALSE,
                               colNames = colnames(x),
                               ...){
  ## ---------------------------------------------------------------------------
  ##
  ## Nos ocupamos de colNames: si es un
  if (is.character(colNames) && length(colNames) == ncol(x))  {
    nombrescolumnas <- colNames
    colNames <- TRUE
  } else {
    if (! is.logical(colNames) ) {
      stop("El argumento colNames debe ser o bien lógico o bien un vector de caracteres de la misma longitud que names(x)")
    } else {
      nombrescolumnas <- colnames(x)
    }
  }
  ## -------------------------------------------------------------------------
  ##
  ## Comprobamos si existe ya sheetName, y en caso contrario creamos la hoja
  ##
  ## -------------------------------------------------------------------------
  if (sum(sheetName %in%  names(wb)) == 0){
    openxlsx::addWorksheet(wb, sheetName = sheetName)
  }
  ## -------------------------------------------------------------------------
  ##
  ## Escribimos el titulo
  ##
  ## -------------------------------------------------------------------------
  if (!is.null(titulo)){
    u <- regexpr("[A-Z]+[1-9][0-9]*:[A-Z]+[1-9][0-9]*", areaTitulo)
    if (!identical(attr(u, "match.length"),
                   stringr::str_length(areaTitulo))){
      stop("Incorrect  specification for areaTitulo, it should be of the form \"A1:B5\" ")
    }
    areaTitulo <-
      unlist(stringr::str_extract_all(areaTitulo, "[A-Z]+[1-9][0-9]*"))
    titulocolumnrow <- getcellrowcolumn(areaTitulo)
    openxlsx::writeData(wb, sheet = sheetName, x = titulo,
                        colNames = FALSE, rowNames = FALSE,
                        startRow = titulocolumnrow$row[1],
                        startCol = titulocolumnrow$col[1])
    ## estilo del título
    if (identical(titleStyle, "defecto")) {
      titleStyle <- openxlsx::createStyle(fontSize = 12,
                                          textDecoration = c("bold", "italic"),
                                          valign = "center",
                                          halign = "center")
    }
    openxlsx::addStyle(wb, sheetName,
                       style = titleStyle, rows = titulocolumnrow$row[1],
                       cols = titulocolumnrow$col[1])
    ## combinamos las celdas del título
    openxlsx::mergeCells(wb, sheetName,
                         cols = titulocolumnrow$column,
                         rows = titulocolumnrow$row)
  } else {
    ## si titulo es NULL, imponenemos que areaTitulo corresponda al valor
    ## por defecto
    titulocolumnrow <- getcellrowcolumn(c("A1", "C1"))
  }
  ## -------------------------------------------------------------------------
  ##
  ## Escribimos el dataframe
  ##
  ## -------------------------------------------------------------------------
  if (is.null(upperleftCell)){
    ulcell <- data.frame(column = min(titulocolumnrow$column),
                         row = max(titulocolumnrow$row) + 2)
  } else {
    ulcell <- getcellrowcolumn(upperleftCell)
  }
  ## -------------------------------------------------------------------------
  ##     Estilo del header del dataframe
  ## -------------------------------------------------------------------------
  if (identical(headerStyle, "defecto")){
    headerStyle <- openxlsx::createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                                         halign = "center", valign = "center", textDecoration = "Bold",
                                         border = "TopBottomLeftRight")
  }
  if (colNames) {
    openxlsx::addStyle(wb, sheetName,
                       style = headerStyle,
                       rows = rep(ulcell$row[1], ncol(x)),
                       cols = ulcell$col[1] + 0:(ncol(x) -1))
  }
  ## -------------------------------------------------------------------------
  ##
  ## Combinamos celdas verticales que corresponden a valores consecutivos
  ## iguales
  ##
  ## -------------------------------------------------------------------------
  if (!is.null(spanColumns)){
    ## obtenemos una lista con las posiciones de las columnas de spanColumns
    f <- function(nombrescolumnas){
      sapply(nombrescolumnas,
             FUN = function(v){
               if (sum(tt <- ! v %in% colnames(x))>0) {
                 stop("Especificación incorrecta de spanColumns: ",
                      v[tt], " no es(son) columna(s) del dataframe")
               }
               which((colnames(x) %in% v))
             })
    }
    columnas <- f(spanColumns)
    ## aplicamos el mergeCellsTabla de utility-functions.R a los distintos
    ## vectores componentes de la lista columnas.
    mergeCellsTabla(wb,
                    sheetName = sheetName,
                    df = x,
                    columnas = columnas,
                    startRow = ulcell$row[1],
                    startCol = ulcell$column[1],
                    transpose = transpose,
                    colNames = colNames)
  }
  colnames(x) <- nombrescolumnas
  if (transpose) {
    x <- as.data.frame(t(x))
    rowNames <- colNames
    colNames <- FALSE
  }
  
  openxlsx::writeData(wb, sheet = sheetName, x = x,
                      headerStyle = headerStyle ,
                      startCol = ulcell$column[1],
                      startRow = ulcell$row[1],
                      colNames = colNames,
                      rowNames = rowNames,
                      ...)
  openxlsx::setColWidths(wb, sheet = sheetName,
                         cols = ulcell$column:(ulcell$column + ncol(x) - 1),
                         widths = "auto", ignoreMergedCells = TRUE)
  
  invisible(wb)
}
#' Transforma una tabla 2d en un dataframe con el mismo formato
#'
#' Para escribir una tabla en openxlsx usando writeDataTable, es necesario
#' que el objeto sea un dataframe. Esta función permite transformar una
#' tabla de dimensión 2 en un dataframe para escribirla.
#' El argumento cabecerafilas permite especificar en el nombre de cabecera
#' de la columna que contiene las  filas del dataframe
#'
#' @param tabla La tabla que hay que convertir.
#' @param cabecerafilas String. El nombre que se quiere asignar 
#' @return Un dataframe que se puede usar para escribir en el fichero Excel con
#' \code{escribirTablaDatos} o \code{writeDataTable}
#' @examples
#' v1 <- sample(LETTERS[1:4], 50, replace = TRUE)
#' v2 <- sample(c("Hombre", "Mujer"), 50, replace = TRUE, prob = c(0.25, 0.75))
#' x <-tabla2df(table(v1, v2), cabecerafilas = "Categoría")
#' @importFrom dplyr '%>%'
#' @export
tabla2df <- function(tabla, cabecerafilas = ""){
    ## Para escribir una tabla en openxlsx usando writeDataTable, es necesario
    ## que el objeto sea un dataframe. Esta función permite transformar una
    ## tabla de dimensión 2 en un dataframe para escribirla.
    ## El argumento cabecerafilas permite especificar en el nombre de cabecera
    ## de la columna que contiene las  filas del dataframe
    if (length(dim(tabla)) != 2) stop("tabla tiene que tener dimensión 2")
    tt <- as.data.frame.array(tabla)  
    tt$nombresfilas <- row.names(tabla)
    tt <- tt %>% 
      dplyr::select(nombresfilas, everything())
    names(tt)[1] <- cabecerafilas
    tt
}
##
##
##
#' Realiza el profiling de un dataframe
#'
#' Proporciona una matriz que contiene información de profiling sobre el
#' dataframe. Está sobre todo pensado para dataframes con columnas strings, o
#' numéricas pero que toman unos pocos valores (típicamente ficheros SIIU). No
#' proporciona resúmenes numéricos de columnas continuas.
#'
#' @param datos El dataframe
#' @return Una matriz de strings, que se puede usar en un informe Rmd por
#'   ejemplo.
#' @examples
#' opada <- data.frame(Nombre = c("Álvaro", "Antonio", "Mari Carmen",
#'                                "Mathieu"),
#'                    Sexo = c("Hombre", "Hombre", "Mujer", "Hombre"),
#'                      Edad = c(25, 25, 23, 25),
#'                      Nacionalidad = c("720", "720", "720", "250"))
#' profilingdf(opada)
#' ## para usar en un informe Rmd, salida html:
#' print(xtable::xtable(profilingdf(opada)), type = "html", include.rownames = FALSE)
#' @importFrom dplyr '%>%'
#' @export
profilingdf <- function(datos){
    ## devuelve una matrix de strings.
    maxrow <- max(apply(datos, 2,
                        function(x) length(row.names(table(profilingcolumna(x),
                                                           useNA = "always")))))
    ## limitamos a 5 los patrones posibles
    maxrow <- min(maxrow, 5)
    matprofiling <-
        t(matrix(unlist(lapply(datos,
                               function(x) cambiaNA(freqprofiling(x, maxrow)))),
                 nrow=maxrow + 6, byrow=F))  
    matprofiling <- as.data.frame(matrix(c(names(datos),matprofiling),
                                         nrow = nrow(matprofiling),
                                         byrow = FALSE))
    names(matprofiling) <- c("Variable",
                             paste("Patrón", 1:(ncol(matprofiling)-7)),
                             "Número distintos",
                             paste("Valor", 1:5))
    matprofiling
}
##
##
##
#' Extrae los nombres de las tablas de una consulta SQL
#'
#' Proporciona una lista con los nombres de las tablas de las que tira la
#' consulta SQL considerada. Es vectorizada, por lo que se puede pasar un vector
#' de consultas SQL. 
#'
#' @param sqlselect String.  Un vector que contiene las consultas select sql.
#' @return Una lista de la misma longitud de `sqlselect`. El elemento i de esta
#'   lista es un vector que contiene las tablas usadas en la consulta elemento i
#'   de sqlselect.
#' @examples
#' sqlselect <- c( "select * from tabla1 t1",
#'                 "select  from tabla2 where tabla2.key = '1'")
#' extraetablas(sqlselect)
#' ## Un ejemplo más complejo:
#' sqlselect <- "select distinct ctm.any_anyaca TIPMAT_ANYACA, tm.codnum  TIPMAT_COD,
#' tm.desid2 TIPMAT_DESC, e.PLA_CODALF EXP_PLA, e.NUMORD EXP_NUMORD,
#' e.ALU_DNIALU, p.LLENIF ALU_DNILETRA
#' from acad_talu_cpstipmatricula ctm
#' left join acad_talu_tipmatricula tm
#'     on ctm.TMA_CODNUM = tm.CODNUM
#' left join (select * from acad_talu_linmatricula where etalma is null) lm
#'     on lm.PLA_CODALF = ctm.PLA_CODALF and lm.EXP_NUMORD = ctm.EXP_NUMORD and
#'  lm.ANY_ANYACA = ctm.ANY_ANYACA
#' left join acad_talu_expedient e
#'     on e.PLA_CODALF = ctm.PLA_CODALF and e.NUMORD = ctm.EXP_NUMORD
#' left join acad_tuib_persona p
#'     on p.DNIPRS = e.ALU_DNIALU
#' where ctm.ANY_ANYACA = '2015-16'
#' order by TIPMAT_DESC"
#' extraetablas(sqlselect)
#' @importFrom dplyr '%>%'
#' @export
extraetablas <- function(sqlselect){
  ## quitamos retorno de carros, sustituimos espacios duplicados por un único
  ## espacio, quitamos espacios antes de coma.
  sqlselect <- sqlselect %>%
    gsub(pattern = "\n", replacement =" ") %>%
    gsub(pattern = " +", replacement = " ") %>%
     gsub(pattern = " +,", replacement = ",")
  ## sustituimos left right outer cross, natural joins por "join".
  sqlselect <- sqlselect %>%
    gsub(pattern = " (left|right) (outer )*join",
         replacement = " join", ignore.case = TRUE) %>%
    gsub(pattern = "( inner| cross)* join",
         replacement = " join", ignore.case = TRUE) %>%
    gsub(pattern = "natural (((left|right)( outer)*) |inner )*join",
         replacement = "join",
         ignore.case = TRUE)
  ## cambiamos los FROM a from
  sqlselect <- sqlselect %>%
    gsub(pattern = " from ",
         replacement = " from ",
         ignore.case = TRUE)
  ## obtenemos una lista con los vectores de subconsultas (getsubconsultas en
  ## utility-functions.R) 
  listsqlselect <- getsubconsultas(sqlselect)
  ## aplicamos la función tablexpression en utility-functions.R para extraer las
  ## tablas. 
  lapply(listsqlselect, function(sqlstring) unlist(tableexpression(sqlstring)))
}
##
#' Sustituye en un vector los NA por un string 
#'
#' Sustituye en un vector los NA, por el string `sustituto` que debe ser de
#' longitud 1.
#'
#' @param x el vector
#' @param sustituto string. Debe de ser longitud 1. Su defecto es "".
#' @return Un vector de misma longitud que x, donde los NA han sido sustituido
#'   por `sustituto`.
#' @examples
#' changeNA(c(NA, "a", "b", NA)
#' changeNA(c(NA, "a", "b", NA), sustituto = "blanco")
#' @export
cambiaNA  <- function(x, sustituto = ""){
  if (length(sustituto)>1) stop("sustituto debe ser de longitud 1")
  x[is.na(x)] <- sustituto
  x
 }
