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
#' \dontrun{
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
    ## no funciona este cambio de names porque impide usar el argumento by en ...
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
      dplyr::group_by(id.i_j.x) %>%
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
      dplyr::group_by(id.i_j.y) %>%
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
    cat("left_:", nrow(xcyc %>% filter(!is.na(id.i_j.x))), "x", ncol(xcyc) - 8, "\t")
    cat("inner_:", nrow(xcyc %>% filter(!is.na(id.i_j.x), !is.na(id.i_j.y))),
        "x", ncol(xcyc) - 8, "\t")
    cat("full_:", nrow(xcyc), "x", ncol(xcyc) - 8, "\n")
    cat("anti_:", nrow(xcyc %>% filter(!is.na(id.i_j.x), is.na(id.i_j.y))), "x", ncol(x), "\t")
    cat("semi_:", nrow(xcyc %>% filter(!is.na(id.i_j.x), !is.na(id.i_j.y))), "x", ncol(x), "\t")
    cat("right_:", nrow(xcyc %>% filter(!is.na(id.i_j.y))), "x", ncol(xcyc) -8, "\n")
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
#' the rows of \code{df2} with no match in \code{df1}.
#'
#' @param df1, df2 dataframes. They should have the same columns, even if they
#' appear in a different order (\code{rbind(df1, df2)} should make sense)
#' @param key String vector. A vector of columns names which serve as a unique identifier.
#' @param file String. The name of an xlsx file where the differences between
#' \code{df1} and \code{df2} are written.
#' @return To access the dataframe that contains the differences, the output of
#' \code{diffdataframe} should be assigned to a variable.
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
#'
#' @importFrom dplyr '%>%'
#' @export
diffdataframe <- function(df1, df2, key, file = NULL)
{
  if (length(dplyr::union(dplyr::setdiff(names(df1), names(df2)),
                  dplyr::setdiff(names(df2), names(df1)))) > 0)
  {
    stop("names of df1 and df2 present differences")
  }
  if (sum(duplicated(df1[key])) > 0)
  {
    stop("key is not a unique key of df1")
  } else
    {
      if (sum(duplicated(df2[key])) > 0)
      {
        stop("key is not a unique key of df2")
      }
    }
  hacambiado <- function(x)
  {
      rep(!duplicated(x)[2], 2)
  }
  df1$origen <- "1"
  df2$origen <- "2"
  df12 <- rbind(df1, df2)
  arrg_cols <- c(key, "origen")
  grp_cols <- key
  # Convert character vector to list of symbols
  dotsarrg <- lapply(arrg_cols, as.symbol)
  dotsgrp <- lapply(grp_cols, as.symbol)
  cambios <- df12 %>%
    dplyr::arrange_(.dots = dotsarrg) %>%
    dplyr::select(-origen) %>%
    dplyr::distinct() %>%
    dplyr::group_by_(.dots = dotsgrp) %>%
    dplyr::mutate(n = n())  %>%
    dplyr::filter(n > 1)

  ## ---------------------------------------------------------------------------
  ## --    Write to xlsx file
  ## ---------------------------------------------------------------------------
  if (!is.null(file)){
      colores <- cambios  %>%
        dplyr::mutate_each(funs(hacambiado(.)))
      wb <- openxlsx::createWorkbook()
      options("openxlsx.borderColour" = "#4F80BD")
      options("openxlsx.borderStyle" = "thin")
      openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Arial Narrow")
      openxlsx::addWorksheet(wb, sheetName = "Diff results")
      openxlsx::addWorksheet(wb, sheetName = "Rows that disappear from df1")
      openxlsx::addWorksheet(wb, sheetName = "Rows that appear in df2")
      openxlsx::writeData(wb, "Diff results", cambios %>% select(-n))
      prevStyle <- openxlsx::createStyle(fontColour = "#FFFFFF",
                                         bgFill = "#FFC7CE")
      actuStyle <- openxlsx::createStyle(fontColour = "#FFFFFF",
                                         bgFill = "red")
      for (j in 1:(ncol(cambios) - 1)){
          openxlsx::addStyle(wb, 1, style = prevStyle, rows = 1 + which(colores[,j] == TRUE),
                   cols = rep(j, sum(colores[,j] == TRUE)))
          openxlsx::addStyle(wb, 1, style = actuStyle, rows = 1 + which(colores[,j] == TRUE),
                   cols = rep(j, sum(colores[,j] == TRUE)))
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
