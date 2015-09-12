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

##' @importFrom dplyr '%>%'
##' @export
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

##' @importFrom dplyr '%>%'
##' @export
info_join <- function(x, y, ...){
    grp_cols <- names(x)
    # Convert character vector to list of symbols
    dots <- lapply(grp_cols, as.symbol)
    xc <- x %>%
      mutate(fila = 1:nrow(x)) %>%
      group_by_(.dots = dots) %>%
      mutate(id = fila[1],
             freq = n(),
             position = 1:n())
    ZeroMapstoOne <- nrow(anti_join(x, y, ...))
    dfjoin <- xc %>%
      filter(position == 1L) %>% 
      semi_join(y, ...) %>%
      left_join(y, ...)
    dfjoin <-
        dfjoin %>%
          group_by(id, freq) %>%
          summarise(ymatches = n())
    UniqueOneMapstoMore <- nrow(dfjoin %>%
                            filter(ymatches >= 2))
    OneMapstoMore <- sum((dfjoin %>%
                            filter(ymatches >= 2))$freq)
    UniqueOneMapstoOne <- nrow(dfjoin %>%
                                 filter(ymatches == 1))
    OneMapstoOne <- sum((dfjoin %>%
                            filter(ymatches == 1))$freq)
    cat(" Info Join:\n")
    cat(paste0(" x: ", nrow(x), " x ", ncol(x)), "\t")
    cat(paste0(" y: ", nrow(y), " x ", ncol(y)), "\n")
    cat(paste0(" 0 -> 1:\t", ZeroMapstoOne), "\n")
    cat(paste0(" 1 -> 1:\t", UniqueOneMapstoOne, " (unique),\t",
               OneMapstoOne, " (total)\n"))
    cat(paste0(" 1 -> more than 1:\t", UniqueOneMapstoMore, " (unique),\t",
               OneMapstoMore, " (total)\n"))
}
      
