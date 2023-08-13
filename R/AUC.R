#' Calculate area under the curve
#' 
#' Note: This function is taken straight from Mike Blazanin's gcplyr package
#' please see https://mikeblazanin.github.io/gcplyr/ for much more functionality
#' 
#' This function takes a vector of \code{x} and \code{y} values
#' and returns a scalar for the area under the curve, calculated using 
#' the trapezoid rule
#'  
#' @param x Numeric vector of x values
#' @param y Numeric vector of y values
#' @param xlim Vector, of length 2, delimiting the x range over which the
#'             area under the curve should be calculated (where NA can be
#'             provided for the area to be calculated from the start or to
#'             the end of the data)
#' @param blank Value to be subtracted from \code{y} values before calculating
#'              area under the curve
#' @param na.rm a logical indicating whether missing values should be removed
#' @param neg.rm a logical indicating whether \code{y} values below zero should 
#'               be treated as zeros. If \code{FALSE}, area under the curve
#'               for negative \code{y} values will be calculated normally,
#'               effectively subtracting from the returned value.
#' 
#' @details 
#' This function is designed to be compatible for use within
#'  \code{dplyr::group_by} and \code{dplyr::summarize}
#'
#' @return A scalar for the total area under the curve
#'             
#' @export

auc <- function(x, y, xlim = NULL, blank = 0, na.rm = TRUE, neg.rm = FALSE) {
  if(!is.vector(x)) {stop(paste("x is not a vector, it is class:", class(x)))}
  if(!is.vector(y)) {stop(paste("y is not a vector, it is class:", class(y)))}
  
  x <- make.numeric(x)
  y <- make.numeric(y)
  
  #remove nas
  dat <- rm_nas(x = x, y = y, na.rm = na.rm, stopifNA = TRUE)
  
  if(length(dat$y) <= 1) {return(NA)}
  
  #reorder
  dat <- reorder_xy(x = dat[["x"]], y = dat[["y"]])
  
  x <- dat[["x"]]
  y <- dat[["y"]]
  
  y <- y - blank
  
  #Check if xlim has been specified
  if(!is.null(xlim)) {
    stopifnot(is.vector(xlim), length(xlim) == 2, any(!is.na(xlim)))
    if(is.na(xlim[1])) {xlim[1] <- x[1]}
    if(is.na(xlim[2])) {xlim[2] <- x[length(x)]}
    if(xlim[1] < x[1]) {
      warning("xlim specifies lower limit below the range of x\n")
      xlim[1] <- x[1]
    } else { #add lower xlim to the x vector and the interpolated y to y vector
      if (!(xlim[1] %in% x)) {
        x <- c(x, xlim[1])
        xndx <- max(which(x < xlim[1]))
        y <- c(y, solve_linear(x1 = x[xndx], y1 = y[xndx],
                               x2 = x[xndx+1], y2 = y[xndx+1],
                               x3 = xlim[1], named = FALSE))
        
        #reorder
        dat <- reorder_xy(x = x, y = y)
        
        x <- dat[["x"]]
        y <- dat[["y"]]
      }
    }
    
    if(xlim[2] > x[length(x)]) {
      warning("xlim specifies upper limit above the range of x\n")
      xlim[2] <- x[length(x)]
    } else { #add upper xlim to the x vector and the interpolated y to y vector
      if (!(xlim[2] %in% x)) {
        x <- c(x, xlim[2])
        xndx <- max(which(x < xlim[2]))
        y <- c(y, solve_linear(x1 = x[xndx], y1 = y[xndx],
                               x2 = x[xndx+1], y2 = y[xndx+1],
                               x3 = xlim[2], named = FALSE))
        
        #reorder
        dat <- reorder_xy(x = x, y = y)
        
        x <- dat[["x"]]
        y <- dat[["y"]]
      }
    }
    y <- y[(x >= xlim[1]) & (x <= xlim[2])]
    x <- x[(x >= xlim[1]) & (x <= xlim[2])]
  }
  
  if(any(y < 0)) {
    if(neg.rm == TRUE) {y[y < 0] <- 0
    } else {warning("some y values are below 0")}
  }
  
  #Calculate auc
  # area = 0.5 * (y1 + y2) * (x2 - x1)
  return(sum(0.5 * 
               (y[1:(length(y)-1)] + y[2:length(y)]) *
               (x[2:length(x)] - x[1:(length(x)-1)])))
}

make.numeric <- function(x, varname) {
  if(is.null(x) | is.numeric(x)) {return(x)
  } else if(canbe.numeric(x)) {return(as.numeric(x))
  } else {stop(paste(varname, "cannot be coerced to numeric"))}
}

rm_nas <- function(..., na.rm, stopifNA = FALSE) {
  out <- list(..., "nas_indices_removed" = NULL)
  
  lengths <- unlist(lapply(list(...), length))
  if(!all_same(lengths[lengths != 0])) {
    stop(paste(paste(names(list(...)), collapse = ","), 
               "are not all the same length"))}
  
  if(na.rm == TRUE) {
    out[["nas_indices_removed"]] <- 
      unique(unlist(lapply(X = list(...), FUN = function(x) which(is.na(x)))))
    if(length(out[["nas_indices_removed"]]) > 0) {
      out[["nas_indices_removed"]] <- 
        out[["nas_indices_removed"]][order(out[["nas_indices_removed"]])]
      out[names(out) != "nas_indices_removed"] <-
        lapply(X = out[names(out) != "nas_indices_removed"],
               FUN = function(x, idx_rem) x[-idx_rem],
               idx_rem = out[["nas_indices_removed"]])
    } else {out["nas_indices_removed"] <- list(NULL)}
  } else { #don't remove NA's
    if(stopifNA == TRUE && any(unlist(lapply(list(...), is.na)))) {
      stop("Some values are NA but na.rm = FALSE")
    }
  }
  
  return(out)
}

all_same <- function(x) {
  if(length(x) == 0 || length(unique(x)) == 1) {return(TRUE)
  } else {return(FALSE)}
}

reorder_xy <- function(x = NULL, y) {
  if(!is.null(x)) {
    #Save orig order info so we can put things back at the end
    start_order <- order(x)
    #Reorder
    y <- y[start_order]
    x <- x[start_order]
  } else {start_order <- 1:length(y)}
  
  return(list(x = x, y = y, order = start_order))
}
