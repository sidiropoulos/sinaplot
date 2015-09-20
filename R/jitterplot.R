#' @title jitterplot
#' @description
#'
#' @author Nikos Sidiropoulos
#'
#' @param x numeric vector of values to be plotted
#' @param groups vector of length x
#' @param labels optional
#' @param plot logical. Default: TRUE
#'
#' @return
#'
#'
#' @export

jitterplot <- function(x, groups, labels = NULL, plot = TRUE) {


    #get min,max y values

    #
    for (i in 1:groups) {
        kati <- .getXcoord(x[groups == unique(groups)[i]], i)
    }
}


.getXcoord <- function(data, groups, yrange){

    #number of groups
    ngroups <- length(unique(groups))

    #get ylim
    ymin <- min(data)
    ymax <- max(data)

    for

    #Create an array
    d <- cbind( as.numeric(data), rep(position, length(data)))


}
