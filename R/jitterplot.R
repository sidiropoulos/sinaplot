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

jitterplot <- function(x, groups, labels = NULL, plot = TRUE, ypercentage = 0.02)
{

    x <- .getXcoord(data, groups, xpercentage = 0.10, ypercentage = 0.02 )
}


.getXcoord <- function(data, groups, xpercentage, ypercentage){

    #number of groups
    ngroups <- length(unique(groups))


    #get ylim
    ymin <- min(data)
    ymax <- max(data)

    window_size <- (ymax - ymin) * ypercentage

    #
    ybreaks <- c()
    for (i in 0:ceiling(1/ypercentage)) {
        ybreaks <- c(ybreaks, ymin + i * window_size)
    }

    #
    xyArray <- c()

    #find the densiest area in the plot

    neighbours <- list()

    #set maxNeighbours
    maxNeighbours <- 0
    for (j in 1:length(levels(groups))){
        cur_xyArray <- as.data.frame(cbind(rep(j, sum(groups == levels(groups)[j])),
                                           as.numeric(data[groups == levels(groups)[j]])))
        colnames(cur_xyArray) <- c("x", "y")

        cur_neighbours <- table(findInterval(cur_xyArray$y, ybreaks))
        cur_maxNeighbours <- max(cur_neighbours)

        if (cur_maxNeighbours > maxNeighbours)
            maxNeighbours <- cur_maxNeighbours

        neighbours <- c(neighbours, list(cur_neighbours))

        xyArray <- c(xyArray, list(cur_xyArray))

    }


    for (j in 1:length(levels(groups))){

        #cur_xyArray <- as.data.frame(cbind(rep(j, sum(groups == levels(groups)[j])),
        #                     as.numeric(data[groups == levels(groups)[j]])))
        #colnames(cur_xyArray) <- c("x", "y")

        #define x-axis spacing of samples within the same neighbourhood as a
        #percentage of 1.0

        #get neighbour counts
        #neighbours <- table(findInterval(cur_xyArray$y, ybreaks))
        #maxNeighbours <- max(neighbours)

        #if the space required to spread the samples in a neighbourhood exceeds
        #1, create a scaling factor to compress the points
        if (maxNeighbours > 1/xpercentage){
            scalingFactor <- (1/xpercentage) / maxNeighbours
        }else
            scalingFactor <- 1

        #
        relScalingFactor <- max(neighbours[[j]]) / maxNeighbours

        for (i in names(neighbours[[j]])){

            if (neighbours[[j]][i] > 1){
                cur_break <- ybreaks[ as.integer(i) : (as.integer(i) + 1)]

                break_levels <- levels(cut(1:100*xpercentage*neighbours[[j]][i],
                                           neighbours[[j]][i]))
                breaks <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1",
                                                        break_levels)),
                                upper = as.numeric( sub("[^,]*,([^]]*)\\]",
                                                        "\\1", break_levels)))
                xTranslate <- apply(breaks, 1, mean) - mean(apply(breaks, 1, mean))
                #neighbours[i]

                points <- findInterval(xyArray[[j]]$y, cur_break) == 1
                xyArray[[j]]$x[points] <- xyArray[[j]]$x[points] +
                    (xTranslate * scalingFactor * relScalingFactor)/ 100
            }
        }


    }

    do.call(rbind, xyArray)

}

