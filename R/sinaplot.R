#' @title sinaplot
#' @description blah blah blah blah blah blah blah blah blah blah blah blah
#'
#' @param x numeric vector of values to be plotted
#' @param groups vector of length x
#' @param labels optional
#' @param plot logical. Default: TRUE
#' @param neighbLimit blah blah
#' @param adjust blah blah blah
#' @param method blah blah blah blah
#' @param ...
#'
#' @return blah blah blah blah
#'
#' @export

sinaplot <- function(x, groups, labels = NULL, plot = TRUE, relScaling = TRUE,
                     ypercentage = 0.02, xpercentage = 0.1, neighbLimit = 1,
                     adjust = 1, method = "neighbours", ...)
{

    ###CHECK STUFF

    ###END OF STUFF CHECKING

    yBins <- .binY(x, ypercentage)

    #calculate new x coordinates
    x <- .getXcoord(x, groups, yBins, xpercentage, relScaling, neighbLimit,
                    adjust, method)

    #number of groups
    ngroups <- length(unique(groups))

    #
    newGroups <- rep(unique(groups), unlist(lapply(x, nrow)))

    x <- do.call(rbind, x)

    if (plot){
        plot(x, xaxt = "n", col = newGroups, ylab = "log2 expression", xlab = "",
             xlim = c(0,(ngroups+1)), ...)
        axis(1, at = 1:ngroups, labels = FALSE)
        text(1:ngroups, par("usr")[3] - 0.4, srt = 45, labels = unique(groups),
             adj = 1, xpd = T)
    } else
        list(x, newGroups)
}

.binY <- function(data, yFraction) {
    #get y value range
    ymin <- min(data)
    ymax <- max(data)

    #window width
    window_size <- (ymax - ymin) * yFraction

    yBins <- c()
    for (i in 0:ceiling(1/yFraction)) {
        yBins <- c(yBins, ymin + i * window_size)
    }

    yBins
}

.getXcoord <- function(data, groups, yBins, xFraction, relScaling,
                       neighbLimit, adjust, method){

    #number of groups
    ngroups <- length(unique(groups))

    xyArray <- c()

    ###find the densiest area in the plot
    neighbours <- list()

    #set maxNeighbours
    maxNeighbours <- 0

    #method == "density"
    if (method == "density"){
        maxDensity <- 0
        densities <- c()
    }

    for (j in 1:ngroups){

        #extract samples per group and store them in a data.frame
        cur_xyArray <- as.data.frame(cbind(rep(j, sum(groups == unique(groups)[j])),
                                           as.numeric(data[groups == unique(groups)[j]])))
        colnames(cur_xyArray) <- c("x", "y")

        #find the densiest neighbourhood in the current group and compare it
        #with the global max
        cur_neighbours <- table(findInterval(cur_xyArray$y, yBins))
        cur_maxNeighbours <- max(cur_neighbours)

        if (cur_maxNeighbours > maxNeighbours)
            maxNeighbours <- cur_maxNeighbours

        if (method == "density"){
            #find the highest density value
            cur_density <- density(cur_xyArray$y, adjust = adjust)
            cur_maxDensity <- max(cur_density$y)

            if (cur_maxDensity > maxDensity)
                maxDensity <- cur_maxDensity

            densities <- c(densities, list(cur_density))
        }

        #store neighbour and sample per group data frames in lists
        neighbours <- c(neighbours, list(cur_neighbours))
        xyArray <- c(xyArray, list(cur_xyArray))
    }

    if (method == "density"){
        if (maxDensity > 0.45)
            scalingFactor <- 0.45 / maxDensity
        else
            scalingFactor <- 1
    }else {
        #if the space required to spread the samples in a neighbourhood exceeds
        #1, create a scaling factor to compress the points
        if (maxNeighbours > 1/xFraction){
            scalingFactor <- (1/xFraction) / maxNeighbours
        }else
            scalingFactor <- 1
    }

    relScalingFactor <- 1

    for (j in 1:ngroups){

        #scale all neighbourhoods based on their density relative to the
        #densiest neighbourhood
        if (relScaling)
            relScalingFactor <- max(neighbours[[j]]) / maxNeighbours

        for (i in names(neighbours[[j]])){

            #examine neighbourhoods with more than 1 samples
            if (neighbours[[j]][i] > neighbLimit){
                cur_bin <- yBins[ as.integer(i) : (as.integer(i) + 1)]

                #find samples in the current bin and translate their X coord.
                points <- findInterval(xyArray[[j]]$y, cur_bin) == 1

                if (method == "density")
                    xMax <- mean(densities[[j]]$y[findInterval(densities[[j]]$x,
                                                               cur_bin) == 1])
                else
                    xMax <- xFraction*neighbours[[j]][i] / 2

                xTranslate <- runif(neighbours[[j]][i], -xMax, xMax )

                xyArray[[j]]$x[points] <- xyArray[[j]]$x[points] +
                    (xTranslate * scalingFactor * relScalingFactor)
            }
        }
    }
    xyArray
}

