#' @title sinaplot
#' @description blah blah blah blah blah blah blah blah blah blah blah blah
#'
#' @param x numeric vector of values to be plotted
#' @param groups vector of length x
#' @param method choose the method to spread the samples within the same
#' neighbourhood along the x-axis. Available methods: "density",
#' "neighbourhood". See \code{Details}.
#' @param scale logical. When set to \code{TRUE} x-coordinate widths across all
#' groups are scaled based on the densiest are in the plot. Default: \code{TRUE}
#' @param yFraction binning factor. The range of the values in \code{x} are
#' binned in windows of length \code{(max(x) - min(x)) * yFraction}. Samples
#' within the same bin belong to the same "neighbourhood".
#' @param neighbLimit if the samples within the same y-axis bin are more than
#' neighbLimit, the samples's X coordinates will be adjusted
#' @param adjust adjusts the bandwidth of the density kernel when
#' \code{method = "density"} (see \code{\link[stats]{density}}).
#' @param xSpread tuning parameter that adjusts the spread of the samples within
#' the same neighbourhood along the x-axis when \code{method = "neighbourhood"}.
#' Accepts values between 0 and 1.
#' @param labels optional arguments that controls the labels along the x-axis.
#' Must be of length \code{unique(groups)}.
#' @param plot logical. When \code{TRUE} the sinaplot is produced, otherwise the
#' function returns the new sample coordinates. Default: \code{TRUE}
#' @param ... Arguments to be passed to methods, such as graphical parameters
#' (see \code{\link{par}}).
#'
#' @details kati
#' \itemize{
#'  \item{\bold{"details"}: blah blah}
#'  \item{"neighbourhood": blsd }
#' }
#' @return if \code{plot = FALSE} a list is returned containing a data.frame
#' with the new sample coordinates and a vector of length x that corresponds to
#' each sample's group.
#'
#' @examples
#'
#' x <- c(rnorm(200, 4, 1), rnorm(200, 5, 2), rnorm(200, 6, 1.5))
#' groups <- c(rep("Cond1", 200), rep("Cond2", 200), rep("Cond3", 200))
#'
#' sinaplot(x, groups)
#' sinaplot(x, groups, scale = FALSE)
#' sinaplot(x, groups, scale = FALSE, adjust = 1/6)
#' sinaplot(x, groups, scale = FALSE, adjust = 3)
#'
#' @export

sinaplot <- function(x, groups, method = "density", scale = TRUE,
                     yFraction = 0.02, neighbLimit = 1, adjust = 3/4,
                     xSpread = 0.1, labels = NULL, plot = TRUE, ...)
{

    ###Check input arguments
    if (length(x) != length(groups))
        stop("x and groups must be of the same length.")

    if (neighbLimit < 1 | !is.numeric(neighbLimit)){
        warning("Invalid neighbLimit value. Neighblimit was set to 1.")
        neighbLimit = 1
    }

    if (!is.null(labels) & length(labels) != length(unique(groups)))
        stop("labels and unique 'groups' values must be of the same length.")

    if (yFraction <= 0)
        stop("yFraction must be > 0")

    if (xSpread <= 0)
        stop("xSpread must be > 0")

    if (!(method %in% c("density", "neighbourhood"))){
        warning("Input method not available. Choosing 'density' instead.")
        message("Available methods: 'density', 'neighbourhood'")
        method <- "density"
    }
    ###end

    yBins <- .binY(x, yFraction)

    #calculate new x coordinates
    x <- .getXcoord(x, groups, yBins, xSpread, scale, neighbLimit,
                    adjust, method)

    #number of groups
    ngroups <- length(unique(groups))

    newGroups <- rep(unique(groups), unlist(lapply(x, nrow)))

    if (!is.null(labels))
        labs <- labels
    else
        labs <- unique(groups)

    x <- do.call(rbind, x)

    if (plot == TRUE){

        if (requireNamespace("ggplot2", quietly = TRUE)) {

            x$groups <- newGroups

            p <- ggplot2::ggplot(ggplot2::aes(x = x, y = y, color = groups),
                                 data = x) + ggplot2::geom_point()
            p <- p + ggplot2::scale_x_discrete(limits = labs) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                                   hjust = 1))
            p <- p + ggplot2::xlab("") + ggplot2::ylab("log2 expression") +
                ggplot2::guides(color=FALSE)
            p

        } else {

            plot(x, xaxt = "n", col = newGroups, ylab = "log2 expression",
                 xlab = "", xlim = c(0,(ngroups+1)), ...)
            axis(1, at = 1:ngroups, labels = FALSE)
            text(1:ngroups, par("usr")[3] - 0.4, srt = 45, labels = labs,
                 adj = 1, xpd = T)
        }
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

.getXcoord <- function(data, groups, yBins, xSpread, scale,
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

    relScalingFactor <- 1

    for (j in 1:ngroups){

        #confine the sample
        if (method == "density"){
            if (max(densities[[j]]$y) > 0.48)
                scalingFactor <- 0.48 / max(densities[[j]]$y)
            else
                scalingFactor <- 1
        }else {
            #if the space required to spread the samples in a neighbourhood exceeds
            #1, create a scaling factor to compress the points
            if (max(neighbours[[j]]) > 1/xSpread){
                scalingFactor <- (1/xSpread) / max(neighbours[[j]])
            }else
                scalingFactor <- 1
        }

        #scale all neighbourhoods based on their density relative to the
        #densiest neighbourhood
        if (scale == TRUE)
            relScalingFactor <- max(neighbours[[j]]) / maxNeighbours

        for (i in names(neighbours[[j]])){

            #examine neighbourhoods with more than 'neighbLimit' samples
            if (neighbours[[j]][i] > neighbLimit){
                cur_bin <- yBins[ as.integer(i) : (as.integer(i) + 1)]

                #find samples in the current bin and translate their X coord.
                points <- findInterval(xyArray[[j]]$y, cur_bin) == 1

                #compute the border margin for the current bin
                if (method == "density")
                    xMax <- mean(densities[[j]]$y[findInterval(densities[[j]]$x,
                                                               cur_bin) == 1])
                else
                    xMax <- xSpread*neighbours[[j]][i] / 2

                #assign the samples uniformely within the specified range
                xTranslate <- runif(neighbours[[j]][i], -xMax, xMax )

                #store new x coordinates
                xyArray[[j]]$x[points] <- xyArray[[j]]$x[points] +
                    (xTranslate * scalingFactor * relScalingFactor)
            }
        }
    }
    xyArray
}

