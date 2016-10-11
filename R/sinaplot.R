#' @title sinaplot
#'
#' @description The SinaPlot is a data visualization chart suitable for plotting
#' any single variable in a multiclass dataset. It is an enhanced jitter strip
#' chart, where the width of the jitter is controlled by the density
#' distribution of the data within each class.
#'
#' @param x numeric vector of values to be plotted.
#'
#' @param groups vector of \code{length(x)}.
#'
#' @param method choose the method to spread the samples within the same
#' bin along the x-axis. Available methods: "density" and "counts".
#' See \code{Details}.
#'
#' @param scale logical. When set to \code{TRUE} x-coordinate widths across all
#' groups are scaled based on the densiest are in the plot. Default: \code{TRUE}
#'
#' @param bins number of bins to divide the y-axis into. Default: 50.
#'
#' @param bin_limit If the samples within the same y-axis bin are more
#' than \code{bin_limit}, the samples's X coordinates will be adjusted.
#'
#' @param adjust adjusts the bandwidth of the density kernel when
#' \code{method == "density"} (see \code{\link[stats]{density}}).
#'
#' @param maxwidth Control the maximum width the points can spread into. Values
#' between 0 and 1.
#'
#' @param plot logical. When \code{TRUE} the sinaplot is produced, otherwise the
#' function returns the new sample coordinates. Default: \code{TRUE}
#'
#' @param ylab Y axis label
#'
#' @param color vector of \code{length(unique(groups))}. Controls the sample
#' color of each group.
#'
#' @param ... arguments to be passed to \code{\link[graphics]{plot}}.
#'
#' @details There are two available ways to define the x-axis borders for the
#' samples to spread within:
#' \itemize{
#'  \item{\code{method = "density"}
#'
#'   A density kernel is estimated along the y-axis for every sample group. The
#'   borders are then defined by the density curve. Tuning parameter
#'   \code{adjust} can be used to control the density bandwidth in the same way
#'   it is used in \code{\link[stats]{density}}. }
#'
#'  \item{\code{method = "counts"}:
#'
#'  The borders are defined by the number of samples that 'live' in the same
#'  bin and the parameter \code{maxwidth} in the following fashion:
#'
#'  \code{xBorder = nsamples * maxwidth}
#'
#'   }
#' }
#'
#' @return
#'
#' If \code{plot == FALSE} the function returns a data.frame with the
#' following columns:
#'
#' \item{x}{discrete x-coordinates, split by group}
#' \item{y}{input values}
#' \item{group}{input groups}
#' \item{bin_counts}{number of samples per bin per group}
#' \item{scaled}{final x-coordinates, adjusted by sinaplot}
#'
#' @examples
#'
#' x <- c(rnorm(200, 4, 1), rnorm(200, 5, 2), rnorm(400, 6, 1.5))
#' groups <- c(rep("Cond1", 200), rep("Cond2", 200), rep("Cond3", 400))
#'
#' sinaplot(x, groups)
#' sinaplot(x, groups, scale = FALSE)
#' sinaplot(x, groups, scale = FALSE, adjust = 1/6)
#' sinaplot(x, groups, scale = FALSE, adjust = 3)
#'
#' #blood
#' data("blood", package = "sinaplot")
#'
#' old.mar <- par()$mar
#' par(mar = c(9,4,4,2) + 0.1)
#'
#' sinaplot(blood$value, blood$type)
#' sinaplot(blood$value, blood$type, method = "counts")
#' sinaplot(blood$value, blood$type, method = "counts", scale = FALSE)
#'
#' par(mar = old.mar)
#'
#' @importFrom plyr ddply mutate
#' @importFrom graphics axis par text
#' @export

sinaplot <- function(x,
                     groups,
                     method = c("density", "counts"),
                     scale = TRUE,
                     bins = 50,
                     bin_limit = 1,
                     adjust = 3/4,
                     maxwidth = 1,
                     plot = TRUE,

                     #Plot parameters
                     ylab = "",
                     color = NULL,
                     ...
                     )
{

    ###Check input arguments
    if (length(x) != length(groups))
        stop("x and groups must be of the same length.")

    if (!is.null(color)){
        if (length(color) != length(unique(groups))){
            warning("color and unique 'groups' values must be of the same length.")
            color = NULL
        }
    }

    if (bin_limit < 1 | !is.numeric(bin_limit)){
        warning("Invalid bin_limit value. bin_limit was set to 1.")
        bin_limit = 1
    }

    if (bins <= 0) {
        warning("bins must be > 0. Set to 50.")
        bins <- 50
    }

    if (maxwidth < 0 | maxwidth > 1){
        warning("maxwidth must be between 0 and 1. maxwidth set to 1.")
        maxwidth <- 1
    }

    bin_counts <- NULL

    method <- match.arg(method)
    ###end

    #remove redundant labels
    groups <- factor(groups)

    yBins <- .binY(range(x), bins)
    data <- data.frame(x = as.numeric(groups), y = x, group = groups,
                       bin_counts = 0, x_translation = 0)

    data <- ddply(data, "group", .compute, method, maxwidth, adjust, bin_limit,
                  yBins)

    if (scale) {
        group_scaling_factor <-
            ddply(data, "group", mutate,
                  group_max = max(bin_counts))$group_max / max(data$bin_counts)
    } else {
        group_scaling_factor <- 1
    }

    data$scaled <- data$x + data$x_translation * group_scaling_factor

    #drop translation column
    data$x_translation <- NULL

    if (plot){

        if (is.null(color))
            color = "#000000"
        else
            color <- color[as.numeric(data$group)]

        plot(data$scaled, data$y, xlab = "", ylab = ylab, xaxt = "n",
             col = color, xlim = c(0.5, length(levels(data$group)) + 0.5), ...)
        axis(1, at = 1:length(levels(data$group)), labels = FALSE)

        text(x = 1:length(levels(data$group)),
             y = par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),
             labels = levels(data$group), srt=45, xpd=TRUE, adj = 1, cex = .8)

    } else
        data
}

.compute <- function(data, method, maxwidth, adjust, bin_limit, bins) {
    #initialize x_translation and bin_counts to 0

    #if group has less than 2 points return as is
    if (nrow(data) < 2) {
        data$max_bin_counts <- 1
        return(data)
    }

    #per bin sample count
    bin_counts <- table(findInterval(data$y, bins))

    #per bin sample density
    if (method == "density") {
        densities <- stats::density(data$y, adjust = adjust)

        #confine the samples in a (-maxwidth/2, -maxwidth/2) area around the
        #group's center
        if (max(densities$y) > 0.5 * maxwidth)
            intra_scaling_factor <- 0.5 * maxwidth / max(densities$y)
        else
            intra_scaling_factor <- 1

    } else {
        #allow up to 50 samples in a bin without scaling
        if (max(bin_counts) > 50 * maxwidth) {
            intra_scaling_factor <- 50 * maxwidth / max(bin_counts)
        } else
            intra_scaling_factor <- 1
    }

    for (i in names(bin_counts)) {
        #examine bins with more than 'bin_limit' samples
        if (bin_counts[i] > bin_limit){
            cur_bin <- bins[ as.integer(i) : (as.integer(i) + 1)]

            #find samples in the current bin and translate their X coord.
            points <- findInterval(data$y, cur_bin) == 1

            #compute the border margin for the current bin.
            if (method == "density")
                xmax <- mean(densities$y[findInterval(densities$x, cur_bin) == 1])
            else
                xmax <- bin_counts[i] / 100

            #assign the samples uniformely within the specified range
            x_translation <- stats::runif(bin_counts[i], - xmax, xmax)

            #scale and store new x coordinates
            data$x_translation[points] <- x_translation * intra_scaling_factor
            #store bin counts. Used for group-wise scaling.
            data$bin_counts[points] <- bin_counts[i]
        }
    }

    data
}

.binY <- function(data, bins) {
    #get y value range
    ymin <- min(data)
    ymax <- max(data)

    #window width
    window_size <- (ymax - ymin) / (bins + 1e-8)

    yBins <- c()
    for (i in 0:ceiling(bins)) {
        yBins <- c(yBins, ymin + i * window_size)
    }

    yBins
}
