#' @title sinaplot
#'
#' @description The SinaPlot is a data visualization chart suitable for plotting
#' any single variable in a multiclass dataset. It is an enhanced jitter strip
#' chart, where the width of the jitter is controlled by the density
#' distribution of the data within each class.
#'
#' @param x numeric vector or a data frame or a list of numeric vectors to be
#' plotted.
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
#'  The borders are defined by the number of samples that occupy the same
#'  bin and the parameter \code{maxwidth} in the following fashion:
#'
#'  \code{xBorder = nsamples * maxwidth}
#'
#'   }
#' }
#'
#' @return
#'
#' \item{x}{discrete x-coordinates, split by group}
#' \item{y}{input values}
#' \item{group}{input groups}
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
#' @rdname sinaplot
#' @export

sinaplot <- function (x, ...)
    UseMethod("sinaplot")

#' @return \code{NULL}
#'
#' @param groups optional vector of \code{length(x)}.
#'
#' @param method choose the method to spread the samples within the same
#' bin along the x-axis. Available methods: "density" and "counts".
#' See \code{Details}.
#'
#' @param scale a logical that indicates whether the width of each group should
#' be scaled relative to the group with the highest density.
#' Default: \code{TRUE}.
#'
#' @param adjust adjusts the bandwidth of the density kernel when
#' \code{method == "density"} (see \code{\link[stats]{density}}).
#'
#' @param bins number of bins to divide the y-axis into when
#' \code{method == "counts"}. Default: 50.
#'
#' @param bin_limit if the samples within the same y-axis bin are more
#' than \code{bin_limit}, the samples's X coordinates will be adjusted.
#'
#' @param maxwidth control the maximum width the points can spread into. Values
#' between 0 and 1.
#'
#' @param plot logical. When \code{TRUE} the sinaplot is produced, otherwise the
#' function returns the new sample coordinates. Default: \code{TRUE}.
#'
#' @param labels labels for each group. Recycled if necessary. By default,
#' these are inferred from the data.
#'
#' @param xlab,ylab axis labels.
#'
#' @param pch,col plotting characters and colors, specified by group.
#' Recycled if necessary.
#'
#' @rdname sinaplot
#' @method sinaplot default
#' @importFrom plyr ddply mutate
#' @importFrom graphics axis par text
#' @importFrom stats complete.cases na.omit
#' @export
sinaplot.default <- function(x,
                     groups = NULL,
                     method = c("density", "counts"),
                     scale = TRUE,
                     adjust = 0.75,
                     bins = 50,
                     bin_limit = 1,
                     maxwidth = 1,
                     plot = TRUE,

                     #Plot parameters
                     labels = NULL,
                     xlab = "",
                     ylab = "",
                     col = NULL,
                     pch = NULL,
                     ...
                     ) {

    ###Check input arguments
    if (!is.null(groups)) {
        if (!is.numeric(x))
            stop("x must be a numeric vector if 'groups' are provided")

        if (length(x) != length(groups))
            stop("x and groups must be of the same length.")
    }

    if (bin_limit < 1 | !is.numeric(bin_limit)){
        warning("Invalid bin_limit value. bin_limit was set to 1.")
        bin_limit <- 1
    }

    if (bins <= 0) {
        warning("bins must be > 0. Set to 50.")
        bins <- 50
    }

    if (maxwidth < 0 | maxwidth > 1){
        warning("maxwidth must be between 0 and 1. maxwidth set to 1.")
        maxwidth <- 1
    }

    if (is.null(names(x))) {
        x.names <- FALSE
    } else {
        x.names <- TRUE
    }

    if (is.null(groups)) {
        if (is.numeric(x)) {

            groups <- 1

        } else {

            groups <- rep(names(x), sapply(x, length))
            x <- unlist(x, use.names = FALSE)
        }
    }

    bin <- NULL

    method <- match.arg(method)
    ###end

    #remove redundant labels
    groups <- factor(groups, levels = unique(groups))

    data <- data.frame(x = as.numeric(groups), y = x, group = groups,
                       x_translation = 0)

    #remove NA's
    count_na <- sum(!complete.cases(data))
    if (count_na > 0) {

        warning(paste("Removed", count_na,
                      "rows containing non-finite values (sinaplot)"),
                sep = " ")

        data <- na.omit(data)
    }

    data <- ddply(data, "group", .compute, method, maxwidth, adjust, bin_limit,
                  bins)

    if (scale) {
        group_max  <- ddply(data, "group", mutate,
                            group_max = max(table(bin)))$group_max
        group_scaling_factor <- group_max / max(group_max)
    } else {
        group_scaling_factor <- 1
    }

    data$scaled <- data$x + data$x_translation * group_scaling_factor

    #drop columns
    data$x_translation <- NULL
    data$bin <- NULL

    if(missing(labels) || is.null(labels)) {
        if(!x.names) {
            if(length(levels(groups)) == 1) {
                labels <- NA
            } else {
                labels <- 1:length(levels(groups))
            }
        } else {
            labels <- levels(groups)
        }
    } else {
        labels <- rep(labels, length.out = length(levels(groups)))
    }

    if (is.null(col))
        col <- par("col")
    else
        col <- rep(rep(col, length.out = length(unique(groups))),
                       times = table(groups))

    if (is.null(pch))
        pch <- par("pch")
    else
        pch <- rep(rep(pch, length.out = length(unique(groups))),
                   times = table(groups))

    data$col <- col
    data$pch <- pch

    if (plot){

        plot(data$scaled, data$y, xlab = xlab, ylab = ylab, xaxt = "n",
             col = col, pch = pch,
             xlim = c(0.5, length(levels(data$group)) + 0.5), ...)
        axis(1, at = 1:length(levels(data$group)), labels = labels)
    }

    invisible(data)
}

#' @return \code{NULL}
#' @rdname sinaplot
#'
#' @param formula a formula, such as y ~ grp, where y is a numeric vector of
#' data values to be split into groups according to the grouping variable grp
#' (usually a factor).
#' @param data a data.frame (or list) from which the variables in formula should
#' be taken.
#' @param subset an optional vector specifying a subset of observations to be
#' used for plotting.
#' @param na.action a function which indicates what should happen when the data
#' contain NAs. The default is to ignore missing values in either the response
#' or the group.
#' @method sinaplot formula
#' @export
sinaplot.formula <-
    function(formula, data = NULL, ..., subset, na.action = NULL,
             xlab, ylab)
    {
        if(missing(formula) || (length(formula) != 3L))
            stop("'formula' missing or incorrect")
        m <- match.call(expand.dots = FALSE)
        if(is.matrix(eval(m$data, parent.frame())))
            m$data <- as.data.frame(data)
        m$... <- NULL
        m$xlab <- NULL
        m$ylab <- NULL
        m$na.action <- na.action # force use of default for this method
        ## need stats:: for non-standard evaluation
        m[[1L]] <- quote(stats::model.frame)
        mf <- eval(m, parent.frame())
        response <- attr(attr(mf, "terms"), "response")
        if (missing(xlab))
            xlab <- as.character(formula)[3]
        if (missing(ylab))
            ylab <- names(mf)[response]
        sinaplot(split(mf[[response]], mf[-response]), xlab = xlab, ylab = ylab,
                 ...)
}

.compute <- function(data, method, maxwidth, adjust, bin_limit, n_bins) {
    #initialize x_translation and bin_counts to 0

    #if group has less than 2 points return as is
    if (nrow(data) < 2) {
        data$bin <- 1
        data
    }

    #per bin sample density
    if (method == "density") {
        density <- stats::density(data$y, adjust = adjust, na.rm = TRUE)

        #confine the samples in a (-maxwidth/2, -maxwidth/2) area around the
        #group's center
        intra_scaling_factor <- 0.5 * maxwidth / max(density$y)

        # assign data points to density bins
        data$bin <- findInterval(data$y, density$x)

        # jitter points based on the density
        set.seed(75)
        x_translation <- sapply(density$y[data$bin], jitter, x = 0, factor = 1)

        #scale and store new x coordinates
        data$x_translation <- x_translation * intra_scaling_factor

    } else {

        bins <- .bin_y(range(data$y), n_bins)
        #per bin sample count
        data$bin <- findInterval(data$y, bins)

        bin_counts <- table(data$bin)

        #allow up to 50 samples in a bin without scaling
        intra_scaling_factor <- 50 * maxwidth / max(bin_counts)

        for (i in names(bin_counts)) {
            #examine bins with more than 'bin_limit' samples
            if (bin_counts[i] > bin_limit){
                cur_bin <- bins[ as.integer(i) : (as.integer(i) + 1)]

                #find samples in the current bin and translate their X coord.
                points <- findInterval(data$y, cur_bin) == 1

                #compute the border margin for the current bin.
                xmax <- bin_counts[i] / 100

                #assign the samples uniformely within the specified range
                set.seed(75)
                x_translation <- stats::runif(bin_counts[i], - xmax, xmax)

                #scale and store new x coordinates
                data$x_translation[points] <-
                    x_translation * intra_scaling_factor
            }
        }
    }

    data
}

.bin_y <- function(data, bins) {
    #get y value range
    ymin <- min(data)
    ymax <- max(data)

    #window width
    window_size <- (ymax - ymin) / (bins + 1e-8)

    ybins <- c()
    for (i in 0:ceiling(bins)) {
        ybins <- c(ybins, ymin + i * window_size)
    }

    ybins
}
