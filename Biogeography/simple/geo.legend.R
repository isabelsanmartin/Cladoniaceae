function (leg = NULL, colors = NULL, alpha = 0.2, ...) 
 {
    if (hasArg(cex)) 
        cex <- list(...)$cex
    else cex <- par()$cex
    if (hasArg(plot)) 
        plot <- list(...)$plot
    else plot <- TRUE
    if (hasArg(show.lines)) 
        show.lines <- list(...)$show.lines
    else show.lines <- TRUE
    obj <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    if (is.null(colors)) {
        colors <- setNames(c(rgb(255, 242, 127, 255, maxColorValue = 255), 
            rgb(255, 230, 25, 255, maxColorValue = 255), rgb(253, 
                154, 82, 255, maxColorValue = 255), rgb(127, 
                198, 78, 255, maxColorValue = 255), rgb(52, 178, 
                201, 255, maxColorValue = 255), rgb(129, 43, 
                146, 255, maxColorValue = 255), rgb(240, 64, 
                40, 255, maxColorValue = 255), rgb(103, 165, 
                153, 255, maxColorValue = 255), rgb(203, 140, 
                55, 255, maxColorValue = 255)), c("Quaternary", "Pliocene", "Late-Miocene", 
            "Middle-Miocene", "Early-Miocene", "Late-Oligocene", "Early_Oligocene", 
            "Late-Eocene", "Early-Eocene"))
    }
    if (is.null(leg)) {
        leg <- rbind(c(2.588, 0), c(5.333, 2.588), c(11.63, 5.333), 
            c(15.97, 11.63), c(23.03, 15.97), c(27.82, 23.03), c(33.90, 
                27.82), c(41.20, 33.90), c(56.00, 41.20))
        rownames(leg) <- c("Quaternary", "Pliocene", "Late-Miocene", 
            "Middle-Miocene", "Early-Miocene", "Late-Oligocene", "Early_Oligocene", 
            "Late-Eocene", "Early-Eocene")
        t.max <- max(obj$xx)
        ii <- which(leg[, 2] <= t.max)
        leg <- leg[ii, ]
        leg[max(ii), 1] <- t.max
    }
    colors <- sapply(colors, make.transparent, alpha = alpha)
    if (plot) {
        y <- c(rep(0, 2), rep(par()$usr[4], 2))
        ylabel <- -1/25 * obj$Ntip
        if (obj$direction == "rightwards") {
            old.usr <- par()$usr
            h <- max(obj$xx)
            new.xlim <- c(h - par()$usr[1], h - par()$usr[2])
            par(usr = c(new.xlim, old.usr[3:4]))
        }
        else old.usr <- par()$usr
        for (i in 1:nrow(leg)) {
            strh <- strheight(rownames(leg)[i])
            polygon(c(leg[i, 1:2], leg[i, 2:1]), y, col = colors[rownames(leg)[i]], 
                border = NA)
            if (show.lines) {
                lines(x = rep(leg[i, 1], 2), y = c(0, par()$usr[4]), 
                  lty = "dotted", col = "grey")
                lines(x = c(leg[i, 1], mean(leg[i, ]) - 0.8 * 
                  cex * get.asp() * strheight(rownames(leg)[i])), 
                  y = c(0, ylabel), lty = "dotted", col = "grey")
                lines(x = c(leg[i, 2], mean(leg[i, ]) + 0.8 * 
                  cex * get.asp() * strheight(rownames(leg)[i])), 
                  y = c(0, ylabel), lty = "dotted", col = "grey")
                lines(x = rep(mean(leg[i, ]) - 0.8 * cex * get.asp() * 
                  strheight(rownames(leg)[i]), 2), y = c(ylabel, 
                  par()$usr[3]), lty = "dotted", col = "grey")
                lines(x = rep(mean(leg[i, ]) + 0.8 * cex * get.asp() * 
                  strheight(rownames(leg)[i]), 2), y = c(ylabel, 
                  par()$usr[3]), lty = "dotted", col = "grey")
            }
            polygon(x = c(leg[i, 1], mean(leg[i, ]) - 0.8 * cex * 
                get.asp() * strh, mean(leg[i, ]) - 0.8 * cex * 
                get.asp() * strh, mean(leg[i, ]) + 0.8 * cex * 
                get.asp() * strh, mean(leg[i, ]) + 0.8 * cex * 
                get.asp() * strh, leg[i, 2]), y = c(0, ylabel, 
                par()$usr[3], par()$usr[3], ylabel, 0), col = colors[rownames(leg)[i]], 
                border = NA)
            strh <- strh * get.asp()
            text(x = mean(leg[i, ]) + if (obj$direction == "leftwards") 
                0.12 * strh
            else -0.12 * strh, y = ylabel, labels = rownames(leg)[i], 
                srt = 90, adj = c(1, 0.5), cex = cex)
        }
    }
    par(usr = old.usr)
    object <- list(leg = leg, colors = colors[1:nrow(leg)])
    class(object) <- "geo.legend"
    invisible(object)
}