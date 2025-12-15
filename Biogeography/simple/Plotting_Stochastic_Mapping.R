#PLOTTING STOCHASTIC CHARACTER MAPPING FROM REVBAYES OUTPUT
library(ggplot2)
library(ggsn)
library(ggtree)
library(RevGadgets)
library(ape)
library(phytools)


character_file = "output-stochastic/simple_marginal_character.tree"

sim = read.simmap(file=character_file, format="phylip")


# Define colours for each area or state
colors = vector()
for (i in 1:length( sim$maps ) ) { 
    colors = c(colors, names(sim$maps[[i]]) )
}
colors
colors = sort(as.numeric(unique(colors)))
colors

# We can use different two colour choices to plot the posterior tree as a "heatmap"
# cols = setNames( heat.colors(length(colors), rev=TRUE), colors)
# cols

# Or use a basic palette with red, yellow, green, blue, pink, etc. For marginal states, this works better.

cols = setNames( rainbow(length(colors), start=0.0, end=0.9), colors)
cols

# fsize is font size for tip labels, lwd = line width for plotting, ftype = b (bold), i (italics)
# pts: whether to plot filled circles at each tree vertex, as well as transition points between mapped states: default is false


plotSimmap(sim, cols, fsize=0.5, lwd=2.0, split.vertical=TRUE, ftype="bi")

# Add geological time scale

# function geo.legend() draws a broad geological time scale from PreCambrian to present
# with a geo.palette with corresponding colors
geo.legend()

# To create your own, finer-scale geological timescale, create a matrix with name of time periods and corresponding values for end and start times

leg <- data.read("matrix_geological_scale.txt", header=TRUE)

#leg
#                 start    end
#Quaternary       2.588  0.000
#Pliocene         5.333  2.588
#Late-Miocene    11.630  5.333
#Mid-Miocene     15.970 11.630
#Early-Miocene   23.030 15.970
#Late-Oligocene  27.820 23.030
#Early-Oligocene 33.900 27.820
#Late-Eocene     41.200 33.900
#Early-Eocene    56.000 41.200


# Or alternatively, modify the corresponding line in geo.legend() function


# leg <- rbind(c(2.588, 0), c(5.333, 2.588), c(11.63, 5.333), 
            # c(15.97, 11.63), c(23.03, 15.97), c(27.82, 23.03), c(33.90, 
                # 27.82), c(41.20, 33.90), c(56.00, 41.20))
        # rownames(leg) <- c("Quaternary", "Pliocene", "Late-Miocene", 
            # "Middle-Miocene", "Early-Miocene", "Late-Oligocene", "Early_Oligocene", 
            # "Late-Eocene", "Early-Eocene")
            
# leg            

# Then, create your own palette of colors for that time period, by modifying the corresponding lines in geo.legend function

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

# colors
     # Quaternary        Pliocene    Late-Miocene  Middle-Miocene   Early-Miocene  Late-Oligocene Early_Oligocene 
    # "#FFF27FFF"     "#FFE619FF"     "#FD9A52FF"     "#7FC64EFF"     "#34B2C9FF"     "#812B92FF"     "#F04028FF" 
    # Late-Eocene    Early-Eocene 
    # "#67A599FF"     "#CB8C37FF" 



# Then, call geo.legend with the modified arguments
geo.legend(leg=leg, colors=colors)

# Alternatively, add from ape (does not look that good...)
# axisPhylo()


# Add legend


# To identify which colour corresponds to which state
leg = names(cols)
leg

add.simmap.legend(leg, colors=cols, cex=0.5, x=0.5, y=0.5, fsize=0.5)

# A message appears in console: "Click where you want to draw legend". Click and draw in RQuartz window to get the legend plotted.

# Save image using Save ----- RPlot

#PLOTTING POSTERIORS OF STOCHASTIC CHARACTER MAPPING BiSSE

posterior_file = "output-stochastic/simple_marginal_posterior.tree"

sim_p = read.simmap(file=posterior_file, format="phylip")


# Define colours for posterior probability 
colors = vector()
for (i in 1:length( sim_p$maps ) ) { 
    colors = c(colors, names(sim_p$maps[[i]]) )
}
colors = sort(as.numeric(unique(colors)))

# We can use different two colour choices to plot the posterior tree as a "heatmap". For posteriors, this works better.

cols = setNames( heat.colors(length(colors), rev=TRUE), colors)

# Or using a basic palette with red, yellow, blue, etc.

# cols = setNames( rainbow(length(colors), start=0.0, end=0.9, rev=TRUE), colors)



# fsize is font size for tipe labels, lwd = line width for plotting, ftype = b (bold), i (italics)
# pts: whether to plot filled circles at each tree vertex, as well as transition points between mapped states: default is false.

plotSimmap(sim_p, cols, fsize=0.5, lwd=2.0, split.vertical=TRUE, ftype="bi", pts=FALSE)

# Add legend
# To identify which colour corresponde to which value of the posterior probability
leg = names(cols)
leg

add.simmap.legend(leg, colors=cols, cex=0.2, x=0.2, y=0.2, fsize=0.3)

# A message appears in console: "Click where you want to draw legend". Click and draw in RQuartz window to get the legend plotted.

# Save image using Save ----- RPlot



