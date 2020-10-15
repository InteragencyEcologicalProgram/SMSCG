
PlotNMDS = function(NMDS1, # results of the NMDS you want to plot
                    data, # Environmental data (not species matrix)
                    textp = T, # do you want to plot text for the species centroids?
                    group, # Group you want to put hulls around
                    group2= NA, # Group you want to put spiders around
                    taxa = NA) { # Vector of relative percentages of a given taxa from your species matrix, used to scale the points.
  require(vegan)
  #set up colors
  cols = c("red", "blue", "green", "purple", "yellow", 
           "pink1", "brown", "orangered", "black", "cyan")
  #set up sapes
  shapes = c(1,2,3,4,5,6)
  #set up group
  names(data)[(which(names(data)== group))] = "group"
  #number of groups
  data = droplevels(data)
  m = length(levels(data$group))
  
  #set parameters so the legend is outside the plot area
  par(xpd=NA,oma=c(3,0,0,0)) 
  
  # plot the sample points
  plot(NMDS1, type="n", shrink = T, cex=1, xlim=c(-1, 1))
  if (is.na(taxa[1])) points(NMDS1, pch=as.numeric(data$group), cex=1, col="blue")  else {
    # scale the points by relative abundance of a certain taxa
    scalef = max(taxa) - min(taxa)
    points(NMDS1, pch=as.numeric(data$group), cex=.1 + taxa/mean(taxa), col="blue")
  }
  
  # plot text for species, if desired
  if(textp) text(NMDS1, dis="species", cex=.8) 
  
  # draw a hull around each group
  for (i in 1:m) {
    
    with(data, ordihull(NMDS1, groups = group, show = levels(group)[i], col=cols[i], lwd=2))
  }
  
  #second group with spiders
  if (is.na(group2)==F) {
    names(data)[(which(names(data)== group2))] = "group2"
    for (j in 1:length(levels(data$group2))) {
      with(data, ordispider(NMDS1, group = group2, show = levels(group2)[j], col=cols[j + length(levels(group))], lwd=1))
    }
    legend(par("usr")[1],par("usr")[3], 
           legend = c(levels(data$group),levels(data$group2)),
           col = cols[1:(length(c(levels(data$group), levels(data$group2))))], 
           lwd=c(rep(2, length(levels(data$group))), rep(1, length(levels(data$group2)))))
  } else legend(par("usr")[1],par("usr")[3], 
                legend = levels(data$group), col = cols[1:length(levels(data$group))], lwd =2, bty = "o")
                #pch = shapes[1:length(levels(data$group))], lwd=2, bty = "o")
#  legend(par("usr")[1]+1,par("usr")[3], legend = levels(data$group), 
 #        pch = shapes[1:length(levels(data$group))], col = "blue", bty = "n")
  
}



PlotNMDS2 = function(NMDS1, # results of the NMDS you want to plot
                     xlimits,  
                     ylimits,
                    data, # Environmental data (not species matrix)
                    textp = T, # do you want to plot text for the species centroids?
                    group, # Group you want to put hulls around
                    group2= NA, # Group you want to put spiders around
                    taxa = NA) { # Vector of relative percentages of a given taxa from your species matrix, used to scale the points.
  require(vegan)
  #set up colors
  cols = c("red", "blue", "green", "purple", "yellow", 
           "pink1", "brown", "orangered", "black", "cyan")
  #set up sapes
  shapes = c(1,2,3,4,5,6)
  #set up group
  names(data)[(which(names(data)== group))] = "group"
  #number of groups
  data = droplevels(data)
  m = length(levels(data$group))
  
  #set parameters so the legend is outside the plot area
  par(xpd=NA,oma=c(3,0,0,0)) 
  
  # plot the sample points
  plot(NMDS1, type="n", shrink = T, cex=1, xlim=xlimits, ylim = ylimits)
  if (is.na(taxa[1])) points(NMDS1, pch=as.numeric(data$group), cex=1, col="blue")  else {
    # scale the points by relative abundance of a certain taxa
    scalef = max(taxa) - min(taxa)
    points(NMDS1, pch=as.numeric(data$group), cex=.1 + taxa/mean(taxa), col="blue")
  }
  
  # plot text for species, if desired
  if(textp) text(NMDS1, dis="species", cex=.8) 
  
  # draw a hull around each group
  for (i in 1:m) {
    
    with(data, ordihull(NMDS1, groups = group, show = levels(group)[i], col=cols[i], lwd=2))
  }
  
  #second group with spiders
  if (is.na(group2)==F) {
    names(data)[(which(names(data)== group2))] = "group2"
    for (j in 1:length(levels(data$group2))) {
      with(data, ordispider(NMDS1, group = group2, show = levels(group2)[j], col=cols[j + length(levels(group))], lwd=1))
    }
    legend(par("usr")[1],par("usr")[3], 
           legend = c(levels(data$group),levels(data$group2)),
           col = cols[1:(length(c(levels(data$group), levels(data$group2))))], 
           lwd=c(rep(2, length(levels(data$group))), rep(1, length(levels(data$group2)))))
  } else legend(par("usr")[1],par("usr")[3], 
                legend = levels(data$group), col = cols[1:length(levels(data$group))], lwd =2, bty = "o")
  #pch = shapes[1:length(levels(data$group))], lwd=2, bty = "o")
  #  legend(par("usr")[1]+1,par("usr")[3], legend = levels(data$group), 
  #        pch = shapes[1:length(levels(data$group))], col = "blue", bty = "n")
  
}