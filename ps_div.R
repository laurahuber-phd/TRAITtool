ordination_function <- function(ps,mthd,dstnc,kf=3,tmf=150,maxf=500) {
  ord <- ordinate(
    physeq=ps,
    k=kf,
    trymax=tmf,
    maxit=maxf,
    method=mthd, #c("DCA", "CCA", "RDA", "CAP", "DPCoA", "NMDS", "MDS", "PCoA")
    distance=dstnc #c("jaccard","bray")
  )
}

ordination_plotting <- function(ps,ordf,type,var1,var2) {
  plot_ordination(physeq=ps,
                  ordination=ordf,
                  type=type,
                  color=var1,
                  shape=var2
                  ) #+ 
  #  geom_point(color="var2",shape="var3", 
  #             size = 2) #+ #color the points 
    #stat_ellipse(aes(color=SiteType),type="norm",linetype=2)
}


#cat("Converged?", jaccard.nmds$converged, fill=TRUE)

  
#cat("Stress", jaccard.nmds$stress, fill=TRUE)


#stressplot(jaccard.nmds)



#plot_ordination(ps.prune, #phyloseq object
#                jaccard.nmds, #distance matrix
#                type="samples", #plot the samples, not the taxa
#                color="filterSize", #color the dots based on their filterSize values
#                shape="site") + #change the shape of the dots based on their Site_Location values 
#  #stat_ellipse(aes(group=filterSize),type="norm") +
#  geom_point(size=3) +
#  stat_ellipse(aes(group=filterSize,
#                   fill=filterSize),
#               geom="polygon",type="norm",alpha=0.1) + #draw a confidence level ellipse around the data based on one of the columns in your metadata file   
#  theme_bw()

