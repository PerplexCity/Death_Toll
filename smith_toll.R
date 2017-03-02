library(ggplot2)
bbi<-element_text(face="bold.italic", color="black")


often <- data.frame(successes = rbinom(10000, 1000, .5))

often_graph <- ggplot(often, aes(x=successes)) +
  geom_histogram(binwidth=1, fill="red") +
  scale_x_continuous(breaks=seq(440,560,20)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title=bbi) +
  labs(title="1 in 2 chance")

rare <- data.frame(successes = rbinom(10000, 1000, .001))

rare_graph <- ggplot(rare, aes(x=successes)) +
  geom_histogram(binwidth=1, fill="red") +
  scale_x_continuous(breaks=0:10) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title=bbi) +
  labs(title="1 in 1000 chance")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(rare_graph, often_graph, cols=2)

