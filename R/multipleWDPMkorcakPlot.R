multipleWDPMkorcakPlot <- function(datafile, filename){
  # compares and plots multiple data sets from WDPM
  # names of files and titles are read from file

  # declare variables
  value <- NULL
  p <- NULL
  Model <- NULL

  # get colorblind safe palette
  colors = brewer.pal(3, "Dark2")

  # read in file of datasets
  datasets <- utils::read.table(datafile, header=TRUE, sep=",")
  datasets.count <- nrow(datasets)
  infile <- as.character(datasets$filename[1])
  caption <- as.character(datasets$caption[1])
  d1 <- utils::read.csv(infile, header=F, sep=",")
  d1.count <- nrow(d1)
  d1 <- cbind(d1,rep.int(caption,nrow(d1)))
  names(d1) <- c("num","value","p","Model")

  d1 <- subset(d1,value>=100)
  d1.korcak <- korcak(d1$value)
  d1.korcak <- subset(d1.korcak,p>0)
  d1.korcak$Model <- rep.int(caption,nrow(d1.korcak))


  # re-do Korcak for values >= 100
  for (i in 2:datasets.count){
    infile <- as.character(datasets$filename[i])
    caption <- as.character(datasets$caption[i])
    d2 <- utils::read.csv(infile, header=F, sep=",")
    d2.count <- nrow(d1)
    d2 <- cbind(d2,rep.int(caption,nrow(d2)))
    names(d2) <- c("num","value","p","Model")

    d2 <- subset(d2,value>=100)
    d2.korcak <- korcak(d2$value)
    d2.korcak <- subset(d2.korcak,p>0)
    d2.korcak$Model <- rep.int(caption,nrow(d2.korcak))

    # add d2 to d1
    d1.korcak <- rbind(d1.korcak, d2.korcak)

  }

  ybreaks=c(0.0001,0.001,0.01,0.1,0.2,0.4,0.6,0.8,1)
  xbreaks=c(10,100,1000,10000)

  p1 <- ggplot2::ggplot(d1.korcak) +
    ggplot2::geom_point(ggplot2::aes(value,p,colour=Model,shape=Model)) +
    ggplot2::xlab(expression(paste('Water area (m',''^{2}, ')', sep = ""))) +
    ggplot2::ylab("Exceedance probability")+
    ggplot2::scale_y_log10() +
    ggplot2::scale_x_log10() + ggplot2::stat_smooth(method = "lm",
                       formula = y ~x+ I(x^2),
                       ggplot2::aes(value,p,color=Model), size=0.25,se=F) +
    ggplot2::coord_equal(ratio = 1)
  # plot graph
  ggsave(filename)

}
