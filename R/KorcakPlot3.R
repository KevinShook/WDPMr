KorcakPlot3 <- function(data1, name1, data2, name2, data3, name3, graphfilename){
  # compares and plots 3 data sets
  # decalre ggplot variables
  value <- NULL
  p <- NULL
  Date <- NULL

   # separate extension from file name
  a  <-  strsplit(graphfilename,".")
  mainfilename  <-  a[[1]][1]
  coeff.filename <- paste(mainfilename,"_coeffs.csv", sep="")

  d1.korcak=korcak(data1[data1>10])
  d2.korcak=korcak(data2[data2>10])
  d3.korcak=korcak(data3[data3>10])

  d1.korcak <- subset(d1.korcak, p>0)
  d2.korcak <- subset(d2.korcak, p>0)
  d3.korcak <- subset(d3.korcak, p>0)


  # fit 2nd order polynomial to log-transformed data
  fit1 <- fitdegree2(d1.korcak)
  fit2 <- fitdegree2(d2.korcak)
  fit3 <- fitdegree2(d3.korcak)
  coeffs <- data.frame(fit1, fit2, fit3)
  names(coeffs) <- c(name1,name2,name3)

  d1.korcak=cbind(d1.korcak,rep.int(name1,nrow(d1.korcak)))
  d2.korcak=cbind(d2.korcak,rep.int(name2,nrow(d2.korcak)))
  d3.korcak=cbind(d3.korcak,rep.int(name3,nrow(d3.korcak)))

  names(d1.korcak)=c("value","p","Date")
  names(d2.korcak)=c("value","p","Date")
  names(d3.korcak)=c("value","p","Date")

  all <- rbind(d1.korcak, d2.korcak, d3.korcak)

  ybreaks <- c(0.01,0.1,0.2,0.4,0.6,0.8,1)
  xbreaks <- c(10,1000,100000)
  p1 <- ggplot2::ggplot(all,ggplot2::aes(value,p,color=Date,shape=Date)) +
    ggplot2::scale_colour_manual(values = c("blue","black","red")) +
    ggplot2::geom_point() +
    ggplot2::xlab(expression(paste('Water area (m',''^{2}, ')', sep = ""))) +
    ggplot2::ylab("Exceedance probability") +
    ggplot2::scale_y_log10(breaks=ybreaks,labels=ybreaks) +
    ggplot2::scale_x_log10(breaks=xbreaks,labels=xbreaks) +
    ggplot2::stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 0.5, se = TRUE)

  # save graph
  ggsave(graphfilename)


  # write coeffs to file
  utils::write.csv(coeffs, file=coeff.filename, row.names=FALSE)

}
