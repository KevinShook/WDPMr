moment_plot <- function(data){ # "data" is a vector containing the sample data
  ##############################################
  ##############################################
  # CV and Skewness functions
  coefvar <- function(data){
    CV <- sd(data)/mean(data)
    return(CV)}
  skewness <- function(data) {
    m_3 <- mean((data-mean(data))^3)
    skew <- m_3/(sd(data)^3)
    return(skew)}

  ##############################################
  ##############################################
  # Computation of CV and Skewness # CV
  CV <- coefvar(data); # Skewness
  skew <- skewness(data) # Rule of Thumb
  if (CV<0 | skew < 0.15)
  {print('Possibly neither Pareto nor lognormal. Thin tails.');
    stop}
  ##############################################
  # Preparation of the plot
  ##############################################
  # Paretian Area
  # The upper limit - Pareto I
  p <- seq(3.001, 400, length.out=250)
  g2brup <- 1/(sqrt(p*(p-2)))
  g3brup <- (1+p)/(p-3)*2/(sqrt(1-2/p))
  # The lower limit, corresponding to the Inverted Gamma
  g2ibup <- seq(0.001,0.999,length.out=250)
  g3ibup <- 4*g2ibup/(1-g2ibup^2)

  ##############################################
  # Lognormal area
  # Upper limit: Lognormal
  w <- seq(1.01,20,length.out=250)
  g2log <- sqrt(w-1)
  g3log <- (w+2)*sqrt(w-1)
  # Lower limit - Gamma
  g2iblow <- seq(0,20,length.out=250)
  g3iblow <- 2*g2iblow
  ##############################################
  # Exponential Area
  # The upper limit corresponds to the lower limit of the
  # lognormal area
  # The lower limit - Bernoulli
  g2below <- seq(0,20,length.out=250)
  g3below <- g2below-1/g2below
  ##############################################
  # The Gray area is obtained for free from
  # the previous lines of code.
  ##############################################
  # Normal / Symmetric distribution
  g2nor <- seq(0,20,length.out=250)
  g3nor <- rep(0,250)
  ##############################################
  # PLOT
  # Limits
  plot(g2iblow,g3iblow,'l',xlab='CV',ylab='Skewness',
       main='Discriminant Moment-ratio Plot',
       xlim=c(0,20),ylim=c(-1,40))
  lines(g2ibup,g3ibup,'l')
  lines(g2brup,g3brup,'l')
  lines(g2below,g3below,'l')
  lines(g2log,g3log,lty=2) # Lognormal
  lines(g2nor,g3nor,lty=2) # Normal # Strictly Paretian Area
  polygon(c(g2ibup,g2brup),c(g3ibup,g3brup),col='green')
  points(0,2,pch=1,cex=0.8) # Pareto limit point
  # Hints for interpretation
  text(-0.2,20,cex=0.8,srt=90,'Pareto I')
  text(1.2,20,cex=0.8,srt=90,'Inverted Gamma')
  text(2.5,12,cex=0.8,srt=70,'Lognormal')
  text(12,21,cex=0.8,srt=23,'Gamma')
  text(14,11,cex=0.8,srt=10,'Bernoulli')
  text(15,1.5,cex=0.8,'Normal or Symmetric')
  points(CV,skew,pch=16,col='red')
  return(c(CV,skew))
}
