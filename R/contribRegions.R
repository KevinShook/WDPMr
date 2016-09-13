contribRegions <- function(waterfile1.name, waterfile2.name, demfile.name, outfile){
  #subtracts water files and identifies contributing and non-contributing regions
  water1 <- SDMTools::read.asc(waterfile1.name)
  water2 <- SDMTools::read.asc(waterfile2.name)

  diff <- water2 - water1

  # write difference
  diffile.name <- paste(outfile,'_diff.asc',sep='')
  SDMTools::write.asc(diff, diffile.name)

  # call program to find contributing areas
  # create command string
  outfile.asc <- paste(outfile,'.asc',sep='')
  outfile.txt <- paste(outfile,'.txt',sep='')
  command.string <- paste('~/bin/ContribRegion', demfile.name, waterfile2.name, diffile.name,
                          outfile.asc, '>', outfile.txt, sep=' ')
  system(command.string)
}
