gabriel.plot <- function(x, y, upper, lower=upper, length=0.1,...){
  meang <- tapply(x,y,mean)
  tem <- barplot(t(meang),ylim=c(0,max(meang)+2*max(upper)),...)
  arrows(tem,meang+upper, tem, meang-lower, angle=90, code=3, length=length, ...)
}