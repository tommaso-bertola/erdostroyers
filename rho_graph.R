if(T){
    df = read.table("network_rho.csv")
    df2 = read.table("network_rho_dist.csv")
} else {
    df = read.table("ba_rho.csv")
    df2 = read.table("ba_rho_dist.csv")
}

x = unlist(df[1,])
x2 = unlist(df2[1,])

df = df[-1,]

y=c()
s=c()
n=c()

for(i in 1:length(x)) {
 y = c(y, mean(df[,i], na.rm=T))
 s = c(s, sd(df[,i], na.rm=T))
 n = c(n, sum(is.na(df[,i]))+1)
}

colors = c("black", "gray50", "red", "pink", "orange")

xmin=min(c(x,x2))
xmax=max(c(x,x2))

plot(x, y, ylim=c(-0.1, 1.1), xlim=c(xmin, xmax), col=colors[n], xlab="p", ylab="\u3c1")
arrows(x, y+s, x, y-s, code=3, len=0.05, angle=90, col=colors[n])
lines(x,y)



df2 = df2[-1,]

y=c()
s=c()
n=c()

for(i in 1:length(x2)) {
 y = c(y, mean(df2[,i], na.rm=T))
 s = c(s, sd(df2[,i], na.rm=T))
 n = c(n, sum(is.na(df2[,i]))+1)
}

colors = c("black", "gray50", "red", "pink", "orange")

points(x2, y, col=colors[n])
arrows(x2, y+s, x2, y-s, code=3, len=0.05, angle=90, col=colors[n])
lines(x2,y)
