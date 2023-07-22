df = read.table("network_rho.csv")

x = unlist(df[1,])
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

plot(x, y, ylim=c(-0.1, 1.1), col=colors[n])
arrows(x, y+s, x, y-s, code=3, len=0.05, angle=90, col=colors[n])
