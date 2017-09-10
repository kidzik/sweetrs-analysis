#install.packages("softImpute")
library("softImpute")
library("reshape2")
source("fc.sample.R")

NMSE = function(X,Y) { mean((X - Y)**2,na.rm= TRUE) / mean((Y)**2,na.rm= TRUE) }
RMSE = function(X,Y) { sqrt(mean((X - Y)**2,na.rm= TRUE)) }

ratings = read.csv("ratings-final.csv")

# Build the matrix
ratings.matrix = as.matrix(dcast(ratings, user ~ product,fun.aggregate = mean))
ratings.matrix = ratings.matrix[,-1] # we don't need the 'user' column any more
ratings.matrix[is.nan(ratings.matrix)] = NA
ratings.matrix[ratings.matrix == 0] = NA

# Remove col means
cmeans = colMeans(ratings.matrix,na.rm = TRUE)
ratings.matrix = t(t(ratings.matrix) - cmeans)
csd = apply(ratings.matrix,MARGIN = 2,FUN = sd,na.rm = TRUE)
ratings.matrix = t(t(ratings.matrix) / csd)

ratings.matrix = ratings.matrix[,colSums(!is.na(ratings.matrix)) > 400]
ratings.matrix = ratings.matrix[rowSums(!is.na(ratings.matrix)) > 35,]

# Run the experiment
perc.test = 1:9 / 10.0
lambdas = 0:20
res.all = c()
res.d.all = c()
df = data.frame(lambda = lambdas)

for (perc in perc.test){
for (i in 1:10){
  ratings.train = fcomplete:::fc.sample(ratings.matrix,one.per.row = FALSE, perc = perc)
  res = c()
  res.d = c()
  for (r in lambdas){
    model = softImpute(ratings.train$train,rank.max = 5,lambda = r)
    completed = softImpute::complete(ratings.train$train, model)
    res = c(res,NMSE(completed,ratings.train$test))
    res.d = c(res.d,sum(model$d> 0.01))
  }
  res.all = rbind(res.all, res)
  res.d.all = rbind(res.d.all, res.d)
}
print(res.all)
print(res.d)
boxplot(res.all)
df[[as.character(perc)]] = colMeans(res.all)
}
df.long = melt(df, id="lambda")
df.long$variable = as.factor(as.numeric(as.character(df.long$variable)))
df.long = df.long[df.long$lambda>4.5,]
df.long = df.long[df.long$lambda<16.5,]

library("ggplot2")
library("latex2exp")
ggplot(df.long, aes(x=lambda, y=value, colour = variable)) + theme_bw() + theme(text = element_text(size=22)) +
  geom_line(size = 2)  + guides(colour = guide_legend(reverse=T,title="test set ratio")) + labs(y = "NMSE", x = TeX("$\\lambda$"))

# Build the final model
model = softImpute(ratings.matrix,rank.max = 13)
