mower.df <- RidingMowers
set.seed(111)
train.index <- sample(1:24,14,replace = FALSE)
train.df <- mower.df[train.index,]
test.df <- mower.df[-train.index,]

# New household
new.df <- data.frame(Income = 60, Lot_Size = 20)

# scatter plot
plot(Lot_Size ~ Income, data = train.df, pch = ifelse(train.df$Ownership=="Owner",1,3))
text(train.df$Income, train.df$Lot_Size, rownames(train.df), pos = 4)
text(60,20,"X")
legend("topright", c("owner","non-owner","newhousehold"), pch = c(1,3,4))


# running kNN


#initialize normalized training, validation data, complete data frames to originals

train.norm.df <- train.df
test.norm.df <- test.df
mower.norm.df <- mower.df

#use preProcess from the caret package tp normalize income and lot size
library(caret)
norm.values <- preProcess(train.df[,1:2], method=c("center","scale"))
train.norm.df[,1:2] <- predict(norm.values, train.df[,1:2])
test.norm.df[,1:2] <- predict(norm.values, test.df[,1:2])
new.norm.df <- predict(norm.values, new.df)

# use knn() to compute knn
# library(class)
library(class)

nn <- knn(train = train.norm.df[,1:2], test = new.norm.df,
          cl = train.norm.df$Ownership, k =3)
row.names(train.df)[attr(nn,"nn.index")]
nn
