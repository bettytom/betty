# DecisionTreeClassifier
# the main libraries to use for this algorthim
library(FSelector) # computes the information gain and entropy

library(rpart) # this aids in partitioning the trees or decisions

library(caret) # handles both train and test data

library(dplyr) # filter data functions

#library(rpart.plot) # plot the tree

#library(data.tree) #dispalying the data into a tree

library(caTools) # splitting data

# selecting data to be used
df<- select(df,survived,class,sex,age)
df<-mutate((df, survived=as.factor(survived), 
            class=as.numeric(class),age=as.numeric(age))
           
# splitting into training and testing data
sample=sample.split(df$survived, SplitRatio = .70)
train=subset(df, sample==TRUE)   # the data to be used
test=subset(df,sample== FALSE)

# training the decision tree classifier
tree<-rpart(survived~.,data=train)

# predictions
tree.survived.predicted<-predict(tree,test,type='class')

# a matrix for the model
confusionMatrix(tree.survived.predicted,tree$survived)

# visualizing the decision tree
# using rpart.plot
prp(tree)
prp()