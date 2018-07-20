#Natural Language Processing

#Step 1: Let's import the data set first

dataset_initial <- read.delim('Restaurant_Reviews.tsv', quote = '', stringsAsFactors = FALSE)
dataset
View(dataset)


#Step 2: Let's clean the text

install.packages('tm')
library(tm)

install.packages('SnowballC')
library(SnowballC)
corpus <- VCorpus(VectorSource(dataset$Review))
corpus <- tm_map(corpus, content_transformer(tolower))
#this line of code

corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, removePunctuation) #this line of code removes punctuations from the text
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)


#Step 3: Let's create the bag of words model

documentTermMax <- DocumentTermMatrix(corpus)
documentTermMax <- removeSparseTerms(documentTermMax, 0.999)


#So far I Cleaned all texts all reviews. I created a bag of words model. 

#used filter to remove non-relevant words

#created a classification model

#Let's pick a classification model

  #We are using Random forest classification
dataset <- as.data.frame(as.matrix(documentTermMax))
dataset$Liked <- dataset_initial$Liked



# Step 4: Encoding the target feature as factor
dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

# Step 5: Let's Split the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

Fitting Random Forest Classification to the Training set
install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692]) #692 is the index of our dependent variable

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred) #692 is the index of our dependent variable
cm
