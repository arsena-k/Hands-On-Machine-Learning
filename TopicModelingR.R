#TOPIC MODELING IN R, ANNOTATED GUIDE FOR DRESSUP 2017

#Text Data is From "First Person Narratives on the American South": http://docsouth.unc.edu/fpn/
#Some R code adapted from: http://tidytextmining.com/topicmodeling.html

#####################################################################################################################
#STEP 1: Getting Started in R

#in case you've been doing other things in R, clear all objects in R such as previous data-sets, variables, etc.
rm(list = ls(all=TRUE))

#tell R what your working directory is (i.e., the folder that has the data file(s) you want to load into R). Note that the my working directory's address is in double quotation marks, and I used foward slashes not backward slashes). Also, you can't use a zipped folder as a working directory so it needs to be unzipped first.
setwd("")

#check your current working directory
getwd()

#if you have never installed the below package in R, install with the below code. If you're not sure, just install anyways:
install.packages("tm") #download reference manual here: https://cran.r-project.org/web/packages/tm/index.html

#tell R you need to use the below package
library(tm)  #tm (i.e., text mining) is a package with lots of functions that make it easier to work with text data, and get data in the structure (document-term matrix) which is the way to input data to the package topicmodels


#list all filenames in your working directory that are .txt files you should have all the txt files from the American South dataset
filenames <- list.files(getwd(),pattern="*.txt") 
filenames


######################################################################################################################
#STEP 2: Load your Data into R. Your data should be in your working directory.


corpus <- Corpus(DirSource(directory="")) #need to get your data into a "corpus" format which can then be turned into a Document-Term Matrix format, since the TopicModels package is trained using this matrix. Can do all this reshaping using the TM pacakge.
corpus #note 150 documents, so you have a quick check that all 150 documents in folder "texts" loaded in



######################################################################################################################
#STEP 3: Re-Structure data as a "Document-Term Matrix" (document in each row, and term in each column)

mymatrix<- DocumentTermMatrix(corpus, control = list(removePunctuation = TRUE, removeNumbers=TRUE, stopwords = TRUE, stemming=TRUE)) #notice all the options you have to easily pre-process text data. If you want control for the fact that some terms are more popular than others across ALL topics, try weighting words based on their popularity: weighting = weightTfIdf

rownames(mymatrix) <- filenames #change the row names to be the same as the document filenames

######################################################################################################################
#STEP 4: Train a Topic Model

install.packages("topicmodels") #run this to download the package topicmodels if you haven't downloaded it before or are not sure. download reference manual here: https://cran.r-project.org/web/packages/topicmodels/index.html
library(topicmodels) #topic models is a package with functions to do topic modeling


mymodel<- LDA(mymatrix, k=5, control = list(seed = 1234)) #training a topic model! use k=5 to specify you want 5 topics, set seed so that results are reproducible since there is some randomness used in the model. This may take a few moments to run...

save(mymodel, file = "mymodel_5.RData") #save your model so you don't have to re-train later, and instead can just load in


######################################################################################################################
#STEP 5: Explore Topic Model Results


topterms<- terms(mymodel, 20) #look at 20 most likely words for each topic
View(topterms)
toptopics<- topics(mymodel, 1) #look at 1 (the 1st most) likely topic for each document
toptopics

write.csv(topterms, "topterms_5.csv") #run this to write top terms data to a CSV file
write.csv(toptopics, "toptopics_5.csv") #run this write top topics data to a CSV file


install.packages("tidytext")  #another package, lots of functions to work with text data
library(tidytext)
install.packages("dplyr")  #another package, lots of functions to re-structure data
library(dplyr)



#Explore the betas! i.e., i.e., the probability each term belongs to each topic

words_topics <- tidy(mymodel, matrix = "beta") #this gives the "betas"
View(words_topics)


View(words_topics[words_topics$topic==1,]) #look at just topic 1
View(words_topics[words_topics$term=="southern",]) #look at just the term "southern"
View(words_topics[words_topics$beta> mean(words_topics$beta),]) #just look at terms that have probability above mean. how do you think you would change this to median? what about .06

hist(words_topics$topic) #look at a histogram to see how popular each topic is
hist(words_topics$beta) #look at a histogram to see distribution of betas

#Explore the gammas! i.e., i.e., the probability each docuemnt belongs to each topic

docs_topics <- tidy(mymodel, matrix = "gamma") #this gives the "gammas"
View(docs_topics)

#look at the the gammas another way
topicProbabilities <- as.data.frame(mymodel@gamma) probabilities that each document is associated with each topic assignment


######################################################################################################################
#STEP 6: Visualize Results

install.packages("ggplot2") #a package for visualization 
library(ggplot2)
install.packages("dplyr") 
library(dplyr)

ap_top_terms <- words_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  ggtitle("The terms that are most common within each topic")

###VISUALIZATION EXAMPLE A

