# Author: Malmi S. Amadoru
# Date: 07/01/2018
# This script computes coherence for each topic.Topic coherence is implemented based on Mmino et. al (2011), intrinsic UMass measure.
# UMass is a non-symmetric pairwise score function
# UMass_Score (wi,wj) = log((D(wi,wj) +1)/D(wi)) 
# Topic coherence is the sum of above
# This implementation is inspired by stm package semanticCoherence implementation for a STM topic model, text2vec package coherence measures
# implementation by Manuel Bickel and textmineR package coherence measure implementation by Tommy Jones.
# Acknowldgement: Manuel Bickel & Tommy Jones for the support
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

library(Matrix)

# compute UMass coherence for specified topics
UMassCoherence <- function(topicList, dtm){
  
  dtm <- MakeSparseDTM(dtm) # make dtm a sparse matrix
  
  num_of_topics <- ncol(topicList) # number of topics
  
  num_of_words <- nrow(topicList) # number of words per topic
  
  uniqueWords <- unique(as.vector(topicList))
  dtm <- dtm[,uniqueWords] # filter dtm from unique words
  
  # creates a binary term co-occurrence matrix
  tcm <- crossprod(sign(dtm)) 
  
  topic_Coherence <- matrix(NA_real_, nrow=1, ncol=num_of_topics)
  colnames(topic_Coherence) <- paste0("Topic ", 1:num_of_topics)
  
  for(k in 1:num_of_topics){ # iterate each topic
    topic_tcm <- tcm[topicList[,k], topicList[,k]] # create tcm for topic k by filtering its top words

    score <- 0
    for(m in 2:num_of_words){
      for(l in seq_len(m-1)){
        word_score <- log(.01 + topic_tcm[m,l]) - log(topic_tcm[l,l] + .01)
        score <- score + word_score[1]
      }
    }
    topic_Coherence[k] = score
  }
  return (topic_Coherence)
}

# convert simple triple matrix dtm to tcm compatible matrix
MakeSparseDTM <- function(dtm){
  # dtm is a simple triplet matrix
  dtm.sparse <- Matrix::sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, 
                                     dims=c(dtm$nrow, dtm$ncol))
  
  rownames(dtm.sparse) <- tm::Docs(dtm)
  colnames(dtm.sparse) <- tm::Terms(dtm)
  
  return(dtm.sparse)
}

# compute coherence and save results to file
ComputeTopicCoherence <- function(topicList, dtm, outFullFileName){
  topicCoherence <- UMassCoherence(topicList, dtm)
  write.csv(topicCoherence, file=outFullFileName, row.names = FALSE)
}