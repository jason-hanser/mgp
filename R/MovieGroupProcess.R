#' Movie Group Process Function
#'
#' This function implements the Movie Group Process outlined by Ying and Wang in their 2014 paper
#' (A Dirichelt Multinomial Mixture Model-based Approach for Short Text Clustering).
#'
#' @param data A data frame.
#' @param text The name of a column within the data frame containing text to cluster. The column name should
#' not be listed in quotes.
#' @param K The upper limit for the number of topics. The function will automatically condense and remove
#' empty clusters.
#' @param alpha A tuning parameter ranging from 0 to 1 controlling a documents affinity for a larger cluster.
#' Default value is set to 0.1.
#' @param beta A tuning parameter ranging from 0 to 1 controlling a documents affinity for a more similar
#' cluster. Default values is set to 0.1.
#' @param iter The upper limit for the number of iterations to perform. The function will terminate
#' earlier if a stable solution is found. Default is set at 30.
#' @param repeat_words A logical vector indicated whether the documents contain repeated words. If TRUE, the
#' function uses a Algorithm 4 from Yin and Wang's paper; if FALSE, the function using Algorithm 3 from their
#' paper. Default is set to FALSE.
#' @param r_stopwords A logical vector indicating whether stop words should be removed. Default is set at TRUE.
#'
#' @return The function returns the original data frame with an additional column containing cluster assigments
#' for each row of text.
#'
#' @examples
#'
#'
#'

MovieGroupProcess <- function(data, text, K, alpha  = 0.1, beta = 0.1, iter = 30, repeat_words = FALSE, r_stopwords = TRUE) {

  ## Tokenizing data and removing null (i.e. "") values

   text <- data[[rlang::quo_name(rlang::enquo(text))]]
   text <- strsplit(x = tolower(text), split = "\\W")
   text <- rapply(text, function(x) x[x != ""], how = "replace")


  ## Removing stop words

  if (r_stopwords == TRUE) {

    text <- lapply(text, setdiff, tidytext::stop_words$word)

  }


  ## creating documents object

  documents <- list()

  for (i in 1:length(text)) {

    documents$document[i] <- i

    if (repeat_words == FALSE) {

      documents$tokens[i] <- list(unique(text[[i]]))

      } else {

      documents$tokens[i] <- list(text[[i]])

      }

    documents$topic[i]    <- sample(x = K, size = 1)

  }

  ## creating topics object

  topics <- list()

  for (i in 1:K) {

    topics$documents[i] <- list(documents$document[documents$topic == i])
    topics$tokens[i]    <- list(unlist(documents$tokens[documents$topic == i]))

  }

  ## GSDMM Alogrithm

  for (i in 1:iter) {

    old_topics <- documents$topic

    for (d in 1:length(documents$document)) {

      ## Removing document from its cluster and reforming clusters

      documents$topic[[d]] <- 0

      topics <- list()

      for (j in 1:K) {

        topics$documents[j] <- list(documents$document[documents$topic == j])
        topics$tokens[j]    <- list(unlist(documents$tokens[documents$topic == j]))

        } ## end of j loop

      p <- list()

      for (t in 1:K) {

        N_1 <- length(topics$documents[[t]]) + alpha
        D_1 <- length(documents$document) - 1 + K*alpha

        N_2 <- 1
        D_2 <- 1

        for (w in documents$tokens[[d]]) {

          if (repeat_words == FALSE) {

            N_2 <- N_2*(sum(w == topics$tokens[[t]]) + beta)

            } else {

              for (n_w_d in 1:sum(w == documents$tokens[[d]])) {

                N_2 <- N_2*(sum(w == topics$tokens[[t]]) + beta)

              }

            }

        }

        for (n_d in 1:length(documents$tokens[[d]])) {

          D_2 <- D_2*(length(topics$tokens[[t]]) + length(unique(unlist(documents$tokens)))*beta + n_d - 1)

        }

        p$prob[[t]] <- (N_1/D_1)*(N_2/D_2)

      }

      ## Assigning document to new cluster/topic and reforming clusters/topics

      documents$topic[[d]] <- which(unlist(p$prob) == max(unlist(p$prob)))

      topics <- list()

      for (j in 1:K) {

        topics$documents[j] <- list(documents$document[documents$topic == j])
        topics$tokens[j]    <- list(unlist(documents$tokens[documents$topic == j]))

      }

    }

    if (sum(documents$topic != old_topics) == 0) {

      print(paste0("MGP converged after ", i, " iterations."))
      break

    }

    print(paste0("Iteration ", i, " is complete."))

  }

  ## Removing emptpy topics

  topics$documents <- topics$documents[which(lengths(topics$tokens) != 0)]
  topics$tokens    <- topics$tokens[which(lengths(topics$tokens) != 0)]

  for (j in 1:length(topics$documents)) {

    documents$topic[topics$documents[[j]]] <- j

  }

  ## Adding cluster to orinal data.frame

  data$text_cluster <- documents$topic

  data

}



