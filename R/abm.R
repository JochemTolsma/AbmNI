#' @title NI
#'
#' @description Function to perform a NI ABM with several implementation
#'   options....
#'
#' @details For examples on how to use ... `selectType = 1`: only one ego;
#'   `selectType = 2`: all egos put in a queue `probs`: sample probability for
#'   ego(s) `selectTypeAlter = 1`, 1 random alter `selectTypeAlter = 2`, 1
#'   random connected alter `selectTypeAlter = 3`, 1 alter, probability based on
#'   similarity/distance `selectTypeAlter = 4`, 1 connected alter (path length
#'   1), probability based on similarity/distance `selectTypeAlter = 5`, all
#'   connected alters (path length 1) `selectTypeAlter = 6`, all alters
#'   `distance`: If alters are selected based on distance between the ego-alter
#'   pair this distance (or rather proximity) matrix is used. Higher scores
#'   means higher probability to be selected. If distance plays are role, but
#'   parameter is left to `NULL` distances based on similarity of opinion and
#'   group is being used: \eqn{w_{ij} + 1}.
#' @family NI
#' @param opinions Discrete variable; `min(opinion) = -1` and `max(opinion) =
#'   1` with 20 steps.
#' @param groups Integer \{-1,1\}
#' @param net Matrix, the adjacency matrix representing the relations between
#'   agents.  Valid values are 0 and 1.
#' @param H numeric \[0,1\], relative influence of opinion (max if H = 0) and
#'   group (max if H = 1)
#' @param selectType numeric \{1,2\}, Determines how egos are sampled. See
#'   Details.
#' @param prob numeric vector, egos are sampled based on probabilities
#' @param selectTypeAlter numeric \{1,2,3,4,5,6\}, Determines how alters are
#'   sampled. See Details.
#' @param distance  numeric matrix. Higher scores indicate that dyads are
#'   closer. See Details.
#' @param iter Integer. Number of pushes in each simulation run.
#' @param keep Logical. If `TRUE` complete chain is saved. If `FALSE` only
#'   final result is saved.
#' @param seed Integer. Allows replication run.
#' @param verbose Logical. If `TRUE` returns results of each iteration on
#'   screen.
#'
#' @return list of all kind of goodies.

#' @examples
#' seeds <- sample(2345:34567, 1000, replace = FALSE)
#' nagents <- 10
#' opinions <- c(-0.1, 1.0, 0.4, 0.4, -0.5, 0.4, 0.7, 0.7, -0.3, 0.6)
#' groups <- c(-1, 1, -1, 1, -1, 1, -1, 1, -1, 1)
#' net1 <- matrix(sample(c(0, 1), 100, replace = TRUE), nrow = 10, ncol = 10)
#' diag(net1) <- 0


#' @importFrom Rdpack reprompt
#' @importFrom stats sd
#' @export
ABM_NI <- function(opinions, groups, net = NULL, H = 0.5, selectType = 1, prob = NULL, selectTypeAlter = 1, distance = NULL, iter, keep = TRUE, seed = NULL, verbose = TRUE) {
  # use a seed to be able to replicate results of specific runs!
  if (!is.null(seed)) set.seed(seed)
  # save results
  results <- list()
  #init
  nagents <- length(opinions)
  distance_n <- distance
  opinions_n <- opinions
  #start sim
  for (i in 1:iter) {
    # update weights
    opweights <- fweights(opinions_n, groups, H) # calculates positive/negative influence
    # update distances if necessary
    if (is.null(distance) & (selectTypeAlter == 3 | selectTypeAlter == 4)) {
      distance_n <- distances(opweights)
    }
    # actual change
    ego <- selectEgo(nagents = nagents, selectType = selectType, prob = NULL) # select an ego
    alter <- selectAlter(nagents = nagents, ego = ego, net = net, distance = distance_n, selectTypeAlter = 1, prob = NULL)
    if (is.na(alter)) break #no saves, perhaps correct/improve later
    push <- opdelta(ego = ego, alter = alter, opinions = opinions_n, simweights = opweights)
    opinions_n <- opupdate(ego = ego, opinions = opinions_n, delta = push)
    # if save everything
    if (keep) {
      res <- list(iter = i, ego = ego, alter = alter, opweights = opweights, distance = distance_n, opinions = opinions_n, seed = seed, sd_op = sd(opinions_n), mean_op = mean(opinions_n))
      if (verbose) print(res)
      results[[i]] <- res
    }
  }
  # if save only final iter
  if (!keep) {
    results[[1]] <- list(iter = i, ego = ego, alter = alter, opweights = opweights, distance = distance_n, opinions = opinions_n, seed = seed, sd_op = sd(opinions_n), mean_op = mean(opinions_n))
  }
  return(results)
}

#'function to assign influence weights to alters (pos is attraction; neg is
#'repulsion) H is relative importance of opinion versus group (H=0 only group;
#'H=1 only opinion)
#' @rdname ABM_NI
#' @export
fweights <- function(opinions, groups, H) {
  opinion_diffs <- abs(outer(opinions, opinions, "-"))
  group_diffs <- abs(outer(groups, groups, "-"))
  weight_mat <- 1 - (opinion_diffs * H + group_diffs * (1 - H))
  diag(weight_mat) <- 0
  return(weight_mat)
}

#' @rdname ABM_NI
#' @export
distances <- function(opinions, groups, H) {
  simweights <- fweights(opinions = opinions, groups = groups, H = H)
  distances <- simweights + 1
  diag(distances) <- 0
  return(distances)
}

#' @rdname ABM_NI
#' @export
#' @param nagents integer, number of agents
selectEgo <- function(nagents, selectType = 1, prob = NULL) {
  if (selectType == 1) {
    sample(x = 1:nagents, size = 1, prob = prob)
  } else {
    sample(x = 1:nagents, size = nagents, prob = prob, replace = FALSE)
  }
}

#'  note that when equal number of egos received pushes, the number of pushes
#'  dealt is not equally distributed among alters with 1-5
#' @rdname ABM_NI
#' @export
#' @param nagents integer, number of agents
#' @param ego integer, selected ego(s) to receive push
selectAlter <- function(nagents,
                        ego,
                        net = NULL,
                        distance = NULL,
                        selectTypeAlter = 1,
                        prob = NULL) {
  agents <- setdiff(1:nagents, ego)
  if (selectTypeAlter == 1) {
    alter <- sample(x = agents, size = 1)
  } else if (selectTypeAlter == 2) {
    probs <- net[ego, ] # possibly no alter, thus NA
    if (sum(probs) == 0) {
      alter <- NA
    } else {
      alter <- sample(x = 1:nagents, size = 1, prob = probs)
    }
  } else if (selectTypeAlter == 3) {
    probs <- distance[ego, ] # possibly no alter, thus NA
    if (sum(probs) == 0) {
      alter <- NA
    } else {
      alter <- sample(x = 1:nagents, size = 1, prob = probs)
    }
  } else if (selectTypeAlter == 4) {
    probs1 <- net[ego, ] # possibly no alter, thus NA
    probs2 <- distance[ego, ] # possibly no alter, thus NA
    probs <- probs1 * probs2
    if (sum(probs) == 0) {
      alter <- NA
    } else {
      alter <- sample(x = 1:nagents, size = 1, prob = probs)
    }
  } else if (selectTypeAlter == 5) {
    probs <- net[ego, ] # possibly no alter, thus NA
    if (sum(probs) == 0) {
      alter <- NA
    } else {
      alter <- which(probs == 1)
    }
  } else if (selectTypeAlter == 6) {
    alter <- agents
  }
  return(alter)
}


#' function to calculate the pushes each alter gives note the 0.5. thus (with
#' weight=1) ego averages its own opinion and of (mean of) alter(s)
#' @rdname ABM_NI
#' @export
#' @param ego integer vector, selected ego(s) to receive push
#' @param alter integer vector, selected alter(s) to give push
#' @param simweights matrix, the similarity scores of the ego-alter pairs
#' calculated by `fweights`
opdelta <- function(ego, alter, opinions, simweights) {
  0.5 * (opinions[alter] - opinions[ego]) * simweights[ego, alter]
}

#' @rdname ABM_NI
#' @export
#' @param delta numeric vector, the push(es) of alter(s)
opupdate <- function(ego, opinions, delta) {
  # function to update opinion of ego. has to be kept in range
  opinions[ego] <- opinions[ego] + mean(delta)
  opinions[ego] <- min(max(opinions[ego], -1), 1)
  return(opinions)
}



