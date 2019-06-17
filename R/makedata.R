##' Robin Eriksson 2019
##'
##' Generate data from SimInf problem.
##'

library(casestudy)

genData <- function(theta, M, N, tau) {

    ## c and p0 are selected from values that "look" good with theta0
    c <- 0.025 ##  see above
    p0 <- 0.035 ## see above
    filter <- TRUE
    retPhi <- FALSE



    ## Generate the observation
    y <- simulator(theta = theta, N = N, M = M, tau = tau, p0 = p0, filter = FALSE,
                   c = c)


    return(y)
}

##' Save the data into a csv file
##'
##' @param data the output from theta
##' @param theta the parameter samples
saveData <- function(data, theta) {
    write.csv(data, file = "data.csv")

    write.csv(theta, file = "theta.csv")
}


##' Generate random samples from a uniform prior
##' centered around input theta
##' @param theta centerpoint for the uniform prior
##' @param R number of samples
uniformPrior <- function(theta, R) {
    outvec <- matrix(0, ncol = length(theta), nrow = R)

    for(j in seq_len(length(theta))) {
        outvec[,j] <- runif(R, min = theta[j]*0.5, max = theta[j]*1.5)
    }

    return(outvec)
}

##' main function
##'
##' @param R (power of 3) number of parameter pairs in output
main <- function(R) {

    ## Generate grid
    theta0  <- c("upsilon" = 1e-2, "gamma" = 0.1, "beta" = 5e-2)
    pert <- c(0.5,1.5)
    allDim <- TRUE

    theta <- uniformPrior(theta0, R) #genGrid(theta0, thetalength, pert, allDim)

    numTheta <- dim(theta)[1]

    ## Generate data
    ## General input for the simulator
    tau <- 10 # measure every 10th timestep
    M <- 1e4 # individuals
    N <- 1e3 # timesteps

    theta_prop <- theta0
    data <- matrix(0, ncol = N/tau+1, nrow =numTheta)
    for(i in seq_len(numTheta)) {
        ## extract proposal
        for(j in seq_len(length(theta0))) {
            theta_prop[j] <- theta[i,j]
        }

        data[i,] <- genData(theta_prop, M, N, tau)
    }

    saveData(data, theta)

    return(0)
}
