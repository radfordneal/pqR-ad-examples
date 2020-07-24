# Compute the output of a network with two hidden layers
# (with tanh activation function), when given 'x' as input.
# Parameters are in the list 'L'.
#
# This version uses arithmetic on lists.

nnet <- function (x, L)
{
    h1 <- tanh (L$b1 + x %*% L$W1)
    h2 <- tanh (L$b2 + h1 %*% L$W2)
    as.vector (L$b3 + h2 %*% L$W3)
}


# Train network with hidden layers of n1 and n2 units  on
# data in X and y.  Initialize with std. dev. 'sd'.  Do
# 'iters gradient descent iterations with stepsize 'step'.

train <- function (X, y, n1, n2, iters, step, sd=0.1)
{
    # Initialize parameters randomly.

    L <- list()
    n0 <- ncol(X)
    L$b1 <- rnorm (n1,sd=sd)
    L$W1 <- matrix (rnorm(n0*n1,sd=sd), n0, n1)
    L$b2 <- rnorm (n2,sd=sd)
    L$W2 <- matrix (rnorm(n1*n2,sd=sd), n1, n2)
    L$b3 <- rnorm (1,sd=sd)
    L$W3 <- rnorm (n2,sd=sd)

    # Train for 'iters' iterations to minimize squared 
    # error predicting y.

    for (i in 1:iters) {

        # Find gradient of squared error (summed over all
        # training examples) with respect to the parameters.

        r <- with gradient (L) {
            e <- 0
            for (i in 1:nrow(X)) {
                o <- nnet (X[i,], L)
                e <- e + (y[i]-o)^2
            }
            e
        }

        g <- attr(r,"gradient")

        if (i %% 100 == 0) 
            cat ("Iteration", i, ": Error", round(r,4), "\n")

        # Update parameters to reduce squared error.

        L <- L - step * g
    }

    L
}


# Example of learning a 2D function.

set.seed(1)

pdf("smnn-listops.pdf",width=8,height=4.6)
par(mfrow=c(1,2))

truef <- function (X) cos (2*sqrt(X[,1]*X[,2])) - 2*(0.4-X[,1])^2

grid <- seq(0,1,length=101)
Xgrid <- cbind (rep(grid,times=101), rep(grid,each=101))

contour (matrix(truef(Xgrid),101,101), levels=seq(-1,1,by=0.1))
title("True function")

N <- 100
X <- cbind (runif(N), runif(N))
y <- truef (X) + rnorm(N,sd=0.01)

print (system.time (L <- train (X, y, 10, 10, 30000, 0.001)))

print(L)

contour (
  matrix (apply (Xgrid, 1, function (x) nnet (x, L)), 101, 101),
  levels=seq(-1,1,by=0.1))
title("Learned function")


# Confirm that there's no allocation of large (eg, 10 x 100) 
# intermediate Jacobian matrices when computing gradients of 
# network parameters.

Rprofmemt(nelem=8)
with gradient (L) nnet (c(0.3,0.6), L)
