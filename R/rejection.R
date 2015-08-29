abc_rejection <- function(data, iter=1e3, eps=1e-1) {

    size <- length(data)
    data_summary <- c(mean(data), sd(sample))

    # allocate space
    theta <- matrix(NA, n, 2)

    i <- 1

    for (s in 1:iter) {
        shape <- runif(n, 1, 6)
        scale <- runif(n, 1, 10)

        sample <- rweibull(n, shape, scale)
        sample_summary <- c(mean(sample), sd(sample))

        deviance <- sqrt(sum((sample_summary - data_summary) ^ 2))

        if (deviance < abs) {
            theta[i, ] <- c(shape, scale)

            i <- i + 1
        }
    }

    theta <- na.omit(theta)

    return(theta)
}
