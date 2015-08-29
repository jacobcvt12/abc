abc_rejection <- function(data, iter=1e3, eps=1e-1) {
    size <- length(data)
    data_summary <- c(mean(data), sd(data))

    # allocate space
    theta <- matrix(NA, iter, 2)

    i <- 1

    for (s in 1:iter) {
        shape <- runif(1, 1, 6)
        scale <- runif(1, 1, 10)

        sample <- rweibull(size, shape, scale)
        sample_summary <- c(mean(sample), sd(sample))

        deviance <- sqrt(sum((sample_summary - data_summary) ^ 2))

        if (deviance < eps) {
            theta[i, ] <- c(shape, scale)

            i <- i + 1
        }
    }

    theta <- na.omit(theta)

    return(theta)
}
