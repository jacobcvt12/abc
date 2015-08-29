simulate_data <- function(n=10) {
    data <- rweibull(n, 2, 5)
    return(data)
}
