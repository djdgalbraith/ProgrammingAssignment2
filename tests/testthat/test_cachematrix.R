library("testthat")

source("cachematrix.R")

test_that("makeCacheMatrix holds a matrix", {
        x = rbind(c(1, -1/4), c(-1/4, 1))
        m = makeCacheMatrix(x)
        expect_equal(x, m$get())
})

test_that("cacheSolve resolves to an inverse", {
        x = rbind(c(1, -1/4), c(-1/4, 1))
        m = makeCacheMatrix(x)
        inverse = solve(x)
        expect_equal(inverse, cacheSolve(m))            
})

test_that("setting a new matrix clears the cached inverse", {
        x = rbind(c(1, -1/4), c(-1/4, 1))
        m = makeCacheMatrix(x)
        expect_null(m$getinverse())
        cacheSolve(m)
        expect_true(!is.null(m$getinverse()))
        m$set(x)
        expect_null(m$getinverse())
})
