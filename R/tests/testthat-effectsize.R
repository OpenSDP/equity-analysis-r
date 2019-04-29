source("../functions.R")

# Generate test data

context("Test the effect size estimators")


set.seed(12532452)


test_that("ES Calculations are Correct", {
  x1 <- rnorm(100, mean = 0.8)
  x2 <- rnorm(150, mean = -0.3, sd = 0.6)
  raw_diff <- mean(x1) - mean(x2)
  simple_es <- raw_diff / sd(c(x1, x2))
  cohens_d <- raw_diff / cohens_d_sd(x1, x2)
  hedges_g <- raw_diff / hedges_g_sd(x1, x2)
  
  expect_true(hedges_g < cohens_d)
  expect_true(raw_diff < cohens_d)
  expect_true(raw_diff < hedges_g)
  expect_true(cohens_d != simple_es)
  expect_true(hedges_g >   adjust_hedges_g(raw_diff, x1, x2))
})

test_that("Cohen's D is correct standard deviation", {
  x1 <- rnorm(100, mean = 5, sd = 1)
  x2 <- rnorm(200, mean = 11, sd = 0.4)
  expect_true(cohens_d_sd(x1, x2) > 0.4)
  expect_true(cohens_d_sd(x1, x2) < 0.8)
  #
  x1 <- rnorm(100, mean = 5, sd = 10)
  x2 <- rnorm(200, mean = 11, sd = 2)
  expect_true(cohens_d_sd(x1, x2) > 5)
  expect_true(cohens_d_sd(x1, x2) < 7)
  #
  x1 <- rnorm(100, mean = -85, sd = 10)
  x2 <- rnorm(200, mean = -200, sd = 2)
  expect_true(cohens_d_sd(x1, x2) > 5)
  expect_true(cohens_d_sd(x1, x2) < 7)
})

test_that("Hedges g is correct standard deviation adjustment", {
  x1 <- rnorm(100, mean = 5, sd = 1)
  x2 <- rnorm(200, mean = 11, sd = 0.4)
  expect_true(hedges_g_sd(x1, x2) > 0.4)
  expect_true(hedges_g_sd(x1, x2) < 0.8)
  expect_true(hedges_g_sd(x1, x2) > cohens_d_sd(x1, x2))
  #
  x1 <- rnorm(100, mean = 5, sd = 10)
  x2 <- rnorm(200, mean = 11, sd = 2)
  expect_true(hedges_g_sd(x1, x2) > 5)
  expect_true(hedges_g_sd(x1, x2) < 7)
  expect_true(hedges_g_sd(x1, x2) > cohens_d_sd(x1, x2))
  #
  x1 <- rnorm(100, mean = -85, sd = 10)
  x2 <- rnorm(200, mean = -200, sd = 2)
  expect_true(hedges_g_sd(x1, x2) > 5)
  expect_true(hedges_g_sd(x1, x2) < 7)
  expect_true(hedges_g_sd(x1, x2) > cohens_d_sd(x1, x2))
})

test_that("Hedges g small sample size correction works", {
  x1 <- rnorm(20, mean = 10, sd = 2)
  x2 <- rnorm(20, mean = 11, sd = 1.5)
  raw_diff <- mean(x1) - mean(x2)
  g <- raw_diff / hedges_g_sd(x1, x2)
  gstar <- adjust_hedges_g(raw_diff, x1, x2)
  expect_true(gstar > g)
  #
  raw_diff <- mean(x2) - mean(x1)
  g <- raw_diff / hedges_g_sd(x2, x1)
  gstar <- adjust_hedges_g(raw_diff, x2, x1)
  expect_true(gstar < g)
  #
  expect_true(adjust_hedges_g(raw_diff, x2, x1) < raw_diff)
  expect_true(adjust_hedges_g(raw_diff, x2, x1) > raw_diff/2)
})
