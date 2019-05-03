# Unit tests for gap_test function

source("../functions.R")
# Generate test data

test_df <- data.frame(math_ss = runif(3000), 
                      grade = sample(c(3, 4), 3000, replace = TRUE), 
                      gender = sample(c("M", "F"), 3000, replace = TRUE), 
                      stringsAsFactors = FALSE
                      )

test_df2 <- data.frame(math_ss = runif(300), 
                      grade = sample(2:8, 300, replace = TRUE), 
                      gender = sample(c("M", "F"), 300, replace = TRUE), 
                      stringsAsFactors = FALSE
)


test_df3 <- data.frame(math_ss = runif(3000), 
                       grade = sample(c(3, 4, 5, 6), 3000, replace = TRUE), 
                       econ = sample(c("F", "R", "N"), 3000, replace = TRUE), 
                       stringsAsFactors = FALSE
)

context("Test fail conditions, warnings, and errors")

test_that("gap test fails when it should", {
  
  expect_error(gap_test(df = test_df, 
                          grade = "grad", 
                          outcome = "math_ss", 
                          features = "gender", 
                          cut = 20, 
                          n = 2))
  
  expect_error(gap_test(df = test_df, 
                          grade = "grade", 
                          outcome = "math_ss", 
                          features = "gender", 
                          cut = 20, 
                          sds = runif(6),
                          n = 2))
  
  # Need to make a better error handling of this case
  expect_error(gap_test(df = test_df[1:2,], 
           grade = "grade", 
           outcome = "math_ss", 
           features = "gender", 
           cut = 20, 
           n = 2)
  )
  #SDs failures
  
})


context("Test result structure returned is correct")

test_that("Row counts are right", {
  gt <- gap_test(df = test_df,
           grade = "grade",
           outcome = "math_ss",
           features = "gender",
           cut = 20,
           n = 2)
  expect_equal(nrow(gt), 2)
  expect_equal(ncol(gt), 7)
  
  gt <- gap_test(df = test_df,
                 grade = "grade",
                 outcome = "math_ss",
                 features = "gender",
                 cut = 20,
                 n = 10, verbose = TRUE)
  
  expect_equal(ncol(gt), 11)
  expect_equal(nrow(gt), 2)
  
  gt <- gap_test(df = test_df3,
                 grade = "grade",
                 outcome = "math_ss",
                 features = "econ",
                 cut = 20,
                 n = 10)
  
  expect_equal(ncol(gt), 7)
  expect_equal(nrow(gt), 10)
  
  gt <- gap_test(df = test_df3,
                 grade = "grade",
                 outcome = "math_ss",
                 features = "econ",
                 cut = 20, verbose = TRUE,
                 n = 22)
  expect_equal(nrow(gt), 12)
  expect_equal(ncol(gt), 11)
})

# Test user specified parameters and expected behavior
context("Test function parameters have desired effect")

test_that("Test cutoff and n count parameters work", {
  gt <- gap_test(df = test_df2,
           grade = "grade",
           outcome = "math_ss",
           features = "gender",
           cut = 5, verbose = TRUE,
           n = 5)
  expect_equal(nrow(gt), 5)
  gt <- gap_test(df = test_df2,
                 grade = "grade",
                 outcome = "math_ss",
                 features = "gender",
                 cut = 5, verbose = TRUE,
                 n = 2)
  expect_equal(nrow(gt), 2)
  
  gt <- gap_test(df = test_df2,
                 grade = "grade",
                 outcome = "math_ss",
                 features = "gender",
                 cut = 500, verbose = TRUE,
                 n = 10)
  expect_true(all(is.na(gt$mean_diff)))
  
  gt <- gap_test(df = test_df2,
                 grade = "grade",
                 outcome = "math_ss",
                 features = "gender",
                 cut = 1000, verbose = TRUE,
                 n = 10)
  expect_true(all(is.na(gt$mean_diff)))

})

test_that("Grade, features and outcome selection all work as intended", {
  expect_error(gap_test(df = test_df,
                 grade = "grade2",
                 outcome = "math_ss",
                 features = "gender",
                 cut = 20,
                 n = 10, verbose = TRUE)
  )
  
  expect_error(gap_test(df = test_df,
                        outcome = "math_ss",
                        features = "gender",
                        cut = 20,
                        n = 10, verbose = TRUE)
  )
  
  expect_error(gap_test(df = test_df,
                        grade = "grade",
                        outcome = "math_ss",
                        features = "gender2",
                        cut = 20,
                        n = 10, verbose = TRUE)
  )
  
  gt <- gap_test(df = test_df3,
                 grade = "grade",
                 outcome = "math_ss",
                 features = "econ",
                 cut = 20,
                 n = 10)
  expect_true(all(gt$feature == "econ"))
  expect_true(all(gt$outcome == "math_ss"))
  expect_true(all(gt$grade_level %in% unique(test_df3$grade)))
  
})


# 
context("Test numeric accuracy of gap test calculations")



test_that("Test cutoff and n count parameters work", {
  
  
  
})

context("Test varying input data types for proper warnings, error messages, etc.")

test_that("Outcome variables work", {
  
  
  
})



# 
# gap_test(df = test_df2, 
#          grade = "grade", 
#          outcome = "math_ss", 
#          features = "gender", 
#          cut = 20, 
#          n = 2)
# 
# gap_test(df = test_df3, 
#          grade = "grade", 
#          outcome = "math_ss", 
#          features = "econ", 
#          cut = 20, 
#          n = 6)
# 
# # Pads NAs, should just return max amount
# gap_test(df = test_df3, 
#          grade = "grade", 
#          outcome = "math_ss", 
#          features = "econ", 
#          cut = 20, 
#          n = 16)
# 
# 
# ex.sds <- data.frame(grade = c(3,4,5,6,7,8),
#                      math_ss = runif(6)
#                      )
# 
# 
# 
# gap_test(df = test_df, 
#          grade = "grade", 
#          outcome = "math_ss", 
#          features = "gender", 
#          sds = ex.sds,
#          cut = 20, 
#          n = 2)
# 
# gap_test(df = test_df, 
#          grade = "grade", 
#          outcome = "math_ss", 
#          features = "gender", 
#          sds = ex.sds,
#          cut = 20, 
#          n = 2, 
#          med = TRUE)
# 
# 
# autoplot.gap_test(gap_test(df = test_df, 
#                            grade = "grade", 
#                            outcome = "math_ss", 
#                            features = "gender", 
#                            sds = ex.sds,
#                            cut = 20, 
#                            n = 2))
# 
# 
# gap.table <- gap_test2(df = texas_data,
#                       grade = "grade_level",
#                       outcome = "math_ss",
#                       features = c('eco_dis','lep','iep','race_ethnicity','male'),
#                       sds = sd_table,
#                       n = 3,
#                       cut = 120,
#                       outlbl = "math score gap")