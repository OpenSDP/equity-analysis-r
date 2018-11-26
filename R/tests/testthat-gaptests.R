# Unit tests for gap.test function

test_df <- data.frame(math_ss = runif(3000), 
                      grade = sample(c(3, 4), 3000, replace = TRUE), 
                      gender = sample(c("M", "F"), 3000, replace = TRUE), 
                      stringsAsFactors = FALSE
                      )

test_df$grade <- as.integer(test_df$grade)

gap.test(df = test_df, 
         grade = "grade", 
         outcome = "math_ss", 
         features = "gender", 
         cut = 20, 
         n = 2)

gap.table <- gap.test(df = texas_data,
                      grade = "grade_level",
                      outcome = "math_ss",
                      features = c('eco_dis','lep','iep','race_ethnicity','male'),
                      sds = sd_table,
                      n = 3,
                      cut = 120,
                      outlbl = "math score gap")