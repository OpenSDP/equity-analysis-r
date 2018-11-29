# Unit tests for gap.test function

test_df <- data.frame(math_ss = runif(3000), 
                      grade = sample(c(3, 4), 3000, replace = TRUE), 
                      gender = sample(c("M", "F"), 3000, replace = TRUE), 
                      stringsAsFactors = FALSE
                      )

test_df2 <- data.frame(math_ss = runif(300), 
                      grade = sample(c(3), 300, replace = TRUE), 
                      gender = sample(c("M", "F"), 300, replace = TRUE), 
                      stringsAsFactors = FALSE
)


test_df3 <- data.frame(math_ss = runif(3000), 
                       grade = sample(c(3, 4, 5, 6), 3000, replace = TRUE), 
                       econ = sample(c("F", "R", "N"), 3000, replace = TRUE), 
                       stringsAsFactors = FALSE
)



gap.test(df = test_df, 
         grade = "grade", 
         outcome = "math_ss", 
         features = "gender", 
         cut = 20, 
         n = 2)

gap.test(df = test_df2, 
         grade = "grade", 
         outcome = "math_ss", 
         features = "gender", 
         cut = 20, 
         n = 2)

gap.test(df = test_df3, 
         grade = "grade", 
         outcome = "math_ss", 
         features = "econ", 
         cut = 20, 
         n = 6)

# Pads NAs, should just return max amount
gap.test(df = test_df3, 
         grade = "grade", 
         outcome = "math_ss", 
         features = "econ", 
         cut = 20, 
         n = 16)


ex.sds <- data.frame(grade = c(3,4,5,6,7,8),
                     math_ss = runif(6)
                     )



gap.test(df = test_df, 
         grade = "grade", 
         outcome = "math_ss", 
         features = "gender", 
         sds = ex.sds,
         cut = 20, 
         n = 2)

gap.test(df = test_df, 
         grade = "grade", 
         outcome = "math_ss", 
         features = "gender", 
         sds = ex.sds,
         cut = 20, 
         n = 2, 
         med = TRUE)


autoplot.gap_test(gap.test(df = test_df, 
                           grade = "grade", 
                           outcome = "math_ss", 
                           features = "gender", 
                           sds = ex.sds,
                           cut = 20, 
                           n = 2))

autoplot.gap_test <- function(df) {
  df$comp_name <- paste(df$level_1, df$level_2, sep = "-")
  
  ylims <- c(min(df$effect_size), max(df$effect_size))
  if (ylims[1] > -0.1) {
    ylims[1] <- -0.1
  }
  if (ylims[2] < 0.1) {
    ylims[2] <- 0.1
  }
  
  # names(gaps)[length(gaps)] <- paste(level1,"-",level2,", ",
  #                                    feature,'\n',"Grade ",gr,
  #                                    ", ",outcome, sep="")
  # 
  barp <- ggplot(df, aes(x= reorder(comp_name, abs(effect_size)), 
                         y = effect_size), group = grade_level) +
    geom_bar(position="dodge",stat="identity") +
    scale_x_discrete(name = "Comparison") +
    scale_y_continuous(name = "Effect Size", 
                       limits = ylims) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))+
    geom_hline(yintercept=0.1, linetype="solid", 
               color = "red", size=1) +
    geom_hline(yintercept=-0.1, linetype="solid", 
               color = "red", size=1) +
    geom_hline(yintercept = 0, linetype = 2, color = "blue", 
               size = 1) + 
    labs(title = paste0("Top ", nrow(df), " gap effect sizes"), 
         caption = expression("Calculation: d = " ~ frac(bar(Delta), sigma[pooled]) ~ "\n" ~
                                "Effect Size = " ~  frac(d, sqrt(d^2 + 4)))) + 
      facet_grid(~feature) + theme_bw()
  print(barp)

}


gap.table <- gap.test2(df = texas_data,
                      grade = "grade_level",
                      outcome = "math_ss",
                      features = c('eco_dis','lep','iep','race_ethnicity','male'),
                      sds = sd_table,
                      n = 3,
                      cut = 120,
                      outlbl = "math score gap")