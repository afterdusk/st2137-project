data_folder = 'D:/Liang Jun/Desktop/Google Drive/AY1819 Sem 1/ST2137/ST2137 Project/Part A Data Analysis/'
funds = read.csv(paste0(data_folder, 'mutual funds csv.txt'), header = TRUE)
attach(funds)

cat("Preview of dataset\n")
print(funds[1:5,])

# Question 1
normalitytest <-function(first, second) {
  cat("Checking normality assumption...\n")
  x <- first
  normal <- T
  for (i in 1:2) {
    test <- ks.test(x, "pnorm", mean(x), sd(x))
    print(test)
    if (test$p.value > 0.05) {
      cat("Since p-value = ", test$p.value, 
          " > 0.05, we do not reject the null hypothesis that the distribution is normal\n\n")
    } else {
      cat("Since p-value = ", test$p.value, 
          " < 0.05, we reject the null hypothesis that the distribution is normal\n\n")
      normal = F
    }
    x <- second
  }
  return(normal)
}

twosamplettest <- function(first, second){
  cat("Checking variance...\n")
  vartest <- var.test(first, second)
  print(vartest)
  test <- NULL
  if (vartest$p.value > 0.05) {
    cat("Since p-value = ", vartest$p.value, 
        " > 0.05, we do not reject the null hypothesis that the variances are equal\n\n")
    cat("Testing relationship between variables (equal variance) ...\n")
    test <- t.test(first, second, var.equal=T)
  } else {
    cat("Since p-value = ", vartest$p.value, 
        " < 0.05, we reject the null hypothesis that the variances are equal\n\n")
    cat("Testing relationship between variables (unequal variance)...\n\n")
    test <- t.test(first, second, var.equal=F)
  }
  print(test)
}

nonparatest <- function(first, second) {
  cat("Testing relationship between variables (non-parametric test)...\n\n")
  print(wilcox.test(first, second))
}

cat(rep("=", 40), "\n")
cat("Q1 Comparing funds with and without fees\n")
cat("Since Fee is a categorical variable and Return_2001, Three_Year_Return, Five_Year_Return are numerical variables, we perform two-sample t-tests\n\n")

#   Question 1a
cat("a. 2001 Return\n")
fee_2001 <- Return_2001[Fees == 'Yes']
nofee_2001 <- Return_2001[Fees == 'No']
if (normalitytest(fee_2001, nofee_2001)) {
  twosamplettest(fee_2001, nofee_2001)
} else {
  nonparatest(fee_2001, nofee_2001)
}

#   Question 1b
cat(rep("-", 40), "\n")
cat("b. 3-Year Return\n")

fee_3year <- Three_Year_Return[Fees == 'Yes']
nofee_3year <- Three_Year_Return[Fees == 'No']
if (normalitytest(fee_3year, nofee_3year)) {
  twosamplettest(fee_3year, nofee_3year)
} else {
  nonparatest(fee_3year, nofee_3year)
}

#   Question 1c
cat(rep("-", 40), "\n")
cat("c. 5-Year Return\n")

fee_5year <- Five_Year_Return[Fees == 'Yes']
nofee_5year <- Five_Year_Return[Fees == 'No']
if (normalitytest(fee_5year, nofee_5year)) {
  twosamplettest(fee_5year, nofee_5year)
} else {
  nonparatest(fee_5year, nofee_5year)
}

#Question 1d Fees with Expense Ratio
cat(rep("-", 40), "\n")
cat("d. Expense Ratio\n")

fee_expense <- Expense_ratio[Fees == 'Yes']
nofee_expense <- Expense_ratio[Fees == 'No']
if (normalitytest(fee_expense,nofee_expense)) {
  twosamplettest(fee_expense,nofee_expense)  
} else {
  nonparatest(fee_expense,nofee_expense)
}

# Question 2
cat(rep("=", 40), "\n")
cat("Q2 Comparing mutual fund types\n")

modelcheck <- function(variable, group) {
  cat("Checking for equal variance...\n")
  vartest <- bartlett.test(variable, group)
  print(vartest)
  varassumption = T
  if (vartest$p.value > 0.05) {
    cat("Since p-value = ", vartest$p.value, 
    " > 0.05, we do not reject the null hypothesis that the variances are equal.\n")
    cat("Equal variance assumption holds\n\n")
  } else {
    cat("Since p-value = ", vartest$p.value, 
        " < 0.05, we reject the null hypothesis that the variances are equal.\n")
    cat("Equal variance assumption violated\n\n")
    varassumption = F
  }
  cat("Checking that errors are independent, normally distributed...\n")
  model <- aov(variable ~ group)
  resid <- model$res
  normtest <- ks.test(resid, "pnorm", mean(resid), sd(resid))
  print(normtest)
  normassumption = T
  if (normtest$p.value > 0.05) {
    cat("Since p-value = ", normtest$p.value, 
        " > 0.05, we do not reject the null hypothesis that the distribution of residuals is normal.\n")
    cat("Error assumption holds\n\n")
    
  } else {
    cat("Since p-value = ", normtest$p.value, 
        " < 0.05, we reject the null hypothesis that the distribution of residuals is normal..\n")
    cat("Error assumption violated\n\n")
    normassumption = F
  }
  if (varassumption == F || normassumption == F) {
    cat("Since not all assumptions hold, proceed with Kruskal-Wallis test\n\n")
    return(F)
  } else {
    cat("Since all assumptions hold, proceed with ANOVA F-test\n\n")
    return(T)
  }
}

lsd <- function(variable, group) {
  means <- tapply(variable, group, mean)
  lengths <- tapply(variable, group, length)

  mse <- sum(aov(variable~group)$res^2/ (length(variable) - 3))

  lsd <- qt(0.975, (length(variable) - 3)) * sqrt(mse*(1/lengths[1] + 1/lengths[2]))
  check.lsd(means,1,2,lsd)
  lsd <- qt(0.975, (length(variable) - 3)) * sqrt(mse*(1/lengths[1] + 1/lengths[3]))
  check.lsd(means,1,3,lsd)
  lsd <- qt(0.975, (length(variable) - 3)) * sqrt(mse*(1/lengths[2] + 1/lengths[3]))
  check.lsd(means,2,3,lsd)
}

check.lsd <-function(means,i,j,lsd){
  mx <-means[i]
  my <-means[j]
  d <-mx-my
  if(abs(d)>lsd) cat("There is significant difference between groups",dimnames(means)[[1]][i],"&",dimnames(means)[[1]][j],"\n","Means=",mx,",",my," Diff =", d," > LSD =",lsd,"\n")
  else cat("There is no significant difference between groups",dimnames(means)[[1]][i],"&",dimnames(means)[[1]][j],"\n","Means=",mx,",",my," Diff =", d," < LSD =",lsd,"\n")
}

cat("During 2001\n")
pval <- NULL
if (modelcheck(Return_2001, Type)) {
  model <- aov(Return_2001 ~ Type)
  summary(model)
  pval <- summary(model)[[1]][["Pr(>F)"]][[1]]
} else {
  kwtest <- kruskal.test(Return_2001, Type)
  print(kwtest)
  pval <- kwtest$p.value
}

if (pval > 0.05) {
  cat("Since p-value = ", pval, " > 0.05, we do not reject the null hypothesis", 
      "that means for all fund types are equal.\n\n")
} else {
  cat("Since p-value = ", pval, " < 0.05, we reject the null hypothesis", 
  "that means for all fund types are equal.\n\n")
  cat("Performing Least Square Difference Test...\n")
  lsd(Return_2001, Type)
}

cat(rep("-", 40), "\n")
cat("1999 - 2001\n")
pval <- NULL
if (modelcheck(Three_Year_Return, Type)) {
  model <- aov(Three_Year_Return ~ Type)
  print(summary(model))
  pval <- summary(model)[[1]][["Pr(>F)"]][[1]]
} else {
  kwtest <- kruskal.test(Three_Year_Return, Type)
  print(kwtest)
  pval <- kwtest$p.value
}

if (pval > 0.05) {
  cat("Since p-value = ", pval, " > 0.05, we do not reject the null hypothesis", 
      "that means for all fund types are equal.\n\n")
} else {
  cat("Since p-value = ", pval, " < 0.05, we reject the null hypothesis", 
      "that means for all fund types are equal.\n\n")
  cat("Performing Least Square Difference Test...\n")
  lsd(Three_Year_Return, Type)
}

cat(rep("-", 40), "\n")
cat("1997 - 2001\n")
model <- aov(Five_Year_Return ~ Type)
pval <- NULL
if (modelcheck(Five_Year_Return, Type)) {
  model <- aov(Five_Year_Return ~ Type)
  summary(model)
  pval <- summary(model)[[1]][["Pr(>F)"]][[1]]
} else {
  kwtest <- kruskal.test(Five_Year_Return, Type)
  print(kwtest)
  pval <- kwtest$p.value
}

if (pval > 0.05) {
  cat("Since p-value = ", pval, " > 0.05, we do not reject the null hypothesis", 
      "that means for all fund types are equal.\n\n")
} else {
  cat("Since p-value = ", pval, " < 0.05, we reject the null hypothesis", 
      "that means for all fund types are equal.\n\n")
  cat("Performing Least Square Difference Test...\n")
  lsd(Five_Year_Return, Type)
}

# Question 3
cat(rep("=", 40), "\n")
cat("Q3 Comparing pairs of variables\n")

#   Question 3a
cat("a. Type and Fee\n")
cat("Since Type and Fees are categorical variables that are individually unrelated/independent, ", 
    "we perform chi-square tests to investigate their relationship\n\n\n")
cat("Frequency Table\n")
freqtable <- table(Type, Fees)
print(freqtable)
cat("Chi-Square Test\n")
chisq.test(freqtable)

#   Question 3b
cat(rep("-", 40), "\n")
cat("a. Risk and Fee\n")
cat("Since Type and Risk are categorical variables that are individually unrelated/independent, ", 
    "we perform chi-square tests to investigate their relationship\n\n\n")
cat("Frequency Table\n")
freqtable <- table(Risk, Fees)
print(freqtable)
cat("Chi-Square Test\n")
chisq.test(freqtable)
cat("\nAs the counts for some categories are small, it is more appropriate to perform a fisher test\n")
cat("Fisher Test\n")
fisher.test(freqtable)

#   Question 3c
cat(rep("-", 40), "\n")
cat("a. Turnover and Fee\n")
cat("Since Turnover and Risk are categorical variables that are individually unrelated/independent, ", 
    "we perform chi-square tests to investigate their relationship\n\n\n")
cat("Frequency Table\n")
freqtable <- table(Turnover, Fees)
print(freqtable)
cat("Chi-Square Test\n")
chisq.test(freqtable)
cat("\nAs the counts for some categories are small, it is more appropriate to perform a fisher test\n")
cat("Fisher Test\n")
fisher.test(freqtable)