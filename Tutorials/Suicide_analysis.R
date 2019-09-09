# Load the necessary package
library(faraway)

# Look at the data
? suicide
suicide
summary(suicide)

# Extract the variables for easy use
y <- suicide$y
cause <- suicide$cause
age <- factor(suicide$age, levels = c("m", "o", "y"), labels = c("Middle-Aged", "Old", "Young"))
sex <- factor(suicide$sex, levels = c("f", "m"), labels = c("Female", "Male"))

# Visualize the relationships in the data
boxplot(y ~ cause, main = "Suicide by Cause", xlab = "Cause", ylab = "Suicide Count")
boxplot(y ~ age, main = "Suicide by Age", xlab = "Age", ylab = "Suicide Count")
boxplot(y ~ sex, main = "Suicide by Sex", xlab = "Sex", ylab = "Suicide Count")

# Let's fit a model
m <- lm(y ~ cause + age + sex)
s <- summary(m)
a <- anova(m)

# How do we interpret each of the betas?

# Is the "cause" variable is significant? In other words, is the expected suicide count the same for each method?
# Manually:
m_red1 <- lm(y ~ age + sex)
ar1 <- anova(m_red1)
t <- ((ar1$`Sum Sq`[3] - a$`Sum Sq`[4])/5)/a$`Mean Sq`[4]
pval <- pf(q = t, df1 = 5, df2 = m$df.residual, lower.tail = FALSE)
# Or automatically:
anova(m_red1, m)

# Is the "age" variable significant? In other words, is the expected suicide count the same for each age?
m_red2 <- lm(y ~ cause + sex)
ar2 <- anova(m_red2)
t <- ((ar2$`Sum Sq`[3] - a$`Sum Sq`[4])/2)/a$`Mean Sq`[4]
pval <- pf(q = t, df1 = 2, df2 = m$df.residual, lower.tail = FALSE)
# Or automatically:
anova(m_red2, m)

# Is the "sex" variable is significant? In other words, is the expected suicide count the same for each sex?
m_red3 <- lm(y ~ cause + age)
ar3 <- anova(m_red3)
t <- ((ar3$`Sum Sq`[3] - a$`Sum Sq`[4])/1)/a$`Mean Sq`[4]
pval <- pf(q = t, df1 = 1, df2 = m$df.residual, lower.tail = FALSE)
# Or automatically:
anova(m_red3, m)

# How much of the response variation is explained by the model?
s$r.squared

# Perform the overall test of significance for this model
s$fstatistic
pval <- pf(q = s$fstatistic[1], df1 = s$fstatistic[2], df2 = s$fstatistic[3], lower.tail = FALSE)

# Is there an interaction between "Cause" and "Age" or between "Cause" and "Sex". In other words
# do suicides rates by different causes seem the same for people of different ages or sex?
interaction.plot(x.factor = age, trace.factor = cause, response = y, xlab = "Age", ylab = "Average Suicide Count", main = "Interaction between Cause and Age")
interaction.plot(x.factor = sex, trace.factor = cause, response = y, xlab = "Sex", ylab = "Average Suicide Count", main = "Interaction between Cause and Sex")

# We should try to account for this in the model
m_full <- lm(y ~ cause + age + sex + cause:age + cause:sex)
sf <-summary(m2)
af <- anova(m2)

# How much of the response variation does this model explain?
sf$r.squared

# Is the cause-by-age interaction significant?
m_red4 <- lm(y ~ cause + age + sex + cause:sex)
anova(m_red4, m_full)

# Is the cause-by-sex interaction significant?
m_red5 <- lm(y ~ cause + age + sex + cause:age)
anova(m_red5, m_full)
