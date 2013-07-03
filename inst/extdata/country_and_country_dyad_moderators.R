require(magic)
require(spellbook)
require(lme4)
require(data.table)

# Load data, if not already in memory
if (!exists("crises")) {
  loadData("friendships_and_donations_per_country")
}

# Define main variables
dv <- "fund_percent"
iv <- "friend_percent"

# Moderator and control variables
moderators <- c("log(target_InternetUsers + 0.0001)", "log(target_Population)", "log(target_GDP)", "log(target_GDP_PPP)", 
                "log(target_Trade)", "log(target_GDP_PerCapita)", "log(target_GDP_PerCapita_PPP)",  "log(target_Growth)", 
                "log(source_InternetUsers)", "log(source_Population)", "log(source_GDP)", "log(source_GDP_PPP)", "log(source_Trade)", 
                "log(source_GDP_PerCapita)", "log(source_GDP_PerCapita_PPP)", "log(source_Growth + 0.001)", "Share_Borders", "Colony", 
                "Borders_Distance", "SharedLanguage", "Distance", "log(source_exports)", "log(target_exports)")

# Quick visual check for which variables need log-transformation
# pdf("moderator_check.pdf", paper="a4r")
#   for (m in moderators) {
#     hist(crises[, get(m)], main=m)
#     hist(log(crises[, get(m)]), main=m)
#   }
# dev.off()

controls <- data.table(variable = moderators, t = 0, main = 0)
mods <- data.table(variable = moderators, t = 0, interaction = 0, main = 0)

for (m in moderators) {
  cat(m, "\n")
  # Create a model with a control variable
  control.cmd <- sprintf("model <- lmer(log(%s) ~ log(%s) + %s + (1|target_country), data = crises[amount > 0 & friendships > 0,])", iv, dv, m)
  # Create a model with a moderator variable
  moderator.cmd <- sprintf("model <- lmer(log(%s) ~ log(%s)*%s + (1|target_country), data = crises[amount > 0 & friendships > 0,])", iv, dv, m)
  controlModel <- eval(parse(text=control.cmd))
  moderatorModel <- eval(parse(text=moderator.cmd))
  
  s.c <- summary(controlModel)
  controls[variable == m, t := s.c@coefs[3,3]]  # Store t-value of control
  controls[variable == m, main := s.c@coefs[2,3]]  # Store t-value of main effect
  
   s.m <- summary(moderatorModel)
   mods[variable == m, t := s.m@coefs[3,3]]  # Store t-value of control
   mods[variable == m, main := s.m@coefs[2,3]]  # Store t-value of main effect
   mods[variable == m, interaction := s.m@coefs[4,3]]  # Store t-value of interaction effect
}

# Show significant control variables
controls[abs(t) > 2, ][order(abs(t))]

# Show significant interaction variables
mods[abs(interaction) > 2, ][order(abs(t))]

# http://stats.stackexchange.com/questions/18480/interpretation-of-log-transformed-predictor?rq=1
# log(DV) = Intercept + B1 * log(IV) + Error 
# "One percent increase in IV is associated with a (B1) percent increase in DV."

