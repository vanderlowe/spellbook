require(magic)
require(spellbook)
require(lme4)
require(multilevel)
require(languageR)
require(data.table)

# Load crisis data, if not already in memory
if (!exists("crises")) {
  # NOTE: Takes a few minutes to run
  loadData("friendships_and_donations_per_country")
}

# Function to center data
center <- function(x) {
  return(scale(x, center = T, scale = F))
}

# Below, ICC1 indicates that only 0.1% of the variance in funding is explained by emergency_id. 
# ICC2 value indicates that emergencies are not reliably identified in terms of funding.
em.id <- aov(log(fund_percent) ~ emergency_id, crises[friend_percent > 0])
ICC1(em.id)
ICC2(em.id)

# ICC1 indicates that 54% of the variance in funding is explained by source_country.
# Likewise, donor countries can be reliably identified from donations.
src.cntry <- aov(log(fund_percent) ~ factor(source_country), crises[friend_percent > 0])
ICC1(src.cntry)
ICC2(src.cntry)

# Below (ICC1) indicates that 35% of the variance in funding is explained by target_country.
# ICC2 shows that target countries can also be reliably differentiated from donations.
targ.cntr <- aov(log(fund_percent) ~ factor(target_country), crises[friend_percent > 0])
ICC1(targ.cntr)
ICC2(targ.cntr)

m <- lmer(log(fund_percent) ~ 
            (1|target_country) +
            (1|source_country) +
            log(friend_percent) +
            log(source_Population) +
            log(target_Population) +
            log(source_GDP) +
            log(target_GDP),
          data = clean)

summary(m)

pvals.fnc(m, ndigits=3)
