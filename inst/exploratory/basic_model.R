require(ggplot2)
require(scales)

# Demonstration of the zero-spike issue

ggplot(crises, aes(x = friend_percent + 0.000001, y = fund_percent, label = source_country)) + 
  geom_point(aes(size = sqrt(source_GDP)), alpha = .25) + 
  #geom_point(alpha = .4) + 
  coord_trans(y = "log2", x = "log2") +  # A visual log-scale representation
  scale_x_continuous(name = "Friend percent") +
  scale_y_continuous(name = "Funding percent") +
  # geom_text(aes(color = source_country), size = 2) +
  theme(legend.position = "none")


# Basic relation between friendship percent and funding percent (with nonzero data)
nonzero <- crises[friendships > 0, ]
ggplot(nonzero, aes(x = friend_percent, y = fund_percent, label = source_country)) + 
  coord_trans(y = "log2", x = "log2") +  # A visual log-scale representation
  scale_x_continuous(name = "Friend percent") +
  scale_y_continuous(name = "Funding percent") +
  geom_text(aes(color = source_country), size = 3, alpha = .75) +
  theme_bw() +
  theme(legend.position = "none")
  

cor.test(log(nonzero$fund_percent), log(nonzero$friend_percent), use = "complete.obs")

hist(sqrt(crises$fund_percent))
hist(sqrt(crises$friend_percent))

require(lme4)
model1 <- lmer(log(fund_percent) ~ 1 + 
                 log(friend_percent) + 
                 log(target_Population) + 
                 log(source_Population) + 
                 log(source_GDP) + 
                 log(target_GDP) + 
                 (1|target_country), 
               data = crises[friend_percent > 0,], na.action = na.omit)
summary(model1)

