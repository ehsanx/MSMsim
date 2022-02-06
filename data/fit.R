# Step 1: Weight denominator model
ww <- glm(A ~ tpoint + Alag + L0 + L + Llag, family = binomial(logit), 
          data = aggregate.data)
# Step 2: Weight numerator model
ww0 <- glm(A ~ tpoint + Alag + L0, family = binomial(logit), 
           data = aggregate.data)
# Step 3: Obtain predictions from the models
aggregate.data$wwp <- with(aggregate.data, 
                           ifelse(A == 0, 1 - fitted(ww), fitted(ww)))
aggregate.data$wwp0 <- with(aggregate.data, 
                            ifelse(A == 0, 1 - fitted(ww0),fitted(ww0)))
# Step 4: Calculate time-dependent IPWs
aggregate.data$sw <- unlist(tapply(aggregate.data$wwp0/aggregate.data$wwp, 
                                   aggregate.data$id, cumprod))
# Step 5: Weighted outcome model
fit.msm <- coxph(Surv(tpoint0, tpoint, Y) ~ A + L0 + cluster(id), 
                 data = aggregate.data, weight = sw, robust = TRUE)