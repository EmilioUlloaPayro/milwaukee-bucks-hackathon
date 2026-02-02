library(tidyverse)
library(tidyr)
library(caret)
library(ranger)
library(pdp)
library(ggplot2)
library(doParallel)
library(dplyr)

Combine <- read.csv("csv_ Combined File 2.24 COMPLETE R.csv")

#---- .data cleaning ----

Combine <- Combine %>%
  mutate(
    Gender       = as.factor(Gender),
    Marital      = as.factor(Marital),
    Tier         = as.factor(Tier),
    OpponentSeen = as.factor(OpponentSeen),
    Children     = as.factor(Children),
    WEEKENDYN    = as.factor(WEEKENDYN),  # If you want weekend (1/0) as a factor
    
    Distance      = as.numeric(Distance),
    Finance       = as.numeric(Finance),
    FoodAMT20     = as.numeric(FoodAMT20),
    FoodAMT21     = as.numeric(FoodAMT21),
    FoodAMT22     = as.numeric(FoodAMT22),
    FoodAMT23     = as.numeric(FoodAMT23),
    FoodTotal     = as.numeric(FoodTotal),
    FoodAVG       = as.numeric(FoodAVG),
    RetailAMT18   = as.numeric(RetailAMT18),
    RetailAMT19   = as.numeric(RetailAMT19),
    RetailAMT20   = as.numeric(RetailAMT20),
    RetailAMT21   = as.numeric(RetailAMT21),
    RetailAMT22   = as.numeric(RetailAMT22),
    RetailAMT23   = as.numeric(RetailAMT23),
    RetailTotal   = as.numeric(RetailTotal),
    RetailAVG     = as.numeric(RetailAVG),
    NetCost18     = as.numeric(NetCost18),
    NetCost19     = as.numeric(NetCost19),
    NetCost20     = as.numeric(NetCost20),
    NetCost21     = as.numeric(NetCost21),
    NetCost22     = as.numeric(NetCost22),
    NetCost23     = as.numeric(NetCost23),
    TicketAMTPAID = as.numeric(TicketAMTPAID),
    TicketAVGCOST = as.numeric(TicketAVGCOST),
    AvgOppRank    = as.numeric(AvgOppRank),
    SPENDING18    = as.numeric(SPENDING18),
    SPENDING19    = as.numeric(SPENDING19),
    SPENDING20    = as.numeric(SPENDING20),
    SPENDING21    = as.numeric(SPENDING21),
    SPENDING22    = as.numeric(SPENDING22),
    SPENDING23    = as.numeric(SPENDING23),
    NETTOTAL      = as.numeric(NETTOTAL),
    STDEVGAMEINSCH = as.numeric(STDEVGAMEINSCH), 
    
    Tick18        = as.integer(Tick18),
    Scans18       = as.integer(Scans18),
    Tick19        = as.integer(Tick19),
    Scans19       = as.integer(Scans19),
    Tick20        = as.integer(Tick20),
    Scans20       = as.integer(Scans20),
    Tick21        = as.integer(Tick21),
    Scans21       = as.integer(Scans21),
    Tick22        = as.integer(Tick22),
    Scans22       = as.integer(Scans22),
    Tick23        = as.integer(Tick23),
    Scans23       = as.integer(Scans23),
    TicketsBought = as.integer(TicketsBought),
    GamesAttend   = as.integer(GamesAttend),
    AVGGAMEINSCH  = as.integer(AVGGAMEINSCH),     
    
    AVGTIME       = as.POSIXct(AVGTIME, format = "%I:%M:%S %p")
  ) %>%
  filter(NETTOTAL != 0)

Combine$PartySize <- round(Combine$PartySize)

Combine <- Combine %>%
  mutate(
    Tier = case_when(
      Tier == 3 & !is.na(TicketAVGCOST) & TicketAVGCOST > 150 ~ 2,  # Assign Tier 2
      Tier %in% c(4, 5) & NETTOTAL == 0 ~ 6,  # Assign Tier 6
      TRUE ~ as.numeric(as.character(Tier))  # Keep existing values as numeric
    ) %>%
      as.factor()  # Convert back to factor
  )

table(Combine$Tier)

table(factor(Combine$Tier, 
             levels = c(1, 2, 3, 4, 5, 6), 
             labels = c("Current Season Ticket Member", "Prospective Season Ticket Member", "Partial Plan Holder", 
                        "Engaged Single Game Ticket Buyer", "Casual Single Game Ticket Buyer", 
                        "Lapsed Fan")))


Sales.model1 <- glm(NETTOTAL~Distance+Finance+OpponentSeen+Tier+AvgOppRank+PartySize,data = Combine)
summary(Sales.model1)
plot(Sales.model1)

#Attendance
Combine$SGamesAttend <- sqrt(Combine$GamesAttend)
AttendanceCombine <- Combine[Combine$SGamesAttend != 0,]

sum(is.na(AttendanceCombine$Distance))
sum(is.na(AttendanceCombine$TicketsBought))
sum(is.na(AttendanceCombine$TicketAVGCOST))
sum(is.na(AttendanceCombine$PartySize))

mean_distance <- mean(AttendanceCombine$Distance, na.rm = TRUE)
AttendanceCombine$Distance[is.na(AttendanceCombine$Distance)] <- mean_distance

Attendance.model <- glm(SGamesAttend~Distance+TicketsBought+TicketAVGCOST+PartySize, data = AttendanceCombine)
summary(Attendance.model)

AttendanceCombine$Fitted <- fitted(Attendance.model)
AttendanceCombine$Residuals <- resid(Attendance.model)

ggplot(AttendanceCombine, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

#Coefficient
library(broom)

coef_AttendanceCombine <- tidy(Attendance.model)

ggplot(coef_AttendanceCombine, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +
  labs(title = "Coefficient Plot for GLM",
       x = "Coefficient Estimate",
       y = "Predictors") +
  theme_minimal()

ggplot(AttendanceCombine, aes(x = Fitted, y = SGamesAttend)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Fitted vs. Observed Values",
       x = "Fitted Values",
       y = "Observed SQRT GamesAttend") +
  theme_minimal()

#--------- -------- Retail Work -------- --------
Combine$LogRetail <- log(Combine$RetailTotal + 1)
RetailCombine <- Combine[Combine$LogRetail != 0,]
mean_finance <- mean(RetailCombine$Finance, na.rm = TRUE)
RetailCombine$Finance[is.na(RetailCombine$Finance)] <- mean_finance

Retail.model <- glm(LogRetail~Marital+Children+Finance+Gender, data = RetailCombine)
summary(Retail.model)

hist(RetailCombine$LogRetail)

RetailCombine$Fitted <- fitted(Retail.model)
RetailCombine$Residuals <- resid(Retail.model)

#Residuals
ggplot(RetailCombine, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

#Coefficient
library(broom)

coef_RetailCombine <- tidy(Retail.model)

ggplot(coef_RetailCombine, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +
  labs(title = "Coefficient Plot for GLM",
       x = "Coefficient Estimate",
       y = "Predictors") +
  theme_minimal()

#fitted vs observed

ggplot(RetailCombine, aes(x = Fitted, y = LogRetail)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Fitted vs. Observed Values",
       x = "Fitted Values",
       y = "Observed LogRetail") +
  theme_minimal()

