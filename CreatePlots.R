
#### Plot: number of words in the x axis, OOB error in y axis, ntrees and words used in the model

library(ggplot2)

rf_tree_errors <- list(rf.100.1000, rf.100.2000, rf.100.3000, rf.100.4000, rf.100.5000, rf.200.1000, rf.200.2000, rf.200.3000, rf.200.4000, rf.200.5000, rf.full.model, rf.full.model.200)
rf_oobs <- sapply(rf_tree_errors, function(x) x[[2]])
rf_ntree <- as.factor(sapply(rf_tree_errors, function(x) x[[1]]$ntree))
rf_words <- c(1000, 2000, 3000, 4000, 5000, 1000, 2000, 3000, 4000, 5000, 8878, 8878)

rf_plotdata <- data.frame(rf_words, rf_oobs, rf_ntree) %>%
  group_by(rf_words) %>%
  arrange(rf_words)

ggplot(data = rf_plotdata, aes(x = rf_words, y = rf_oobs, col = rf_ntree)) + 
  geom_line() + geom_point() + coord_cartesian(xlim = c(0, 10000), ylim = c(0.24, 0.28))+
  scale_x_discrete(limits = seq(from = 0, to = 9000, by = 1000)) +
  scale_color_manual(values = c("#3342FF", "#BB0600")) +
  labs(title = "Random Forest Out of Bound Error Plot", 
       x = "Power Words", y = "OOB Error", color = "Tree Size")
