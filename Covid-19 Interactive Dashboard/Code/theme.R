grim <- theme_minimal() + theme(
  panel.background = element_rect(fill="white"),
  plot.background = element_rect(fill="white"),
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.text = element_text(color = "black"),
  plot.title = element_text(color = "black", face = "bold"),
  axis.title = element_text(color = "black"),
  axis.text = element_text(color = "black")
)

starbucks <- grim + theme(
  panel.background = element_rect(fill="#006341", color="black"),
  plot.background = element_rect(fill="#006341", color="black")
)