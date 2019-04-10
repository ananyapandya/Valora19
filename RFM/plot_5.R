p=ggplot(df_RFM, aes(x=df_RFM$frequenci, y=df_RFM$log_monitery, color=Recency_group)) + 
  geom_point(size=6, alpha=0.6)
ggplotly(p)
p %>% offline(height = 600)