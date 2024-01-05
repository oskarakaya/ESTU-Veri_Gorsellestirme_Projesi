install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")

df1 <- data.frame(data$`Biyolojik cinsiyetiniz`, 
                  as.numeric(data$`Not Ortalamanız (En son bitirdiğiniz dönemin birikimli not ortalaması)`), 
                  data$`Günlük ortalama uyku süreniz nedir?`)
colnames(df1)[1] <- "Cinsiyet"
colnames(df1)[2] <- "Not"
colnames(df1)[3] <- "Uyku"


df2 <- data.frame(
  Biyolojik_cinsiyet = data$`Biyolojik cinsiyetiniz`,
  Not_ortalama = as.numeric(data$`Not Ortalamanız (En son bitirdiğiniz dönemin birikimli not ortalaması)`),
  Uyku_kalitesi = factor(data$`Genel olarak, uyku kalitenizi aşağıdaki seviyelerden hangisi en iyi tanımlar?`, levels = c("Kötü", "Orta", "İyi")))
colnames(df2)[1] <- "Cinsiyet"
colnames(df2)[2] <- "Not"
colnames(df2)[3] <- "Uyku"


ggplot(df1, aes(x = Uyku, y = Not)) +
  labs(x = "", y = "", title = "Öğrencilerin Uyku Sürelerine Göre Not Ortalaması Dağılımları") +
  geom_violin(color = "skyblue4") + 
  geom_point(color = "chocolate1") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 4), breaks = seq(0, 4, by = 1)) +
  theme_bw()

ggplot(df2, aes(x = Uyku, y = Not)) +
  labs(x = "", y = "", title = "Öğrencilerin Uyku Kalitelerine Göre Not Ortalaması Dağılımları") + 
  geom_violin(color = "skyblue4") + 
  geom_point(color = "chocolate1") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 4), breaks = seq(0, 4, by = 1)) +
  theme_bw() 

df1_summary <- df1 %>%
  group_by(Uyku, Cinsiyet) %>%
  summarise(ortalama_not = mean(Not))

ggplot(df1_summary, aes(x = Uyku, y = ortalama_not, fill = Cinsiyet)) +
  labs(x = "", y = "", title = "Öğrencilerin Uyku Sürelerine Göre Not Ortalaması Dağılımları") +
  scale_fill_manual(values = c("Kadın" = "chocolate1", "Erkek" = "skyblue4"))+
  geom_bar(position = "dodge2", stat = "identity") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 4), breaks = seq(0, 4, by = 1)) +
  theme_bw()

df2_summary <- df2 %>%
  group_by(Uyku, Cinsiyet) %>%
  summarise(ortalama_not = mean(Not)) %>%
  ungroup() %>%
  mutate(Uyku = factor(Uyku, levels = c("Kötü", "Orta", "İyi")))
         

ggplot(df2_summary, aes(x = Uyku, y = ortalama_not, fill = Cinsiyet)) +
  labs(x = "", y = "", title = "Öğrencilerin Uyku Kalitelerine Göre Not Ortalaması Dağılımları") +
  scale_fill_manual(values = c("Kadın" = "chocolate1", "Erkek" = "skyblue4"))+
  geom_bar(position = "dodge2", stat = "identity") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 4), breaks = seq(0, 4, by = 1)) +
  theme_bw()

