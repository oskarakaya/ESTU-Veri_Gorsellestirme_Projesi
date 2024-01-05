# ESTU-Veri_Gorsellestirme_Projesi
Eskişehir Teknik Üniversitesi İstatistik Bölümü Veri Görselleştirme Dersi Final Projesi

Veri seti anket sonuçlarına dayalı olup, 2023 Güz Akademik döneminde Eskişehir Teknik Üniversitesi öğrencileri tarafından cevaplanmıştır. 
10 kategorik(yaş grubu, biyolojik cinsiyet, haftada düzenli uyunan gün sayısı aralığı, günlük uyuma süresi aralığı, uyumadan önce teknolojik cihaz kullanımı, uyumadan önce yeme/içme alışkanlığı, uykusuz hissedildiğinde baş aşrısı dikkat dağınıklığı olup olmadığı, Uyku Kalitesi), 3 nümerik değişken(not ortalaması, stres seviyesi, anksiyete skoru); 52 veri içermektedir.

![Rplot01](https://github.com/oskarakaya/ESTU-Veri_Gorsellestirme_Projesi/assets/118755772/f468f5ba-ea51-49e4-9cfc-44f9ce5c2570)


df2 <- data.frame(
  Biyolojik_cinsiyet = data$`Biyolojik cinsiyetiniz`,
  Not_ortalama = as.numeric(data$`Not Ortalamanız (En son bitirdiğiniz dönemin birikimli not ortalaması)`),
  Uyku_kalitesi = factor(data$`Genel olarak, uyku kalitenizi aşağıdaki seviyelerden hangisi en iyi tanımlar?`, levels = c("Kötü", "Orta", "İyi")))
  
colnames(df2)[1] <- "Cinsiyet"
colnames(df2)[2] <- "Not"
colnames(df2)[3] <- "Uyku"

ggplot(df2, aes(x = Uyku, y = Not)) +
  labs(x = "", y = "", title = "Öğrencilerin Uyku Kalitelerine Göre Not Ortalaması Dağılımları") + 
  geom_violin(color = "skyblue4") + 
  geom_point(color = "chocolate1") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 4), breaks = seq(0, 4, by = 1)) +
  theme_bw() 
  
![Rplot](https://github.com/oskarakaya/ESTU-Veri_Gorsellestirme_Projesi/assets/118755772/b3ca60ca-26ed-4ef9-ac03-5c490b10e6f2)

df1 <- data.frame(data$`Biyolojik cinsiyetiniz`, 
                  as.numeric(data$`Not Ortalamanız (En son bitirdiğiniz dönemin birikimli not ortalaması)`), 
                  data$`Günlük ortalama uyku süreniz nedir?`)
                  
colnames(df1)[1] <- "Cinsiyet"
colnames(df1)[2] <- "Not"
colnames(df1)[3] <- "Uyku"

ggplot(df1, aes(x = Uyku, y = Not)) +
  labs(x = "", y = "", title = "Öğrencilerin Uyku Sürelerine Göre Not Ortalaması Dağılımları") +
  geom_violin(color = "skyblue4") + 
  geom_point(color = "chocolate1") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 4), breaks = seq(0, 4, by = 1)) +
  theme_bw()

![Rplot03](https://github.com/oskarakaya/ESTU-Veri_Gorsellestirme_Projesi/assets/118755772/b0725b2c-653a-417e-b4ce-0624662ddae0)


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
  
![Rplot02](https://github.com/oskarakaya/ESTU-Veri_Gorsellestirme_Projesi/assets/118755772/e64b1b33-495f-4adc-bf85-7b4eb5e8874b)

df1_summary <- df1 %>%
  group_by(Uyku, Cinsiyet) %>%
  summarise(ortalama_not = mean(Not))

ggplot(df1_summary, aes(x = Uyku, y = ortalama_not, fill = Cinsiyet)) +
  labs(x = "", y = "", title = "Öğrencilerin Uyku Sürelerine Göre Not Ortalaması Dağılımları") +
  scale_fill_manual(values = c("Kadın" = "chocolate1", "Erkek" = "skyblue4"))+
  geom_bar(position = "dodge2", stat = "identity") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 4), breaks = seq(0, 4, by = 1)) +
  theme_bw()

  


