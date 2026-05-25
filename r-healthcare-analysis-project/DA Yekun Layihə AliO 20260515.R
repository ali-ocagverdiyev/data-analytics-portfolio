library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(reshape2)
library(readr)
library(tidyr)

# DATALAR
pasientler = read_rds("~/Downloads/patients.rds")
muracietler = read_rds("~/Downloads/encounters (1).rds")
musahideler = read_rds("~/Downloads/observations (1).rds")

# DATA HAZIRLIĞI
pasientler_join = pasientler %>%
  mutate(
    yas = as.numeric(floor(interval(BIRTHDATE, as.Date("2017-05-24")) / years(1))),
    yas_qrupu = case_when(
      yas < 18 ~ "0-17",
      yas >= 18 & yas < 35 ~ "18-34",
      yas >= 35 & yas < 50 ~ "35-49",
      yas >= 50 & yas < 65 ~ "50-64",
      yas >= 65 ~ "65+",
      TRUE ~ "Bilinmir"
    )
  ) %>%
  select(ID, BIRTHDATE, DEATHDATE, GENDER, MARITAL, RACE, ETHNICITY,
         BIRTHPLACE, yas, yas_qrupu)

muracietler_join = muracietler %>%
  select(
    ID,
    muraciet_tarixi = DATE,
    muraciet_kodu = CODE,
    muraciet_novu = DESCRIPTION,
    sebeb_kodu = REASONCODE,
    sebeb_aciqlamasi = REASONDESCRIPTION
  )

umumi_data = musahideler %>%
  left_join(muracietler_join, by = c("ENCOUNTER" = "ID")) %>%
  left_join(pasientler_join, by = c("PATIENT" = "ID")) %>%
  mutate(VALUE_NUM = as.numeric(VALUE))

esas_gostericiler = c(
  "Body Mass Index",
  "Systolic Blood Pressure",
  "Diastolic Blood Pressure",
  "Glucose",
  "Hemoglobin A1c/Hemoglobin.total in Blood",
  "Total Cholesterol",
  "Triglycerides"
)

# PROQNOZ DATASI
proqnoz_gostericiler = c(
  "Body Mass Index",
  "Systolic Blood Pressure",
  "Glucose",
  "Hemoglobin A1c/Hemoglobin.total in Blood"
)

proqnoz_data = umumi_data %>%
  filter(DESCRIPTION %in% proqnoz_gostericiler) %>%
  filter(!is.na(VALUE_NUM)) %>%
  select(PATIENT, DESCRIPTION, VALUE_NUM) %>%
  group_by(PATIENT, DESCRIPTION) %>%
  summarise(orta_deyer = mean(VALUE_NUM, na.rm = TRUE), .groups = "drop")

proqnoz_data = dcast(
  proqnoz_data,
  PATIENT ~ DESCRIPTION,
  value.var = "orta_deyer"
)

proqnoz_data = proqnoz_data %>%
  left_join(
    pasientler_join %>% select(ID, yas, yas_qrupu),
    by = c("PATIENT" = "ID")
  ) %>%
  mutate(
    risk_siqnali_sayi = case_when(
      !is.na(`Body Mass Index`) &
        !is.na(`Systolic Blood Pressure`) &
        !is.na(Glucose) ~
        as.numeric(`Body Mass Index` >= 30) +
        as.numeric(`Systolic Blood Pressure` >= 140) +
        as.numeric(Glucose >= 126),
      TRUE ~ NA_real_
    )
  )

# UI
ui = fluidPage(
  
  titlePanel("DA307 - Synthea Hospital Data Analizi"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId = "bolme",
        label = "Bölmə seçin:",
        choices = c(
          "Mərkəzi tendensiya və statistik xülasə",
          "Deskriptiv təhlil",
          "Anomaliyalar",
          "Proqnoz və Fərziyyələr",
          "Xülasə"
        ),
        selected = "Mərkəzi tendensiya və statistik xülasə"
      ),
      
      selectInput(
        inputId = "analiz",
        label = "Analiz seçin:",
        choices = c(
          "Əsas klinik göstəricilər üzrə statistik xülasə",
          "Pasiyent başına müşahidə sayı",
          "Klinik göstəricilər üzrə vahidlər"
        )
      )
    ),
    
    mainPanel(
      h4(textOutput("basliq")),
      p(textOutput("serh")),
      br(),
      tableOutput("cedvel"),
      br(),
      plotOutput("qrafik", height = "450px")
    )
  )
)

# SERVER
server = function(input, output, session) {
  
  observeEvent(input$bolme, {
    
    if(input$bolme == "Mərkəzi tendensiya və statistik xülasə") {
      
      updateSelectInput(
        session,
        "analiz",
        choices = c(
          "Əsas klinik göstəricilər üzrə statistik xülasə",
          "Pasiyent başına müşahidə sayı",
          "Klinik göstəricilər üzrə vahidlər"
        )
      )
      
    } else if(input$bolme == "Deskriptiv təhlil") {
      
      updateSelectInput(
        session,
        "analiz",
        choices = c(
          "Cins üzrə pasiyent paylanması",
          "Yaş qrupları üzrə pasiyent paylanması",
          "Müraciət növünə görə paylanma",
          "İzlənmə qrupları üzrə pasiyent paylanması",
          "BMI qrupuna görə orta sistolik təzyiq",
          "Glucose qrupuna görə orta A1c"
        )
      )
      
    } else if(input$bolme == "Anomaliyalar") {
      
      updateSelectInput(
        session,
        "analiz",
        choices = c(
          "Müşahidə və müraciət tarixləri arasında böyük fərq",
          "Ölüm kağızı və ölüm tarixi uyğunsuzluğu",
          "Vəfatdan sonra klinik və müraciət qeydləri",
          "A1c mənfi dəyərlər",
          "Müraciət səbəbi boşluğu"
        )
      )
      
    } else if(input$bolme == "Proqnoz və Fərziyyələr") {
      
      updateSelectInput(
        session,
        "analiz",
        choices = c(
          "Risk datasının xülasəsi",
          "Glucose qrupları üzrə yüksək A1c faizi",
          "Yaş qrupları üzrə risk siqnalı",
          "Risk siqnalı sayına görə pasiyent paylanması",
          "Risk siqnalı olmayan pasiyentlərdə sərhədə yaxın göstəricilər"
        )
      )
      
    } else if(input$bolme == "Xülasə") {
      
      updateSelectInput(
        session,
        "analiz",
        choices = c("Layihənin ümumi xülasəsi")
      )
    }
  })
  
  output$basliq = renderText({
    input$analiz
  })
  
  output$serh = renderText({
    
    if(input$analiz == "Əsas klinik göstəricilər üzrə statistik xülasə") {
      "Bu hissədə əsas klinik göstəricilər üzrə ölçmə sayı, minimum, orta, median və maksimum dəyərlər göstərilir."
      
    } else if(input$analiz == "Pasiyent başına müşahidə sayı") {
      "Bu hissə hər pasiyent üzrə neçə klinik müşahidə olduğunu göstərir. Məqsəd pasiyentlərin izlənmə səviyyəsini müqayisə etməkdir."
      
    } else if(input$analiz == "Klinik göstəricilər üzrə vahidlər") {
      "Bu hissə klinik göstəricilərin hansı ölçü vahidləri ilə qeyd edildiyini göstərir."
      
    } else if(input$analiz == "Cins üzrə pasiyent paylanması") {
      "Bu analiz datasetdə kişi və qadın pasiyentlərin paylanmasını göstərir."
      
    } else if(input$analiz == "Yaş qrupları üzrə pasiyent paylanması") {
      "Bu analiz pasiyentlərin yaş qrupları üzrə necə bölündüyünü göstərir."
      
    } else if(input$analiz == "Müraciət növünə görə paylanma") {
      "Bu analiz ən çox qeydə alınan müraciət növlərini göstərir."
      
    } else if(input$analiz == "İzlənmə qrupları üzrə pasiyent paylanması") {
      "Bu analiz pasiyentləri müşahidə sayına görə az, orta və çox izlənən qruplara ayırır."
      
    } else if(input$analiz == "BMI qrupuna görə orta sistolik təzyiq") {
      "Bu analiz BMI qrupu artdıqca orta sistolik qan təzyiqinin necə dəyişdiyini göstərir."
      
    } else if(input$analiz == "Glucose qrupuna görə orta A1c") {
      "Bu analiz glucose qrupları üzrə orta A1c dəyərini göstərir."
      
    } else if(input$analiz == "Müşahidə və müraciət tarixləri arasında böyük fərq") {
      "Bu analiz klinik müşahidə tarixi ilə müraciət tarixi arasında 1 ildən çox fərq olan qeydləri göstərir."
      
    } else if(input$analiz == "Ölüm kağızı və ölüm tarixi uyğunsuzluğu") {
      "Bu analiz ölüm kağızı qeydləri ilə pasiyent datasındakı ölüm tarixi arasında uyğunsuzluğu göstərir."
      
    } else if(input$analiz == "Vəfatdan sonra klinik və müraciət qeydləri") {
      "Bu analiz pasiyentin vəfat tarixindən sonra klinik ölçmə və real müraciət qeydinin olub-olmadığını göstərir."
      
    } else if(input$analiz == "A1c mənfi dəyərlər") {
      "Bu analiz A1c göstəricisində mənfi dəyərləri göstərir. A1c faiz göstəricisi olduğu üçün mənfi dəyər real klinik nəticə deyil."
      
    } else if(input$analiz == "Müraciət səbəbi boşluğu") {
      "Bu analiz müraciət səbəbi məlumatının nə qədər boş olduğunu göstərir."
      
    } else if(input$analiz == "Risk datasının xülasəsi") {
      "Bu hissədə risk analizi üçün istifadə olunan pasiyent səviyyəsində data göstərilir. Risk yalnız BMI, sistolik təzyiq və glucose birlikdə olan pasiyentlər üzrə hesablanıb."
      
    } else if(input$analiz == "Glucose qrupları üzrə yüksək A1c faizi") {
      "Fərziyyə: glucose səviyyəsi yüksəldikcə yüksək A1c faizi də arta bilər."
      
    } else if(input$analiz == "Yaş qrupları üzrə risk siqnalı") {
      "Fərziyyə: yaş qrupu dəyişdikcə risk siqnalı olan pasiyentlərin faizi də dəyişə bilər."
      
    } else if(input$analiz == "Risk siqnalı sayına görə pasiyent paylanması") {
      "Bu analiz risk hesablanan pasiyentlərin neçə risk siqnalına sahib olduğunu göstərir."
      
    } else if(input$analiz == "Risk siqnalı olmayan pasiyentlərdə sərhədə yaxın göstəricilər") {
      "Bu analiz risk siqnalı olmayan pasiyentlərin risk sərhədinə nə qədər yaxın olduğunu göstərir."
      
    } else {
      "Bu hissə layihənin ümumi nəticəsini göstərir. Nəticələr diaqnoz deyil, analitik müşahidə kimi qiymətləndirilməlidir."
    }
  })
  
  output$cedvel = renderTable({
    
    if(input$analiz == "Əsas klinik göstəricilər üzrə statistik xülasə") {
      
      umumi_data %>%
        filter(DESCRIPTION %in% esas_gostericiler) %>%
        filter(!is.na(VALUE_NUM)) %>%
        group_by(DESCRIPTION, UNITS) %>%
        summarise(
          olcme_sayi = n(),
          minimum_deyer = round(min(VALUE_NUM, na.rm = TRUE), 2),
          orta_deyer = round(mean(VALUE_NUM, na.rm = TRUE), 2),
          median_deyer = round(median(VALUE_NUM, na.rm = TRUE), 2),
          maksimum_deyer = round(max(VALUE_NUM, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        arrange(desc(olcme_sayi))
      
    } else if(input$analiz == "Pasiyent başına müşahidə sayı") {
      
      musahideler %>%
        count(PATIENT, name = "mushahide_sayi") %>%
        summarise(
          minimum = min(mushahide_sayi),
          q1 = as.numeric(quantile(mushahide_sayi, 0.25)),
          median = median(mushahide_sayi),
          orta = round(mean(mushahide_sayi), 1),
          q3 = as.numeric(quantile(mushahide_sayi, 0.75)),
          maksimum = max(mushahide_sayi)
        )
      
    } else if(input$analiz == "Klinik göstəricilər üzrə vahidlər") {
      
      umumi_data %>%
        filter(DESCRIPTION %in% esas_gostericiler) %>%
        filter(!is.na(UNITS)) %>%
        count(UNITS, name = "olcme_sayi") %>%
        arrange(desc(olcme_sayi))
      
    } else if(input$analiz == "Cins üzrə pasiyent paylanması") {
      
      pasientler %>%
        filter(!is.na(GENDER)) %>%
        count(GENDER, name = "pasiyent_sayi") %>%
        mutate(faiz = round(pasiyent_sayi / sum(pasiyent_sayi) * 100, 1))
      
    } else if(input$analiz == "Yaş qrupları üzrə pasiyent paylanması") {
      
      pasientler_join %>%
        filter(!is.na(yas_qrupu)) %>%
        count(yas_qrupu, name = "pasiyent_sayi") %>%
        mutate(faiz = round(pasiyent_sayi / sum(pasiyent_sayi) * 100, 1))
      
    } else if(input$analiz == "Müraciət növünə görə paylanma") {
      
      muracietler %>%
        filter(!is.na(DESCRIPTION)) %>%
        count(DESCRIPTION, name = "muraciet_sayi") %>%
        mutate(faiz = round(muraciet_sayi / sum(muraciet_sayi) * 100, 1)) %>%
        arrange(desc(muraciet_sayi)) %>%
        head(10)
      
    } else if(input$analiz == "İzlənmə qrupları üzrə pasiyent paylanması") {
      
      musahideler %>%
        count(PATIENT, name = "mushahide_sayi") %>%
        mutate(
          izlenme_qrupu = case_when(
            mushahide_sayi <= 20 ~ "Az izlənən",
            mushahide_sayi > 20 & mushahide_sayi <= 55 ~ "Orta izlənən",
            mushahide_sayi > 55 ~ "Çox izlənən",
            TRUE ~ "Bilinmir"
          )
        ) %>%
        count(izlenme_qrupu, name = "pasiyent_sayi") %>%
        mutate(faiz = round(pasiyent_sayi / sum(pasiyent_sayi) * 100, 1))
      
    } else if(input$analiz == "BMI qrupuna görə orta sistolik təzyiq") {
      
      bmi_sistolik_data = umumi_data %>%
        filter(DESCRIPTION %in% c("Body Mass Index", "Systolic Blood Pressure")) %>%
        filter(!is.na(VALUE_NUM)) %>%
        select(PATIENT, DESCRIPTION, VALUE_NUM) %>%
        group_by(PATIENT, DESCRIPTION) %>%
        summarise(orta_deyer = mean(VALUE_NUM, na.rm = TRUE), .groups = "drop")
      
      bmi_sistolik_data = dcast(
        bmi_sistolik_data,
        PATIENT ~ DESCRIPTION,
        value.var = "orta_deyer"
      )
      
      bmi_sistolik_data %>%
        filter(!is.na(`Body Mass Index`)) %>%
        filter(!is.na(`Systolic Blood Pressure`)) %>%
        mutate(
          bmi_qrupu = case_when(
            `Body Mass Index` < 18.5 ~ "Aşağı BMI",
            `Body Mass Index` >= 18.5 & `Body Mass Index` < 25 ~ "Normal BMI",
            `Body Mass Index` >= 25 & `Body Mass Index` < 30 ~ "Artıq çəki",
            `Body Mass Index` >= 30 & `Body Mass Index` < 40 ~ "Piylənmə",
            `Body Mass Index` >= 40 ~ "Yüksək piylənmə",
            TRUE ~ "Bilinmir"
          )
        ) %>%
        group_by(bmi_qrupu) %>%
        summarise(
          pasiyent_sayi = n(),
          orta_sistolik_tezyiq = round(mean(`Systolic Blood Pressure`, na.rm = TRUE), 1),
          .groups = "drop"
        )
      
    } else if(input$analiz == "Glucose qrupuna görə orta A1c") {
      
      glucose_a1c_data = umumi_data %>%
        filter(DESCRIPTION %in% c("Glucose", "Hemoglobin A1c/Hemoglobin.total in Blood")) %>%
        filter(!is.na(VALUE_NUM)) %>%
        select(PATIENT, DESCRIPTION, VALUE_NUM) %>%
        group_by(PATIENT, DESCRIPTION) %>%
        summarise(orta_deyer = mean(VALUE_NUM, na.rm = TRUE), .groups = "drop")
      
      glucose_a1c_data = dcast(
        glucose_a1c_data,
        PATIENT ~ DESCRIPTION,
        value.var = "orta_deyer"
      )
      
      glucose_a1c_data %>%
        filter(!is.na(Glucose)) %>%
        filter(!is.na(`Hemoglobin A1c/Hemoglobin.total in Blood`)) %>%
        mutate(
          glucose_qrupu = case_when(
            Glucose < 70 ~ "Aşağı glucose",
            Glucose >= 70 & Glucose < 100 ~ "Normal glucose",
            Glucose >= 100 & Glucose < 126 ~ "Yüksək glucose",
            Glucose >= 126 ~ "Çox yüksək glucose",
            TRUE ~ "Bilinmir"
          )
        ) %>%
        group_by(glucose_qrupu) %>%
        summarise(
          pasiyent_sayi = n(),
          orta_a1c = round(mean(`Hemoglobin A1c/Hemoglobin.total in Blood`, na.rm = TRUE), 2),
          median_a1c = round(median(`Hemoglobin A1c/Hemoglobin.total in Blood`, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
    } else if(input$analiz == "Müşahidə və müraciət tarixləri arasında böyük fərq") {
      
      umumi_data %>%
        filter(!is.na(DATE)) %>%
        filter(!is.na(muraciet_tarixi)) %>%
        mutate(tarix_ferqi = as.numeric(DATE - muraciet_tarixi)) %>%
        filter(abs(tarix_ferqi) > 365) %>%
        summarise(
          boyuk_ferq_qeyd_sayi = n(),
          boyuk_ferq_pasiyent_sayi = n_distinct(PATIENT),
          boyuk_ferq_muraciet_sayi = n_distinct(ENCOUNTER),
          minimum_tarix_ferqi = min(tarix_ferqi, na.rm = TRUE),
          maksimum_tarix_ferqi = max(tarix_ferqi, na.rm = TRUE)
        )
      
    } else if(input$analiz == "Ölüm kağızı və ölüm tarixi uyğunsuzluğu") {
      
      death_cert_data = muracietler %>%
        filter(DESCRIPTION == "Death Certification") %>%
        left_join(
          pasientler %>% select(ID, DEATHDATE),
          by = c("PATIENT" = "ID")
        )
      
      death_cert_data %>%
        summarise(
          death_cert_qeyd_sayi = n(),
          death_cert_pasiyent_sayi = n_distinct(PATIENT),
          deathdate_bos_qeyd_sayi = sum(is.na(DEATHDATE)),
          deathdate_bos_pasiyent_sayi = n_distinct(PATIENT[is.na(DEATHDATE)]),
          olumden_evvel_death_cert_qeyd_sayi = sum(!is.na(DEATHDATE) & DATE < DEATHDATE),
          olumden_evvel_death_cert_pasiyent_sayi = n_distinct(PATIENT[!is.na(DEATHDATE) & DATE < DEATHDATE])
        )
      
    } else if(input$analiz == "Vəfatdan sonra klinik və müraciət qeydləri") {
      
      vefatdan_sonra_klinik = umumi_data %>%
        filter(!is.na(DEATHDATE)) %>%
        filter(!is.na(DATE)) %>%
        filter(DATE > DEATHDATE)
      
      vefatdan_sonra_muraciet = muracietler %>%
        left_join(
          pasientler %>% select(ID, DEATHDATE),
          by = c("PATIENT" = "ID")
        ) %>%
        filter(!is.na(DEATHDATE)) %>%
        filter(!is.na(DATE)) %>%
        filter(DATE > DEATHDATE) %>%
        filter(DESCRIPTION != "Death Certification")
      
      data.frame(
        yoxlama = c("Vəfatdan sonra klinik ölçmə", "Vəfatdan sonra real müraciət"),
        qeyd_sayi = c(nrow(vefatdan_sonra_klinik), nrow(vefatdan_sonra_muraciet)),
        pasiyent_sayi = c(
          n_distinct(vefatdan_sonra_klinik$PATIENT),
          n_distinct(vefatdan_sonra_muraciet$PATIENT)
        )
      )
      
    } else if(input$analiz == "A1c mənfi dəyərlər") {
      
      umumi_data %>%
        filter(DESCRIPTION == "Hemoglobin A1c/Hemoglobin.total in Blood") %>%
        filter(!is.na(VALUE_NUM)) %>%
        filter(VALUE_NUM < 0) %>%
        count(VALUE_NUM, name = "qeyd_sayi") %>%
        arrange(VALUE_NUM)
      
    } else if(input$analiz == "Müraciət səbəbi boşluğu") {
      
      muracietler %>%
        summarise(
          toplam_muraciet = n(),
          reason_na = sum(is.na(REASONDESCRIPTION)),
          reason_bos_string = sum(REASONDESCRIPTION == "", na.rm = TRUE),
          reason_umumi_bos = reason_na + reason_bos_string,
          reason_dolu = toplam_muraciet - reason_umumi_bos,
          reason_bos_faiz = round(reason_umumi_bos / toplam_muraciet * 100, 1),
          reason_dolu_faiz = round(reason_dolu / toplam_muraciet * 100, 1)
        )
      
    } else if(input$analiz == "Risk datasının xülasəsi") {
      
      proqnoz_data %>%
        summarise(
          pasiyent_sayi = n(),
          bmi_melumatli = sum(!is.na(`Body Mass Index`)),
          sistolik_melumatli = sum(!is.na(`Systolic Blood Pressure`)),
          glucose_melumatli = sum(!is.na(Glucose)),
          a1c_melumatli = sum(!is.na(`Hemoglobin A1c/Hemoglobin.total in Blood`)),
          risk_hesablanan_pasiyent = sum(!is.na(risk_siqnali_sayi))
        )
      
    } else if(input$analiz == "Glucose qrupları üzrə yüksək A1c faizi") {
      
      proqnoz_data %>%
        filter(!is.na(Glucose)) %>%
        filter(!is.na(`Hemoglobin A1c/Hemoglobin.total in Blood`)) %>%
        mutate(
          glucose_qrupu = case_when(
            Glucose < 70 ~ "Aşağı glucose",
            Glucose >= 70 & Glucose < 100 ~ "Normal glucose",
            Glucose >= 100 & Glucose < 126 ~ "Yüksək glucose",
            Glucose >= 126 ~ "Çox yüksək glucose",
            TRUE ~ "Bilinmir"
          ),
          glucose_qrupu = factor(
            glucose_qrupu,
            levels = c("Aşağı glucose", "Normal glucose", "Yüksək glucose", "Çox yüksək glucose")
          ),
          yuksek_a1c = case_when(
            `Hemoglobin A1c/Hemoglobin.total in Blood` >= 6.5 ~ 1,
            `Hemoglobin A1c/Hemoglobin.total in Blood` < 6.5 ~ 0,
            TRUE ~ NA_real_
          )
        ) %>%
        group_by(glucose_qrupu) %>%
        summarise(
          pasiyent_sayi = n(),
          yuksek_a1c_pasiyent_sayi = sum(yuksek_a1c == 1, na.rm = TRUE),
          yuksek_a1c_faizi = round(yuksek_a1c_pasiyent_sayi / pasiyent_sayi * 100, 1),
          orta_a1c = round(mean(`Hemoglobin A1c/Hemoglobin.total in Blood`, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
    } else if(input$analiz == "Yaş qrupları üzrə risk siqnalı") {
      
      proqnoz_data %>%
        filter(!is.na(yas_qrupu)) %>%
        filter(!is.na(risk_siqnali_sayi)) %>%
        filter(yas_qrupu != "0-17") %>%
        mutate(
          yas_qrupu = factor(yas_qrupu, levels = c("18-34", "35-49", "50-64", "65+")),
          riskli_pasiyent = case_when(
            risk_siqnali_sayi >= 1 ~ 1,
            risk_siqnali_sayi == 0 ~ 0,
            TRUE ~ NA_real_
          )
        ) %>%
        group_by(yas_qrupu) %>%
        summarise(
          pasiyent_sayi = n(),
          riskli_pasiyent_sayi = sum(riskli_pasiyent == 1, na.rm = TRUE),
          risk_faizi = round(riskli_pasiyent_sayi / pasiyent_sayi * 100, 1),
          .groups = "drop"
        )
      
    } else if(input$analiz == "Risk siqnalı sayına görə pasiyent paylanması") {
      
      proqnoz_data %>%
        filter(!is.na(risk_siqnali_sayi)) %>%
        mutate(
          risk_qrupu = case_when(
            risk_siqnali_sayi == 0 ~ "Risk siqnalı yoxdur",
            risk_siqnali_sayi == 1 ~ "1 risk siqnalı",
            risk_siqnali_sayi == 2 ~ "2 risk siqnalı",
            risk_siqnali_sayi == 3 ~ "3 risk siqnalı",
            TRUE ~ "Digər"
          ),
          risk_qrupu = factor(
            risk_qrupu,
            levels = c("Risk siqnalı yoxdur", "1 risk siqnalı", "2 risk siqnalı", "3 risk siqnalı")
          )
        ) %>%
        count(risk_qrupu, name = "pasiyent_sayi") %>%
        mutate(faiz = round(pasiyent_sayi / sum(pasiyent_sayi) * 100, 1))
      
    } else if(input$analiz == "Risk siqnalı olmayan pasiyentlərdə sərhədə yaxın göstəricilər") {
      
      risksiz_data = proqnoz_data %>%
        filter(!is.na(risk_siqnali_sayi)) %>%
        filter(risk_siqnali_sayi == 0)
      
      data.frame(
        serhed_gosterici = c(
          "BMI sərhədə yaxın",
          "Sistolik təzyiq sərhədə yaxın",
          "Glucose sərhədə yaxın"
        ),
        pasiyent_sayi = c(
          sum(risksiz_data$`Body Mass Index` >= 25 & risksiz_data$`Body Mass Index` < 30, na.rm = TRUE),
          sum(risksiz_data$`Systolic Blood Pressure` >= 130 & risksiz_data$`Systolic Blood Pressure` < 140, na.rm = TRUE),
          sum(risksiz_data$Glucose >= 100 & risksiz_data$Glucose < 126, na.rm = TRUE)
        ),
        umumi_risksiz_pasiyent = nrow(risksiz_data)
      ) %>%
        mutate(faiz = round(pasiyent_sayi / umumi_risksiz_pasiyent * 100, 1))
      
    } else {
      
      data.frame(
        bolme = c(
          "Data strukturu",
          "Deskriptiv nəticə",
          "Anomaliyalar",
          "Risk siqnalı",
          "Məhdudiyyət"
        ),
        esas_netice = c(
          "3 data birləşdirildi və əsas analiz müşahidələr üzərində quruldu.",
          "BMI və sistolik təzyiq yaş qrupları üzrə dəyişir.",
          "Tarix uyğunsuzluğu, mənfi A1c və boş səbəb məlumatı əsas data keyfiyyəti problemləridir.",
          "Risk siqnalı BMI, sistolik təzyiq və glucose göstəricilərinə əsaslanan analitik göstəricidir.",
          "Nəticələr klinik diaqnoz deyil, Synthea datası üzrə analitik müşahidədir."
        )
      )
    }
  })
  
  output$qrafik = renderPlot({
    
    if(input$analiz == "Əsas klinik göstəricilər üzrə statistik xülasə") {
      
      qrafik_data = umumi_data %>%
        filter(DESCRIPTION %in% esas_gostericiler) %>%
        filter(!is.na(VALUE_NUM)) %>%
        count(DESCRIPTION, name = "olcme_sayi")
      
      ggplot(qrafik_data, aes(x = reorder(DESCRIPTION, olcme_sayi),
                              y = olcme_sayi,
                              fill = DESCRIPTION)) +
        geom_col() +
        geom_text(aes(label = olcme_sayi), hjust = -0.1, size = 3) +
        coord_flip() +
        labs(title = "Əsas klinik göstəricilər üzrə ölçmə sayı",
             x = "Klinik göstərici",
             y = "Ölçmə sayı") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "Pasiyent başına müşahidə sayı" |
              input$analiz == "İzlənmə qrupları üzrə pasiyent paylanması") {
      
      qrafik_data = musahideler %>%
        count(PATIENT, name = "mushahide_sayi") %>%
        mutate(
          izlenme_qrupu = case_when(
            mushahide_sayi <= 20 ~ "Az izlənən",
            mushahide_sayi > 20 & mushahide_sayi <= 55 ~ "Orta izlənən",
            mushahide_sayi > 55 ~ "Çox izlənən",
            TRUE ~ "Bilinmir"
          ),
          izlenme_qrupu = factor(izlenme_qrupu, levels = c("Az izlənən", "Orta izlənən", "Çox izlənən"))
        ) %>%
        count(izlenme_qrupu, name = "pasiyent_sayi")
      
      ggplot(qrafik_data, aes(x = izlenme_qrupu,
                              y = pasiyent_sayi,
                              fill = izlenme_qrupu)) +
        geom_col() +
        geom_text(aes(label = pasiyent_sayi), vjust = -0.3, size = 4) +
        labs(title = "İzlənmə qrupları üzrə pasiyent paylanması",
             x = "İzlənmə qrupu",
             y = "Pasiyent sayı") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "Klinik göstəricilər üzrə vahidlər") {
      
      qrafik_data = umumi_data %>%
        filter(DESCRIPTION %in% esas_gostericiler) %>%
        filter(!is.na(UNITS)) %>%
        count(UNITS, name = "olcme_sayi")
      
      ggplot(qrafik_data, aes(x = reorder(UNITS, olcme_sayi),
                              y = olcme_sayi,
                              fill = UNITS)) +
        geom_col() +
        geom_text(aes(label = olcme_sayi), hjust = -0.1, size = 4) +
        coord_flip() +
        labs(title = "Klinik göstəricilər üzrə vahidlər",
             x = "Vahid",
             y = "Ölçmə sayı") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "Cins üzrə pasiyent paylanması") {
      
      qrafik_data = pasientler %>%
        filter(!is.na(GENDER)) %>%
        count(GENDER, name = "pasiyent_sayi") %>%
        mutate(faiz = round(pasiyent_sayi / sum(pasiyent_sayi) * 100, 1))
      
      ggplot(qrafik_data, aes(x = GENDER,
                              y = pasiyent_sayi,
                              fill = GENDER)) +
        geom_col() +
        geom_text(aes(label = paste0(pasiyent_sayi, " (", faiz, "%)")),
                  vjust = -0.3, size = 4) +
        labs(title = "Cins üzrə pasiyent paylanması",
             x = "Cins",
             y = "Pasiyent sayı") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "Yaş qrupları üzrə pasiyent paylanması") {
      
      qrafik_data = pasientler_join %>%
        filter(!is.na(yas_qrupu)) %>%
        count(yas_qrupu, name = "pasiyent_sayi") %>%
        mutate(yas_qrupu = factor(yas_qrupu,
                                  levels = c("0-17", "18-34", "35-49", "50-64", "65+")))
      
      ggplot(qrafik_data, aes(x = yas_qrupu,
                              y = pasiyent_sayi,
                              fill = yas_qrupu)) +
        geom_col() +
        geom_text(aes(label = pasiyent_sayi), vjust = -0.3, size = 4) +
        labs(title = "Yaş qrupları üzrə pasiyent paylanması",
             x = "Yaş qrupu",
             y = "Pasiyent sayı") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "Müraciət növünə görə paylanma") {
      
      qrafik_data = muracietler %>%
        filter(!is.na(DESCRIPTION)) %>%
        count(DESCRIPTION, name = "muraciet_sayi") %>%
        arrange(desc(muraciet_sayi)) %>%
        head(10)
      
      ggplot(qrafik_data, aes(x = reorder(DESCRIPTION, muraciet_sayi),
                              y = muraciet_sayi,
                              fill = DESCRIPTION)) +
        geom_col() +
        geom_text(aes(label = muraciet_sayi), hjust = -0.1, size = 3) +
        coord_flip() +
        labs(title = "Ən çox görülən müraciət növləri",
             x = "Müraciət növü",
             y = "Müraciət sayı") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "BMI qrupuna görə orta sistolik təzyiq") {
      
      bmi_sistolik_data = umumi_data %>%
        filter(DESCRIPTION %in% c("Body Mass Index", "Systolic Blood Pressure")) %>%
        filter(!is.na(VALUE_NUM)) %>%
        select(PATIENT, DESCRIPTION, VALUE_NUM) %>%
        group_by(PATIENT, DESCRIPTION) %>%
        summarise(orta_deyer = mean(VALUE_NUM, na.rm = TRUE), .groups = "drop")
      
      bmi_sistolik_data = dcast(
        bmi_sistolik_data,
        PATIENT ~ DESCRIPTION,
        value.var = "orta_deyer"
      )
      
      qrafik_data = bmi_sistolik_data %>%
        filter(!is.na(`Body Mass Index`)) %>%
        filter(!is.na(`Systolic Blood Pressure`)) %>%
        mutate(
          bmi_qrupu = case_when(
            `Body Mass Index` < 18.5 ~ "Aşağı BMI",
            `Body Mass Index` >= 18.5 & `Body Mass Index` < 25 ~ "Normal BMI",
            `Body Mass Index` >= 25 & `Body Mass Index` < 30 ~ "Artıq çəki",
            `Body Mass Index` >= 30 & `Body Mass Index` < 40 ~ "Piylənmə",
            `Body Mass Index` >= 40 ~ "Yüksək piylənmə",
            TRUE ~ "Bilinmir"
          ),
          bmi_qrupu = factor(bmi_qrupu, levels = c("Aşağı BMI", "Normal BMI", "Artıq çəki", "Piylənmə", "Yüksək piylənmə"))
        ) %>%
        group_by(bmi_qrupu) %>%
        summarise(orta_sistolik_tezyiq = round(mean(`Systolic Blood Pressure`, na.rm = TRUE), 1),
                  .groups = "drop")
      
      ggplot(qrafik_data, aes(x = bmi_qrupu,
                              y = orta_sistolik_tezyiq,
                              fill = bmi_qrupu)) +
        geom_col() +
        geom_text(aes(label = orta_sistolik_tezyiq), vjust = -0.3, size = 4) +
        labs(title = "BMI qrupuna görə orta sistolik təzyiq",
             x = "BMI qrupu",
             y = "Orta sistolik təzyiq") +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 20, hjust = 1))
      
    } else if(input$analiz == "Glucose qrupuna görə orta A1c") {
      
      glucose_a1c_data = umumi_data %>%
        filter(DESCRIPTION %in% c("Glucose", "Hemoglobin A1c/Hemoglobin.total in Blood")) %>%
        filter(!is.na(VALUE_NUM)) %>%
        select(PATIENT, DESCRIPTION, VALUE_NUM) %>%
        group_by(PATIENT, DESCRIPTION) %>%
        summarise(orta_deyer = mean(VALUE_NUM, na.rm = TRUE), .groups = "drop")
      
      glucose_a1c_data = dcast(
        glucose_a1c_data,
        PATIENT ~ DESCRIPTION,
        value.var = "orta_deyer"
      )
      
      qrafik_data = glucose_a1c_data %>%
        filter(!is.na(Glucose)) %>%
        filter(!is.na(`Hemoglobin A1c/Hemoglobin.total in Blood`)) %>%
        mutate(
          glucose_qrupu = case_when(
            Glucose < 70 ~ "Aşağı glucose",
            Glucose >= 70 & Glucose < 100 ~ "Normal glucose",
            Glucose >= 100 & Glucose < 126 ~ "Yüksək glucose",
            Glucose >= 126 ~ "Çox yüksək glucose",
            TRUE ~ "Bilinmir"
          ),
          glucose_qrupu = factor(glucose_qrupu, levels = c("Aşağı glucose", "Normal glucose", "Yüksək glucose", "Çox yüksək glucose"))
        ) %>%
        group_by(glucose_qrupu) %>%
        summarise(orta_a1c = round(mean(`Hemoglobin A1c/Hemoglobin.total in Blood`, na.rm = TRUE), 2),
                  .groups = "drop")
      
      ggplot(qrafik_data, aes(x = glucose_qrupu,
                              y = orta_a1c,
                              fill = glucose_qrupu)) +
        geom_col() +
        geom_text(aes(label = orta_a1c), vjust = -0.3, size = 4) +
        labs(title = "Glucose qrupuna görə orta A1c",
             x = "Glucose qrupu",
             y = "Orta A1c") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "Müşahidə və müraciət tarixləri arasında böyük fərq") {
      
      qrafik_data = umumi_data %>%
        filter(!is.na(DATE)) %>%
        filter(!is.na(muraciet_tarixi)) %>%
        mutate(tarix_ferqi = as.numeric(DATE - muraciet_tarixi)) %>%
        filter(abs(tarix_ferqi) > 365) %>%
        mutate(
          ferq_il = abs(tarix_ferqi) / 365,
          ferq_qrupu = case_when(
            ferq_il >= 1 & ferq_il < 3 ~ "1-3 il fərq",
            ferq_il >= 3 & ferq_il < 5 ~ "3-5 il fərq",
            ferq_il >= 5 ~ "5 ildən çox fərq",
            TRUE ~ "Digər"
          ),
          ferq_qrupu = factor(ferq_qrupu, levels = c("1-3 il fərq", "3-5 il fərq", "5 ildən çox fərq"))
        ) %>%
        count(ferq_qrupu, name = "qeyd_sayi")
      
      ggplot(qrafik_data, aes(x = ferq_qrupu,
                              y = qeyd_sayi,
                              fill = ferq_qrupu)) +
        geom_col() +
        geom_text(aes(label = qeyd_sayi), vjust = -0.3, size = 4) +
        labs(title = "Müşahidə və müraciət tarixləri arasında böyük fərq",
             x = "Tarix fərqi",
             y = "Qeyd sayı") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "Ölüm kağızı və ölüm tarixi uyğunsuzluğu") {
      
      death_cert_data = muracietler %>%
        filter(DESCRIPTION == "Death Certification") %>%
        left_join(
          pasientler %>% select(ID, DEATHDATE),
          by = c("PATIENT" = "ID")
        )
      
      xulase = death_cert_data %>%
        summarise(
          deathdate_bos_qeyd_sayi = sum(is.na(DEATHDATE)),
          olumden_evvel_death_cert_qeyd_sayi = sum(!is.na(DEATHDATE) & DATE < DEATHDATE)
        )
      
      qrafik_data = data.frame(
        anomaliya = c("Ölüm tarixi boşdur", "Sertifikat ölüm tarixindən əvvəldir"),
        qeyd_sayi = c(xulase$deathdate_bos_qeyd_sayi,
                      xulase$olumden_evvel_death_cert_qeyd_sayi)
      )
      
      ggplot(qrafik_data, aes(x = anomaliya,
                              y = qeyd_sayi,
                              fill = anomaliya)) +
        geom_col() +
        geom_text(aes(label = qeyd_sayi), vjust = -0.3, size = 4) +
        labs(title = "Ölüm məlumatlarında tarix uyğunsuzluğu",
             x = "Uyğunsuzluq",
             y = "Qeyd sayı") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "Vəfatdan sonra klinik və müraciət qeydləri") {
      
      vefatdan_sonra_klinik = umumi_data %>%
        filter(!is.na(DEATHDATE)) %>%
        filter(!is.na(DATE)) %>%
        filter(DATE > DEATHDATE)
      
      vefatdan_sonra_muraciet = muracietler %>%
        left_join(
          pasientler %>% select(ID, DEATHDATE),
          by = c("PATIENT" = "ID")
        ) %>%
        filter(!is.na(DEATHDATE)) %>%
        filter(!is.na(DATE)) %>%
        filter(DATE > DEATHDATE) %>%
        filter(DESCRIPTION != "Death Certification")
      
      qrafik_data = data.frame(
        yoxlama = c("Vəfatdan sonra klinik ölçmə", "Vəfatdan sonra real müraciət"),
        qeyd_sayi = c(nrow(vefatdan_sonra_klinik), nrow(vefatdan_sonra_muraciet))
      )
      
      ggplot(qrafik_data, aes(x = yoxlama,
                              y = qeyd_sayi,
                              fill = yoxlama)) +
        geom_col() +
        geom_text(aes(label = qeyd_sayi), vjust = -0.3, size = 4) +
        labs(title = "Vəfatdan sonra qeydə alınan məlumatlar",
             x = "Yoxlama",
             y = "Qeyd sayı") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "A1c mənfi dəyərlər") {
      
      qrafik_data = umumi_data %>%
        filter(DESCRIPTION == "Hemoglobin A1c/Hemoglobin.total in Blood") %>%
        filter(!is.na(VALUE_NUM)) %>%
        filter(VALUE_NUM < 0) %>%
        count(VALUE_NUM, name = "qeyd_sayi")
      
      ggplot(qrafik_data, aes(x = factor(VALUE_NUM),
                              y = qeyd_sayi,
                              fill = factor(VALUE_NUM))) +
        geom_col() +
        geom_text(aes(label = qeyd_sayi), vjust = -0.3, size = 4) +
        labs(title = "A1c göstəricisində mənfi dəyərlər",
             x = "A1c dəyəri",
             y = "Qeyd sayı") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "Müraciət səbəbi boşluğu") {
      
      reason_bosluq = muracietler %>%
        summarise(
          toplam_muraciet = n(),
          sebeb_bos = sum(is.na(REASONDESCRIPTION) | REASONDESCRIPTION == ""),
          sebeb_dolu = toplam_muraciet - sebeb_bos
        )
      
      qrafik_data = data.frame(
        status = c("Səbəb məlumatı boşdur", "Səbəb məlumatı mövcuddur"),
        qeyd_sayi = c(reason_bosluq$sebeb_bos, reason_bosluq$sebeb_dolu)
      )
      
      ggplot(qrafik_data, aes(x = status,
                              y = qeyd_sayi,
                              fill = status)) +
        geom_col() +
        geom_text(aes(label = qeyd_sayi), vjust = -0.3, size = 4) +
        labs(title = "Müraciət səbəbi məlumatının doluluğu",
             x = "Status",
             y = "Qeyd sayı") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "Risk datasının xülasəsi") {
      
      qrafik_data = proqnoz_data %>%
        summarise(
          `Ümumi pasiyent` = n(),
          `BMI məlumatı olan` = sum(!is.na(`Body Mass Index`)),
          `Sistolik təzyiq məlumatı olan` = sum(!is.na(`Systolic Blood Pressure`)),
          `Glucose məlumatı olan` = sum(!is.na(Glucose)),
          `Risk hesablanan` = sum(!is.na(risk_siqnali_sayi))
        ) %>%
        pivot_longer(cols = everything(),
                     names_to = "gosterici",
                     values_to = "pasiyent_sayi")
      
      ggplot(qrafik_data, aes(x = reorder(gosterici, pasiyent_sayi),
                              y = pasiyent_sayi,
                              fill = gosterici)) +
        geom_col() +
        geom_text(aes(label = pasiyent_sayi), hjust = -0.1, size = 4) +
        coord_flip() +
        labs(title = "Risk analizi üçün data doluluğu",
             x = "Göstərici",
             y = "Pasiyent sayı") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "Glucose qrupları üzrə yüksək A1c faizi") {
      
      qrafik_data = proqnoz_data %>%
        filter(!is.na(Glucose)) %>%
        filter(!is.na(`Hemoglobin A1c/Hemoglobin.total in Blood`)) %>%
        mutate(
          glucose_qrupu = case_when(
            Glucose < 70 ~ "Aşağı glucose",
            Glucose >= 70 & Glucose < 100 ~ "Normal glucose",
            Glucose >= 100 & Glucose < 126 ~ "Yüksək glucose",
            Glucose >= 126 ~ "Çox yüksək glucose",
            TRUE ~ "Bilinmir"
          ),
          glucose_qrupu = factor(glucose_qrupu, levels = c("Aşağı glucose", "Normal glucose", "Yüksək glucose", "Çox yüksək glucose")),
          yuksek_a1c = case_when(
            `Hemoglobin A1c/Hemoglobin.total in Blood` >= 6.5 ~ 1,
            `Hemoglobin A1c/Hemoglobin.total in Blood` < 6.5 ~ 0,
            TRUE ~ NA_real_
          )
        ) %>%
        group_by(glucose_qrupu) %>%
        summarise(
          yuksek_a1c_faizi = round(sum(yuksek_a1c == 1, na.rm = TRUE) / n() * 100, 1),
          .groups = "drop"
        )
      
      ggplot(qrafik_data, aes(x = glucose_qrupu,
                              y = yuksek_a1c_faizi,
                              fill = glucose_qrupu)) +
        geom_col() +
        geom_text(aes(label = paste0(yuksek_a1c_faizi, "%")), vjust = -0.3, size = 4) +
        labs(title = "Glucose qrupları üzrə yüksək A1c faizi",
             x = "Glucose qrupu",
             y = "Yüksək A1c faizi") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "Yaş qrupları üzrə risk siqnalı") {
      
      qrafik_data = proqnoz_data %>%
        filter(!is.na(yas_qrupu)) %>%
        filter(!is.na(risk_siqnali_sayi)) %>%
        filter(yas_qrupu != "0-17") %>%
        mutate(
          yas_qrupu = factor(yas_qrupu, levels = c("18-34", "35-49", "50-64", "65+")),
          riskli_pasiyent = ifelse(risk_siqnali_sayi >= 1, 1, 0)
        ) %>%
        group_by(yas_qrupu) %>%
        summarise(
          risk_faizi = round(sum(riskli_pasiyent == 1, na.rm = TRUE) / n() * 100, 1),
          .groups = "drop"
        )
      
      ggplot(qrafik_data, aes(x = yas_qrupu,
                              y = risk_faizi,
                              fill = yas_qrupu)) +
        geom_col() +
        geom_text(aes(label = paste0(risk_faizi, "%")), vjust = -0.3, size = 4) +
        labs(title = "Yaş qrupları üzrə risk siqnalı",
             x = "Yaş qrupu",
             y = "Risk faizi") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "Risk siqnalı sayına görə pasiyent paylanması") {
      
      qrafik_data = proqnoz_data %>%
        filter(!is.na(risk_siqnali_sayi)) %>%
        mutate(
          risk_qrupu = case_when(
            risk_siqnali_sayi == 0 ~ "Risk siqnalı yoxdur",
            risk_siqnali_sayi == 1 ~ "1 risk siqnalı",
            risk_siqnali_sayi == 2 ~ "2 risk siqnalı",
            risk_siqnali_sayi == 3 ~ "3 risk siqnalı",
            TRUE ~ "Digər"
          ),
          risk_qrupu = factor(risk_qrupu, levels = c("Risk siqnalı yoxdur", "1 risk siqnalı", "2 risk siqnalı", "3 risk siqnalı"))
        ) %>%
        count(risk_qrupu, name = "pasiyent_sayi")
      
      ggplot(qrafik_data, aes(x = risk_qrupu,
                              y = pasiyent_sayi,
                              fill = risk_qrupu)) +
        geom_col() +
        geom_text(aes(label = pasiyent_sayi), vjust = -0.3, size = 4) +
        labs(title = "Risk siqnalı sayına görə pasiyent paylanması",
             x = "Risk qrupu",
             y = "Pasiyent sayı") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if(input$analiz == "Risk siqnalı olmayan pasiyentlərdə sərhədə yaxın göstəricilər") {
      
      risksiz_data = proqnoz_data %>%
        filter(!is.na(risk_siqnali_sayi)) %>%
        filter(risk_siqnali_sayi == 0)
      
      qrafik_data = data.frame(
        serhed_gosterici = c(
          "BMI sərhədə yaxın",
          "Sistolik təzyiq sərhədə yaxın",
          "Glucose sərhədə yaxın"
        ),
        pasiyent_sayi = c(
          sum(risksiz_data$`Body Mass Index` >= 25 & risksiz_data$`Body Mass Index` < 30, na.rm = TRUE),
          sum(risksiz_data$`Systolic Blood Pressure` >= 130 & risksiz_data$`Systolic Blood Pressure` < 140, na.rm = TRUE),
          sum(risksiz_data$Glucose >= 100 & risksiz_data$Glucose < 126, na.rm = TRUE)
        )
      )
      
      ggplot(qrafik_data, aes(x = reorder(serhed_gosterici, pasiyent_sayi),
                              y = pasiyent_sayi,
                              fill = serhed_gosterici)) +
        geom_col() +
        geom_text(aes(label = pasiyent_sayi), hjust = -0.1, size = 4) +
        coord_flip() +
        labs(title = "Risk siqnalı olmayan pasiyentlərdə sərhədə yaxın göstəricilər",
             x = "Sərhədə yaxın göstərici",
             y = "Pasiyent sayı") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else {
      
      qrafik_data = data.frame(
        bolme = c("Deskriptiv təhlil", "Anomaliyalar", "Proqnoz və fərziyyələr"),
        say = c(21, 5, 4)
      )
      
      ggplot(qrafik_data, aes(x = bolme, y = say, fill = bolme)) +
        geom_col() +
        geom_text(aes(label = say), vjust = -0.3, size = 5) +
        labs(title = "Layihənin ümumi strukturu",
             x = "Bölmə",
             y = "Analiz sayı") +
        theme_minimal() +
        theme(legend.position = "none")
    }
  })
}

shinyApp(ui = ui, server = server)