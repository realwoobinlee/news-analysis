library(mosaic)
print(getwd()) # um sicherzustellen, ob das Projekt sich im selben Dict befindet

freq <- read.csv2("data/processed/frequency/dist_freq.csv", sep = ",")

arr_freq <- c() # 9 ~ 21 => -6 bis +6 Tage daher einwöchlige Zeitspanne jeweils vor/nach der Trend-Erscheinung

# Data-Preprocessing zur Ermöglichung des Bootstrappings
for (row in 1:nrow(freq)) {
  reped <- rep(row, freq[row, "freq"])
  arr_freq = append(arr_freq, reped)
}

##### 
# 1.
# H0: Es gibt keinen zeitlichen Zusammenhang zwischen Nachrichten und Suchtrends
#     (50 Prozent innerhalb dem 95%-Konfidenzintervalls)
# Werte:
#  2.5%      97.5% 
#  0.7913793 0.8603448 
# Ergebnis: H0 ist nun zu verwerfen, denn der Wert (0,5) befindet sich außerhalb dem 95% Konfidenzintervall.
# Visualization: visualization/histogram/time-dependency.png
#####
get_total_prop <- function(start, end) {
  resampled = resample(arr_freq)
  total_prop = 0.0
  
  for (i in start:end) {
    total_prop = total_prop + prop(~ resampled, success=i)
  }
  
  return(total_prop)
}
# Test
get_total_prop(9, 21)

set.seed(1896) # Reproduzierbarkeit
Bootvtlg <- do(10000)* get_total_prop(9, 21) # Eine Woche vor/nach der Erscheinung des Trends

gf_histogram( ~ prop_9, data = Bootvtlg)

quantile( ~ prop_9, data = Bootvtlg, probs = c(0.025, 0.975))

#####
# 2. 
# H0: Durschschnittlich werden die Suchtrends zeitgleich für die Nachrichten in Bezug genommen.
#     (17[+2 Tag], 13[- 2Tag] innerhalb dem 95% Konfidenzintervalls des Mean-Wertes)
# Werte:
#  2.5%     97.5% 
#  15.39957 15.76682
# Ergebnis: H0 ist nun zu verwerfen, denn die beiden Werte (17, 13) befinden sich außerhalb dem 95% Konfidenzintervall.
# Visualization: visualization/histogram/mean-time-dependency.png
#####
MeanBootvtlg <- do(10000) * mean(resample(arr_freq))
gf_histogram( ~ mean, data = MeanBootvtlg)
quantile( ~ mean, data = MeanBootvtlg, probs = c(0.025, 0.975))
