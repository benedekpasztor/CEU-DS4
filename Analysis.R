library(rvest)

my_url <- "https://www.parlament.hu/orszaggyulesi-naplo-elozo-ciklusbeli-adatai?p_auth=Whv7dzoh&p_p_id=pairproxy_WAR_pairproxyportlet_INSTANCE_9xd2Wc9jP4z8&p_p_lifecycle=1&p_p_state=normal&p_p_mode=view&p_p_col_id=column-1&p_p_col_count=1&_pairproxy_WAR_pairproxyportlet_INSTANCE_9xd2Wc9jP4z8_pairAction=%2Finternet%2Fcplsql%2Fogy_naplo.naplo_fszoveg2%3Fp_ckl%3D36%26p_query%3D182_62%2C182_66%2C182_78%2C182_82%2C182_94%2C182_108%2C182_112%2C184_20-22%2C184_30%2C184_34%2C184_53%2C184_57%2C184_69%2C184_75%2C184_133%2C185_6-8%2C185_22%2C195_10%2C195_24%2C196_83%2C196_87%2C196_123%2C196_127%2C200_50%2C200_56%2C200_60%2C200_99%2C200_103%2C209_6%2C210_131%2C210_135%2C210_139%2C210_143%2C210_163%2C210_167%2C210_179%2C210_183-185%2C214_22%2C214_30%2C214_34%2C217_6%2C217_20%2C218_30%2C218_34%2C220_4%2C220_18%2C224_33%2C231_57%2C231_61%2C231_105%2C231_109%2C233_2-10%2C244_16%2C247_126%2C247_130%2C247_134%2C247_138%2C247_164%2C247_168%2C250_2%2C250_16%2C251_91%2C251_95%2C251_131%2C251_135%2C251_139%2C251_143%2C252_22%2C%26p_stilus%3D"
my_page <- read_html(my_url)

my_page %>% 
  html_nodes('.scrollable') %>%
  html_text() 

my_page %>% 
  html_nodes('justify') %>%
  html_text() 
