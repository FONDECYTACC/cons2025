sr_1_std_bins_sex_fot <- popEpi::sir(c_SISTRAT_std_bins_fot, coh.obs = 'from0to1',
                                     coh.pyrs = 'pyrs',
                                     ref.data = mx_national_clean,
                                     ref.rate = "mx",
                                     print = c("sex", "fot"),
                                     adjust = c("agegroup", "sex", "year"),
                                     test.type = "homogeneity",
                                     conf.type = "wald",
                                     conf.level = 0.95, EAR = T)
r2_std_bins_sex_fot <- popEpi::rate( data = c_SISTRAT_std_bins_fot,
                                     obs = from0to1,
                                     pyrs = pyrs,
                                     print = c("sex","fot"),
                                     adjust = c("year", "agegroup"),
                                     weights =  list(year = w_year_5y, agegroup = w_age_5y),
)
rates_std_bins_sex_df_fot <- r2_std_bins_sex_fot %>%
  mutate(
    rate_adj_1k     = rate.adj     * 1e3,
    rate_adj_lo_1k  = rate.adj.lo  * 1e3,
    rate_adj_hi_1k  = rate.adj.hi  * 1e3
  )

rates_std_bins_sex_df_fot_df <- 
structure(list(sex = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), levels = c("Male", 
"Female"), class = "factor"), fot = c(0, 0.0386, 0.2465, 0.5, 
1, 3, 5, 7, 9, 0, 0.0386, 0.2465, 0.5, 1, 3, 5, 7, 9), from0to1 = c(40, 
113, 110, 200, 821, 544, 368, 159, 41, 9, 22, 25, 39, 207, 152, 
84, 50, 12), pyrs = c(2055.76317315668, 10967.8829200015, 13159.8510323752, 
25398.399910173, 88652.35028071, 64468.7998877165, 39919.1296429376, 
19339.3100830904, 4363.74441949266, 645.155757287223, 3440.83079866757, 
4127.85818541807, 7966.31964967439, 27769.4552211993, 20348.4195074483, 
12889.9233250993, 6681.88633131233, 1630.89798637629), rate.adj = c(0.0197494844081704, 
0.0146048847157057, 0.00928804646656347, 0.00780905594607365, 
0.00981658597665386, 0.0072001088852152, 0.00758724108692182, 
0.0039686529611959, 0.00386207077731235, 0.0111899801552815, 
0.00760970983162055, 0.00643653007200708, 0.00585191941366406, 
0.00891126105953711, 0.00633947968838036, 0.00323317090730936, 
0.00312773974183286, 0.00177754364971925), SE.rate.adj = c(0.00341854903925466, 
0.004566006019786, 0.00138071306313774, 0.000747492605265317, 
0.000623880412991266, 0.000821946455873918, 0.00105265582448245, 
0.000910043559146447, 0.00149977824797925, 0.00390929144579909, 
0.00213009215819342, 0.00172104298206648, 0.00130763802084855, 
0.00108902493121662, 0.000874277726376859, 0.000638129440575086, 
0.000980521797656578, 0.000826486430214378), rate.adj.lo = c(0.0140673989719837, 
0.00791366002943484, 0.00694043713631193, 0.00647319902964026, 
0.00866687371410015, 0.00575660241305651, 0.00578077129904972, 
0.00253193341806212, 0.00180410821620904, 0.00564223440908983, 
0.00439640178637888, 0.00381107582557879, 0.00377650430147746, 
0.00701316169439562, 0.00483795964782162, 0.00219595947814021, 
0.00169192348619465, 0.000714559601207435), rate.adj.hi = c(0.0277266703791769, 
0.0269537302292083, 0.012429736840882, 0.00942059011157844, 0.0111188143978907, 
0.00900558423860807, 0.00995822604511923, 0.00622062421312163, 
0.008267569846953, 0.0221925653556446, 0.0131716086325127, 0.0108706625803121, 
0.00906789932971105, 0.0113230775407162, 0.00830701486679058, 
0.00476028552435985, 0.00578203209096847, 0.00442183048316495
), rate = c(0.019457494191113, 0.0103028087393172, 0.00835875723284282, 
0.00787451180811957, 0.00926089378792975, 0.00843819027106863, 
0.00921863786339103, 0.00822159628843349, 0.00939560067194923, 
0.0139501196390831, 0.00639380466151352, 0.00605640961414666, 
0.00489561073557901, 0.00745423337804536, 0.0074698676201541, 
0.00651671836064651, 0.00748291687718368, 0.00735790963030306
), SE.rate = c(0.00307649996017063, 0.000969206718404063, 0.000796976231410164, 
0.000556812069805489, 0.000323207421721604, 0.000361784423162888, 
0.000480554718122336, 0.000652014997367659, 0.00146734630214143, 
0.00465003987969436, 0.00136316373407252, 0.00121128192282933, 
0.000783925108836632, 0.000518105035022606, 0.000605886270500033, 
0.000711032265961218, 0.00105824425338237, 0.00212404555286421
), rate.lo = c(0.0142724212410837, 0.00856800794982709, 0.006933960094478, 
0.00685541095615606, 0.00864858819977793, 0.00775806972160845, 
0.00832327016765844, 0.00703801573830574, 0.0069180988055486, 
0.00725836612006237, 0.00420997241676099, 0.00409234089210958, 
0.00357686906163439, 0.00650488018238866, 0.00637191648082857, 
0.00526203270481267, 0.00567140461509695, 0.00417858474298325
), rate.hi = c(0.0265262686549216, 0.0123888619782494, 0.0100763231292957, 
0.00904510854459167, 0.00991654958823431, 0.00917793440969424, 
0.0102103238684435, 0.00960421914973656, 0.0127603427571649, 
0.0268112457715289, 0.00971045270672917, 0.00896310898367503, 
0.00670055404917875, 0.00854213970071332, 0.00875700779043653, 
0.00807057283265196, 0.00987304711813147, 0.0129562609011621), 
    rate_adj_1k = c(19.7494844081704, 14.6048847157057, 9.28804646656347, 
    7.80905594607365, 9.81658597665387, 7.2001088852152, 7.58724108692182, 
    3.9686529611959, 3.86207077731235, 11.1899801552815, 7.60970983162055, 
    6.43653007200708, 5.85191941366406, 8.91126105953711, 6.33947968838036, 
    3.23317090730936, 3.12773974183286, 1.77754364971925), rate_adj_lo_1k = c(14.0673989719837, 
    7.91366002943484, 6.94043713631193, 6.47319902964026, 8.66687371410015, 
    5.75660241305651, 5.78077129904972, 2.53193341806212, 1.80410821620904, 
    5.64223440908983, 4.39640178637888, 3.81107582557879, 3.77650430147746, 
    7.01316169439562, 4.83795964782162, 2.19595947814021, 1.69192348619465, 
    0.714559601207435), rate_adj_hi_1k = c(27.7266703791769, 
    26.9537302292083, 12.429736840882, 9.42059011157844, 11.1188143978907, 
    9.00558423860807, 9.95822604511923, 6.22062421312163, 8.26756984695301, 
    22.1925653556446, 13.1716086325127, 10.8706625803121, 9.06789932971105, 
    11.3230775407162, 8.30701486679058, 4.76028552435985, 5.78203209096847, 
    4.42183048316495)), row.names = c(NA, -18L), class = c("rate", 
"data.table", "data.frame"), rate.meta = list(obs = "from0to1", 
    pyrs = "pyrs", weights = list(adjust1sU2bLOr5Nt = c(`2010` = 0.0844304889857223, 
    `2011` = 0.0856617566910571, `2012` = 0.0868834173629828, 
    `2013` = 0.0879980095741958, `2014` = 0.0891281650489123, 
    `2015` = 0.0902485698092307, `2016` = 0.0914445703626899, 
    `2017` = 0.0930032609184196, `2018` = 0.0950120713583012, 
    `2019` = 0.0970926635141577, `2020` = 0.0990970263743305), 
        adjust2ZTDthNBrhP = c(`15` = 0.0967060636891063, `20` = 0.10509713794915, 
        `25` = 0.105312065116015, `30` = 0.0988588620729512, 
        `35` = 0.0944152531842548, `40` = 0.0905758881643608, 
        `45` = 0.087278043730563, `50` = 0.0821232857862027, 
        `55` = 0.0728637121945439, `60` = 0.0603672867489698, 
        `65` = 0.0465857589438183, `70` = 0.0345606920449701, 
        `75` = 0.0252559503750942)), adjust = c("year", "agegroup"
    ), print = c("sex", "fot"), call = popEpi::rate(data = c_SISTRAT_std_bins_fot, 
        obs = from0to1, pyrs = pyrs, print = c("sex", "fot"), 
        adjust = c("year", "agegroup"), weights = list(year = w_year_5y, 
            agegroup = w_age_5y)), NAs = NA))


p_std_bins_rate2 <- ggplot(rates_std_bins_sex_df_fot_df, aes(x = fot, y = rate_adj_1k, fill=sex)) +
  geom_ribbon(aes(ymin = rate_adj_lo_1k, ymax = rate_adj_hi_1k, fill=sex),
              alpha = .20) +
  geom_line(aes(color=sex), linewidth = 1.5) +
  geom_point(size = 3,aes(color=sex, shape=sex)) +
  scale_x_continuous("Years since discharge",
                     breaks = setdiff(round(rates_df_fot$fot,2),c(0.04, 0.25))) +
  scale_y_continuous("Adjusted rate
(deaths ×1,000 PY)",
                     limits = c(0, NA)) +
  theme_minimal(base_family = "serif")+
  theme(axis.title.x = element_blank())+
  scale_colour_manual(
    values = c(Male = "#0092bd", Female = "#cba052")
  ) +
  scale_shape_manual(values = c(Male = 16, Female = 17))+
  scale_fill_manual(
    values = c(Male = alpha("#0092bd", 0.25),
               Female = alpha("#cba052", 0.25))
  )+
  guides(
    fill = guide_legend(title = "Sex"),
    color = guide_legend(title = "Sex"),
    shape = guide_legend(title = "Sex")
  )+theme_minimal(base_family = "serif")+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size=12, face = "bold"),
        axis.text.x = ggplot2::element_text(family = "serif", size = 12, face = "bold"))


sr_1_std_bins_sex_fot_df <- 
  structure(list(sex = c("Female", "Female", "Female", "Female", 
"Female", "Female", "Female", "Female", "Female", "Male", "Male", 
"Male", "Male", "Male", "Male", "Male", "Male", "Male"), fot = c(0, 
0.0386, 0.2465, 0.5, 1, 3, 5, 7, 9, 0, 0.0386, 0.2465, 0.5, 1, 
3, 5, 7, 9), observed = c(9, 22, 25, 39, 207, 152, 84, 50, 12, 
40, 113, 110, 200, 821, 544, 368, 159, 41), expected = c(0.6625, 
3.5578, 4.3419, 8.6184, 32.5189, 26.7001, 18.1859, 10.3682, 2.7696, 
4.7965, 25.7269, 31.1748, 61.3521, 226.4529, 176.6326, 114.2652, 
58.014, 14.318), pyrs = c(645.1558, 3440.8308, 4127.8582, 7966.3196, 
27769.4552, 20348.4195, 12889.9233, 6681.8863, 1630.898, 2055.7632, 
10967.8829, 13159.851, 25398.3999, 88652.3503, 64468.7999, 39919.1296, 
19339.3101, 4363.7444), sir = c(13.5846, 6.1835, 5.7578, 4.5252, 
6.3655, 5.6929, 4.619, 4.8224, 4.3328, 8.3394, 4.3923, 3.5285, 
3.2599, 3.6255, 3.0798, 3.2206, 2.7407, 2.8635), sir.lo = c(7.0683, 
4.0716, 3.8906, 3.3063, 5.5548, 4.8561, 3.7297, 3.655, 2.4606, 
6.1171, 3.6527, 2.927, 2.838, 3.3858, 2.8316, 2.9078, 2.3462, 
2.1085), sir.hi = c(26.1084, 9.391, 8.5212, 6.1936, 7.2945, 6.6738, 
5.7203, 6.3627, 7.6294, 11.3689, 5.2816, 4.2535, 3.7445, 3.8822, 
3.3498, 3.567, 3.2016, 3.889), p_value = c(0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), EAR = c(12.923, 5.36, 5.005, 
3.814, 6.283, 6.158, 5.106, 5.931, 5.66, 17.124, 7.957, 5.99, 
5.459, 6.707, 5.698, 6.356, 5.222, 6.114)), row.names = c(NA, 
-18L), class = c("sir", "data.table", "data.frame"), sorted = c("sex", "fot"), sir.meta = list(adjust = c("agegroup", "sex", "year"), 
    print = c("sex", "fot"), call = popEpi::sir(coh.data = c_SISTRAT_std_bins_fot, 
        coh.obs = "from0to1", coh.pyrs = "pyrs", ref.data = mx_national_clean, 
        ref.rate = "mx", print = c("sex", "fot"), adjust = c("agegroup", 
            "sex", "year"), test.type = "homogeneity", conf.type = "wald", 
        conf.level = 0.95, EAR = T), lrt.test = 1.18476720797598e-28, 
    conf.type = "wald", conf.level = 0.95, lrt.test.type = "homogeneity", 
    pooled.sir = structure(list(observed = 2996, expected = 820.4564, 
        pyrs = 353825.9781, sir = 3.6516, sir.lo = 3.522, sir.hi = 3.7848, 
        p_value = 0), row.names = c(NA, -1L), class = c("data.table", 
    "data.frame"))))


p_std_bins_sir2 <- ggplot(sr_1_std_bins_sex_fot_df, aes(x = fot, y = sir, fill= sex)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = sir.lo, ymax = sir.hi, fill=sex),
              alpha = .20) +
  geom_line(aes(color=sex), linewidth = 1.5) +
  geom_point(aes(color=sex, shape=sex),size = 3) +
  scale_x_continuous("Years since discharge",
                     breaks = setdiff(round(rates_df_fot$fot,2),c(0.04, 0.25))) +
  theme_minimal(base_family = "serif")+
  theme(axis.title.x = element_blank(),
        axis.text.x = ggplot2::element_text(family = "serif", size = 12, face = "bold"),
        axis.text.y = element_text(size=12, face = "bold"))+
  # scale_y_log10(
  #   "Adjusted SMR",
  #   breaks = setdiff(round(as.numeric(gsub(9,11,rates_df_fot$fot)),2),c(.04, .25)),
  #   labels = scales::number_format(accuracy = 0.1)
  # ) +
  scale_y_log10(
    "Adjusted SMR",
    limits = c(1.5, 16),
    breaks = c(3, 4, 6, 10)
  )+
  scale_colour_manual(
    values = c(Male = "#0092bd", Female = "#cba052")
  ) +
  scale_shape_manual(values = c(Male = 16, Female = 17))+
  scale_fill_manual(
    values = c(Male = alpha("#0092bd", 0.25),
               Female = alpha("#cba052", 0.25))
  )+ theme(legend.position="none")+
  guides(
    fill = guide_legend(title = "Sex"),
    color = guide_legend(title = "Sex"),
    shape = guide_legend(title = "Sex")
  )
legend_shared_std_bins <- ggpubr::get_legend(
  p_std_bins_rate2 +
    theme(legend.position = "bottom", base_size=18) +
    guides(
      fill = guide_legend(title = "Sex"),
      color = guide_legend(title = "Sex"),
      shape = guide_legend(title = "Sex")
    )+
    ggplot2::theme( legend.title = 
        element_text(family = "serif", 
                     size = 14, 
                     face = "bold"), 
        legend.text = element_text(family = "serif", 
                    size = 14), 
        legend.key.size = grid::unit(1.0, "cm"), # size of legend key (width & height) 
        legend.key.width = grid::unit(1.0, "cm"), # explicit width 
        legend.key.height = grid::unit(0.6, "cm"), # explicit height 
        legend.spacing = grid::unit(0.4, "cm"), # space between legends (if multiple) 
        legend.margin = margin(4, 4, 4, 4), # margin around legend box 
        legend.background = element_rect(fill = "white", colour = NA))
)
panels_std_bins <- cowplot::plot_grid(
  p_std_bins_rate2+ theme(legend.position="none"), p_std_bins_sir2+ theme(legend.position="none"),
  ncol             = 1
)
xlab_shared_std_bins <- cowplot::ggdraw() +
  cowplot::draw_label("Years since discharge",
                      fontfamily = "serif",
                      fontface = "plain", size = 16, hjust = 0.5)
final_std_bins_plot <- cowplot::plot_grid(
  panels_std_bins,
  xlab_shared_std_bins,
  legend_shared_std_bins,
  ncol = 1,
  rel_heights = c(.84, 0.05, 0.11)
)
print(final_std_bins_plot)
figexp<- 1.75
ggsave(
  paste0(gsub("/cons","",getwd()), "/cons/_figs/Figure_1_rates_and_SIR_by_fot_updated_postrev_poster.png"),
  dpi = 600,
  width = 85 *figexp,
  height = 60 * figexp,
  units = "mm"
)