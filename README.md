# How biased is the sample? Reverse engineering the ranking algorithm of Facebook’s Graph application programming interface
Repository for the paper: Ho, J.C. (2020) How biased is the sample? Reverse engineering the ranking algorithm of Facebook’s Graph application programming interface. *Big Data &amp; Society* 7(1). URL: https://journals.sagepub.com/doi/full/10.1177/2053951720905874

All R scripts are located in the `R` folder. Data in the `R/data` subfolder.

List of files:
- `bootstrap.R` produces the comparison of likes, comments, and shares, word clouds, and also the top term analysis.
- `logisitic.R` produces the logisitic regression models.
- `sentanalysis.R` produces the sentiment analysis.

All scripts are tested on 19 February with the following session:
```
R version 3.6.0 (2019-04-26)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS High Sierra 10.13.2

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

Random number generation:
 RNG:     Mersenne-Twister
 Normal:  Inversion
 Sample:  Rounding

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] tidytext_0.2.0  scales_1.1.0    stringr_1.4.0   readtext_0.74   caret_6.0-84   
 [6] lattice_0.20-38 pscl_1.5.2      lmtest_0.9-37   zoo_1.8-5       stargazer_5.2.2
[11] qwraps2_0.4.1   Kendall_2.2     quanteda_1.5.2  gridExtra_2.3   ggplot2_3.2.1  
[16] reshape2_1.4.3  tidyr_0.8.3     magrittr_1.5    rlang_0.4.4     dplyr_0.8.3    
[21] lubridate_1.7.4 ROCR_1.0-7      gplots_3.0.1.1

loaded via a namespace (and not attached):
 [1] httr_1.4.1          splines_3.6.0       foreach_1.4.8       prodlim_2018.04.18
 [5] gtools_3.8.1        RcppParallel_4.4.4  assertthat_0.2.1    stats4_3.6.0       
 [9] yaml_2.2.1          sessioninfo_1.1.1   ipred_0.9-9         backports_1.1.4    
[13] pillar_1.4.3        glue_1.3.1          digest_0.6.24       colorspace_1.4-1   
[17] recipes_0.1.5       Matrix_1.2-17       plyr_1.8.5          timeDate_3043.102  
[21] pkgconfig_2.0.3     ISOcodes_2019.12.22 broom_0.5.2         purrr_0.3.3        
[25] gdata_2.18.0        gower_0.2.1         lava_1.6.5          tibble_2.1.3       
[29] generics_0.0.2      farver_2.0.3        withr_2.1.2         nnet_7.3-12        
[33] lazyeval_0.2.2      cli_2.0.1           survival_2.44-1.1   crayon_1.3.4       
[37] tokenizers_0.2.1    janeaustenr_0.1.5   stopwords_1.0       fansi_0.4.1        
[41] nlme_3.1-140        SnowballC_0.6.0     MASS_7.3-51.4       class_7.3-15       
[45] tools_3.6.0         data.table_1.12.8   lifecycle_0.1.0     munsell_0.5.0      
[49] compiler_3.6.0      e1071_1.7-1         caTools_1.17.1.2    grid_3.6.0         
[53] iterators_1.0.12    rstudioapi_0.10     bitops_1.0-6        labeling_0.3       
[57] boot_1.3-22         gtable_0.3.0        ModelMetrics_1.2.2  codetools_0.2-16   
[61] R6_2.4.1            knitr_1.23          fastmatch_1.1-0     KernSmooth_2.23-15
[65] stringi_1.4.5       Rcpp_1.0.3          spacyr_1.2          rpart_4.1-15       
[69] tidyselect_0.2.5    xfun_0.7
```
