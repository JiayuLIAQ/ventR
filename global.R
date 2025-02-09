library(shiny)
library(ggplot2)
library(shinythemes)
library(magrittr)
library(data.table)
library(stringr)
library(patchwork)


co2_gen_met_dt <-
  fread(here::here("co2_gen_met_dt.csv")) %>% melt(
    id = c("age_lower", "age_upper", "body_mass",  "BMR", "gender"),
    variable.name = "met",
    value.name = "E_l_s"
  ) %>% .[, met := as.numeric(paste(met))]

LJYtheme_basic <- theme(
  plot.title = element_text(size = 12, vjust = 0, face = "bold"),
  axis.text.x = element_text(size = 12, hjust=.5, vjust=1, colour="black"),
  axis.text.y = element_text(size = 12, hjust=1, vjust=.5, colour="black"),
  axis.title.y = element_text(size = 12, color = "black", face = "bold", vjust = 0.5, hjust = 0.5),
  axis.title.x = element_text(size = 12, color = "black", face = "bold", vjust = 0.5, hjust = 0.5),
  axis.line = element_line(color = "black"),
  # panel.grid.major=element_blank(),
  # panel.grid.major = element_line(colour="#f0f0f0"),
  panel.grid.major = element_line(colour="#f7f7f7"),
  # panel.grid.major = element_blank(),
  # panel.grid.minor=element_line(colour="#f0f0f0",size = 0.1),
  panel.grid.minor=element_blank(),
  # panel.background=element_rect(fill='white',colour='black'),
  legend.text = element_text(size = 12),
  legend.key = element_rect(colour = NA, fill = "white"),
  panel.background = element_blank(),
  # legend.position = "bottom",
  # legend.direction = "horizontal",
  # legend.key.size= unit(0.3, "cm"),
  # legend.margin = margin(0,0,0,0,"cm"),
  legend.title = element_text(face = "bold", size = 12),
  strip.background = element_rect(colour= NA, fill="#f0f0f0"),
  strip.text = element_text(face = "bold", size = 12)
)

# time_default--------------------------------------
time_default <- c("1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31
32
33
34
35
36
37
38
39
40
41
42
43
44
45
46
47
48
49
50
51
52
53
54
55
56
57
58
59
60
61
62
63
64
65
66
67
68
69
70
71
72
73
74
75
76
77
78
79
80
81
82
83
84
85
86
87
88
89
90
91
92
93
94
95
96
97
98
99
100
101
102
103
104
105
106
107
108
109
110
111
112
113
114
115
116
117
118
119
120
121")

# indoor_co2_default--------------------------------------
indoor_co2_default <- c("648
654
666
661
660
651
634
634
607
601
585
571
560
550
541
530
518
509
502
499
496
489
492
494
492
523
562
585
611
618
633
645
648
650
650
656
668
685
696
712
709
711
726
728
748
751
751
759
765
767
774
781
795
800
802
813
822
835
844
856
859
867
897
921
932
942
945
948
949
953
959
963
974
982
995
1005
1019
1061
1086
1124
1175
1196
1236
1275
1292
1329
1371
1403
1425
1456
1504
1517
1532
1554
1573
1597
1617
1617
1621
1638
1670
1668
1670
1674
1691
1712
1716
1727
1738
1750
1759
1767
1781
1772
1764
1768
1773
1790
1810
1817
1815")

# outdoor_co2_default--------------------------------------

outdoor_co2_default <- c("400
404
404
404
405
400
400
398
398
398
397
397
396
395
397
397
397
397
397
398
398
397
396
399
399
400
397
396
396
397
398
395
394
394
394
395
394
393
395
394
395
394
397
398
411
399
399
397
396
394
395
393
391
394
394
395
396
393
395
396
393
393
393
393
394
394
391
393
394
395
393
393
390
390
392
394
394
391
392
390
391
415
418
414
423
402
398
393
396
396
400
401
397
394
392
389
393
390
390
391
390
389
389
393
393
391
390
392
390
391
390
390
390
387
390
390
388
389
389
390
391")


# occup_default--------------------------------------
occup_default <- c("0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18
18")
