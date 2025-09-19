\name{data_AlgMetrics}
\alias{data_AlgMetrics}
\docType{data}
\title{
A Capitalized Title for the Data Set
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("data_AlgMetrics")}
\format{
  A data frame with 124 observations on the following 56 variables.
  \describe{
    \item{\code{StationID_Master}}{a factor with levels \code{LCBEN002.57} \code{LCBRB006.74} \code{LCCHC081.26} \code{LCCLE063.52} \code{LCCOY000.71} \code{LCECL018.17} \code{LCECL021.13} \code{LCECL040.69} \code{LCELR000.13} \code{LCELR007.19} \code{LCHAL004.59} \code{LCHAL008.83} \code{LCHAL010.20} \code{LCLCR211.73} \code{LCLCR311.31} \code{LCLCR340.02} \code{LCLCR342.03} \code{LCLCR360.06} \code{LCLVL001.32} \code{LCMIN018.05} \code{LCMLK001.18} \code{LCMRS043.17} \code{LCRIG004.87} \code{LCRUD003.45} \code{LCRUD007.23} \code{LCSHL026.50} \code{LCSHL029.75} \code{LCSHL031.05} \code{LCSIL041.04} \code{LCSIL043.84} \code{LCSLR001.42} \code{LCSLR003.72} \code{MGAFR110.01} \code{MGHSR002.67} \code{MGHSR048.20} \code{MGHSR058.80} \code{MGSSW000.38} \code{SCCIE004.25} \code{SCCIE006.69} \code{SCCIE020.88} \code{SCCIE026.68} \code{SCMAD012.71} \code{SCRED005.58} \code{SCSAB005.40} \code{SCSAB007.15} \code{SCSAB009.77} \code{SCSCR034.31} \code{SCSCR039.63} \code{SCSCR045.13} \code{SCSCR096.72} \code{SCSCR103.45} \code{SCSCR111.66} \code{SCSCR128.27} \code{SCSCR168.93} \code{SCSCR169.35} \code{SCSON014.52} \code{SPARA010.19} \code{SPARA019.41} \code{SPRMC011.11} \code{SPSPR014.18} \code{SPSPR015.42} \code{SPSPR150.09} \code{SRCDY002.95} \code{SRDCN001.90} \code{SRDEV000.38} \code{SRFIS010.92} \code{SRHAY003.25} \code{SRHRS001.41} \code{SRNBE000.86} \code{SRNFB012.35} \code{SRPNT008.48} \code{SRREY001.14} \code{SRTON061.37} \code{SRTON062.28} \code{SRUDC000.02} \code{SRUEF000.87} \code{SRUUE000.03} \code{SRWRK002.83} \code{UGBLR028.77} \code{UGCAV016.84} \code{UGCOL003.48} \code{UGEAG013.81} \code{UGEEG001.57} \code{UGFRY009.52} \code{UGGLR401.85} \code{UGKPK011.18} \code{UGSFR006.42} \code{UGSFR151.22} \code{UGSTO003.44} \code{VRBON002.71} \code{VREVR023.23} \code{VROAK042.78} \code{VRSPN002.92} \code{VRWCL036.37} \code{VRWEB010.36} \code{VRWOK000.82}}
    \item{\code{Algae.Metrics.SampID}}{a factor with levels \code{LCBEN002.57_2007-05-09_49_1} \code{LCBRB006.74_2007-06-20_56_1} \code{LCCHC081.26_2007-06-18_57_1} \code{LCCLE063.52_2007-05-07_39_1} \code{LCCOY000.71_2007-04-11_48_1} \code{LCECL018.17_2007-05-01_52_1} \code{LCECL021.13_2007-05-02_36_1} \code{LCECL021.13_2016-05-02_95_1} \code{LCECL040.69_2007-04-30_30_1} \code{LCELR000.13_2007-06-12_55_1} \code{LCELR007.19_2007-06-13_47_1} \code{LCHAL004.59_2007-06-04_43_1} \code{LCHAL008.83_2007-06-05_31_1} \code{LCHAL010.20_2007-06-07_34_1} \code{LCLCR211.73_2007-04-25_53_1} \code{LCLCR211.73_2007-05-14_54_1} \code{LCLCR311.31_2007-04-10_50_1} \code{LCLCR340.02_2007-04-18_61_1} \code{LCLCR342.03_2007-04-12_41_1} \code{LCLCR360.06_2007-06-06_45_1} \code{LCLVL001.32_2007-06-13_62_1} \code{LCMIN018.05_2007-06-29_38_1} \code{LCMLK001.18_2007-05-22_33_1} \code{LCMRS043.17_2007-05-24_32_1} \code{LCRIG004.87_2007-05-10_42_1} \code{LCRUD003.45_2007-05-23_46_1} \code{LCRUD007.23_2007-06-28_60_1} \code{LCSHL026.50_2007-04-17_40_1} \code{LCSHL029.75_2007-06-26_58_1} \code{LCSHL031.05_2007-04-16_44_1} \code{LCSIL041.04_2007-04-24_35_1} \code{LCSIL043.84_2007-06-27_51_1} \code{LCSLR001.42_2007-05-21_37_1} \code{LCSLR003.72_2007-06-21_59_1} \code{MGAFR110.01_2015-04-14_66_1} \code{MGAFR110.01_2016-04-04_96_1} \code{MGHSR002.67_2015-04-08_67_1} \code{MGHSR048.20_2015-04-15_68_1} \code{MGHSR058.80_2015-04-15_69_1} \code{MGSSW000.38_2015-04-22_70_1} \code{SCCIE004.25_2001-07-04_1_1} \code{SCCIE004.25_2001-07-04_1_2} \code{SCCIE006.69_2001-07-04_2_1} \code{SCCIE020.88_2001-07-04_3_1} \code{SCCIE020.88_2001-07-04_3_2} \code{SCCIE026.68_2001-07-04_4_1} \code{SCCIE026.68_2001-07-04_4_2} \code{SCMAD012.71_2001-07-04_5_1} \code{SCMAD012.71_2001-07-04_5_2} \code{SCRED005.58_2001-07-04_6_1} \code{SCSAB005.40_2001-07-04_7_1} \code{SCSAB007.15_2015-05-05_71_1} \code{SCSAB009.77_2001-07-04_8_1} \code{SCSAB009.77_2001-07-04_8_2} \code{SCSCR034.31_2007-07-04_9_1} \code{SCSCR034.31_2007-07-04_9_2} \code{SCSCR034.31_2007-07-04_9_3} \code{SCSCR039.63_2015-04-29_72_1} \code{SCSCR039.63_2016-04-06_97_1} \code{SCSCR045.13_2007-07-04_10_1} \code{SCSCR045.13_2007-07-04_10_2} \code{SCSCR045.13_2007-07-04_10_3} \code{SCSCR096.72_2007-07-04_11_1} \code{SCSCR096.72_2007-07-04_11_2} \code{SCSCR096.72_2007-07-04_11_3} \code{SCSCR103.45_2015-04-07_73_1} \code{SCSCR103.45_2016-04-05_98_1} \code{SCSCR111.66_2007-07-04_14_1} \code{SCSCR111.66_2007-07-04_14_2} \code{SCSCR111.66_2007-07-04_14_3} \code{SCSCR128.27_2001-07-04_15_1} \code{SCSCR168.93_2007-07-04_13_1} \code{SCSCR168.93_2007-07-04_13_2} \code{SCSCR168.93_2007-07-04_13_3} \code{SCSCR169.35_2001-07-04_12_1} \code{SCSCR169.35_2001-07-04_12_2} \code{SCSON014.52_2001-07-04_16_1} \code{SPARA010.19_2015-04-21_74_1} \code{SPARA019.41_2015-06-11_75_1} \code{SPARA019.41_2015-06-11_75_2} \code{SPRMC011.11_2015-06-17_76_1} \code{SPSPR014.18_2015-04-06_77_1} \code{SPSPR014.18_2016-04-05_99_1} \code{SPSPR015.42_2015-04-06_78_1} \code{SPSPR015.42_2016-04-05_100_1} \code{SPSPR150.09_2015-06-15_79_1} \code{SRCDY002.95_2016-06-21_101_1} \code{SRDCN001.90_2016-06-22_102_1} \code{SRDEV000.38_2016-05-17_103_1} \code{SRFIS010.92_2016-06-20_104_1} \code{SRFIS010.92_2016-06-20_93_2} \code{SRHAY003.25_2016-06-21_105_1} \code{SRHRS001.41_2016-06-20_106_1} \code{SRNBE000.86_2016-06-21_107_1} \code{SRNFB012.35_2016-06-14_108_1} \code{SRPNT008.48_2016-05-04_109_1} \code{SRREY001.14_2016-06-29_110_1} \code{SRTON061.37_2015-05-07_80_1} \code{SRTON062.28_2015-05-07_81_1} \code{SRUDC000.02_2016-06-22_111_1} \code{SRUEF000.87_2016-06-13_112_1} \code{SRUUE000.03_2016-06-13_113_1} \code{SRWRK002.83_2016-06-16_114_1} \code{UGBLR028.77_2015-06-01_82_1} \code{UGBLR028.77_2015-06-01_82_2} \code{UGCAV016.84_2015-06-16_83_1} \code{UGCOL003.48_2015-06-03_84_1} \code{UGEAG013.81_2015-05-19_85_1} \code{UGEEG001.57_2015-05-19_86_1} \code{UGFRY009.52_2015-05-11_87_1} \code{UGFRY009.52_2015-05-11_87_2} \code{UGGLR401.85_2015-05-18_88_1} \code{UGKPK011.18_2015-05-20_89_1} \code{UGSFR006.42_2015-05-18_90_1} \code{UGSFR151.22_2015-06-02_91_1} \code{UGSTO003.44_2015-06-02_92_1} \code{VRBON002.71_2016-05-03_115_1} \code{VREVR023.23_2016-05-03_116_1} \code{VROAK042.78_2016-05-09_117_1} \code{VRSPN002.92_2016-05-10_118_1} \code{VRWCL036.37_2016-06-16_119_1} \code{VRWEB010.36_2016-05-03_120_1} \code{VRWEB010.36_2016-05-03_94_2} \code{VRWOK000.82_2016-05-10_121_1}}
    \item{\code{AlgalSampIdent}}{a numeric vector}
    \item{\code{RepNum}}{a numeric vector}
    \item{\code{CollDate}}{a factor with levels \code{4/10/2007} \code{4/11/2007} \code{4/12/2007} \code{4/14/2015} \code{4/15/2015} \code{4/16/2007} \code{4/17/2007} \code{4/18/2007} \code{4/21/2015} \code{4/22/2015} \code{4/24/2007} \code{4/25/2007} \code{4/29/2015} \code{4/30/2007} \code{4/4/2016} \code{4/5/2016} \code{4/6/2015} \code{4/6/2016} \code{4/7/2015} \code{4/8/2015} \code{5/1/2007} \code{5/10/2007} \code{5/10/2016} \code{5/11/2015} \code{5/14/2007} \code{5/17/2016} \code{5/18/2015} \code{5/19/2015} \code{5/2/2007} \code{5/2/2016} \code{5/20/2015} \code{5/21/2007} \code{5/22/2007} \code{5/23/2007} \code{5/24/2007} \code{5/3/2016} \code{5/4/2016} \code{5/5/2015} \code{5/7/2007} \code{5/7/2015} \code{5/9/2007} \code{5/9/2016} \code{6/1/2015} \code{6/11/2015} \code{6/12/2007} \code{6/13/2007} \code{6/13/2016} \code{6/14/2016} \code{6/15/2015} \code{6/16/2015} \code{6/16/2016} \code{6/17/2015} \code{6/18/2007} \code{6/2/2015} \code{6/20/2007} \code{6/20/2016} \code{6/21/2007} \code{6/21/2016} \code{6/22/2016} \code{6/26/2007} \code{6/27/2007} \code{6/28/2007} \code{6/29/2007} \code{6/29/2016} \code{6/3/2015} \code{6/4/2007} \code{6/5/2007} \code{6/6/2007} \code{6/7/2007} \code{7/4/2001} \code{7/4/2007}}
    \item{\code{PollTolClass.1.tot}}{a numeric vector}
    \item{\code{PollTolClass.2.tot}}{a numeric vector}
    \item{\code{PollTolClass.3.tot}}{a numeric vector}
    \item{\code{PollTolClass.11.tot}}{a numeric vector}
    \item{\code{pHClass.2.tot}}{a numeric vector}
    \item{\code{pHClass.3.tot}}{a numeric vector}
    \item{\code{pHClass.4.tot}}{a numeric vector}
    \item{\code{pHClass.5.tot}}{a numeric vector}
    \item{\code{pHClass.6.tot}}{a numeric vector}
    \item{\code{SalinityClass.1.tot}}{a numeric vector}
    \item{\code{SalinityClass.2.tot}}{a numeric vector}
    \item{\code{SalinityClass.3.tot}}{a numeric vector}
    \item{\code{SalinityClass.4.tot}}{a numeric vector}
    \item{\code{N_UptakeClass.1.tot}}{a numeric vector}
    \item{\code{N_UptakeClass.2.tot}}{a numeric vector}
    \item{\code{N_UptakeClass.3.tot}}{a numeric vector}
    \item{\code{N_UptakeClass.4.tot}}{a numeric vector}
    \item{\code{OxygenClass.1.tot}}{a numeric vector}
    \item{\code{OxygenClass.2.tot}}{a numeric vector}
    \item{\code{OxygenClass.3.tot}}{a numeric vector}
    \item{\code{OxygenClass.4.tot}}{a numeric vector}
    \item{\code{OxygenClass.5.tot}}{a numeric vector}
    \item{\code{SaprobityClass.1.tot}}{a numeric vector}
    \item{\code{SaprobityClass.2.tot}}{a numeric vector}
    \item{\code{SaprobityClass.3.tot}}{a numeric vector}
    \item{\code{SaprobityClass.4.tot}}{a numeric vector}
    \item{\code{SaprobityClass.5.tot}}{a numeric vector}
    \item{\code{TrophicClass.1.tot}}{a numeric vector}
    \item{\code{TrophicClass.2.tot}}{a numeric vector}
    \item{\code{TrophicClass.3.tot}}{a numeric vector}
    \item{\code{TrophicClass.4.tot}}{a numeric vector}
    \item{\code{TrophicClass.5.tot}}{a numeric vector}
    \item{\code{TrophicClass.6.tot}}{a numeric vector}
    \item{\code{TrophicClass.7.tot}}{a numeric vector}
    \item{\code{MoistureClass.1.tot}}{a numeric vector}
    \item{\code{MoistureClass.2.tot}}{a numeric vector}
    \item{\code{MoistureClass.3.tot}}{a numeric vector}
    \item{\code{MoistureClass.4.tot}}{a numeric vector}
    \item{\code{MoistureClass.5.tot}}{a numeric vector}
    \item{\code{MotilityClass.H.tot}}{a numeric vector}
    \item{\code{MotilityClass.M.tot}}{a numeric vector}
    \item{\code{MotilityClass.N.tot}}{a numeric vector}
    \item{\code{MotilityClass.V.tot}}{a numeric vector}
    \item{\code{PhosphorusClass.H.tot}}{a numeric vector}
    \item{\code{PhosphorusClass.L.tot}}{a numeric vector}
    \item{\code{NitrogenClass.H.tot}}{a numeric vector}
    \item{\code{NitrogenClass.L.tot}}{a numeric vector}
    \item{\code{COMID_NHD2}}{a numeric vector}
    \item{\code{ElevCategory}}{a factor with levels \code{HI} \code{LO}}
    \item{\code{clust.hi}}{a numeric vector}
    \item{\code{clust.lo}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(data_AlgMetrics)
## maybe str(data_AlgMetrics) ; plot(data_AlgMetrics) ...
}
\keyword{datasets}
