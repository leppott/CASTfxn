\name{data_CoOccur_AZ_Hi}
\alias{data_CoOccur_AZ_Hi}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("data_CoOccur_AZ_Hi")}
\format{
  A data frame with 351 observations on the following 285 variables.
  \describe{
    \item{\code{StationID_Master}}{a factor with levels \code{BWBRO029.91} \code{BWBSR034.68} \code{BWBSR037.79} \code{BWBWR025.86} \code{BWBWR038.52} \code{BWFRA000.04} \code{BWPEE003.51} \code{BWTRT002.43} \code{CGBDW000.99} \code{CGVGR039.41} \code{CGVGR044.58} \code{CGVGR052.23} \code{MGAFR087.06} \code{MGAFR087.35} \code{MGAFR110.01} \code{MGHSR002.67} \code{MGHSR048.20} \code{MGHSR058.80} \code{MGLAS004.52} \code{MGLSY000.42} \code{MGLSY000.91} \code{MGSSW000.38} \code{MGSYD009.13} \code{MGSYD009.51} \code{SCCIE002.89} \code{SCSCR034.56} \code{SCSCR039.63} \code{SCSCR103.45} \code{SCSON014.52} \code{SCSON015.35} \code{SCSON016.78} \code{SCSYR004.21} \code{SPARA010.19} \code{SPARA013.78} \code{SPARA019.41} \code{SPARA026.35} \code{SPBAS001.64} \code{SPBHC004.31} \code{SPHSC010.67} \code{SPSPR006.75} \code{SPSPR014.18} \code{SPSPR015.42} \code{SPSPR126.35} \code{SPSPR150.09} \code{SRCGN009.78} \code{SRCHE011.90} \code{SRCHE013.65} \code{SRCOO001.92} \code{SRDEV000.38} \code{SRPNL005.12} \code{SRSLR139.75} \code{SRSPI011.63} \code{SRTON053.87} \code{SRWPN004.47} \code{SRWRK002.83} \code{UGBLR008.19} \code{UGBON002.00} \code{UGBON003.68} \code{UGBON006.41} \code{UGEAG011.09} \code{UGEAG013.81} \code{UGEAG015.23} \code{UGEAG040.33} \code{UGGLR446.02} \code{UGGLR448.61} \code{UGGLR451.46} \code{UGGLR452.43} \code{UGSFR006.08} \code{UGSFR006.42} \code{UGSFR019.04} \code{UGSFR034.57} \code{VREVR023.23} \code{VREVR023.59} \code{VRFOS011.88} \code{VRFOS014.33} \code{VROAK013.95} \code{VROAK031.38} \code{VROAK034.02} \code{VRPIE008.19} \code{VRROU002.93} \code{VRSPN002.92} \code{VRSYH000.25} \code{VRSYW001.72} \code{VRVER137.53} \code{VRVER165.07} \code{VRWBV002.97} \code{VRWBV006.54} \code{VRWBV012.56} \code{VRWCL005.10} \code{VRWCL010.66}}
    \item{\code{Group}}{a numeric vector}
    \item{\code{SampYear}}{a numeric vector}
    \item{\code{ChemSampleID}}{a factor with levels \code{BWBRO029.91_2/25/2010} \code{BWBRO029.91_3/13/2013} \code{BWBRO029.91_4/20/2010} \code{BWBRO029.91_4/29/2013} \code{BWBSR034.68_2/24/2010} \code{BWBSR034.68_5/18/2010} \code{BWBSR037.79_2/24/2010} \code{BWBSR037.79_3/6/2013} \code{BWBSR037.79_4/21/2010} \code{BWBSR037.79_5/16/2013} \code{BWBWR025.86_2/22/2010} \code{BWBWR025.86_6/10/2010} \code{BWBWR038.52_2/22/2010} \code{BWBWR038.52_6/10/2010} \code{BWFRA000.04_2/25/2010} \code{BWFRA000.04_4/19/2010} \code{BWPEE003.51_2/14/2013} \code{BWPEE003.51_4/2/2013} \code{BWTRT002.43_3/6/2013} \code{BWTRT002.43_4/1/2010} \code{BWTRT002.43_5/16/2013} \code{BWTRT002.43_5/19/2010} \code{BWTRT002.43_6/11/2010} \code{CGBDW000.99_3/9/2010} \code{CGBDW000.99_4/27/2010} \code{CGVGR039.41_3/9/2010} \code{CGVGR039.41_4/27/2010} \code{CGVGR039.41_6/15/2010} \code{CGVGR044.58_2/26/2013} \code{CGVGR044.58_4/24/2013} \code{CGVGR044.58_6/16/2010} \code{CGVGR052.23_3/9/2010} \code{CGVGR052.23_4/27/2010} \code{CGVGR052.23_6/15/2010} \code{MGAFR087.06_2/14/2012} \code{MGAFR087.06_2/3/2009} \code{MGAFR087.06_4/6/2009} \code{MGAFR087.06_5/23/2012} \code{MGAFR087.35_2/14/2012} \code{MGAFR087.35_5/23/2012} \code{MGAFR110.01_1/27/2015} \code{MGAFR110.01_2/3/2009} \code{MGAFR110.01_4/14/2015} \code{MGAFR110.01_4/4/2016} \code{MGAFR110.01_4/6/2009} \code{MGHSR002.67_1/28/2013} \code{MGHSR002.67_5/16/2013} \code{MGHSR002.67_9/16/2013} \code{MGHSR048.20_2/4/2009} \code{MGHSR048.20_2/4/2015} \code{MGHSR048.20_3/28/2012} \code{MGHSR048.20_4/15/2015} \code{MGHSR048.20_6/4/2009} \code{MGHSR048.20_6/6/2012} \code{MGHSR058.80_2/4/2009} \code{MGHSR058.80_2/4/2015} \code{MGHSR058.80_4/15/2015} \code{MGHSR058.80_4/7/2009} \code{MGLAS004.52_1/20/2012} \code{MGLAS004.52_2/5/2009} \code{MGLAS004.52_3/27/2012} \code{MGLAS004.52_4/8/2009} \code{MGLAS004.52_6/12/2012} \code{MGLSY000.42_2/5/2009} \code{MGLSY000.42_2/7/2013} \code{MGLSY000.42_4/25/2013} \code{MGLSY000.42_4/8/2009} \code{MGLSY000.91_4/25/2013} \code{MGSSW000.38_1/24/2013} \code{MGSSW000.38_1/27/2015} \code{MGSSW000.38_4/22/2015} \code{MGSSW000.38_4/29/2013} \code{MGSYD009.13_1/20/2012} \code{MGSYD009.13_3/27/2012} \code{MGSYD009.13_6/12/2012} \code{MGSYD009.51_4/21/2009} \code{SCCIE002.89_2/27/2013} \code{SCCIE002.89_4/18/2013} \code{SCSCR034.56_2/10/2009} \code{SCSCR034.56_6/10/2009} \code{SCSCR039.63_1/29/2015} \code{SCSCR039.63_11/17/2014} \code{SCSCR039.63_2/24/2011} \code{SCSCR039.63_4/14/2014} \code{SCSCR039.63_4/17/2013} \code{SCSCR039.63_4/18/2012} \code{SCSCR039.63_4/29/2015} \code{SCSCR039.63_4/6/2016} \code{SCSCR039.63_5/12/2011} \code{SCSCR039.63_8/27/2014} \code{SCSCR103.45_2/23/2011} \code{SCSCR103.45_4/16/2012} \code{SCSCR103.45_4/6/2016} \code{SCSCR103.45_4/7/2015} \code{SCSCR103.45_5/11/2011} \code{SCSON014.52_2/15/2006} \code{SCSON014.52_3/4/2009} \code{SCSON014.52_4/12/2006} \code{SCSON014.52_4/27/2009} \code{SCSON015.35_2/13/2013} \code{SCSON015.35_4/18/2013} \code{SCSON016.78_1/18/2012} \code{SCSON016.78_2/23/2012} \code{SCSON016.78_4/17/2012} \code{SCSON016.78_5/11/2011} \code{SCSYR004.21_3/3/2009} \code{SCSYR004.21_4/28/2009} \code{SPARA010.19_1/26/2015} \code{SPARA010.19_4/21/2015} \code{SPARA013.78_2/11/2009} \code{SPARA013.78_2/6/2012} \code{SPARA013.78_4/2/2012} \code{SPARA013.78_6/24/2009} \code{SPARA019.41_3/31/2015} \code{SPARA019.41_6/11/2015} \code{SPARA026.35_3/26/2009} \code{SPARA026.35_3/5/2013} \code{SPARA026.35_4/4/2013} \code{SPARA026.35_5/14/2009} \code{SPBAS001.64_2/7/2012} \code{SPBAS001.64_4/2/2013} \code{SPBAS001.64_4/4/2012} \code{SPBHC004.31_2/14/2013} \code{SPBHC004.31_4/16/2013} \code{SPHSC010.67_2/7/2012} \code{SPHSC010.67_3/24/2009} \code{SPHSC010.67_4/4/2012} \code{SPHSC010.67_5/11/2009} \code{SPSPR006.75_2/6/2012} \code{SPSPR006.75_4/3/2012} \code{SPSPR014.18_4/15/2014} \code{SPSPR014.18_4/3/2013} \code{SPSPR014.18_4/5/2016} \code{SPSPR014.18_4/6/2015} \code{SPSPR015.42_2/22/2013} \code{SPSPR015.42_2/6/2012} \code{SPSPR015.42_4/15/2014} \code{SPSPR015.42_4/3/2012} \code{SPSPR015.42_4/3/2013} \code{SPSPR015.42_4/5/2016} \code{SPSPR015.42_4/6/2015} \code{SPSPR126.35_1/18/2012} \code{SPSPR126.35_2/22/2012} \code{SPSPR126.35_4/16/2012} \code{SPSPR126.35_5/28/2009} \code{SPSPR150.09_10/20/2015} \code{SPSPR150.09_11/17/2015} \code{SPSPR150.09_3/10/2015} \code{SPSPR150.09_6/15/2015} \code{SRCGN009.78_11/18/2008} \code{SRCGN009.78_5/2/2008} \code{SRCGN009.78_6/17/2008} \code{SRCHE011.90_3/20/2013} \code{SRCHE011.90_5/8/2013} \code{SRCHE013.65_3/20/2013} \code{SRCHE013.65_3/21/2008} \code{SRCHE013.65_3/23/2011} \code{SRCHE013.65_5/1/2008} \code{SRCHE013.65_5/23/2011} \code{SRCHE013.65_5/8/2013} \code{SRCHE013.65_6/18/2008} \code{SRCOO001.92_3/21/2008} \code{SRCOO001.92_4/20/2008} \code{SRCOO001.92_6/18/2008} \code{SRDEV000.38_2/9/2016} \code{SRDEV000.38_3/20/2013} \code{SRDEV000.38_3/23/2011} \code{SRDEV000.38_5/17/2016} \code{SRDEV000.38_5/6/2013} \code{SRDEV000.38_6/1/2011} \code{SRPNL005.12_4/23/2008} \code{SRPNL005.12_6/17/2008} \code{SRPNL005.12_9/30/2008} \code{SRSLR139.75_3/24/2011} \code{SRSLR139.75_5/25/2011} \code{SRSPI011.63_11/5/2008} \code{SRSPI011.63_3/26/2008} \code{SRSPI011.63_5/12/2008} \code{SRTON053.87_11/4/2008} \code{SRTON053.87_3/25/2008} \code{SRTON053.87_5/1/2008} \code{SRTON053.87_5/15/2008} \code{SRTON053.87_5/27/2008} \code{SRTON053.87_6/24/2008} \code{SRTON053.87_7/7/2008} \code{SRTON053.87_8/12/2008} \code{SRTON053.87_9/3/2008} \code{SRWPN004.47_3/20/2008} \code{SRWPN004.47_4/15/2008} \code{SRWPN004.47_4/6/2011} \code{SRWPN004.47_6/17/2008} \code{SRWPN004.47_6/2/2011} \code{SRWPN004.47_9/30/2008} \code{SRWRK002.83_3/8/2016} \code{SRWRK002.83_6/16/2016} \code{UGBLR008.19_2/29/2012} \code{UGBLR008.19_5/15/2012} \code{UGBLR008.19_6/16/2009} \code{UGBLR008.19_6/25/2009} \code{UGBLR008.19_8/9/2005} \code{UGBON002.00_6/3/2013} \code{UGBON003.68_4/25/2006} \code{UGBON006.41_1/7/2013} \code{UGBON006.41_2/27/2013} \code{UGBON006.41_3/12/2009} \code{UGBON006.41_5/13/2009} \code{UGBON006.41_5/7/2013} \code{UGEAG011.09_1/7/2013} \code{UGEAG011.09_2/27/2013} \code{UGEAG011.09_3/18/2009} \code{UGEAG011.09_4/28/2009} \code{UGEAG011.09_6/4/2013} \code{UGEAG013.81_3/24/2015} \code{UGEAG013.81_5/19/2015} \code{UGEAG015.23_3/18/2009} \code{UGEAG015.23_4/29/2009} \code{UGEAG040.33_2/29/2012} \code{UGEAG040.33_3/1/2006} \code{UGEAG040.33_5/16/2012} \code{UGEAG040.33_5/17/2006} \code{UGGLR446.02_1/9/2013} \code{UGGLR446.02_2/28/2013} \code{UGGLR446.02_5/7/2013} \code{UGGLR448.61_10/25/2007} \code{UGGLR448.61_11/16/2007} \code{UGGLR448.61_12/8/2007} \code{UGGLR448.61_2/14/2007} \code{UGGLR448.61_3/1/2007} \code{UGGLR448.61_3/25/2007} \code{UGGLR448.61_6/29/2007} \code{UGGLR448.61_8/6/2007} \code{UGGLR451.46_10/25/2007} \code{UGGLR451.46_11/13/2007} \code{UGGLR451.46_12/8/2007} \code{UGGLR451.46_2/14/2007} \code{UGGLR451.46_3/1/2007} \code{UGGLR451.46_3/25/2007} \code{UGGLR451.46_6/29/2007} \code{UGGLR451.46_8/6/2007} \code{UGGLR452.43_10/22/2007} \code{UGGLR452.43_11/13/2007} \code{UGGLR452.43_12/8/2007} \code{UGGLR452.43_2/14/2007} \code{UGGLR452.43_2/28/2007} \code{UGGLR452.43_3/25/2007} \code{UGGLR452.43_6/27/2007} \code{UGGLR452.43_8/6/2007} \code{UGSFR006.08_3/18/2009} \code{UGSFR006.08_4/29/2009} \code{UGSFR006.42_1/8/2013} \code{UGSFR006.42_10/3/2006} \code{UGSFR006.42_2/15/2006} \code{UGSFR006.42_2/28/2013} \code{UGSFR006.42_3/23/2015} \code{UGSFR006.42_4/18/2006} \code{UGSFR006.42_5/18/2015} \code{UGSFR006.42_6/4/2013} \code{UGSFR006.42_9/5/2006} \code{UGSFR019.04_2/15/2006} \code{UGSFR019.04_2/28/2012} \code{UGSFR019.04_3/25/2009} \code{UGSFR019.04_4/18/2006} \code{UGSFR019.04_5/12/2009} \code{UGSFR019.04_5/15/2012} \code{UGSFR019.04_6/25/2009} \code{UGSFR019.04_9/5/2006} \code{UGSFR034.57_1/8/2013} \code{UGSFR034.57_2/28/2006} \code{UGSFR034.57_2/28/2013} \code{UGSFR034.57_3/25/2009} \code{UGSFR034.57_5/12/2009} \code{UGSFR034.57_5/18/2006} \code{UGSFR034.57_5/22/2013} \code{VREVR023.23_3/15/2016} \code{VREVR023.23_5/3/2016} \code{VREVR023.59_11/6/2008} \code{VREVR023.59_3/19/2013} \code{VREVR023.59_3/26/2008} \code{VREVR023.59_5/20/2013} \code{VREVR023.59_6/25/2008} \code{VRFOS011.88_3/18/2013} \code{VRFOS011.88_4/2/2008} \code{VRFOS011.88_5/1/2013} \code{VRFOS011.88_6/12/2008} \code{VRFOS014.33_6/13/2008} \code{VROAK013.95_3/24/2011} \code{VROAK013.95_4/27/2011} \code{VROAK031.38_3/19/2008} \code{VROAK031.38_6/4/2008} \code{VROAK031.38_7/1/2005} \code{VROAK031.38_7/2/2005} \code{VROAK031.38_7/3/2005} \code{VROAK031.38_7/4/2005} \code{VROAK031.38_7/5/2005} \code{VROAK034.02_2/25/2013} \code{VROAK034.02_4/23/2013} \code{VRPIE008.19_3/18/2013} \code{VRPIE008.19_4/11/2011} \code{VRPIE008.19_5/2/2013} \code{VRPIE008.19_6/23/2011} \code{VRROU002.93_1/24/2013} \code{VRROU002.93_3/25/2008} \code{VRROU002.93_4/24/2013} \code{VRROU002.93_4/30/2008} \code{VRSPN002.92_2/10/2016} \code{VRSPN002.92_5/10/2016} \code{VRSYH000.25_3/25/2008} \code{VRSYH000.25_5/16/2008} \code{VRSYW001.72_2/25/2013} \code{VRSYW001.72_3/20/2008} \code{VRSYW001.72_4/22/2013} \code{VRSYW001.72_6/11/2008} \code{VRVER137.53_2/25/2013} \code{VRVER137.53_4/22/2013} \code{VRVER165.07_11/3/2008} \code{VRVER165.07_2/22/2011} \code{VRVER165.07_2/25/2013} \code{VRVER165.07_3/20/2008} \code{VRVER165.07_4/23/2013} \code{VRVER165.07_4/26/2011} \code{VRVER165.07_6/5/2008} \code{VRWBV002.97_2/7/2013} \code{VRWBV002.97_4/28/2008} \code{VRWBV002.97_4/3/2008} \code{VRWBV002.97_4/30/2013} \code{VRWBV006.54_2/7/2013} \code{VRWBV006.54_4/23/2013} \code{VRWBV012.56_1/8/2008} \code{VRWBV012.56_11/6/2008} \code{VRWBV012.56_2/6/2013} \code{VRWBV012.56_4/22/2013} \code{VRWBV012.56_4/30/2008} \code{VRWCL005.10_4/29/2008} \code{VRWCL005.10_4/3/2008} \code{VRWCL010.66_2/25/2014} \code{VRWCL010.66_3/24/2011} \code{VRWCL010.66_4/22/2014} \code{VRWCL010.66_6/9/2011}}
    \item{\code{SampDate}}{a factor with levels \code{1/18/2012} \code{1/20/2012} \code{1/24/2013} \code{1/26/2015} \code{1/27/2015} \code{1/28/2013} \code{1/29/2015} \code{1/7/2013} \code{1/8/2008} \code{1/8/2013} \code{1/9/2013} \code{10/20/2015} \code{10/22/2007} \code{10/25/2007} \code{10/3/2006} \code{11/13/2007} \code{11/16/2007} \code{11/17/2014} \code{11/17/2015} \code{11/18/2008} \code{11/3/2008} \code{11/4/2008} \code{11/5/2008} \code{11/6/2008} \code{12/8/2007} \code{2/10/2009} \code{2/10/2016} \code{2/11/2009} \code{2/13/2013} \code{2/14/2007} \code{2/14/2012} \code{2/14/2013} \code{2/15/2006} \code{2/22/2010} \code{2/22/2011} \code{2/22/2012} \code{2/22/2013} \code{2/23/2011} \code{2/23/2012} \code{2/24/2010} \code{2/24/2011} \code{2/25/2010} \code{2/25/2013} \code{2/25/2014} \code{2/26/2013} \code{2/27/2013} \code{2/28/2006} \code{2/28/2007} \code{2/28/2012} \code{2/28/2013} \code{2/29/2012} \code{2/3/2009} \code{2/4/2009} \code{2/4/2015} \code{2/5/2009} \code{2/6/2012} \code{2/6/2013} \code{2/7/2012} \code{2/7/2013} \code{2/9/2016} \code{3/1/2006} \code{3/1/2007} \code{3/10/2015} \code{3/12/2009} \code{3/13/2013} \code{3/15/2016} \code{3/18/2009} \code{3/18/2013} \code{3/19/2008} \code{3/19/2013} \code{3/20/2008} \code{3/20/2013} \code{3/21/2008} \code{3/23/2011} \code{3/23/2015} \code{3/24/2009} \code{3/24/2011} \code{3/24/2015} \code{3/25/2007} \code{3/25/2008} \code{3/25/2009} \code{3/26/2008} \code{3/26/2009} \code{3/27/2012} \code{3/28/2012} \code{3/3/2009} \code{3/31/2015} \code{3/4/2009} \code{3/5/2013} \code{3/6/2013} \code{3/8/2016} \code{3/9/2010} \code{4/1/2010} \code{4/11/2011} \code{4/12/2006} \code{4/14/2014} \code{4/14/2015} \code{4/15/2008} \code{4/15/2014} \code{4/15/2015} \code{4/16/2012} \code{4/16/2013} \code{4/17/2012} \code{4/17/2013} \code{4/18/2006} \code{4/18/2012} \code{4/18/2013} \code{4/19/2010} \code{4/2/2008} \code{4/2/2012} \code{4/2/2013} \code{4/20/2008} \code{4/20/2010} \code{4/21/2009} \code{4/21/2010} \code{4/21/2015} \code{4/22/2013} \code{4/22/2014} \code{4/22/2015} \code{4/23/2008} \code{4/23/2013} \code{4/24/2013} \code{4/25/2006} \code{4/25/2013} \code{4/26/2011} \code{4/27/2009} \code{4/27/2010} \code{4/27/2011} \code{4/28/2008} \code{4/28/2009} \code{4/29/2008} \code{4/29/2009} \code{4/29/2013} \code{4/29/2015} \code{4/3/2008} \code{4/3/2012} \code{4/3/2013} \code{4/30/2008} \code{4/30/2013} \code{4/4/2012} \code{4/4/2013} \code{4/4/2016} \code{4/5/2016} \code{4/6/2009} \code{4/6/2011} \code{4/6/2015} \code{4/6/2016} \code{4/7/2009} \code{4/7/2015} \code{4/8/2009} \code{5/1/2008} \code{5/1/2013} \code{5/10/2016} \code{5/11/2009} \code{5/11/2011} \code{5/12/2008} \code{5/12/2009} \code{5/12/2011} \code{5/13/2009} \code{5/14/2009} \code{5/15/2008} \code{5/15/2012} \code{5/16/2008} \code{5/16/2012} \code{5/16/2013} \code{5/17/2006} \code{5/17/2016} \code{5/18/2006} \code{5/18/2010} \code{5/18/2015} \code{5/19/2010} \code{5/19/2015} \code{5/2/2008} \code{5/2/2013} \code{5/20/2013} \code{5/22/2013} \code{5/23/2011} \code{5/23/2012} \code{5/25/2011} \code{5/27/2008} \code{5/28/2009} \code{5/3/2016} \code{5/6/2013} \code{5/7/2013} \code{5/8/2013} \code{6/1/2011} \code{6/10/2009} \code{6/10/2010} \code{6/11/2008} \code{6/11/2010} \code{6/11/2015} \code{6/12/2008} \code{6/12/2012} \code{6/13/2008} \code{6/15/2010} \code{6/15/2015} \code{6/16/2009} \code{6/16/2010} \code{6/16/2016} \code{6/17/2008} \code{6/18/2008} \code{6/2/2011} \code{6/23/2011} \code{6/24/2008} \code{6/24/2009} \code{6/25/2008} \code{6/25/2009} \code{6/27/2007} \code{6/29/2007} \code{6/3/2013} \code{6/4/2008} \code{6/4/2009} \code{6/4/2013} \code{6/5/2008} \code{6/6/2012} \code{6/9/2011} \code{7/1/2005} \code{7/2/2005} \code{7/3/2005} \code{7/4/2005} \code{7/5/2005} \code{7/7/2008} \code{8/12/2008} \code{8/27/2014} \code{8/6/2007} \code{8/9/2005} \code{9/16/2013} \code{9/3/2008} \code{9/30/2008} \code{9/5/2006}}
    \item{\code{BenCollDate}}{a factor with levels \code{4/12/2006} \code{4/14/2014} \code{4/14/2015} \code{4/15/2008} \code{4/15/2014} \code{4/15/2015} \code{4/16/2012} \code{4/16/2013} \code{4/17/2012} \code{4/17/2013} \code{4/18/2006} \code{4/18/2012} \code{4/18/2013} \code{4/19/2010} \code{4/2/2012} \code{4/2/2013} \code{4/20/2008} \code{4/20/2010} \code{4/21/2005} \code{4/21/2009} \code{4/21/2015} \code{4/22/2013} \code{4/22/2014} \code{4/22/2015} \code{4/23/2008} \code{4/23/2013} \code{4/24/2013} \code{4/25/2006} \code{4/25/2013} \code{4/26/2011} \code{4/27/2009} \code{4/27/2010} \code{4/27/2011} \code{4/28/2008} \code{4/28/2009} \code{4/29/2008} \code{4/29/2009} \code{4/29/2013} \code{4/29/2015} \code{4/3/2012} \code{4/3/2013} \code{4/30/2008} \code{4/30/2013} \code{4/4/2012} \code{4/4/2013} \code{4/4/2016} \code{4/5/2016} \code{4/6/2009} \code{4/6/2015} \code{4/6/2016} \code{4/7/2009} \code{4/7/2015} \code{4/8/2009} \code{5/1/2008} \code{5/1/2013} \code{5/10/2016} \code{5/11/2009} \code{5/11/2011} \code{5/12/2008} \code{5/12/2009} \code{5/12/2011} \code{5/13/2009} \code{5/14/2007} \code{5/14/2009} \code{5/15/2007} \code{5/15/2008} \code{5/15/2012} \code{5/16/2008} \code{5/16/2012} \code{5/16/2013} \code{5/17/2006} \code{5/17/2016} \code{5/18/2006} \code{5/18/2015} \code{5/19/2015} \code{5/2/2008} \code{5/2/2013} \code{5/20/2013} \code{5/22/2013} \code{5/23/2011} \code{5/23/2012} \code{5/24/2005} \code{5/25/2011} \code{5/28/2009} \code{5/3/2016} \code{5/6/2013} \code{5/7/2013} \code{5/8/2013} \code{6/1/2010} \code{6/1/2011} \code{6/10/2009} \code{6/10/2010} \code{6/11/2008} \code{6/11/2010} \code{6/11/2015} \code{6/12/2008} \code{6/12/2012} \code{6/13/2008} \code{6/14/2010} \code{6/15/2010} \code{6/15/2015} \code{6/16/2009} \code{6/16/2010} \code{6/16/2016} \code{6/2/2011} \code{6/23/2011} \code{6/24/2009} \code{6/25/2008} \code{6/3/2013} \code{6/4/2008} \code{6/4/2009} \code{6/4/2013} \code{6/5/2008} \code{6/6/2012} \code{6/9/2011}}
    \item{\code{IBI}}{a numeric vector}
    \item{\code{X124Trichlorobenzene_uf_ug_L}}{a logical vector}
    \item{\code{X12Dichlorobenzene_uf_ug_L}}{a logical vector}
    \item{\code{X13Dichlorobenzene_uf_ug_L}}{a logical vector}
    \item{\code{X14Dichlorobenzene_uf_ug_L}}{a logical vector}
    \item{\code{X245Trichlorophenol_uf_ug_L}}{a logical vector}
    \item{\code{X246Trichlorophenol_uf_ug_L}}{a logical vector}
    \item{\code{X24Dichlorophenol_uf_ug_L}}{a logical vector}
    \item{\code{X24Dimethylphenol_uf_ug_L}}{a logical vector}
    \item{\code{X24Dinitrophenol_uf_ug_L}}{a logical vector}
    \item{\code{X24Dinitrotoluene_uf_ug_L}}{a logical vector}
    \item{\code{X26Dinitrotoluene_uf_ug_L}}{a logical vector}
    \item{\code{X2Chlorophenol_uf_ug_L}}{a logical vector}
    \item{\code{X2Methylnaphthalene_uf_ug_L}}{a logical vector}
    \item{\code{X2Methylphenol_uf_ug_L}}{a logical vector}
    \item{\code{X2Nitroaniline_uf_ug_L}}{a logical vector}
    \item{\code{X2Nitrophenol_uf_ug_L}}{a logical vector}
    \item{\code{X33primeDichlorobenzidine_uf_ug_L}}{a logical vector}
    \item{\code{X3Nitroaniline_uf_ug_L}}{a logical vector}
    \item{\code{X4Chlorophenylphenylether_uf_ug_L}}{a logical vector}
    \item{\code{X4Methylphenol_uf_ug_L}}{a logical vector}
    \item{\code{X4Nitroaniline_uf_ug_L}}{a logical vector}
    \item{\code{Acenaphthene_uf_ug_L}}{a logical vector}
    \item{\code{Acenaphthylene_uf_ug_L}}{a logical vector}
    \item{\code{Aldrin_uf_ug_L}}{a logical vector}
    \item{\code{Alkalinity_phenolphthalein_uf_mg_L}}{a numeric vector}
    \item{\code{Alpha_grossAsUNatural_calc_pCi_L}}{a logical vector}
    \item{\code{Alpha_grossAsUNatural_uf_pCi_L}}{a logical vector}
    \item{\code{alphaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
    \item{\code{Aluminum_f_mg_L}}{a logical vector}
    \item{\code{Aluminum_uf_ug_L}}{a numeric vector}
    \item{\code{Ammonia_N_calc_mg_kg}}{a logical vector}
    \item{\code{Aniline_uf_ug_L}}{a logical vector}
    \item{\code{Anthracene_uf_ug_L}}{a logical vector}
    \item{\code{Antimony_f_mg_L}}{a numeric vector}
    \item{\code{Antimony_uf_mg_L}}{a numeric vector}
    \item{\code{Arsenic_inorg_calc_mg_kg}}{a logical vector}
    \item{\code{Arsenic_inorg_f_ug_L}}{a numeric vector}
    \item{\code{Arsenic_inorg_susp_ug_L}}{a logical vector}
    \item{\code{Arsenic_inorg_uf_ug_L}}{a numeric vector}
    \item{\code{Azobenzene_uf_ug_L}}{a logical vector}
    \item{\code{Barium_cmpds_calc_mg_kg}}{a logical vector}
    \item{\code{Barium_cmpds_f_ug_L}}{a numeric vector}
    \item{\code{Barium_cmpds_uf_pCi_L}}{a logical vector}
    \item{\code{Barium_cmpds_uf_ug_L}}{a numeric vector}
    \item{\code{BarometricPressure_mmHg}}{a numeric vector}
    \item{\code{Benzo_a_antracene_uf_ug_L}}{a logical vector}
    \item{\code{Benzo_a_pyrene_uf_ug_L}}{a logical vector}
    \item{\code{Benzo_b_flouranthene_uf_ug_L}}{a logical vector}
    \item{\code{Benzo_ghi_perylene_uf_ug_L}}{a logical vector}
    \item{\code{Benzo_k_fluoranthene_uf_ug_L}}{a logical vector}
    \item{\code{BenzoicAcid_uf_ug_L}}{a logical vector}
    \item{\code{BenzylAlcohol_uf_ug_L}}{a logical vector}
    \item{\code{Beryllium_cmpds_calc_mg_kg}}{a logical vector}
    \item{\code{Beryllium_cmpds_f_mg_L}}{a logical vector}
    \item{\code{Beryllium_cmpds_uf_mg_L}}{a numeric vector}
    \item{\code{Beta_radiation_calc_pCi_L}}{a logical vector}
    \item{\code{Beta_radiation_uf_pCi_L}}{a logical vector}
    \item{\code{betaChloronapthalene_uf_ug_L}}{a logical vector}
    \item{\code{betaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
    \item{\code{Bis_2chloroethoxy_methane_uf_ug_L}}{a logical vector}
    \item{\code{Bis_2chloroisopropyl_ether_uf_ug_L}}{a logical vector}
    \item{\code{Bis_chloroethyl_ether_uf_ug_L}}{a logical vector}
    \item{\code{Bismuth_f_pCi_L}}{a logical vector}
    \item{\code{BOD_uf_mg_L}}{a numeric vector}
    \item{\code{Bolstar_uf_ug_L}}{a logical vector}
    \item{\code{Boron_borates_f_mg_L}}{a logical vector}
    \item{\code{Boron_borates_uf_mg_L}}{a numeric vector}
    \item{\code{ButylBenzylPhthalate_uf_ug_L}}{a logical vector}
    \item{\code{CaCO3_calc_mg_L}}{a logical vector}
    \item{\code{CaCO3_f_mg_L}}{a logical vector}
    \item{\code{CaCO3_mg_L}}{a numeric vector}
    \item{\code{CaCO3_other_mg_L}}{a logical vector}
    \item{\code{CaCO3_uf_mg_L}}{a numeric vector}
    \item{\code{Cadmium_calc_mg_kg}}{a logical vector}
    \item{\code{Cadmium_f_mg_L}}{a numeric vector}
    \item{\code{Cadmium_uf_mg_L}}{a numeric vector}
    \item{\code{Caffeine_f_mg_L}}{a logical vector}
    \item{\code{Calcium_f_mg_L}}{a numeric vector}
    \item{\code{Calcium_uf_mg_L}}{a numeric vector}
    \item{\code{Carbazole_uf_ug_L}}{a logical vector}
    \item{\code{Carbon_calc_.}}{a logical vector}
    \item{\code{Carbon_f_mg_L}}{a logical vector}
    \item{\code{Carbon_uf_mg_L}}{a logical vector}
    \item{\code{Carbonate_uf_mg_L}}{a numeric vector}
    \item{\code{Cesium_134_uf_pCi_L}}{a logical vector}
    \item{\code{Cesium_137_uf_pCi_L}}{a logical vector}
    \item{\code{Chlordane_cis_uf_ug_L}}{a logical vector}
    \item{\code{Chlordane_gamma_uf_ug_L}}{a logical vector}
    \item{\code{Chlordane_uf_ug_L}}{a logical vector}
    \item{\code{Chloride_uf_mg_L}}{a numeric vector}
    \item{\code{Chlorine_other_mg_L}}{a logical vector}
    \item{\code{Chlorine_uf_mg_L}}{a numeric vector}
    \item{\code{Chlorophyll_a_uf_ug_L}}{a numeric vector}
    \item{\code{Chlorpyrifos_uf_ug_L}}{a logical vector}
    \item{\code{Chromium_calc_mg_kg}}{a logical vector}
    \item{\code{Chromium_f_mg_L}}{a logical vector}
    \item{\code{Chromium_mg_L}}{a logical vector}
    \item{\code{Chromium_uf_mg_L}}{a numeric vector}
    \item{\code{Chrysene_uf_ug_L}}{a logical vector}
    \item{\code{Cobalt_58_uf_pCi_L}}{a logical vector}
    \item{\code{Cobalt_60_uf_pCi_L}}{a logical vector}
    \item{\code{Cobalt_f_mg_L}}{a logical vector}
    \item{\code{Cobalt_uf_ug_L}}{a logical vector}
    \item{\code{Copper_f_ug_L}}{a numeric vector}
    \item{\code{Copper_uf_mg_kg}}{a logical vector}
    \item{\code{Copper_uf_ug_L}}{a numeric vector}
    \item{\code{Coumaphos_uf_ug_L}}{a logical vector}
    \item{\code{CrossSectionalArea_calc}}{a numeric vector}
    \item{\code{Cyanide_uf_mg_L}}{a logical vector}
    \item{\code{deltaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
    \item{\code{Demeton_o_uf_ug_L}}{a logical vector}
    \item{\code{Demeton_s_total_uf_ug_L}}{a logical vector}
    \item{\code{Depth_ft}}{a numeric vector}
    \item{\code{Di_2ethylhexylPhthalate_uf_ug_L}}{a logical vector}
    \item{\code{Di_n_octylPhthalate_uf_ug_L}}{a logical vector}
    \item{\code{Diazinon_uf_ug_L}}{a logical vector}
    \item{\code{Dibenz_ah_anthracene_uf_ug_L}}{a logical vector}
    \item{\code{Dibenzofuran_uf_ug_L}}{a logical vector}
    \item{\code{DibutylPhthalate_uf_ug_L}}{a logical vector}
    \item{\code{Dichlorvos_uf_ug_L}}{a logical vector}
    \item{\code{Dieldrin_uf_ug_L}}{a logical vector}
    \item{\code{DiethylPhthalate_uf_ug_L}}{a logical vector}
    \item{\code{Dimethoate_uf_ug_L}}{a logical vector}
    \item{\code{DimethylPhthalate_uf_ug_L}}{a logical vector}
    \item{\code{Dinitro_o_cresol_uf_ug_L}}{a logical vector}
    \item{\code{Disulfoton_uf_ug_L}}{a logical vector}
    \item{\code{DO_f_.}}{a numeric vector}
    \item{\code{DO_f_mg_L}}{a numeric vector}
    \item{\code{Ecoli_uf_CFU_100}}{a numeric vector}
    \item{\code{Ecoli_uf_mg_L}}{a logical vector}
    \item{\code{Endosulfan_alpha_uf_ug_L}}{a logical vector}
    \item{\code{Endosulfan_beta_uf_ug_L}}{a logical vector}
    \item{\code{EndosulfanSulfate_uf_ug_L}}{a logical vector}
    \item{\code{Endrin_uf_ug_L}}{a logical vector}
    \item{\code{EndrinAldehyde_uf_ug_L}}{a logical vector}
    \item{\code{EndrinKetone_uf_ug_L}}{a logical vector}
    \item{\code{Ethoprop_std_ug_L}}{a logical vector}
    \item{\code{Ethyl_p_nitrophenylPhenylphosphorothioate_uf_ug_L}}{a logical vector}
    \item{\code{Fensulfothion_uf_ug_L}}{a logical vector}
    \item{\code{Fenthion_uf_ug_L}}{a logical vector}
    \item{\code{Flow_calc_cfs}}{a logical vector}
    \item{\code{Flow_cfs}}{a numeric vector}
    \item{\code{Flow_ft_sec}}{a numeric vector}
    \item{\code{Fluoranthene_uf_ug_L}}{a logical vector}
    \item{\code{Fluorene_uf_ug_L}}{a logical vector}
    \item{\code{Fluoride_f_mg_L}}{a logical vector}
    \item{\code{Fluoride_uf_mg_L}}{a numeric vector}
    \item{\code{gammaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
    \item{\code{Hardness_CaCO3_MgCO3_calc_mg_L}}{a numeric vector}
    \item{\code{Hardness_CaCO3_MgCO3_f_mg_L}}{a logical vector}
    \item{\code{Heptachlor_uf_ug_L}}{a logical vector}
    \item{\code{HeptachlorEpoxide_uf_ug_L}}{a logical vector}
    \item{\code{Hexachlorobenzene_uf_ug_L}}{a logical vector}
    \item{\code{Hexachlorobutadiene_uf_ug_L}}{a logical vector}
    \item{\code{Hexachlorocyclopentadiene_uf_ug_L}}{a logical vector}
    \item{\code{Hexachloroethane_uf_ug_L}}{a logical vector}
    \item{\code{HydrogenCarbonate_uf_mg_L}}{a numeric vector}
    \item{\code{Indeno_123cd_pyrene_uf_ug_L}}{a logical vector}
    \item{\code{Iodine_131_uf_pCi_L}}{a logical vector}
    \item{\code{Iron_59_uf_pCi_L}}{a logical vector}
    \item{\code{Iron_f_mg_L}}{a logical vector}
    \item{\code{Iron_uf_ug_L}}{a numeric vector}
    \item{\code{Isophorone_uf_ug_L}}{a logical vector}
    \item{\code{Kjeldahl_N_calc_mg_kg}}{a logical vector}
    \item{\code{Kjeldahl_N_uf_mg_L}}{a numeric vector}
    \item{\code{Lanthanum_140_uf_pCi_L}}{a logical vector}
    \item{\code{Lead_214_uf_pCi_L}}{a logical vector}
    \item{\code{Lead_cmpdsInorg_calc_mg_kg}}{a logical vector}
    \item{\code{Lead_cmpdsInorg_f_mg_L}}{a numeric vector}
    \item{\code{Lead_cmpdsInorg_susp_mg_L}}{a logical vector}
    \item{\code{Lead_cmpdsInorg_uf_mg_L}}{a numeric vector}
    \item{\code{Magnesium_f_mg_L}}{a numeric vector}
    \item{\code{Magnesium_uf_mg_L}}{a numeric vector}
    \item{\code{Malathion_uf_ug_L}}{a logical vector}
    \item{\code{Manganese_54_uf_pCi_L}}{a logical vector}
    \item{\code{Manganese_f_ug_L}}{a numeric vector}
    \item{\code{Manganese_uf_ug_L}}{a numeric vector}
    \item{\code{Mercury_elemental_calc_mg_kg}}{a logical vector}
    \item{\code{Mercury_elemental_f_mg_L}}{a numeric vector}
    \item{\code{Mercury_elemental_susp_mg_kg}}{a logical vector}
    \item{\code{Mercury_elemental_uf_mg_L}}{a numeric vector}
    \item{\code{Mercury_elemental_uf_ng_g}}{a logical vector}
    \item{\code{Merphos_uf_ug_L}}{a logical vector}
    \item{\code{Methoxychlor_uf_ug_L}}{a logical vector}
    \item{\code{MethylAzinphos_uf_ug_L}}{a logical vector}
    \item{\code{Methylmercury_uf_ng_g}}{a logical vector}
    \item{\code{Methylmercury_uf_ng_L}}{a logical vector}
    \item{\code{MethylParathion_uf_ug_L}}{a logical vector}
    \item{\code{Mevinphos_uf_ug_L}}{a logical vector}
    \item{\code{Molybdenum_f_mg_L}}{a logical vector}
    \item{\code{Molybdenum_uf_ug_L}}{a logical vector}
    \item{\code{Monocrotophos_uf_ug_L}}{a logical vector}
    \item{\code{N_nitrosodi_N_propylamine_uf_ug_L}}{a logical vector}
    \item{\code{N_nitrosodiphenylamine_uf_ug_L}}{a logical vector}
    \item{\code{Naled_uf_ug_L}}{a logical vector}
    \item{\code{Naphthalene_uf_ug_L}}{a logical vector}
    \item{\code{NH3_N_uf_mg_L}}{a numeric vector}
    \item{\code{Nickel_calc_mg_kg}}{a logical vector}
    \item{\code{Nickel_f_mg_L}}{a logical vector}
    \item{\code{Nickel_uf_ug_L}}{a logical vector}
    \item{\code{Niobium_95_uf_pCi_L}}{a logical vector}
    \item{\code{Nitrobenzene_uf_ug_L}}{a logical vector}
    \item{\code{Nitrogen_uf_mg_L}}{a logical vector}
    \item{\code{NO2_N_uf_mg_L}}{a logical vector}
    \item{\code{NO2NO3_calc_mg_kg}}{a logical vector}
    \item{\code{NO3_N_uf_mg_L}}{a logical vector}
    \item{\code{NO3NO2_uf_mg_L}}{a numeric vector}
    \item{\code{p_BromodiphenylEther_uf_ug_L}}{a logical vector}
    \item{\code{p_Chloroaniline_uf_ug_L}}{a logical vector}
    \item{\code{p_Nitrophenol_uf_ug_L}}{a logical vector}
    \item{\code{p_p_primeDichlorodiphenyldichloroethane_uf_ug_L}}{a logical vector}
    \item{\code{p_p_primeDichlorodiphenyldichloroethylene_uf_ug_L}}{a logical vector}
    \item{\code{p_p_primeDichlorodiphenyltrichloroethane_uf_ug_L}}{a logical vector}
    \item{\code{ParachlorometaCresol_uf_ug_L}}{a logical vector}
    \item{\code{Parathion_uf_ug_L}}{a logical vector}
    \item{\code{Pentachlorophenol_uf_ug_L}}{a logical vector}
    \item{\code{Perchlorate_uf_ug_L}}{a numeric vector}
    \item{\code{pH_SU}}{a numeric vector}
    \item{\code{Phenanthrene_uf_ug_L}}{a logical vector}
    \item{\code{Phenol_uf_ug_L}}{a logical vector}
    \item{\code{PheophytinA_uf_ug_L}}{a numeric vector}
    \item{\code{Phorate_uf_ug_L}}{a logical vector}
    \item{\code{Phosphorus_calc_mg_kg}}{a logical vector}
    \item{\code{Phosphorus_uf_mg_L}}{a numeric vector}
    \item{\code{Potassium_40_uf_pCi_L}}{a logical vector}
    \item{\code{Potassium_f_mg_L}}{a numeric vector}
    \item{\code{Potassium_uf_mg_L}}{a numeric vector}
    \item{\code{Pyrene_uf_ug_L}}{a logical vector}
    \item{\code{Radium_226_228_calc_pCi_L}}{a logical vector}
    \item{\code{Radium_226_228_uf_pCi_L}}{a logical vector}
    \item{\code{Radium_226_uf_pCi_L}}{a logical vector}
    \item{\code{Radium_228_f_pCi_L}}{a logical vector}
    \item{\code{Radium_228_uf_pCi_L}}{a logical vector}
    \item{\code{RedoxPotential_uf_mV}}{a numeric vector}
    \item{\code{Ronnel_uf_ug_L}}{a logical vector}
    \item{\code{Selenium_cmpds_calc_mg_kg}}{a logical vector}
    \item{\code{Selenium_cmpds_f_mg_L}}{a logical vector}
    \item{\code{Selenium_cmpds_uf_mg_L}}{a numeric vector}
    \item{\code{Silica_uf_mg_L}}{a logical vector}
    \item{\code{Silicon_uf_ug_L}}{a logical vector}
    \item{\code{Silver_calc_mg_kg}}{a logical vector}
    \item{\code{Silver_f_mg_L}}{a logical vector}
    \item{\code{Silver_uf_mg_L}}{a logical vector}
    \item{\code{Sodium_f_mg_L}}{a numeric vector}
    \item{\code{Sodium_uf_mg_L}}{a numeric vector}
    \item{\code{SolidsInSediment_calc_.}}{a logical vector}
    \item{\code{SpecCond_umhos_cm}}{a numeric vector}
    \item{\code{StreamWidth_ft}}{a numeric vector}
    \item{\code{Strontium_f_mg_L}}{a logical vector}
    \item{\code{Sulfate_uf_mg_L}}{a numeric vector}
    \item{\code{TDS_calc_mg_L}}{a logical vector}
    \item{\code{TDS_f_mg_L}}{a numeric vector}
    \item{\code{Temp_degC}}{a numeric vector}
    \item{\code{Temp_degF}}{a logical vector}
    \item{\code{Tetrachlorovinphos_uf_ug_L}}{a logical vector}
    \item{\code{Tetraethyldithiopyrophosphate_uf_ug_L}}{a logical vector}
    \item{\code{Tetraethylpyrophosphate_uf_ug_L}}{a logical vector}
    \item{\code{Thallium_208_uf_pCi_L}}{a logical vector}
    \item{\code{Thallium_f_mg_L}}{a logical vector}
    \item{\code{Thallium_uf_mg_L}}{a logical vector}
    \item{\code{Tokuthion_uf_ug_L}}{a logical vector}
    \item{\code{Toxaphene_uf_ug_L}}{a logical vector}
    \item{\code{Trichloronate_uf_ug_L}}{a logical vector}
    \item{\code{TSS_coarse_susp_mg_L}}{a numeric vector}
    \item{\code{TSS_fine_susp_mg_L}}{a numeric vector}
    \item{\code{TSS_susp_mg_L}}{a numeric vector}
    \item{\code{Turbidity_NTU}}{a numeric vector}
    \item{\code{Uranium_natural_f_ug_L}}{a logical vector}
    \item{\code{Uranium_Natural_uf_pCi_L}}{a logical vector}
    \item{\code{Uranium_Natural_uf_ug_L}}{a logical vector}
    \item{\code{Vanadium_f_mg_L}}{a logical vector}
    \item{\code{Vanadium_uf_mg_L}}{a logical vector}
    \item{\code{Velocity_ft_sec}}{a numeric vector}
    \item{\code{Zinc_65_uf_pCi_L}}{a logical vector}
    \item{\code{Zinc_calc_mg_kg}}{a logical vector}
    \item{\code{Zinc_f_mg_L}}{a numeric vector}
    \item{\code{Zinc_uf_mg_L}}{a numeric vector}
    \item{\code{Zirconium_95_uf_pCi_L}}{a logical vector}
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
data(data_CoOccur_AZ_Hi)
## maybe str(data_CoOccur_AZ_Hi) ; plot(data_CoOccur_AZ_Hi) ...
}
\keyword{datasets}
