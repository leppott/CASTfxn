\name{data_CoOccur_AZ_Hi}
\alias{data_CoOccur_AZ_Hi}
\docType{data}
\title{
A Capitalized Title for the Data Set
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("data_CoOccur_AZ_Hi")}
\format{
  A data frame with 336 observations on the following 307 variables.
  \describe{
    \item{\code{StationID_Master}}{a factor with levels \code{LCBEN002.57} \code{LCBIL000.01} \code{LCBIL000.15} \code{LCBRB006.74} \code{LCCHC073.26} \code{LCCHC081.26} \code{LCCOL003.03} \code{LCCOY000.71} \code{LCECL009.39} \code{LCECL018.17} \code{LCECL021.13} \code{LCELR000.13} \code{LCELR005.67} \code{LCELR007.19} \code{LCELR008.70} \code{LCFIS003.86} \code{LCHAL004.59} \code{LCHAL008.83} \code{LCHAL010.20} \code{LCLCR211.73} \code{LCLCR311.31} \code{LCLCR323.60} \code{LCLCR336.72} \code{LCLCR340.02} \code{LCLCR342.03} \code{LCLCR360.06} \code{LCLIL002.16} \code{LCLVL001.32} \code{LCMAM002.05} \code{LCMIN018.05} \code{LCMLK001.18} \code{LCMRS043.17} \code{LCNUT011.29} \code{LCNUT020.72} \code{LCPAD001.30} \code{LCRIG004.87} \code{LCROS000.25} \code{LCRUD003.45} \code{LCRUD007.23} \code{LCRUD008.43} \code{LCSHL026.50} \code{LCSHL029.75} \code{LCSHL031.05} \code{LCSIL027.05} \code{LCSIL041.04} \code{LCSIL043.84} \code{LCSLR001.42} \code{LCSLR003.72} \code{LCWLR000.92} \code{LCWLR004.09} \code{LCWLR007.37} \code{MGHSR110.58} \code{SPBIG007.41} \code{SPCRC013.37} \code{SPGDN007.55} \code{SPGRA007.71} \code{SPMLC013.56} \code{SPRMC011.11} \code{SPTUR028.53} \code{SRBEV007.28} \code{SRBGY002.86} \code{SRBLR070.45} \code{SRBLR102.24} \code{SRBON001.69} \code{SRBWL005.79} \code{SRCDY002.95} \code{SRCKN001.61} \code{SRCYN045.73} \code{SRCYN048.21} \code{SRDCN001.90} \code{SREFB008.27} \code{SRFIS004.49} \code{SRFIS010.92} \code{SRHAG013.09} \code{SRHAY000.04} \code{SRHAY003.25} \code{SRHRS001.41} \code{SRNBE000.86} \code{SRNFB012.35} \code{SRREY001.14} \code{SRSTI000.38} \code{SRTON058.63} \code{SRTON059.43} \code{SRTON061.37} \code{SRTON062.28} \code{SRUDC000.02} \code{SRUEF000.87} \code{SRUNF002.31} \code{SRUUE000.03} \code{SRWFB003.45} \code{SRWFB005.34} \code{SRWRK007.97} \code{UGBLR028.77} \code{UGBLR028.99} \code{UGBLR030.24} \code{UGSTO003.44} \code{UGTRY001.56} \code{VRBON002.71} \code{VRELL009.02} \code{VREVR045.50} \code{VREVR051.15} \code{VROAK045.64} \code{VROAK048.92} \code{VRPIE016.49} \code{VRWCL036.37} \code{VRWEB009.13} \code{VRWEB010.36} \code{VRWOK000.82}}
    \item{\code{Group}}{a numeric vector}
    \item{\code{SampYear}}{a numeric vector}
    \item{\code{ChemSampleID}}{a factor with levels \code{LCBEN002.57_2007-04-05} \code{LCBEN002.57_2007-05-09} \code{LCBIL000.01_2001-06-26} \code{LCBIL000.01_2010-03-09} \code{LCBIL000.01_2010-05-06} \code{LCBIL000.15_2014-03-24} \code{LCBIL000.15_2014-05-13} \code{LCBRB006.74_1997-05-19} \code{LCBRB006.74_2007-06-18} \code{LCBRB006.74_2014-03-26} \code{LCBRB006.74_2014-05-21} \code{LCCHC073.26_1997-05-20} \code{LCCHC073.26_2010-05-25} \code{LCCHC073.26_2010-12-09} \code{LCCHC073.26_2014-05-19} \code{LCCHC081.26_2007-04-25} \code{LCCHC081.26_2007-06-19} \code{LCCOL003.03_2001-06-11} \code{LCCOL003.03_2014-04-14} \code{LCCOL003.03_2014-05-21} \code{LCCOY000.71_2007-03-07} \code{LCCOY000.71_2007-04-10} \code{LCCOY000.71_2007-10-16} \code{LCECL009.39_2010-05-24} \code{LCECL009.39_2014-03-26} \code{LCECL009.39_2014-05-20} \code{LCECL018.17_2007-05-02} \code{LCECL018.17_2007-06-20} \code{LCECL021.13_2007-04-30} \code{LCECL021.13_2007-05-03} \code{LCECL021.13_2007-05-16} \code{LCECL021.13_2016-03-23} \code{LCECL021.13_2016-05-02} \code{LCELR000.13_2007-03-27} \code{LCELR000.13_2007-06-12} \code{LCELR005.67_2014-04-29} \code{LCELR007.19_2007-04-19} \code{LCELR007.19_2007-06-13} \code{LCELR008.70_2010-06-24} \code{LCELR008.70_2014-04-29} \code{LCFIS003.86_2001-06-18} \code{LCFIS003.86_2010-05-04} \code{LCFIS003.86_2010-06-29} \code{LCHAL004.59_2007-06-04} \code{LCHAL008.83_2001-06-19} \code{LCHAL008.83_2007-06-04} \code{LCHAL008.83_2007-06-05} \code{LCHAL010.20_2007-06-07} \code{LCLCR211.73_2007-03-06} \code{LCLCR211.73_2007-04-24} \code{LCLCR211.73_2007-05-15} \code{LCLCR311.31_2007-03-07} \code{LCLCR311.31_2007-04-09} \code{LCLCR323.60_2010-06-03} \code{LCLCR323.60_2014-02-19} \code{LCLCR323.60_2014-05-20} \code{LCLCR336.72_2010-05-05} \code{LCLCR336.72_2010-06-30} \code{LCLCR336.72_2014-02-19} \code{LCLCR336.72_2014-05-21} \code{LCLCR340.02_2000-06-28} \code{LCLCR340.02_2001-06-12} \code{LCLCR340.02_2003-06-10} \code{LCLCR340.02_2004-06-09} \code{LCLCR340.02_2005-06-09} \code{LCLCR340.02_2007-03-28} \code{LCLCR340.02_2007-04-18} \code{LCLCR342.03_2007-03-06} \code{LCLCR342.03_2007-04-11} \code{LCLCR342.03_2007-10-15} \code{LCLCR360.06_2007-03-27} \code{LCLCR360.06_2007-06-05} \code{LCLCR360.06_2007-10-25} \code{LCLIL002.16_2010-06-24} \code{LCLVL001.32_2001-06-19} \code{LCLVL001.32_2007-04-19} \code{LCLVL001.32_2007-06-13} \code{LCLVL001.32_2010-06-03} \code{LCLVL001.32_2010-06-24} \code{LCMAM002.05_2010-06-24} \code{LCMIN018.05_2001-06-05} \code{LCMIN018.05_2007-06-26} \code{LCMLK001.18_2007-04-10} \code{LCMLK001.18_2007-05-22} \code{LCMRS043.17_2007-03-28} \code{LCMRS043.17_2007-05-23} \code{LCNUT011.29_2014-02-19} \code{LCNUT011.29_2014-05-28} \code{LCNUT020.72_2004-06-09} \code{LCPAD001.30_2010-06-22} \code{LCRIG004.87_2007-03-28} \code{LCRIG004.87_2007-05-10} \code{LCROS000.25_2014-03-17} \code{LCROS000.25_2014-04-28} \code{LCRUD003.45_2007-03-06} \code{LCRUD003.45_2007-05-23} \code{LCRUD003.45_2007-10-23} \code{LCRUD007.23_2007-06-27} \code{LCRUD007.23_2010-06-23} \code{LCRUD008.43_2010-05-05} \code{LCRUD008.43_2010-07-01} \code{LCSHL026.50_2007-04-16} \code{LCSHL029.75_2007-06-27} \code{LCSHL031.05_2007-03-05} \code{LCSHL031.05_2007-04-17} \code{LCSHL031.05_2007-10-26} \code{LCSHL031.05_2014-03-24} \code{LCSHL031.05_2014-05-12} \code{LCSIL027.05_2010-03-09} \code{LCSIL027.05_2010-05-04} \code{LCSIL027.05_2010-06-01} \code{LCSIL027.05_2010-06-25} \code{LCSIL027.05_2014-03-24} \code{LCSIL027.05_2014-05-12} \code{LCSIL041.04_2007-03-06} \code{LCSIL041.04_2007-04-24} \code{LCSIL041.04_2007-10-17} \code{LCSIL043.84_2001-06-05} \code{LCSIL043.84_2007-06-28} \code{LCSIL043.84_2011-03-07} \code{LCSIL043.84_2011-05-09} \code{LCSLR001.42_1998-06-02} \code{LCSLR001.42_2007-03-07} \code{LCSLR001.42_2007-05-24} \code{LCSLR001.42_2007-10-24} \code{LCSLR001.42_2013-06-10} \code{LCSLR001.42_2013-09-24} \code{LCSLR001.42_2013-10-28} \code{LCSLR003.72_2007-06-21} \code{LCSLR003.72_2010-05-06} \code{LCSLR003.72_2010-06-30} \code{LCWLR000.92_2000-06-28} \code{LCWLR000.92_2001-06-28} \code{LCWLR000.92_2003-06-10} \code{LCWLR000.92_2005-06-08} \code{LCWLR000.92_2007-03-26} \code{LCWLR000.92_2007-06-12} \code{LCWLR000.92_2012-04-25} \code{LCWLR000.92_2012-06-20} \code{LCWLR000.92_2013-06-10} \code{LCWLR004.09_2001-06-27} \code{LCWLR004.09_2003-06-16} \code{LCWLR004.09_2010-06-23} \code{LCWLR007.37_2014-03-17} \code{LCWLR007.37_2014-04-30} \code{MGHSR110.58_2009-02-04} \code{MGHSR110.58_2009-04-07} \code{SPBIG007.41_2014-06-03} \code{SPCRC013.37_2009-07-10} \code{SPGDN007.55_2009-03-04} \code{SPGDN007.55_2009-04-30} \code{SPGRA007.71_1997-04-28} \code{SPGRA007.71_2005-06-14} \code{SPGRA007.71_2009-03-12} \code{SPGRA007.71_2009-06-25} \code{SPGRA007.71_2012-02-28} \code{SPGRA007.71_2012-05-16} \code{SPGRA007.71_2012-05-17} \code{SPMLC013.56_2009-03-12} \code{SPMLC013.56_2009-04-29} \code{SPRMC011.11_2009-03-04} \code{SPRMC011.11_2009-04-28} \code{SPRMC011.11_2015-02-10} \code{SPRMC011.11_2015-06-17} \code{SPTUR028.53_2012-01-16} \code{SPTUR028.53_2012-04-09} \code{SPTUR028.53_2012-05-14} \code{SRBEV007.28_2008-04-22} \code{SRBEV007.28_2008-06-18} \code{SRBEV007.28_2008-10-22} \code{SRBGY002.86_2014-06-11} \code{SRBLR070.45_2012-03-01} \code{SRBLR070.45_2012-06-26} \code{SRBLR102.24_2008-07-10} \code{SRBLR102.24_2013-05-21} \code{SRBLR102.24_2013-06-19} \code{SRBON001.69_2008-04-22} \code{SRBON001.69_2008-06-16} \code{SRBON001.69_2008-10-22} \code{SRBWL005.79_2013-06-11} \code{SRCDY002.95_2016-03-28} \code{SRCDY002.95_2016-06-21} \code{SRCKN001.61_2012-04-26} \code{SRCKN001.61_2012-06-19} \code{SRCKN001.61_2013-06-11} \code{SRCYN045.73_1997-05-14} \code{SRCYN045.73_2008-05-07} \code{SRCYN045.73_2008-05-13} \code{SRCYN045.73_2008-10-21} \code{SRCYN045.73_2014-02-26} \code{SRCYN045.73_2014-04-16} \code{SRCYN048.21_2011-03-08} \code{SRCYN048.21_2011-05-10} \code{SRDCN001.90_2016-03-28} \code{SRDCN001.90_2016-06-22} \code{SREFB008.27_2012-05-14} \code{SREFB008.27_2012-06-18} \code{SREFB008.27_2012-07-31} \code{SREFB008.27_2012-10-18} \code{SREFB008.27_2013-05-21} \code{SREFB008.27_2013-06-19} \code{SREFB008.27_2013-09-16} \code{SREFB008.27_2013-10-22} \code{SREFB008.27_2014-04-14} \code{SREFB008.27_2014-06-09} \code{SRFIS004.49_1998-06-09} \code{SRFIS004.49_2008-05-20} \code{SRFIS004.49_2008-06-24} \code{SRFIS010.92_2016-03-28} \code{SRFIS010.92_2016-06-20} \code{SRHAG013.09_2008-03-26} \code{SRHAG013.09_2008-05-14} \code{SRHAG013.09_2008-10-21} \code{SRHAG013.09_2014-02-26} \code{SRHAG013.09_2014-04-16} \code{SRHAY000.04_2002-06-12} \code{SRHAY000.04_2008-04-23} \code{SRHAY000.04_2008-06-17} \code{SRHAY003.25_2016-03-29} \code{SRHAY003.25_2016-06-21} \code{SRHRS001.41_2016-02-25} \code{SRHRS001.41_2016-06-20} \code{SRHRS001.41_2016-06-21} \code{SRNBE000.86_1998-06-11} \code{SRNBE000.86_2016-03-29} \code{SRNBE000.86_2016-06-21} \code{SRNFB012.35_2016-03-30} \code{SRNFB012.35_2016-06-14} \code{SRREY001.14_2011-03-23} \code{SRREY001.14_2011-06-08} \code{SRSTI000.38_2008-06-18} \code{SRSTI000.38_2014-04-15} \code{SRSTI000.38_2014-05-29} \code{SRTON058.63_2014-02-26} \code{SRTON058.63_2014-04-23} \code{SRTON058.63_2014-07-09} \code{SRTON058.63_2014-07-23} \code{SRTON058.63_2014-08-06} \code{SRTON058.63_2014-08-27} \code{SRTON058.63_2014-09-10} \code{SRTON058.63_2014-09-24} \code{SRTON059.43_2008-03-25} \code{SRTON059.43_2008-05-14} \code{SRTON059.43_2014-02-26} \code{SRTON059.43_2014-06-03} \code{SRTON061.37_2015-05-07} \code{SRTON062.28_2011-05-23} \code{SRTON062.28_2011-06-30} \code{SRTON062.28_2011-07-28} \code{SRTON062.28_2011-09-07} \code{SRTON062.28_2011-10-11} \code{SRTON062.28_2015-05-07} \code{SRUDC000.02_2016-03-28} \code{SRUDC000.02_2016-06-22} \code{SRUEF000.87_2016-03-30} \code{SRUEF000.87_2016-06-13} \code{SRUNF002.31_2014-04-16} \code{SRUNF002.31_2014-05-19} \code{SRUUE000.03_2016-03-30} \code{SRUUE000.03_2016-06-13} \code{SRWFB003.45_2013-06-12} \code{SRWFB003.45_2014-04-15} \code{SRWFB003.45_2014-06-10} \code{SRWFB005.34_2008-04-23} \code{SRWFB005.34_2008-06-17} \code{SRWFB005.34_2008-10-22} \code{SRWRK007.97_2008-05-01} \code{SRWRK007.97_2008-06-16} \code{SRWRK007.97_2008-11-18} \code{SRWRK007.97_2014-03-25} \code{SRWRK007.97_2014-05-15} \code{UGBLR028.77_2015-02-18} \code{UGBLR028.77_2015-06-01} \code{UGBLR028.99_2000-06-13} \code{UGBLR028.99_2006-04-04} \code{UGBLR028.99_2006-06-21} \code{UGBLR030.24_2009-04-01} \code{UGBLR030.24_2009-06-17} \code{UGBLR030.24_2014-03-18} \code{UGBLR030.24_2014-07-08} \code{UGSTO003.44_2015-02-17} \code{UGSTO003.44_2015-06-02} \code{UGTRY001.56_2009-04-01} \code{UGTRY001.56_2009-06-17} \code{VRBON002.71_2016-03-30} \code{VRBON002.71_2016-05-03} \code{VRELL009.02_2011-04-12} \code{VRELL009.02_2011-06-15} \code{VREVR045.50_2008-03-27} \code{VREVR045.50_2008-05-01} \code{VREVR051.15_2008-03-27} \code{VREVR051.15_2008-06-24} \code{VREVR051.15_2008-10-20} \code{VREVR051.15_2014-02-26} \code{VREVR051.15_2014-04-17} \code{VROAK045.64_2011-02-08} \code{VROAK045.64_2011-05-17} \code{VROAK048.92_2008-01-06} \code{VROAK048.92_2008-03-19} \code{VROAK048.92_2008-04-24} \code{VROAK048.92_2008-06-09} \code{VROAK048.92_2008-11-05} \code{VRPIE016.49_1997-05-21} \code{VRPIE016.49_2011-04-12} \code{VRPIE016.49_2011-06-14} \code{VRWCL036.37_1997-05-22} \code{VRWCL036.37_2008-05-08} \code{VRWCL036.37_2008-07-09} \code{VRWCL036.37_2016-06-16} \code{VRWEB009.13_1997-05-20} \code{VRWEB010.36_2016-03-16} \code{VRWEB010.36_2016-05-03} \code{VRWOK000.82_1998-05-07} \code{VRWOK000.82_2004-05-27} \code{VRWOK000.82_2008-03-21} \code{VRWOK000.82_2008-06-10} \code{VRWOK000.82_2008-11-05} \code{VRWOK000.82_2016-02-10} \code{VRWOK000.82_2016-05-10}}
    \item{\code{SampDate}}{a factor with levels \code{1/16/2012} \code{1/6/2008} \code{10/11/2011} \code{10/15/2007} \code{10/16/2007} \code{10/17/2007} \code{10/18/2012} \code{10/20/2008} \code{10/21/2008} \code{10/22/2008} \code{10/22/2013} \code{10/23/2007} \code{10/24/2007} \code{10/25/2007} \code{10/26/2007} \code{10/28/2013} \code{11/18/2008} \code{11/5/2008} \code{12/9/2010} \code{2/10/2015} \code{2/10/2016} \code{2/17/2015} \code{2/18/2015} \code{2/19/2014} \code{2/25/2016} \code{2/26/2014} \code{2/28/2012} \code{2/4/2009} \code{2/8/2011} \code{3/1/2012} \code{3/12/2009} \code{3/16/2016} \code{3/17/2014} \code{3/18/2014} \code{3/19/2008} \code{3/21/2008} \code{3/23/2011} \code{3/23/2016} \code{3/24/2014} \code{3/25/2008} \code{3/25/2014} \code{3/26/2007} \code{3/26/2008} \code{3/26/2014} \code{3/27/2007} \code{3/27/2008} \code{3/28/2007} \code{3/28/2016} \code{3/29/2016} \code{3/30/2016} \code{3/4/2009} \code{3/5/2007} \code{3/6/2007} \code{3/7/2007} \code{3/7/2011} \code{3/8/2011} \code{3/9/2010} \code{4/1/2009} \code{4/10/2007} \code{4/11/2007} \code{4/12/2011} \code{4/14/2014} \code{4/15/2014} \code{4/16/2007} \code{4/16/2014} \code{4/17/2007} \code{4/17/2014} \code{4/18/2007} \code{4/19/2007} \code{4/22/2008} \code{4/23/2008} \code{4/23/2014} \code{4/24/2007} \code{4/24/2008} \code{4/25/2007} \code{4/25/2012} \code{4/26/2012} \code{4/28/1997} \code{4/28/2009} \code{4/28/2014} \code{4/29/2009} \code{4/29/2014} \code{4/30/2007} \code{4/30/2009} \code{4/30/2014} \code{4/4/2006} \code{4/5/2007} \code{4/7/2009} \code{4/9/2007} \code{4/9/2012} \code{5/1/2008} \code{5/10/2007} \code{5/10/2011} \code{5/10/2016} \code{5/12/2014} \code{5/13/2008} \code{5/13/2014} \code{5/14/1997} \code{5/14/2008} \code{5/14/2012} \code{5/15/2007} \code{5/15/2014} \code{5/16/2007} \code{5/16/2012} \code{5/17/2011} \code{5/17/2012} \code{5/19/1997} \code{5/19/2014} \code{5/2/2007} \code{5/2/2016} \code{5/20/1997} \code{5/20/2008} \code{5/20/2014} \code{5/21/1997} \code{5/21/2013} \code{5/21/2014} \code{5/22/1997} \code{5/22/2007} \code{5/23/2007} \code{5/23/2011} \code{5/24/2007} \code{5/24/2010} \code{5/25/2010} \code{5/27/2004} \code{5/28/2014} \code{5/29/2014} \code{5/3/2007} \code{5/3/2016} \code{5/4/2010} \code{5/5/2010} \code{5/6/2010} \code{5/7/1998} \code{5/7/2008} \code{5/7/2015} \code{5/8/2008} \code{5/9/2007} \code{5/9/2011} \code{6/1/2010} \code{6/1/2015} \code{6/10/2003} \code{6/10/2008} \code{6/10/2013} \code{6/10/2014} \code{6/11/1998} \code{6/11/2001} \code{6/11/2013} \code{6/11/2014} \code{6/12/2001} \code{6/12/2002} \code{6/12/2007} \code{6/12/2013} \code{6/13/2000} \code{6/13/2007} \code{6/13/2016} \code{6/14/2005} \code{6/14/2011} \code{6/14/2016} \code{6/15/2011} \code{6/16/2003} \code{6/16/2008} \code{6/16/2016} \code{6/17/2008} \code{6/17/2009} \code{6/17/2015} \code{6/18/2001} \code{6/18/2007} \code{6/18/2008} \code{6/18/2012} \code{6/19/2001} \code{6/19/2007} \code{6/19/2012} \code{6/19/2013} \code{6/2/1998} \code{6/2/2015} \code{6/20/2007} \code{6/20/2012} \code{6/20/2016} \code{6/21/2006} \code{6/21/2007} \code{6/21/2016} \code{6/22/2010} \code{6/22/2016} \code{6/23/2010} \code{6/24/2008} \code{6/24/2010} \code{6/25/2009} \code{6/25/2010} \code{6/26/2001} \code{6/26/2007} \code{6/26/2012} \code{6/27/2001} \code{6/27/2007} \code{6/28/2000} \code{6/28/2001} \code{6/28/2007} \code{6/29/2010} \code{6/3/2010} \code{6/3/2014} \code{6/30/2010} \code{6/30/2011} \code{6/4/2007} \code{6/5/2001} \code{6/5/2007} \code{6/7/2007} \code{6/8/2005} \code{6/8/2011} \code{6/9/1998} \code{6/9/2004} \code{6/9/2005} \code{6/9/2008} \code{6/9/2014} \code{7/1/2010} \code{7/10/2008} \code{7/10/2009} \code{7/23/2014} \code{7/28/2011} \code{7/31/2012} \code{7/8/2014} \code{7/9/2008} \code{7/9/2014} \code{8/27/2014} \code{8/6/2014} \code{9/10/2014} \code{9/16/2013} \code{9/24/2013} \code{9/24/2014} \code{9/7/2011}}
    \item{\code{BenCollDate}}{a factor with levels \code{4/10/2007} \code{4/11/2007} \code{4/16/2007} \code{4/16/2014} \code{4/17/2007} \code{4/17/2014} \code{4/18/2007} \code{4/23/2014} \code{4/24/2007} \code{4/28/1997} \code{4/28/2009} \code{4/28/2014} \code{4/29/2009} \code{4/29/2014} \code{4/30/2009} \code{4/30/2014} \code{4/7/2009} \code{4/9/2007} \code{5/1/2008} \code{5/10/2007} \code{5/10/2011} \code{5/10/2016} \code{5/12/2014} \code{5/13/2008} \code{5/13/2014} \code{5/14/1997} \code{5/14/2008} \code{5/14/2012} \code{5/15/2007} \code{5/15/2014} \code{5/16/2012} \code{5/17/2011} \code{5/19/1997} \code{5/19/2014} \code{5/2/2007} \code{5/2/2016} \code{5/20/1997} \code{5/20/2014} \code{5/21/1997} \code{5/21/2014} \code{5/22/1997} \code{5/22/2007} \code{5/23/2007} \code{5/24/2007} \code{5/24/2010} \code{5/24/2011} \code{5/25/2010} \code{5/27/2004} \code{5/28/2014} \code{5/29/2014} \code{5/3/2007} \code{5/3/2016} \code{5/7/1998} \code{5/7/2015} \code{5/9/2007} \code{5/9/2011} \code{6/1/2015} \code{6/10/2003} \code{6/10/2008} \code{6/10/2013} \code{6/10/2014} \code{6/11/1998} \code{6/11/2001} \code{6/11/2013} \code{6/11/2014} \code{6/12/2001} \code{6/12/2002} \code{6/12/2007} \code{6/12/2013} \code{6/13/2000} \code{6/13/2007} \code{6/13/2016} \code{6/14/2005} \code{6/14/2011} \code{6/14/2016} \code{6/15/2011} \code{6/16/2003} \code{6/16/2008} \code{6/16/2016} \code{6/17/2008} \code{6/17/2009} \code{6/17/2015} \code{6/18/2001} \code{6/18/2007} \code{6/18/2008} \code{6/18/2012} \code{6/19/2001} \code{6/19/2007} \code{6/19/2012} \code{6/19/2013} \code{6/2/1998} \code{6/2/2015} \code{6/20/2012} \code{6/20/2016} \code{6/21/2006} \code{6/21/2007} \code{6/21/2016} \code{6/22/2010} \code{6/22/2016} \code{6/23/2010} \code{6/24/2008} \code{6/24/2010} \code{6/25/2009} \code{6/25/2010} \code{6/26/2001} \code{6/26/2007} \code{6/26/2012} \code{6/27/2001} \code{6/27/2007} \code{6/28/2000} \code{6/28/2001} \code{6/28/2007} \code{6/29/2010} \code{6/3/2010} \code{6/3/2014} \code{6/30/2010} \code{6/4/2007} \code{6/5/2001} \code{6/5/2007} \code{6/7/2007} \code{6/8/2005} \code{6/8/2011} \code{6/9/1998} \code{6/9/2004} \code{6/9/2005} \code{6/9/2008} \code{6/9/2014} \code{7/1/2010} \code{7/10/2008} \code{7/10/2009} \code{7/8/2014} \code{7/9/2008}}
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
    \item{\code{Alkalinity_bicarbonate_uf_mg_L}}{a numeric vector}
    \item{\code{Alkalinity_carbonate_uf_mg_L}}{a logical vector}
    \item{\code{Alkalinity_hydroxide_uf_mg_L}}{a logical vector}
    \item{\code{Alkalinity_phenolphthalein_uf_mg_L}}{a numeric vector}
    \item{\code{Alkalinity_total_uf_mg_L}}{a numeric vector}
    \item{\code{Alpha_grossAsUNatural_calc_pCi_L}}{a numeric vector}
    \item{\code{Alpha_grossAsUNatural_uf_pCi_L}}{a numeric vector}
    \item{\code{alphaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
    \item{\code{Aluminum_f_mg_L}}{a logical vector}
    \item{\code{Aluminum_uf_ug_L}}{a logical vector}
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
    \item{\code{Barium_cmpds_f_mg_L}}{a logical vector}
    \item{\code{Barium_cmpds_f_ug_L}}{a numeric vector}
    \item{\code{Barium_cmpds_uf_pCi_L}}{a logical vector}
    \item{\code{Barium_cmpds_uf_ug_L}}{a numeric vector}
    \item{\code{BarometricPressure_mmHg}}{a logical vector}
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
    \item{\code{Beta_radiation_calc_pCi_L}}{a numeric vector}
    \item{\code{Beta_radiation_uf_pCi_L}}{a numeric vector}
    \item{\code{betaChloronapthalene_uf_ug_L}}{a logical vector}
    \item{\code{betaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
    \item{\code{Bis_2chloroethoxy_methane_uf_ug_L}}{a logical vector}
    \item{\code{Bis_2chloroisopropyl_ether_uf_ug_L}}{a logical vector}
    \item{\code{Bis_chloroethyl_ether_uf_ug_L}}{a logical vector}
    \item{\code{Bismuth_f_pCi_L}}{a logical vector}
    \item{\code{BOD_uf_mg_L}}{a logical vector}
    \item{\code{Bolstar_uf_ug_L}}{a logical vector}
    \item{\code{Boron_borates_f_mg_L}}{a logical vector}
    \item{\code{Boron_borates_uf_mg_L}}{a numeric vector}
    \item{\code{ButylBenzylPhthalate_uf_ug_L}}{a logical vector}
    \item{\code{CaCO3_calc_mg_L}}{a logical vector}
    \item{\code{CaCO3_f_mg_L}}{a logical vector}
    \item{\code{CaCO3_mg_L}}{a numeric vector}
    \item{\code{CaCO3_uf_mg_L}}{a numeric vector}
    \item{\code{Cadmium_calc_mg_kg}}{a logical vector}
    \item{\code{Cadmium_f_mg_L}}{a numeric vector}
    \item{\code{Cadmium_uf_mg_L}}{a numeric vector}
    \item{\code{Caffeine_f_mg_L}}{a logical vector}
    \item{\code{Calcium_f_mg_L}}{a numeric vector}
    \item{\code{Calcium_uf_mg_L}}{a numeric vector}
    \item{\code{Carbazole_uf_ug_L}}{a logical vector}
    \item{\code{Carbon_calc_.}}{a logical vector}
    \item{\code{Carbon_f_mg_L}}{a numeric vector}
    \item{\code{Carbon_uf_mg_L}}{a numeric vector}
    \item{\code{Carbonate_uf_mg_L}}{a numeric vector}
    \item{\code{Cesium_134_uf_pCi_L}}{a logical vector}
    \item{\code{Cesium_137_uf_pCi_L}}{a logical vector}
    \item{\code{Chlordane_cis_uf_ug_L}}{a logical vector}
    \item{\code{Chlordane_gamma_uf_ug_L}}{a logical vector}
    \item{\code{Chlordane_uf_ug_L}}{a logical vector}
    \item{\code{Chloride_uf_mg_L}}{a numeric vector}
    \item{\code{Chlorine_other_mg_L}}{a logical vector}
    \item{\code{Chlorine_uf_mg_L}}{a logical vector}
    \item{\code{Chlorophyll_a_uf_ug_L}}{a numeric vector}
    \item{\code{Chlorophyll_pheophytin_uf_ratio}}{a logical vector}
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
    \item{\code{Depth_unk}}{a logical vector}
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
    \item{\code{DO_f_unk}}{a logical vector}
    \item{\code{DOSat_f_.}}{a numeric vector}
    \item{\code{DOSat_f_unk}}{a logical vector}
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
    \item{\code{EventCond_none}}{a logical vector}
    \item{\code{Fensulfothion_uf_ug_L}}{a logical vector}
    \item{\code{Fenthion_uf_ug_L}}{a logical vector}
    \item{\code{Fines_lt2mm_pct_Reach}}{a numeric vector}
    \item{\code{Fines_lt2mm_pct_Riffle}}{a numeric vector}
    \item{\code{Flow_cfs}}{a numeric vector}
    \item{\code{Flow_unk}}{a logical vector}
    \item{\code{Fluoranthene_uf_ug_L}}{a logical vector}
    \item{\code{Fluorene_uf_ug_L}}{a logical vector}
    \item{\code{Fluoride_f_mg_L}}{a logical vector}
    \item{\code{Fluoride_uf_mg_L}}{a numeric vector}
    \item{\code{gammaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
    \item{\code{Hardness_CaCO3_MgCO3_calc_mg_L}}{a numeric vector}
    \item{\code{Hardness_CaCO3_MgCO3_f_mg_L}}{a logical vector}
    \item{\code{Hardness_CaCO3_MgCO3_uf_mg_L}}{a numeric vector}
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
    \item{\code{Iron_uf_ug_L}}{a logical vector}
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
    \item{\code{Manganese_f_ug_L}}{a logical vector}
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
    \item{\code{NH3_N_calc_mg_kg}}{a logical vector}
    \item{\code{NH3_N_uf_mg_L}}{a numeric vector}
    \item{\code{Nickel_calc_mg_kg}}{a logical vector}
    \item{\code{Nickel_f_mg_L}}{a logical vector}
    \item{\code{Nickel_uf_ug_L}}{a logical vector}
    \item{\code{Niobium_95_uf_pCi_L}}{a logical vector}
    \item{\code{Nitrobenzene_uf_ug_L}}{a logical vector}
    \item{\code{Nitrogen_uf_mg_L}}{a numeric vector}
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
    \item{\code{Selenium_cmpds_uf_mg_L}}{a logical vector}
    \item{\code{Silica_uf_mg_L}}{a logical vector}
    \item{\code{Silicon_uf_ug_L}}{a logical vector}
    \item{\code{Silver_calc_mg_kg}}{a logical vector}
    \item{\code{Silver_f_mg_L}}{a logical vector}
    \item{\code{Silver_uf_mg_L}}{a logical vector}
    \item{\code{Sodium_f_mg_L}}{a numeric vector}
    \item{\code{Sodium_uf_mg_L}}{a numeric vector}
    \item{\code{SolidsInSediment_calc_.}}{a logical vector}
    \item{\code{SpecCond_umhos_cm}}{a numeric vector}
    \item{\code{SpecCond_unk}}{a logical vector}
    \item{\code{StreamCrossSectArea_unk}}{a numeric vector}
    \item{\code{StreamWidth_ft}}{a numeric vector}
    \item{\code{StreamWidth_unk}}{a logical vector}
    \item{\code{Strontium_f_mg_L}}{a logical vector}
    \item{\code{Sulfate_uf_mg_L}}{a numeric vector}
    \item{\code{TDS_calc_mg_L}}{a numeric vector}
    \item{\code{TDS_f_mg_L}}{a numeric vector}
    \item{\code{TDS_f_unk}}{a logical vector}
    \item{\code{TDS_uf_mg_L}}{a logical vector}
    \item{\code{Temp_air_degC}}{a numeric vector}
    \item{\code{Temp_air_unk}}{a logical vector}
    \item{\code{Temp_degC}}{a numeric vector}
    \item{\code{Temp_degF}}{a logical vector}
    \item{\code{Temp_unk}}{a logical vector}
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
    \item{\code{Turbidity_unk}}{a logical vector}
    \item{\code{Uranium_natural_f_ug_L}}{a logical vector}
    \item{\code{Uranium_Natural_uf_pCi_L}}{a logical vector}
    \item{\code{Uranium_natural_uf_ug_L}}{a numeric vector}
    \item{\code{Vanadium_f_mg_L}}{a logical vector}
    \item{\code{Vanadium_uf_mg_L}}{a logical vector}
    \item{\code{Velocity_ft_sec}}{a numeric vector}
    \item{\code{Velocity_unk}}{a logical vector}
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
