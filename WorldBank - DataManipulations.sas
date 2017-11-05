/*****************************************************************************/
/* Import                                                                    */
/*****************************************************************************/
proc import
	datafile = "...\20170918 - WorldDevelopmentIndicators.xlsx"
	out = Data00
	dbms = xlsx
	replace;
	sheet = "Data";
run;

proc import
	datafile = "...\CountryRegion.xlsx"
	out = CountryRegion
	dbms = xlsx
	replace;
	sheet = "Groups";
run;


/*****************************************************************************/
/* Manipulations                                                             */
/*****************************************************************************/
proc sql;
	create table Data01 (drop = CountryCode) as
	select b.Region, a.*
	from Data00 a inner join CountryRegion b
	on a.CountryCode = b.CountryCode
	order by Region, SeriesName, CountryName;
quit;

proc sort
	data = Data01 (keep = SeriesCode SeriesName)
	out = SeriesNames nodupkey;
	by SeriesCode SeriesName;
run;

proc sort
	data = Data01;
	by Region CountryName SeriesCode;
run;

proc transpose 
	data = Data01 (drop = SeriesName)
    out = Data02 (drop = _LABEL_ rename = (_NAME_ = Year));
	var YR1960-YR2016;
	by Region CountryName;
	id SeriesCode;
run;
/* 57 time points * 217 countries = 12,369 rows */

data Data03 (drop = Year rename = (Year1 = Year));
	format Region CountryName RegionNum Year;
	format Year1 8.;
	set Data02;	
	where Year notin("YR1960", "YR1961", "YR1962", "YR1963", "YR1964", "YR1965", "YR1966", "YR1967", "YR1968", "YR1969",
				     "YR1970", "YR1971", "YR1972", "YR1973", "YR1974", "YR1975", "YR1976", "YR1977", "YR1988", "YR1979",
					 "YR1980", "YR1981", "YR1982", "YR1983", "YR1984", "YR1985", "YR1986", "YR1987", "YR1978", "YR1989",
					 "YR2016");
	Year1 = input(substr(Year,3,6),8.) - 1990;
	if Region = "Africa" then RegionNum = 1;
	else if Region = "America" then RegionNum = 2;
	else if Region = "Asia & Oceania" then RegionNum = 3;
	else if Region = "Europe" then RegionNum = 4;
run;

proc format;
	value  missfmt  .  = 'Missing' other = 'Not Missing';
run;
 
proc freq 
	data = Data03; 
	format _numeric_ missfmt.;
	tables _numeric_ / outcum missing missprint nocum nopercent;
run;

data Data04 (rename = ("AG.LND.FRST.K2"n = ForestArea
                       "AG.SRF.TOTL.K2"n = SurfaceArea
					   "BX.KLT.DINV.WD.GD.ZS"n = ForeignInvest
					   "EN.ATM.CO2E.PC"n = CO2Emi
                       "EN.POP.DNST"n = PopDensity
					   "IT.CEL.SETS.P2"n = MobileSub
					   "NE.EXP.GNFS.ZS"n = Export
					   "NE.IMP.GNFS.ZS"n = Import
					   "NY.GDP.DEFL.KD.ZG"n = Inflation
					   "NY.GDP.MKTP.CD"n = GDP
					   "NY.GDP.MKTP.KD.ZG"n = GDPG
					   "NY.GNP.PCAP.PP.CD"n = GNI
					   "SH.DYN.MORT"n = MortalityU5
					   "SH.H2O.SAFE.ZS"n = Water
					   "SH.IMM.MEAS"n = Measles
					   "SH.STA.ACSN"n = Sanitation
					   "SP.DYN.TFRT.IN"n = Fertility
					   "SP.POP.GROW"n = PopG
					   "SP.POP.TOTL"n = Pop
					   "SP.URB.GROW"n = UrbanPopG));
	format Region CountryName Year RegionNum "SH.DYN.MORT"n;
	set Data03 (keep = Region CountryName RegionNum Year
					   "AG.LND.FRST.K2"n "AG.SRF.TOTL.K2"n
        			   "BX.KLT.DINV.WD.GD.ZS"n
                       "EN.ATM.CO2E.PC"n "EN.POP.DNST"n
                       "IT.CEL.SETS.P2"n 
                       "NE.EXP.GNFS.ZS"n "NE.IMP.GNFS.ZS"n
                       "NY.GDP.DEFL.KD.ZG"n "NY.GDP.MKTP.CD"n "NY.GDP.MKTP.KD.ZG"n "NY.GNP.PCAP.PP.CD"n
                       "SH.DYN.MORT"n "SH.H2O.SAFE.ZS"n "SH.IMM.MEAS"n "SH.STA.ACSN"n
					   "SP.DYN.TFRT.IN"n "SP.POP.GROW"n "SP.POP.TOTL"n "SP.URB.GROW"n);
	if cmiss(of _all_) then delete;
run;

proc sort 
	data = Data04;
	by CountryName Year;
run;

data NotConsec;
	retain b;
	set Data04;
	by CountryName Year;
	a = dif(year);
	if first.CountryName then do; 
		a = 0;
		b = Year;
	end;
	if last.CountryName then do; 
		c = Year;
	end;
	if a > 1 or b > 0 or c not in(.,24);
run;

proc sort 
	data = NotConsec (keep = CountryName) nodupkey;
	by CountryName;
run;

proc sql;
	create table Data05 as 
		select *
		from Data04 a left join NotConsec b
     		on a.CountryName = b.CountryName
		where b.CountryName is null;
quit;

proc sort 
	data = Data05;
	by Region CountryName Year;
run;

data Data05_diff (drop = help1 help2 help3 help4 help5 help6 help7);
	format Region CountryName RegionNum Year 
           MortalityU5 MortalityU5D GNID WaterD MeaslesD SanitationD FertilityD UrbanPopGD;
	set Data05;
	by Region CountryName;
	retain help1 help2 help3 help4 help5 help6 help7;
  	if first.CountryName then do; 
		MortalityU5D = .;
		GNID = .;
	    WaterD = .;
	    MeaslesD = .;
	    SanitationD = .;
	    FertilityD = .;
	    UrbanPopGD = .;
		help1 = 0;
		help2 = 0;
		help3 = 0;
		help4 = 0;
		help5 = 0;
		help6 = 0;
		help7 = 0;
	end;
	if not first.CountryName then do; 
		MortalityU5D = MortalityU5 - help1;
		GNID = GNI - help2;
		WaterD = Water - help3;
		MeaslesD = Measles - help4;
		SanitationD = Sanitation - help5;
		FertilityD = Fertility - help6;
		UrbanPopGD = UrbanPopG - help7;
	end;
	output;
	help1 = MortalityU5;
	help2 = GNI;
	help3 = Water;
	help4 = Measles;
	help5 = Sanitation;
	help6 = Fertility;
	help7 = UrbanPopG;
run;

proc means noprint missing
	data = Data05_diff;
	ways 1,2;
	class Region CountryName;
	var MortalityU5;
	output out = Data05_01 (keep = Region CountryName _freq_ 
                            rename = (_freq_ = NumberObs)
                            where = (Region not= "")) 
           sum =;
run;

proc transpose
	data = Data03 (drop = Region CountryName Year)
	out = FinalSeriesNames (keep = _name_ rename = (_name_ = SeriesCode));
run;

proc sql;
	create table FinalSeriesNames as
	select a.SeriesCode, b.SeriesName
	from FinalSeriesNames a left join SeriesNames b
	on a.SeriesCode = b.SeriesCode
	order by SeriesCode;
quit;

proc sql;
	create table Data05_diff (drop = MortalityU5D rename = (hej = MortalityU5D)) as
	select *, MortalityU5D - mean(MortalityU5D) as hej
	from Data05_diff
	group by Region
	order by Region, Year, CountryName;
quit;

data Data05_diff;
	format Region CountryName RegionNum Year MortalityU5 MortalityU5D;
	set Data05_diff;
run;


/*****************************************************************************/
/* Export to CSV                                                             */
/*****************************************************************************/
proc export 
	data = Data05_diff
	dbms = xlsx 
	outfile = "...\Export_Data.xlsx" 
	replace;
run;

proc export 
	data = Data05_01
	dbms = xlsx 
	outfile = "...\Export_Data_NumObs.xlsx" 
	replace;
run;

proc export 
	data = FinalSeriesNames
	dbms = xlsx 
	outfile = "...\Export_FinalSeriesNames.xlsx" 
	replace;
run;


/*****************************************************************************/
/* Create tables for investigating lag correlations                          */
/*****************************************************************************/
data data_lag;
	set data05_diff (keep = Region CountryName RegionNum Year MortalityU5 MortalityU5D GNID WaterD MeaslesD SanitationD FertilityD UrbanPopGD);
	if cmiss(of _all_) then delete;
run;

proc sort 
	data = data_lag;
	by Region CountryName Year;
run;

data data_lag1;
	format Region CountryName RegionNum Year MortalityU5 MortalityU5D MortalityU5DLag1;
	set data_lag end = lastone;
	set data_lag (firstobs = 2 keep = MortalityU5D rename = (MortalityU5D = MortalityU5DLag1))
	    data_lag (obs = 1 drop = _all_);
	MortalityU5DLag1 = ifn(lastone, (.), MortalityU5DLag1);
	if year = 24 then MortalityU5DLag1 =.;
run;

data data_lag2;
	format Region CountryName RegionNum Year MortalityU5 MortalityU5D MortalityU5DLag2;
	set data_lag end = lastone;
	set data_lag (firstobs = 3 keep = MortalityU5D rename = (MortalityU5D = MortalityU5DLag2))
	    data_lag (obs = 1 drop = _all_);
	MortalityU5DLag2 = ifn(lastone, (.), MortalityU5DLag2);
	if year >= 23 then delete;
run;

data data_lag3;
	format Region CountryName RegionNum Year MortalityU5 MortalityU5D MortalityU5DLag3;
	set data_lag end = lastone;
	set data_lag (firstobs = 4 keep = MortalityU5D rename = (MortalityU5D = MortalityU5DLag3))
	    data_lag (obs = 1 drop = _all_);
	MortalityU5DLag3 = ifn(lastone, (.), MortalityU5DLag3);
	if year >= 22 then delete;
run;

data data_lag4;
	format Region CountryName RegionNum Year MortalityU5 MortalityU5D MortalityU5DLag4;
	set data_lag end = lastone;
	set data_lag (firstobs = 5 keep = MortalityU5D rename = (MortalityU5D = MortalityU5DLag4))
	    data_lag (obs = 1 drop = _all_);
	MortalityU5DLag4 = ifn(lastone, (.), MortalityU5DLag4);
	if year >= 21 then delete;
run;

data data_lag5;
	format Region CountryName RegionNum Year MortalityU5 MortalityU5D MortalityU5DLag5;
	set data_lag end = lastone;
	set data_lag (firstobs = 6 keep = MortalityU5D rename = (MortalityU5D = MortalityU5DLag5))
	    data_lag (obs = 1 drop = _all_);
	MortalityU5DLag5 = ifn(lastone, (.), MortalityU5DLag5);
	if year >= 20 then delete;
run;

proc sql;
	create table data_lag_final as
	select a.Region, a.CountryName, a.RegionNum, a.Year, a.MortalityU5, a.MortalityU5D, a.MortalityU5DLag1,
	       b.MortalityU5DLag2, a.GNID, a.WaterD, a.MeaslesD, a.SanitationD, a.FertilityD, a.UrbanPopGD
	from data_lag1 a left join data_lag2 b
		on a.CountryName = b.CountryName and a.Year = b.Year
	order by a.Region, a.CountryName, a.Year;
quit;

proc sql;
	create table data_lag_final as
	select a.Region, a.CountryName, a.RegionNum, a.Year, a.MortalityU5, a.MortalityU5D, a.MortalityU5DLag1,
	       a.MortalityU5DLag2, b.MortalityU5DLag3, a.GNID, a.WaterD, a.MeaslesD, a.SanitationD, a.FertilityD, a.UrbanPopGD
	from data_lag_final a left join data_lag3 b
		on a.CountryName = b.CountryName and a.Year = b.Year
	order by a.Region, a.CountryName, a.Year;
quit;

proc sql;
	create table data_lag_final as
	select a.Region, a.CountryName, a.RegionNum, a.Year, a.MortalityU5, a.MortalityU5D, a.MortalityU5DLag1,
	       a.MortalityU5DLag2, a.MortalityU5DLag3, b.MortalityU5DLag4, a.GNID, a.WaterD, a.MeaslesD, a.SanitationD, a.FertilityD, a.UrbanPopGD
	from data_lag_final a left join data_lag4 b
		on a.CountryName = b.CountryName and a.Year = b.Year
	order by a.Region, a.CountryName, a.Year;
quit;

proc sql;
	create table data_lag_final as
	select a.Region, a.CountryName, a.RegionNum, a.Year, a.MortalityU5, a.MortalityU5D, a.MortalityU5DLag1,
	       a.MortalityU5DLag2, a.MortalityU5DLag3, a.MortalityU5DLag4, b.MortalityU5DLag5, a.GNID, a.WaterD, a.MeaslesD, a.SanitationD, a.FertilityD, a.UrbanPopGD
	from data_lag_final a left join data_lag5 b
		on a.CountryName = b.CountryName and a.Year = b.Year
	order by a.Region, a.CountryName, a.Year;
quit;

proc export 
	data = data_lag_final
	dbms = xlsx 
	outfile = "...\Export_Data_Lag.xlsx" 
	replace;
run;