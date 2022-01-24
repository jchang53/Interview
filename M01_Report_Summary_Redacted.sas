/****************************************************************************************/
/* PROGRAM: M01_Report Summary.sas														*/
/* PURPOSE: Produce model report summary tables and graphs that track outlier status 	*/
/*	changes and Highlights extreme raw values and ORs									*/
/****************************************************************************************/
/* REQUIRED ARGUMENTS:																	*/
/* For Extreme OR values																*/
/*	- &HighOR: High OR cutoff (set at 4); &LowOR is an inverse of &HighOR				*/
/* BMR Tables have to be created prior to running this report							*/
/* - RawOutcomes table which is generated from SC9_Raw Estimates.sas					*/
/* - Outcomes table which is generated from BMDC1_Outcomes.sas							*/
/* - "&ReportDir.\Report\Site Specific\Data\&Outcome.\OddsRatios_&Outcome." which are 	*/
/* created at the Modeling stage.														*/
/* "&ReportDir.\Report\Site Specific\Data\ModelFitStats_&ReportCyc." which is created	*/
/* at the Modeling stage.																*/
/****************************************************************************************/
;
%let HighOR		= 4;
%let LowOR		= 1/(&HighOR);

%macro ConditionalDataset(Dataset);
%if %sysfunc(exist(&Dataset)) %then %do;
   %put NOTE: The external file &Dataset already exists!;
  %end;
%else %do;
		data &Dataset; set &ReportLib..&Dataset; 
		run; 
  %end;
%mend ConditionalDataset;
%ConditionalDataset(TQIPAnalyze);


/************************************************************/
/*** II. Extreme Risk-Adjusted Odds Ratio Values ***/


%macro ExtremeRAOddsRatios();

/*	Total Outliers Percent */
data OutlierStatus(keep=TQIPID Cohort Outcome OutlierChange HLFlag&Previous. HLFlag&Current.
                        HLFlag&Previous.  HLFlag&Current. OR&Previous. OR&Current. ); 
    length HLFlag&Previous HLFlag&Current $12;
	set "&ReportDir.\Report\Site Specific\Data\Outcomes";

	if OR&Previous. IN (., .N)	then HLFlag&Previous	= 'NA'; else
	if HLFlag&Previous.	='' 	then HLFlag&Previous	= 'Avg';

	if OR&Current. IN (., .N) 	then HLFlag&Current		= 'NA'; else
	if HLFlag&Current.	='' 	then HLFlag&Current		= 'Avg';
	OutlierChange=compress(cat(HLFlag&Previous.,':',HLFlag&Current.));
    proc sort; by Outcome Cohort TQIPID;
run;

%if &Product EQ Level III %then %do;
proc means data=RptData.Outcomes qrange q1 q3 median mean;
	where outcome=20;
	var OR&Current;
	output out=AdjIQR qrange=IQR q1=q1 q3=q3;
run;

proc sql noprint;
	select q1-1.5*IQR, q3+1.5*IQR
	into :AdjLowIQR, :AdjUpIQR
	from AdjIQR;
quit; %end;

/* Adding UnadOR, Outlier Status for Raw */
proc sql;
	create table ExtremeOutlierStatus as 
		select 	distinct o.TQIPID, o.Outcome, o.Cohort, 
				r.Total, r.Events, r.Rate,
				round(r.UnadOR,.01) as UnadOR, round(o.OR&Previous.,.01) as OR&Previous.,  round(o.OR&Current.,.01) as OR&Current., 
				r.RawOutlier as OutlierRaw, o.HLFlag&Previous. as Outlier&Previous., o.HLFlag&Current. as Outlier&Current.
	from 			OutlierStatus 			o 
	inner join 
		 			rptdata.RawOutcomes		r
	on 				o.TQIPID=r.TQIPID 	AND
					o.Outcome=r.Outcome AND
					o.Cohort=r.Cohort 
	where 	(o.Outcome NE 20 and o.OR&Current>=&HighOR)	OR (o.Outcome NE 20 and 0<=o.OR&Current<=&LowOR) OR
			(o.Outcome NE 20 and r.UnadOR >&HighOR)		OR (o.Outcome NE 20 and 0<=r.UnadOR<=&LowOR)
%if &Product EQ Level III %then %do;
	OR ((0<=o.OR&Current<=&LowOR) OR (o.Outcome=20 and ((o.OR&Current<&AdjLowIQR) or (o.OR&Current>&AdjUpIQR))))
%end;
	AND (r.RawOutlier NE 'Avg'  OR HLFlag&Current NE 'Avg')
	order by o.TQIPID, o.Outcome, o.Cohort;
quit;

%if &Product EQ Level III %then %do;
proc means data=RptData.Outcomes qrange q1 q3 median mean;
	where outcome=20;
	var OR&Current;
	output out=AdjIQR qrange=IQR q1=q1 q3=q3;
run;

proc sql noprint;
	select q1-1.5*IQR, q3+1.5*IQR
	into :AdjLowIQR, :AdjUpIQR
	from AdjIQR;
quit;
%end;


data Outcomes (keep= Outcome Cohort TQIPID Events&Season%substr(&Year,3) Rate&Season%substr(&Year,3) TotalN&Season%substr(&Year,3) OR&Current.);
set "&ReportDir.\Report\Site Specific\Data\Outcomes";
run;


proc sort data=Outcomes;by Outcome Cohort TQIPID;run;


data OutlierStatusAll;
	merge OutlierStatus (in=a)
		  Outcomes;
	by Outcome Cohort TQIPID;
	if a;
	proc sort; by Outcome Cohort OR&Current;
run;


proc sql;    title 'III. Extreme Outlier Changes from H<=>L';
	create table HighLowOutliers as 
	select TQIPID, Outcome, Cohort, OR&Previous., HLFlag&Previous., Rate&Current., OR&Current., HLFlag&Current.
	from OutlierStatusAll
	where OutlierChange IN ('High:Low', 'Low:High') 
;  
quit;


proc sql noprint;
  select distinct Outcome, Cohort, max(OR&Current) as MaxOR, min(OR&Current) as MinOR
  from OutlierStatusAll
  group by Outcome, Cohort;
quit;


/* Creating Table Comparing Outlier Status of Previous to Current Report */
proc sql ;
	create table OutlierChanges as
	select Outcome, Cohort, count(*) 		as n 'Sites',
		sum(OutlierChange IN (	'Avg:High','Avg:Low','High:Avg','Low:Avg','High:Low','Low:High')) as Changes 'Outlier Changes',
		calculated Changes/count(*) 					as ChangedSitePercent '% Changed (Sites)' format=percent10.2,
		sum(HLFlag&Current.='Avg') 						as Avg,
		sum(HLFlag&Current.='Low') 						as Low,
		sum(HLFlag&Current.='High') 					as High,
		sum(HLFlag&Current.='NA')						as NA,
		sum(OutlierChange= 'Avg:High')	 				as AvgHigh  	'Avg:High',
		sum(OutlierChange= 'Avg:Low') 	 				as AvgLow   	'Avg:Low',
		sum(OutlierChange='High:Avg')					as HighAvg 		'High:Avg',
		sum(OutlierChange= 'Low:Avg')					as LowAvg   	'Low:Avg',
		sum(OutlierChange='NA:High')					as NAHigh		'NA:High',
		sum(OutlierChange='High:NA')					as HighNA		'High:NA',
		sum(OutlierChange='NA:Low')						as NALow		'NA:Low',
		sum(OutlierChange='Low:NA')						as LowNA		'Low:NA',
		sum(OutlierChange='NA:Avg')						as NAAvg		'NA:Avg',
		sum(OutlierChange='Avg:NA')						as AvgNA		'Avg:NA',
		sum(OutlierChange='High:Low')	 				as HighLow 		'High:Low',
		sum(OutlierChange= 'Low:High')					as LowHigh  	'Low:High',
		sum(OutlierChange IN ('Avg:Low','Avg:High')) 	as AvgOutlier 	'Avg:Outlier',
		sum(OutlierChange IN ('Low:Avg','High:Avg')) 	as OutlierAvg 	'Outlier:Avg',
		sum(OutlierCHange IN ('Low:High','High:Low')) 	as Extreme		'Extreme Changes'
	from OutlierStatus
	where 	OR&Previous. is not missing AND 
			OR&Current.  is not missing	
	group by Outcome, Cohort
	/*having ChangedSitePercent>.15*/ /* optional: set filter for when model sees more than 15% changes */
	order by Outcome,Cohort
;
quit;

%mend;

%ExtremeRAOddsRatios();


%macro AllOutlierStatus(Outcome=,Cohort=);

/* Caterpillars */

/* Intercept is used in XferHrs OR calculation */
%if &Outcome=XferHrs %then %do;
	proc sql noprint;
	select Estimate 
	into :Intercept
	from "&ReportDir.\Report\Site Specific\Data\&Outcome\Intercept";
	quit; 
%end;

/* Creating GraphCat&Outcome.&Cohort. used in Caterpillars. Contains calculated OR, Lower OR, and Upper OR */
proc sql noprint;
	create table GraphCat&Outcome.&Cohort as
	select TQIPID,
%if &Outcome=XferHrs %then %do;
	  round((((exp((Estimate+&Intercept)/100))-60) - ((exp(&Intercept/100))-60)), .1)	as OR,
	  max(calculated OR) as MaxOR,
	  min(calculated OR) as MinOR,
	  round((((exp((Lower+&Intercept)/100))-60)	- ((exp(&Intercept/100))-60)), 1) 	 	as LowerOR,
	  round((((exp((Upper+&Intercept)/100))-60)	- ((exp(&Intercept/100))-60)), 1)	 	as UpperOR,
	  ProbT																				as Pvalue,
	  *
	from "&ReportDir.\Report\Site Specific\Data\&Outcome.\OddsRatios_&Outcome."

%end;
%else %do;
	  round(exp(Estimate), 0.001)	as OR,
	  max(calculated OR) as MaxOR,
	  min(calculated OR) as MinOR,
	  round(exp(Lower), 0.001) 	 	as LowerOR,
	  round(exp(Upper), 0.001)	 	as UpperOR,
	  case when exp(Estimate) GT 3 and exp(Lower)>1  	then 'High'
		   when exp(Estimate) LT (1/3) and exp(Upper)<1 then 'Low'
							 							else 'Avg' end  AS Outlier
	from "&ReportDir.\Report\Site Specific\Data\&Outcome.\OddsRatios_&Cohort."
	order by Outlier
%end;
;quit;


%if &Outcome=XferHrs %then %do;
proc means data=GraphCat&Outcome.&Cohort qrange q1 q3 median mean;
	var or;
	output out=IQR qrange=IQR q1=q1 q3=q3;
run;

/* Creating Low/High cutoffs for Outlier Status Used in TQIP Report */
proc sql;
	select q1-1.5*IQR, q3+1.5*IQR
	into :LowIQR, :UpIQR
	from IQR;
quit;


data GraphCat&Outcome.&Cohort;
	set GraphCat&Outcome.&Cohort;
	length Outlier $12.;
	if pValue LT .01 and OR<&LowIQR then Outlier='Low'; else 
	if pValue LT .01 and OR>&UpIQR	then Outlier='High';
									else Outlier='Avg';
run;

proc sort data=GraphCat&Outcome.&Cohort;; by outlier;run;
%end;



proc sql;
	create table OS_&Outcome.&Cohort. AS 
    select distinct r.Outcome format=Outcomes., r.Cohort format=Cohorts.,
		/* Non Risk-Adjusted */
		%if &Outcome EQ XferHrs %then %do;
		compress(cat( put(round(min(r.UnadOR),1),best.),'-',put(round(max(r.UnadOR),1),best.) ))		 		AS RawT2Xfer  'Delta Time to Transfer', %end; %else %do; 
		cat( put(round(min(exp(r.Estimate)),.001),z4.2),'-',put(round(max(exp(r.Estimate)),.001),z4.2) ) 		AS RawORrange 'Raw OR Range', %end;
		sum(r.RawOutlier IN ('Low')) 																	 		AS ExtremeLowRawOR 'Extreme Low Raw OR',
		sum(r.RawOutlier IN ('High'))																	 		AS ExtremeHighRawOR 'Extreme High Raw OR',

		/* Risk-Adjusted */
		%if &Outcome EQ XferHrs %then %do;
		compress(cat( put(round(min(a.OR),1),best.),'-', put(round(max(a.OR),1),best.) )) 						AS T2XferORrange 'Delta Time to Transfer', 	%end; %else %do;
		cat( put(round(min(exp(a.Estimate)),.001),z4.2),'-',put(round(max(exp(a.Estimate)),.001),z4.2) ) 		AS ORRange 'RA OR Range', 		%end;
		%if &Outcome EQ XferHrs %then %do;
		sum(a.Outlier IN ('Low')) 		AS ExtremeLowOR 	'Extreme Low RA OR',
		sum(a.Outlier IN ('High')) 		AS ExtremeHighOR 	'Extreme High RA OR' %end; %else %do;
		sum(a.Estimate<log(&LowOR.)) 	AS ExtremeLowOR 	'Extreme Low RA OR', 
		sum(a.Estimate>log(&HighOR.)) 	AS ExtremeHighOR 	'Extreme High RA OR' %end;
	from 		%if &Outcome EQ XferHrs
		%then %do; GraphCatXferHrs	  %end; 
		%else %do; "&ReportDir.\Report\Site Specific\Data\&Outcome.\OddsRatios_&Cohort." %end;	a 
	 inner join %if &Outcome EQ XferHrs
		%then %do; "&ReportDir.\Modeling\&Outcome.\&Cohort.\Raw_OddRatio_XferHrs"  %end;
		%else %do; "&ReportDir.\Modeling\&Outcome.\&Cohort.\Raw_OddRatio_&Cohort." %end;		r 
		on a.TQIPID=r.TQIPID
;
quit;

%let CohortName = &Cohort.; /*Need for &CohortN. */
%OutcomeLoop;
%CohortLoop;

proc sql;
	create table MFS_&Outcome.&Cohort. as 
	select 	distinct m.Outcome, m.Cohort,
		compress(cat(put(round(min(o.OR&Current.),.01),best.),'-', put(round(max(OR&Current.),.01),best.) )) as ORRange 'OR Range',
		m.*
	from "&ReportDir.\Modeling\ModelFitStats_&ReportCyc." %if &Outcome=XferHrs %then %do;
																			(where=(Outcome=20)) %end;
																			%else %do;
																			(where=(Outcome=&OutcomeN. AND Cohort=&CohortN.)) %end; m,
		 "&ReportDir.\Report\Site Specific\Data\outcomes" %if &Outcome=XferHrs %then %do;
																			(where=(Outcome=20)) %end;
																			%else %do;
																			(where=(Outcome=&OutcomeN. AND Cohort=&CohortN.)) %end; o
;
quit;

%mend AllOutlierStatus;

%macro RunAllOutlierStatus();

%if &Product EQ Adult %then %do;
%AllOutlierStatus(Cohort=AllPt, 	Outcome=Mortality)
%AllOutlierStatus(Cohort=Blt,   	Outcome=Mortality)
%AllOutlierStatus(Cohort=Pen,   	Outcome=Mortality)
%AllOutlierStatus(Cohort=Shock, 	Outcome=Mortality)
%AllOutlierStatus(Cohort=sTBIq, 	Outcome=Mortality)
%AllOutlierStatus(Cohort=Eld,   	Outcome=Mortality)
%AllOutlierStatus(Cohort=BltEld,	Outcome=Mortality)
%AllOutlierStatus(Cohort=IHF,   	Outcome=Mortality)

%AllOutlierStatus(Cohort=AllPt, 	Outcome=MajComps)
%AllOutlierStatus(Cohort=Blt,   	Outcome=MajComps)
%AllOutlierStatus(Cohort=Pen,   	Outcome=MajComps)
%AllOutlierStatus(Cohort=Shock, 	Outcome=MajComps)
%AllOutlierStatus(Cohort=sTBIq, 	Outcome=MajComps)
%AllOutlierStatus(Cohort=Eld,   	Outcome=MajComps)
%AllOutlierStatus(Cohort=BltEld,	Outcome=MajComps)
%AllOutlierStatus(Cohort=IHF,   	Outcome=MajComps)

%AllOutlierStatus(Cohort=AllPt, 	Outcome=CompsDth)
%AllOutlierStatus(Cohort=Blt,   	Outcome=CompsDth)
%AllOutlierStatus(Cohort=Pen,   	Outcome=CompsDth)
%AllOutlierStatus(Cohort=Shock, 	Outcome=CompsDth)
%AllOutlierStatus(Cohort=sTBIq,  	Outcome=CompsDth)
%AllOutlierStatus(Cohort=Eld,   	Outcome=CompsDth)
%AllOutlierStatus(Cohort=BltEld,	Outcome=CompsDth)
%AllOutlierStatus(Cohort=IHF,   	Outcome=CompsDth)

%AllOutlierStatus(Cohort=AllPt, 	Outcome=Embolism)
%AllOutlierStatus(Cohort=AllPt, 	Outcome=Kidney)
%AllOutlierStatus(Cohort=Shock, 	Outcome=Kidney)
%AllOutlierStatus(Cohort=AllPt, 	Outcome=PA_VAP)
%AllOutlierStatus(Cohort=sTBIq,		Outcome=PA_VAP)
%AllOutlierStatus(Cohort=AllPt, 	Outcome=ReturnOR)
%AllOutlierStatus(Cohort=AllPt, 	Outcome=SSI)
%AllOutlierStatus(Cohort=AllPt, 	Outcome=UnplannedICU)
%AllOutlierStatus(Cohort=AllPt, 	Outcome=UTI_CAUTI)
;
%end;

%if &Product EQ Pediatric %then %do;
%AllOutlierStatus(Cohort=AllPt, 	Outcome=Mortality)
%AllOutlierStatus(Cohort=AgeLE13, 	Outcome=Mortality)
%AllOutlierStatus(Cohort=AgeGE14, 	Outcome=Mortality)
%AllOutlierStatus(Cohort=sTBIq,		Outcome=Mortality)
%AllOutlierStatus(Cohort=sTBIqLE13, Outcome=Mortality)
%AllOutlierStatus(Cohort=sTBIqGE14, Outcome=Mortality)

%AllOutlierStatus(Cohort=AllPt, 	Outcome=MajComps)
%AllOutlierStatus(Cohort=AgeLE13, 	Outcome=MajComps)
%AllOutlierStatus(Cohort=AgeGE14, 	Outcome=MajComps)
%AllOutlierStatus(Cohort=sTBIq,		Outcome=MajComps)
%AllOutlierStatus(Cohort=sTBIqLE13, Outcome=MajComps)
%AllOutlierStatus(Cohort=sTBIqGE14, Outcome=MajComps)

%AllOutlierStatus(Cohort=AllPt, 	Outcome=CompsDth)
%AllOutlierStatus(Cohort=AgeLE13, 	Outcome=CompsDth)
%AllOutlierStatus(Cohort=AgeGE14, 	Outcome=CompsDth)
%AllOutlierStatus(Cohort=sTBIq,		Outcome=CompsDth)
%AllOutlierStatus(Cohort=sTBIqLE13, Outcome=CompsDth)
%AllOutlierStatus(Cohort=sTBIqGE14, Outcome=CompsDth)
;
%end;

%if "&Product." EQ "Level III" %then %do;
%AllOutlierStatus(Cohort=AllPt, Outcome=Mortality)
%AllOutlierStatus(Cohort=Eld, 	Outcome=Mortality)
%AllOutlierStatus(Cohort=IHF, 	Outcome=Mortality)

%AllOutlierStatus(Cohort=AllPt, Outcome=MajComps)
%AllOutlierStatus(Cohort=Eld,   Outcome=MajComps)
%AllOutlierStatus(Cohort=IHF,   Outcome=MajComps)

%AllOutlierStatus(Cohort=AllPt, Outcome=CompsDth)
%AllOutlierStatus(Cohort=Eld, 	Outcome=CompsDth)
%AllOutlierStatus(Cohort=IHF, 	Outcome=CompsDth)
%AllOutlierStatus(Cohort=, 		Outcome=XferHrs)
%end;

data AllOutlierStatus; set OS_:; run;
proc sort data=AllOutlierStatus; by Outcome Cohort; run;

data AllMFS; set MFS_: ; run;
proc sort data=AllMFS; by Outcome Cohort; run;

%mend RunAllOutlierStatus;

%RunAllOutlierStatus;


/*%RiskAdjustedORTable;*/
/*** III. Total Outliers by Outcome & Cohort ***/
/* Outlier status changes "High to Low" or "Low to High" */

%macro TotalOutliers();

%if &Product EQ Level III %then %do;
proc means data=RptData.Outcomes qRange q1 q3 median mean;
	where Outcome=20; /*XferHrs*/
	var OR&Current;
	output out=AdjIQR qRange=IQR q1=q1 q3=q3;
run;

proc sql noprint;
	select q1-1.5*IQR, q3+1.5*IQR
	into :AdjLowIQR, :AdjUpIQR
	from AdjIQR;
quit;	%end;



/*  Outlier % by Outcome model */
proc sql ;
	create table OutlierPercent as
	select Outcome, Cohort 'Cohort',
		sum(HLFlag&Current ='Avg')		/ sum(HLFlag&Current. IN ('Avg' 'High' 'Low'))  as Avg 		format=percent10.2,
		sum(HLFlag&Current ='High')		/ sum(HLFlag&Current. IN ('Avg' 'High' 'Low'))  as High 	format=percent10.2,
		sum(HLFlag&Current ='Low')		/ sum(HLFlag&Current. IN ('Avg' 'High' 'Low'))  as Low		format=percent10.2
	from OutlierStatus
	where OR&Current is not null
	group by  Outcome, Cohort
	order by Outcome, Cohort
;  
quit;

%mend;

%TotalOutliers();


proc sql;
	drop   table ORR;
	create table ORR (
		Outcome			char(12), 
		Cohort			char(6),
		OutcomeCohort 	char(20),
		Min				num format=8.3,
		Max				num format=8.3)
;
quit;

%macro ORRange(Outcome,Cohort);
%let VarList = TQIPID, Estimate;

proc sql; create table CRA as
	select &VarList
	from "&ReportDir.\Report\Site Specific\Data\&Outcome\OddsRatios_&Cohort"
	where Effect IN ('Intercept')
;
	insert into ORR
	select distinct "&Outcome" length=20, "&Cohort" length=20, cats("&Outcome",'_',"&Cohort") as OutcomeCohort,
		min(exp(Estimate)), max(exp(Estimate))
	from CRA
;
quit;

%mend ORRange;


%macro PrintORRange();
/**** Mortality ****/
	%ORRange(Mortality,	AllPt)
%if &Product = Adult %then %do;
	%ORRange(Mortality,	Blt)
	%ORRange(Mortality,	Pen)
	%ORRange(Mortality,	Shock)
	%ORRange(Mortality,	sTBIq)
	%ORRange(Mortality,	BltEld) 
  %end;
%if &Product NE Pediatric %then %do;
	%ORRange(Mortality,	Eld) 
	%ORRange(Mortality,	IHF);
  %end;

%if &Product = Pediatric %then %do;
	%ORRange(Mortality,	AgeLE13)
	%ORRange(Mortality,	AgeGE14)
	%ORRange(Mortality, sTBIq)
  	%ORRange(Mortality,	sTBIqLE13)
	%ORRange(Mortality,	sTBIqGE14)
  %end;

/**** MajComps ****/
	%ORRange(MajComps,	AllPt)
%if &Product = Adult %then %do;
	%ORRange(MajComps,	Blt)
	%ORRange(MajComps,	Pen)
	%ORRange(MajComps,	Shock)
	%ORRange(MajComps,	sTBIq)
	%ORRange(MajComps,	BltEld) 
  %end;
%if &Product NE Pediatric %then %do;
	%ORRange(MajComps,	Eld) 
	%ORRange(MajComps,	IHF)
%end;

%if &Product = Pediatric %then %do;
	%ORRange(MajComps,	AgeLE13)
	%ORRange(MajComps,	AgeGE14)
	%ORRange(MajComps,	sTBIq)
	%ORRange(MajComps,	sTBIqLE13)
	%ORRange(MajComps,	sTBIqGE14)
  %end;


/**** CompsDth ****/
	%ORRange(CompsDth,	AllPt)
%if &Product = Adult %then %do;
	%ORRange(CompsDth,	Blt)
	%ORRange(CompsDth,	Pen)
	%ORRange(CompsDth,	Shock)
	%ORRange(CompsDth,	sTBIq)
	%ORRange(CompsDth,	BltEld)
  %end;

%if &Product NE Pediatric %then %do;
	%ORRange(CompsDth,		Eld)
	%ORRange(CompsDth,		IHF)
  %end;

%if &Product = Pediatric %then %do;
	%ORRange(CompsDth,	AgeLE13)
	%ORRange(CompsDth,	AgeGE14)
	%ORRange(CompsDth,	sTBIq)
	%ORRange(CompsDth,	sTBIqLE13)
	%ORRange(CompsDth,	sTBIqGE14)
  %end;


/**** Indiv. Comps. ****/
%if &Product = Adult %then %do;
	%ORRange(Embolism,		AllPt)
	%ORRange(Kidney,		AllPt)
	%ORRange(Kidney,		Shock)
	%ORRange(PA_VAP,		AllPt)
	%ORRange(PA_VAP,		sTBIq)
	%ORRange(ReturnOR,		AllPt)
	%ORRange(SSI,			AllPt)
	%ORRange(UnplannedICU,	AllPt)
	%ORRange(UTI_CAUTI,		AllPt)
  %end;


/**** T2Xfer ****/
%if &Product EQ Level III %then %do;
proc sql; 
	insert into ORR
	select distinct 'EarlyXfer', 'EarlyXfer', cats('EarlyXfer','_','XferMins') as OutcomeCohort,
		min(OR), max(OR)
	from  GraphCatXferHrs 
;
quit;
  %end;

ods graphics / height=12in width=12in imagefmt=PNG;
proc sgplot data=ORR noautolegend; where outcome NE 'EarlyXfer';
	title "Risk-Adjusted OR Ranges, &ReportCyc.";
	highlow y=OutcomeCohort low=Min high=Max / type=bar group=Outcome;
	refline 'MajComps_AllPt' 'CompsDth_AllPt' 'Embolism_AllPt' / discreteoffset=-.5;
	refline 1 / axis=x lineattrs=(thickness=2 color=DarkGrey);
	yaxistable Min Max;
	xaxis grid 	  label='Odds Ratio' type=log min=.08 max=20.0 values=(0.1 .25 .5 1 2 4 8 12 16);
	yaxis reverse label='Outcome/Cohort';
run;
%if &Product EQ Level III %then %do;
proc sgplot data=ORR noautolegend; where outcome EQ 'EarlyXfer';
	title "Risk-Adjusted Time to Transfer (mins) Ranges, &ReportCyc.";
	highlow y=OutcomeCohort low=Min high=Max / type=bar group=Outcome;
	refline 0 / axis=x lineattrs=(thickness=2 color=DarkGrey);
	yaxistable Min Max ;
	xaxis grid 	  label='Delta (mins)'  min=-100 max=100 ;
run;
  %end;

%mend PrintORRange;

%PrintORRange;
**************** PRINTING EXCEL SPREADSHEET WITH RAW AND RISK-ADJUSTED TABLES FOR MODEL REPORT SUMMARY *********************************;


ods excel file="&ReportDir.\Modeling\Model Report Summary Tables, &ReportCyc. %sysfunc(date(),yymmdd10.).xlsx"; 
%macro PrintRAExcel();
* FIRST TAB: Raw rates table ;
ods excel options(sheet_name='All Outcomes' autofilter="1-9" frozen_headers="1"); 
proc report data="&ReportDir.\Report\Site Specific\Data\Outcomes" nowd
	style(report) ={font_face=Calibri font_size=10		BorderStyle=solid BorderColor=black }
	style(header) ={font_face=Calibri font_size=W.75 	BorderStyle=solid BorderColor=black foreground=black background=gainsboro fontweight=bold 	just=left }
	style(column) ={font_face=Calibri font_size=W.95 	BorderStyle=solid BorderColor=black foreground=black background=white 						just=left };
	columns Outcome Cohort TQIPID Outlier&Current. TotalN&Current. Observed&Current. Expected&Current. 
												   Events&Current. Rate&Current. 	 OR&Current.;

	define TotalN&Current.		/ display center format=comma5.;
	define Observed&Current.	/ display center;
	define Expected&Current.	/ display center; 

	define Events&Current.		/ display center format=comma5.;
	define Rate&Current.		/ display center;

	define OR&Current.			/ display center format=8.3;
	define Outlier&Current.		/ display center ;

	compute Outlier&Current.;
		if Outlier&Current.='High'	then call define (_col_,'style','style=[background=Pink]'); else 
		if Outlier&Current.='Low'	then call define (_col_,'style','style=[background=lightgreen]') ;
	endcomp;
run;

* TITLE ;
ods excel options(sheet_name='I. Extreme Outliers' autoFilter="1-9" frozen_headers="1");

proc report data=AllOutlierStatus nowd split='|'
	style(report) ={font_face=Calibri font_size=10		BorderStyle=solid BorderColor=black }
	style(header) ={font_face=Calibri font_size=W.75 	BorderStyle=solid BorderColor=black foreground=black background=gainsboro fontweight=bold just=left }
	style(column) ={font_face=Calibri font_size=W.95 	BorderStyle=solid BorderColor=black foreground=black background=white just=left };

	columns Outcome Cohort
%if &Product EQ Level III %then %do; RawT2Xfer %end;  
		RawORrange 	ExtremeLowRawOR ExtremeHighRawOR
%if &Product EQ Level III %then %do; T2XferORrange %end;
		ORrange	ExtremeLowOR ExtremeHighOR
;
	define Outcome				/ style(column)={background=greye6} center;
	define Cohort				/ style(column)={background=greye6} center;
	%if &Product EQ "Level III" %then %do;
	define RawORrange			/ display 'Range of Unadjusted|Odds Ratios' center; %end;
	define RawT2Xfer			/ display 'Range of Unadjusted|Time to Transfer' center;
	define ExtremeLowRawOR		/ display 'Extreme Low' center;
	define ExtremeHighRawOR		/ display 'Extreme High' center;
	define ORrange				/ display 'Range of Adjusted|Odds Ratios' center;
	%if &Product EQ Level III %then %do;
	define T2XferORrange		/ display 'Range of Adjusted|Time to Transfer' center style(column)=[width=10%]; %end;
	define ExtremeLowOR			/ display 'Extreme Low' center;
	define ExtremeHighOR		/ display 'Extreme High' center;

	compute ExtremeLowRawOR;
		if ExtremeLowRawOR GT 0	 then call define (_col_,'style','style=[background=lightgreen]'); 
	endcomp;

	compute ExtremeHighRawOR;
		if ExtremeHighRawOR GT 0 then call define (_col_,'style','style=[background=Pink]'); 
	endcomp;

	compute ExtremeLowOR;
		if ExtremeLowOR GT 0	 then call define (_col_,'style','style=[background=lightgreen]'); 
	endcomp;

	compute ExtremeHighOR;
		if ExtremeHighOR GT 0	 then call define (_col_,'style','style=[background=Pink]'); 
	endcomp;
run;


* SEVENTH TAB: I.a RISK-ADJUSTED OR Ranges Printing Risk-adjusted OR Bar Graph ;
ods excel options(sheet_name='I.a Risk Adjusted OR Ranges');

proc sql;
	drop table ORR;
	create table ORR (
		Outcome			char(12), 
		Cohort			char(6),
		OutcomeCohort 	char(20),
		Min				num format=8.3,
		Max				num format=8.3)
;
quit;


* OUTLIER PERCENT
* Print Outlier % by Outcome model  and flag models with more than 95% Averages ;
ods excel options(sheet_name='II. Outlier Percent' autofilter='1-9' frozen_headers='1');

proc report data=OutlierPercent nowd
	style(report) ={font_face=Calibri font_size=10		BorderStyle=solid BorderColor=black }
	style(header) ={font_face=Calibri font_size=W.75 	BorderStyle=solid BorderColor=black foreground=black background=gainsboro fontweight=bold just=left }
	style(column) ={font_face=Calibri font_size=W.95 	BorderStyle=solid BorderColor=black foreground=black background=white just=left };

	columns Outcome Cohort Avg High Low;
	define Outcome	/ group   style(column)={background=greye6} format=OutcomesLong. /*OutcomeLong*/ order=data;
	define Cohort	/ style(column)={background=greye6} 		format=CohortsLong.;
	define Avg		/ display format=percent10.1;
	define High		/ display format=percent10.1;
	define Low		/ display format=percent10.1;

	compute Avg;

if Avg >.95 then call define (_col_,'style','style=[background=Pink]') ;
	  if Avg <.50 then call define (_col_,'style','style=[background=Pink]') ;
	endcomp;
run;


* OUTLIER CHANGES ;
ods excel options(sheet_name='III. Outlier Changes' autofilter="1-11" frozen_headers="1");

proc report data=OutlierChanges nowd
	style(report) ={font_face=Calibri font_size=10		BorderStyle=solid BorderColor=black }
	style(header) ={font_face=Calibri font_size=W.75 	BorderStyle=solid BorderColor=black foreground=black background=gainsboro fontweight=bold just=left }
	style(column) ={font_face=Calibri font_size=W.95 	BorderStyle=solid BorderColor=black foreground=black background=white just=left };

	columns Outcome Cohort Changes n ChangedSitePercent  AvgHigh AvgLow HighAvg HighLow LowAvg LowHigh AvgOutlier OutlierAvg Extreme;
	define Outcome				/ group   	style(column)={background=greye6} format=OutcomesLong. /*OutcomeLong.*/ order=data ;
	define Cohort				/ 			style(column)={background=greye6} display;
	define Changes				/ display;
	define n					/ display format=best.2;
	define ChangedSitePercent	/ display format=percent10.1;
	define HighLow				/ display ;
	define LowHigh				/ display ;
	define AvgOutlier			/ display ;
	define OutlierAvg			/ display ;
	define Extreme				/ display ;

	compute ChangedSitePercent;
	  if ChangedSitePercent >.15 then call define (_col_,'style','style=[background=Pink]') ;
	endcomp;

	compute HighLow;
	  if HighLow >0 			 then call define (_col_,'style','style=[background=Pink]') ;
	endcomp;

	compute LowHigh;
	  if LowHigh >0				 then call define (_col_,'style','style=[background=Pink]') ;
	endcomp;

	compute Extreme;
	  if Extreme >0				 then call define (_col_,'style','style=[background=Pink]') ;
	endcomp;

run;

* THIRD TAB: EXTREME OUTLIER STATUS ;
ods excel options(sheet_name='Extreme Outlier Status' autofilter="1-10" frozen_headers="1");

proc report data=ExtremeOutlierStatus  nowd
	style(report) ={font_face=Calibri font_size=10		BorderStyle=solid BorderColor=black }
	style(header) ={font_face=Calibri font_size=W.75 	BorderStyle=solid BorderColor=black foreground=black background=gainsboro fontweight=bold just=left }
	style(column) ={font_face=Calibri font_size=W.95 	BorderStyle=solid BorderColor=black foreground=black background=white just=left };

	columns TQIPID Outcome Cohort Total Events Rate OR&Previous. UnadOR OR&Current. OutlierRaw  Outlier&Previous. Outlier&Current.
;
	define TQIPID				/style(column)={background=greye6};
	define Outcome 				/ style(column)={background=greye6} format=OutcomesLong. /*OutcomeLong*/;
	define Cohort 				/ style(column)={background=greye6} format=CohortsLong.  ;
	define Total 				/ format=comma7.;
	define Events 				/ format=comma7.;
	define OR&Previous. 		/ display ;
	define UnadOR				/ display 'RawOR';
	define OR&Current. 			/ display ;
	define Outlier&Previous. 	/ display;
	define OutlierRaw 			/ display;
	define Outlier&Current. 	/ display;

    compute TQIPID ;
	    IF TQIPID > 0 then do;
			CALL DEFINE(_COL_, 'style', 'style=[background=greye6]');
		END;
	endcomp;

	compute Outlier&Previous.;
	  if Outlier&Previous.= 'High' then call define (_col_,'style','style=[background=Pink]') ;
	  if Outlier&Previous.= 'Low'  then call define (_col_,'style','style=[background=lightgreen]') ;
	endcomp;

	compute OutlierRaw;
	  if OutlierRaw= 'High' then call define (_col_,'style','style=[background=Pink]') ;
	  if OutlierRaw= 'Low'  then call define (_col_,'style','style=[background=lightgreen]') ;
	endcomp;

	compute Outlier&Current.;
	  if Outlier&Current.= 'High' then call define (_col_,'style','style=[background=Pink]') ;
	  if Outlier&Current.= 'Low'  then call define (_col_,'style','style=[background=lightgreen]') ;
	endcomp;
run;


* FOURTH TAB: HIGH TO LOW OUTLIERS (May not print if no High to Low/Low to High ;
ods excel options(sheet_name='High to Low' autofilter='1-9' frozen_headers='1');

proc report data=HighLowOutliers nowd
	style(report) ={font_face=Calibri font_size=10		BorderStyle=solid BorderColor=black }
	style(header) ={font_face=Calibri font_size=W.75 	BorderStyle=solid BorderColor=black foreground=black background=gainsboro fontweight=bold just=left }
	style(column) ={font_face=Calibri font_size=W.95 	BorderStyle=solid BorderColor=black foreground=black background=white just=left };

	columns TQIPID Outcome Cohort Rate&Current. OR&Previous. OR&Current. HLFlag&Previous. HLFlag&Current.
;
	define TQIPID 			/ style(column)={background=greye6} 		order=data;
	define Outcome 			/ group  style(column)={background=greye6}	order=data	format=OutcomesLong. /*OutcomeLong.*/ ;
	define Cohort 			/ style(column)={background=greye6} 					format=CohortsLong.;
	define OR&Previous. 	/ display format=10.1;
	define OR&Current. 		/ display format=10.1;

    compute TQIPID ;
	    IF TQIPID > 0 then do;
			CALL DEFINE(_ROW_, 'style', 'style=[background=greye6]');
		end;
	endcomp;
run;


* MODEL FIT STATISTICS ;
ods excel options(sheet_name='IV. Model Fit Statistics' autofilter='1-7' frozen_headers='1');
proc report data=AllMFS nowd split='|'
	style(report)={font_face=Calibri font_size=10 	BorderStyle=solid BorderColor=black }
	style(header)={font_face=Calibri font_size=W.75 BorderStyle=solid BorderColor=black foreground=black background=gainsboro fontweight=bold just=left }
	style(column)={font_face=Calibri font_size=W.95 BorderStyle=solid BorderColor=black foreground=black background=white					  just=left};
	columns (Outcome Cohort ORRange AUC AdjRSquare HosmerLD  ProbChiSq Status pdG);
		define ORRange 		/ display 'Risk-Adjusted|OR Range';
		define AUC			/ display 'c-index';
		define AdjRSquare	/ display 'R-Square';
		define HosmerLD 	/ display 'Calibration|Curve Dev.';
		define ProbChiSq	/ display 'Pearson/DF';
		define Status		/ display 'Model Convergence';
		define pdG			/ display 'Positive|Definite G-matrix';

	compute AUC;
		if AUC LT 0.70 then do;
			call define (_col_,'style','style=[background=Pink]');
		end;
	endcomp;

	compute HosmerLD;
		if abs(HosmerLD) GE 0.05 then do;
			call define (_col_,'style','style=[background=Pink]');
		end;
	endcomp;
run; 


%PrintORRange;

%mend;

%PrintRAExcel;


ods excel close;


proc datasets NoDetails NoList; delete os_: graphcat: mfs_: ; run;
