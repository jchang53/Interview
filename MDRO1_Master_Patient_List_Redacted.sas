*************************************************************************;
/* Program Name: MDRO1_Master_Patient_List.sas 		 					*/
/* Purpose: To create a Patient Master List (from XDRO and INEDSS)		*/
/* that accounts for misspellings in First and Last Name, DOB. This		*/
/* outputs a single version of First/Last Name/DOB.						*/;
*************************************************************************;
* PURPOSE: To create a de-duplicated Master Patient list for patients	*
* unique to 1) INEDSS only, 2) XDRO only, and 3) In INEDSS and XDRO		*
* while accounting for data entry errors in First Name, Last Name, DOB  *
* METHODS: 																*
* 1. Use %SpecimenSource and %FacilityName to standardize variables 	*
*  between data sources.												*
* 	a) For XDRO: Creates a Facility Culture var by combining Reporting  *
	and Other Facility Culture var. 									*
* 	b) For INEDSS: Creates Facility culture var by taking Ordering 		*
* 	Facility variable 													*
* 2. Use %AddKeys Clean up Names (remove hyphen, remove suffix (double 	*
*	check this))														*
* 3. Use %AddKeys to create all keys needed to detect duplicates		*
* 4. Use %AddKeys to de-duplicate at patient level (with data ent error)*
* 5. Use %DeDup to de-duplicate already de-duplicated IENDSS and XDRO	*
* datasets by each Key. If more than one patient has the same key, the  *
* latter patient will be removed from the respective dataset.			*
* 6. Use &KeyMerge to merge all de-duplicated by key dataset via an 	*
* 	INNER JOIN so patients who were removed at Step 5 would not be		*
*   re-introduced at the MERGE step.									*
* 7. Use %KeyMatch to create a merged dataset (FULL JOIN) containing 	*
* all patients in XDRO and INEDSS and which keys they matched on.		*
* 8. Create match_all dataset by de-duplicating by key and performing 	*
*	INNER merge again (essentially repeating steps 5 and 6 for combined *
*	INEDSS/XDRO dataset).												*
**** CAN USE THIS MASTER PATIENT LIST TO CREATE A DE-DUP XDRO/INEDSS DATASET*
* 1. Read in match_all dataset											*
* 2. Perform an outer LEFT JOIN on Master List, XDRO, and INEDSS dataset*
* 3. De-duplicate by patient identifiers (name, DOB, CAD)				*
*************************************************************************;

proc sql;
	create table XDRO_Recode as 
	select *,
	/* Re-coding Specimen Source so it aligns with INEDSS */
	%SpecimenSource(Source=Specimen_Source),
	case when upcase(Organism_Name) IN ('ACINETOBACTER BAUMANNII') 		  	then 'CRAB'
	 	 when upcase(Organism_Name) IN ('CANDIDA AURIS')		   		  	then 'CANDIDA AURIS'
		 when upcase(Organism_Name) IN ('SCREEN ONLY (ORGANISM UNKNOWN)') 	then 'UNKNOWN'
		 when upcase(Organism_Name) IN ('PSEUDOMONAS AERUGINOSA',
								'PSEUDOMONAS SPP.')							then 'CRPA'
		 when upcase(Organism_Name) IN ('CANDIDA HAEMULONII')			  	then 'CANDIDA HAEMULONII'
		 														  			else 'CRE'
	end															  			AS Organism_Name_Recode,
	case when upcase(Organism_Name) 	like '%SCREEN%' OR
			  upcase(Specimen_Source) 	like '%SCREEN%'		then 0
															else 1
	end														AS Clinical,
	OtherFacilityCulture, Reporting_Facility,
		case 	when OtherFacilityCulture 		 is missing 	AND /* can assume Reporting Facility is facility where culture was acquired when OtherFacilityCulture is missing*/
					 strip(Reporting_Facility) 	 is not missing AND
					 strip(upcase(Reporting_Facility)) NOT IN (		'ILLINOIS DEPARTMENT OF PUBLIC HEALTH',
																	'CHICAGO DEPARTMENT OF PUBLIC HEALTH',
																	'LIFESCAN LABORATORY, INC.',
																	'NICL LABORATORIES',
																	'LONG TERM CARE LABORATORY',
																	'ACL LABORATORIES',
																	'CENTRAL CLINICAL LABORATORY, INC',
																	'CENTRAL CLINICAL LABS, INC.'
																	'QUEST DIAGNOSTICS',
																	'ALVERNO LABORATORIES',
																	'L MEDICINE LABS, LLC' 
																	'UNKNOWN')
																								then strip(upcase(Reporting_Facility))
				 when 			   OtherFacilityCulture is not missing AND
				 	  strip(upcase(OtherFacilityCulture)) like '%SHIRLEY%'						then strip(upcase(OtherFacilityCulture))
				 when strip(OtherFacilityCulture) is not missing AND
							strip(upcase(OtherFacilityCulture)) like '%LAB%'					then 'LAB'
				 when strip(OtherFacilityCulture) is not missing 								then strip(upcase(OtherFacilityCulture))
				 when strip(upcase(Reporting_Facility)) IN ('NOT LISTED','',' ','UNKNOWN')
				 	OR strip(upcase(OtherFacilityCulture)) IN ('NOT LISTED','',' ','UNKNOWN') 	then 'UNKNOWN'
				
																								else '.x'
			end																					AS XDROFacility,
			/* Re-coding Reporting_Facility/OtherFacilityCulture to align with INEDSS */
		case %FacilityName(FacilityVar=calculated XDROFacility)
		end		AS XDROFacility_Recode, 
		Address

	from XDRO
;
quit;
proc freq data=XDRO_Recode;
tables XDROFacility_Recode*XDROFacility*Reporting_Facility*OtherFacilityCulture/list missing;
run;
data XDRO_Clean;
set XDRO_Recode;

/* Correcting data entry errors (verifiable) */
if 	Report_ID=15167 then Last_Name='Khan'; else 	/* `khan > Khan */

Last_Name=Last_Name;

/*if Report_ID=11145 then First_Name='Thomas'; else 	 */
/*if Report_ID=17048 then First_Name='Michael'; else */
if Report_ID=9815  then First_Name='L B'; else 
if Report_ID=9426  then First_Name='Laura'; /* Kaura with same DOB */
						First_Name=First_Name;


run;

/* light data processing so INEDSS matches XDRO columns and stacking INEDSS datasets */
data INEDSS_Recode;
set		INEDSS_Stacked (rename=(
						LastName=Last_Name	
						FirstName=First_Name
/*						Birth_Date=DOB*/
/*						ReportDate=Report_Date*/
/*						Admission_Date=Date_of_Admission*/
						Specimen_Collection_Date=Culture_Acquisition_Date 
						OrganismName=Organism_Name
						Current_Gender_at_Onset=Gender
						));
length Organism_Name_Recode $20.;

if upcase(Organism_Name)='CARBAPENEM RESISTANT ACINETOBACTER BAUMANNII' then Organism_Name_Recode='CRAB'; else
if upcase(Organism_Name) IN ('CANDIDA AURIS, CLINICAL',
								'CANDIDA AURIS, SCREENING')				then Organism_Name_Recode='CANDIDA AURIS';else
if upcase(Organism_Name)='CARBAPENEM RESISTANT ENTEROBACTERIACEAE' 		then Organism_Name_Recode='CRE'; else
if upcase(Organism_Name)='CARBAPENEM RESISTANT PSEUDOMONAS AERUGINOSA' 	then Organism_Name_Recode='CRPA'; else 
																			 Organism_Name_Recode='Unknown';


drop Current_Last_Name Current_First_Name Specimen_Collection_Date;
run;

proc sql;
	create table INEDSS_Clean as
	select *,
		%SpecimenSource(Source=Specimen_Source),
			case when 	upcase(Organism_Name) 	like '%SCREEN%' OR
						upcase(Specimen_Source) like '%SCREEN%'		then 0
																	else 1
	end																AS Clinical,
		case %FacilityName(FacilityVar=Ordering_Facility_Name)
		end		AS Ordering_Facility_Recode
	from INEDSS_Recode
;
quit;

%AddKeys(Dataset=XDRO);	  *10,027;
%AddKeys(Dataset=INEDSS); 	*n=861;

proc sort data=XDRO_Keys nodupkey out=nodup_XDRO_Keys; 
by Last_Name First_Name DOB;
run;
proc sort data=INEDSS_Keys nodupkey out=nodup_INEDSS_Keys; 
by Last_Name First_Name DOB;
run;

/* Flag duplicates within XDRO and INEDSS */
%macro DeDup(Key=);
%let DataList= XDRO INEDSS;

%let count=%sysfunc(countw("&DataList")); /*Loop over list of Datasets*/
	%do i = 1 %to &count;
		%let DataName=%scan(&DataList,&i);

/* de-duplicate each :Keys dataset by each Key */
proc sort data=NoDup_&DataName._Keys nodupkey out=NoDup_&DataName._&Key. dupout=Dupe_&DataName._&key.;
by &Key.;
run;

/* Sort again to prepare for merging */
proc sort data=NoDup_&DataName._&Key.;
by Last_Name_Recode First_Name_Recode DOB;
run;
	%end;

%mend DeDup;
%DeDup(Key=Key1);
%DeDup(Key=Key2); 	
%DeDup(Key=Key3); 	
%DeDup(Key=Key4); 
%DeDup(Key=Key5); 	
%DeDup(Key=Key6); 	
%DeDup(Key=Key7); 	
%DeDup(Key=Key8); 	
* 9. Accounts for misspellings after 4th letter of FN, and last two digits of DOB_Key e.g. "FLOYD SHEI 19";
%DeDup(Key=Key9); 	
* switching first and last name and taking first/last two digits of DOB year;
%DeDup(Key=Key10); 	
%DeDup(Key=Key11); 	
/*%DeDup(Key=Key12); 	*/
%DeDup(Key=finalorig_ID); 
%DeDup(Key=Key13); 	
%DeDup(Key=Key14); 
%DeDup(Key=Key15); 
%DeDup(Key=Key16);
%DeDup(Key=Key17);
%DeDup(Key=Key18);

* INNER JOIN: This dataset contains perfect matches + a unique pt from each set of pts with First/Last Name errors;

%macro KeyMerge();
%let DataList= XDRO INEDSS;

%let count=%sysfunc(countw("&DataList")); /*Loop over list of Datasets*/
	%do i = 1 %to &count;
		%let DataName=%scan(&DataList,&i);

data nodup_&DataName._final;
merge   nodup_&DataName._key1(in=k1)
		nodup_&DataName._key2(in=k2)
		nodup_&DataName._key3(in=k3)
		nodup_&DataName._key4(in=k4)
		nodup_&DataName._key5(in=k5)
		nodup_&DataName._key6(in=k6)
		nodup_&DataName._key7(in=k7)
		nodup_&DataName._key7(in=k8)
		nodup_&DataName._key9(in=k9)
		nodup_&DataName._key10(in=k10)
		nodup_&DataName._key11(in=k11)
		nodup_&DataName._key13(in=k13)
		nodup_&DataName._key14(in=k14)	/* DOB year*/
		nodup_&DataName._key15(in=k15)	/* DOB year */
		nodup_&DataName._key16(in=k16)	/* DOB day */
		nodup_&DataName._key17(in=k17)	/* DOB day */
		nodup_&DataName._key18(in=k18);	/* DOB month */
by Last_Name_Recode First_Name_Recode DOB;
if k1 and k2 and k3 and k4 and k5 and k6 and
	k7 and k8 and k9 and k10 and k11 and
	k13 and k14 and k15 and k16 and k17 and k18;
run;

%end;

%mend KeyMerge;
%KeyMerge;



**************KEY 1-12 MATCH**************;

%macro KeyMatch(Key=);

proc sql;
	create table INEDSS_XDRO_&Key. as 
	select 	i.State_Case_Number,
			x.Report_ID,
			i.First_Name as FN_INEDSS,
			x.First_Name as FN_XDRO,
			coalesce(i.First_Name,x.First_Name) as First_Name_Combined,
			i.Last_Name as LN_INEDSS,
			x.Last_Name as LN_XDRO,
			coalesce(i.Last_Name,x.Last_Name) as Last_Name_Combined,
			i.DOB as DOB_INEDSS,
			x.DOB as DOB_XDRO, 
			coalesce(i.DOB,x.DOB) as DOB_Combined format=mmddyy10.,
			coalesce(i.&Key.,x.&Key.) as &Key._Combined,
			1 as &Key._ID,
			coalesce(i.DOB,x.DOB) as DOB_Combined,
			case when INEDSS and XDRO then 'INEDSS and XDRO' 
				 when INEDSS		  then 'INEDSS'
				 when XDRO			  then 'XDRO'
				 					  else 'Unknown'
			end							AS Source
	from NoDup_INEDSS_Final 	i
	full join NoDup_XDRO_Final  x ON i.&Key.=x.&Key.;
quit;

/* de-duplicate INEDSS/XDRO dataset by each unique key - not really necessary (a check shows all dupes_: datasets are empty) */
proc sort data=INEDSS_XDRO_&Key. nodupkey dupout=dupes_&Key. out=nodup_INEDSSXDRO_&key.;
by &Key._Combined;
run;

/* sort again to prepare to merge */
proc sort data=nodup_INEDSSXDRO_&key.;
 by Last_Name_Combined First_Name_Combined DOB_Combined;
run;
%mend KeyMatch;
%KeyMatch(Key=Key1); 	
* 2. Accounts for misspellings in second letter of LN, first letter of FN e.g. "F OYD HEILA";
%KeyMatch(Key=Key2); 	
%KeyMatch(Key=Key3); 
%KeyMatch(Key=Key4); 
%KeyMatch(Key=Key5); 
%KeyMatch(Key=Key6); 	
%KeyMatch(Key=Key7); 
%KeyMatch(Key=Key8); 
* 9. Accounts for misspellings after 4th letter of FN, and last two digits of DOB_Key e.g. "FLOYD SHEI 19";
%KeyMatch(Key=Key9); 
* switching first and last name and taking first/last two digits of DOB year;
%KeyMatch(Key=Key10); 	
%KeyMatch(Key=Key11); 	
/*%KeyMatch(Key=Key12); 	*/
* Original Key;
%KeyMatch(Key=finalorig_ID); 

* New Keys added 10/22/2021;
%KeyMatch(Key=Key13); 	
%KeyMatch(Key=Key14); 
%KeyMatch(Key=Key15); 
%KeyMatch(Key=Key16);
%KeyMatch(Key=Key17);
%KeyMatch(Key=Key18);

/* INNER JOIN: merge all perfect match + unique pts from each key */
*n=9,835 vs. 9,283;
data match_all;
merge 	nodup_INEDSSXDRO_key1(in=key1)
		nodup_INEDSSXDRO_key2(in=key2)
		nodup_INEDSSXDRO_key3(in=key3)
		nodup_INEDSSXDRO_key4(in=key4)
		nodup_INEDSSXDRO_key5(in=key5)
		nodup_INEDSSXDRO_key6(in=key6)
		nodup_INEDSSXDRO_key7(in=key7)
		nodup_INEDSSXDRO_key8(in=key8)
		nodup_INEDSSXDRO_key9(in=key9)
		nodup_INEDSSXDRO_key10(in=key10)
		nodup_INEDSSXDRO_key11(in=key11)
/*		nodup_INEDSSXDRO_key12*/
		nodup_INEDSSXDRO_key13(in=key13)
		nodup_INEDSSXDRO_key14(in=key14)
		nodup_INEDSSXDRO_key15(in=key15)
		nodup_INEDSSXDRO_key16(in=key16)
		nodup_INEDSSXDRO_key17(in=key17)
		nodup_INEDSSXDRO_key18(in=key18);
by Last_Name_Combined First_Name_Combined DOB_Combined;
if key1 and key2 and key3 and key4 and key5 and key6 and
	key7 and key8 and key9 and key10 and key11 and key13 and
	key14 and key15 and key16 and key17 and key18 ;
run;

/* Now we can use this master list of patients (keys 1-18) to de-duplicate future versions of INEDSS/XDRO (see example code below) */
proc export data=Match_all
	outfile="&Output.\XDRO and INEDSS Match, %sysfunc(date(),yymmdd10.).xlsx" 
	dbms=xlsx replace;
/*	sheet="&timestamp."; */
run;

PROC IMPORT OUT=match_all DATAFILE= "&Output.\XDRO and INEDSS Match, %sysfunc(date(),yymmdd10.).xlsx"
            DBMS=XLSX REPLACE; GETNAMES=YES;  
RUN;

data match_all_key_Recoded;
set match_all;

rename 	key1_combined = Key1
		key2_combined = Key2
		Key3_Combined = Key3
		Key4_Combined = Key4
		Key5_Combined = Key5
		Key6_Combined = Key6
		Key7_Combined = Key7
		Key8_Combined = Key8
		Key9_Combined = Key9
		Key10_Combined = Key10
		Key11_Combined = Key11
/*		Key12_Combined = Key12*/
		Key13_Combined = Key13
		Key14_Combined = Key14
		Key15_Combined = Key15
		Key16_Combined = Key16
		Key17_Combined = Key17
		Key18_Combined = Key18
;
run;
proc sort data=match_all_Key_Recoded;
by Key1 Key2 Key3 Key4 Key5 Key6 Key7 Key8
	Key9 Key10 Key11 /*Key12*/ Key13 Key14 
	Key15 Key16 Key17 Key18;
run;
proc sort data=XDRO_Keys;
by Key1 Key2 Key3 Key4 Key5 Key6 Key7 Key8
	Key9 Key10 Key11 /*Key12*/ Key13 Key14 
	Key15 Key16 Key17 Key18;
run;
proc sort data=INEDSS_Keys;
by Key1 Key2 Key3 Key4 Key5 Key6 Key7 Key8
	Key9 Key10 Key11 /*Key12*/ Key13 Key14 
	Key15 Key16 Key17 Key18;
run;

data MDRO_DeDup_Test;
merge 	match_all_key_recoded
		XDRO_Keys
		INEDSS_Keys;
by 	Key1 
	Key2 
	Key3 
	Key4 
	Key5 
	Key6 
	Key7 
	Key8 
	Key9 
	Key10 
	Key11 
/*	Key12 */
	Key13 
	Key14 
	Key15 
	Key16 
	Key17 
	Key18
;
if Key1 OR Key2 OR Key3 OR Key4 OR
	Key5 OR Key6 OR Key7 OR Key8 OR
	Key9 OR Key10 OR Key11 OR /*Key12*/
	Key13 OR Key14 OR Key15 OR Key16 OR
	Key17 OR Key18;
run;	
	
*n=9,494;
proc sql;
	create table MDRO_DeDup as
	select  distinct
			x.Report_Date, i.ReportDate, coalesce(x.Report_Date,i.ReportDate) as Report_Date_Combined format=mmddyy10.,
			x.Report_ID,
			i.State_Case_Number,
			i.State_ID,

	/* Take INEDSS value, then XDRO value if INEDSS is missing */
			/* First and Last Name */
			mpl.First_Name_Combined as FN_ML,
			x.First_Name as FN_XDRO,
			i.First_Name as FN_INEDSS,
			coalesce(i.First_Name,x.First_Name) as FirstName_Recoded,
			mpl.Last_Name_Combined as LN_ML,
			x.Last_Name as LN_XDRO,
			i.Last_Name as LN_INEDSS,
			coalesce(i.Last_Name,x.Last_Name) as LastName_Recoded,
			/* DOB */
			mpl.DOB_Combined as DOB_ML,
			x.DOB as DOB_XDRO,
			i.DOB as DOB_INEDSS,
			coalesce(i.DOB,x.DOB) as DOB_Recoded format=mmddyy10.,
			coalesce(i.Gender,x.Gender) as Gender,
			coalesce(i.Patient_Address_Line_1,Address,x.Address) as Address_Recoded,
			/* This info should be the same between INEDSS and XDRO */
			i.Admission_date as AD_INEDSS, x.Date_of_Admission as AD_XDRO,
			coalesce(i.Admission_Date,x.Date_of_Admission) as AdmissionDate format=mmddyy8.,
			i.CAD1 as CAD1_INEDSS, x.CAD1 as CAD1_XDRO,
			coalesce(i.CAD1,x.CAD1) as CAD1_Combined,
			i.CAD2 as CAD2_INEDSS, x.CAD2 as CAD2_XDRO,
			coalesce(i.CAD1,x.CAD2) as CAD2_Combined,
			i.Culture_Acquisition_Date as CAD_INEDSS,
			x.Culture_Acquisition_Date as CAD_XDRO,
			coalesce(i.Culture_Acquisition_Date,x.Culture_Acquisition_Date) as CAD_Combined format=mmddyy10.,

			/* Use XDRO value for Laboratory Data */
			x.Clinical as Clinical_XDRO, i.Clinical as Clinical_INEDSS,
			coalesce(x.Clinical, i.Clinical) as Clinical_Combined,

			x.Specimen_Source_Recode as SpecimenSource_XDRO, i.Specimen_Source_Recode as SpecimenSource_INEDSS,
			case when x.Specimen_Source_Recode is missing AND 
						i.Specimen_Source_Recode is missing then .m
															else coalesce(x.Specimen_Source_Recode,i.Specimen_Source_Recode) 
			end												AS Specimen_Source_Combined,
			x.Organism_Name_Recode as Organism_XDRO, i.Organism_Name_Recode as Organism_INEDSS,
				coalesce(x.Organism_Name_Recode,i.Organism_Name_Recode) as Organism_Name_Combined,
			case 	when calculated Organism_Name_Combined='CANDIDA AURIS' 					then 'NA'
					when i.If_Other_Unknown is not missing 
						AND upcase(i.If_Other_Unknown) NOT IN ('CANDIDA AURIS',
																'MDRO',
																'MECHANISM NOT DETECTED',
																'NO MECHANISM DETECTED',
																'NOT LISTED',
																'SENT TO STATE LAB',
																'TO BE SENT TO STATE LA')	then i.If_Other_Unknown
					when  (i.Mechanism_of_Resistance is missing AND 
						 x.Mechanism_of_Resistance is missing )
					OR upcase(x.Mechanism_of_Resistance) IN ('UNKNOWN','NO MECHANISM TESTING DONE')
					OR upcase(i.Mechanism_of_Resistance) IN ('UNKNOWN','NO MECHANISM TESTING DONE')
																						then 'Unknown'
				 when  i.Mechanism_of_Resistance IN ('CRE-KPC-Enterobacter cloacae',
							 				 'CRE-KPC-Escherichia coli',
											 'CRE-KPC-Klebsiella pneumoniae',
											 'CRE-KPC-Morganella morganii',
											 'CRE-KPC-Screen only (organism unknown)') 	then 'KPC'
				 when i.Mechanism_of_Resistance IN (  'CRE-NDM-Enterobacter cloacae',
											 'CRE-NDM-Escherichia coli',
											 'CRE-NDM-Klebsiella pneumoniae',
											 'CRE-NDM-Screen only (organism unknown)')
					OR x.Mechanism_of_Resistance IN ('NDM','NDM-1') 
					OR i.Mechanism_of_Resistance IN ('NDM','NDM-1')						then 'NDM-1'
				when i.Mechanism_of_Resistance IN   ('CRE-OXA-Escherichia coli',
											'CRE-OXA-Klebsiella pneumoniae') 			then 'OXA'
				when i.Mechanism_of_Resistance IN ('CRE-VIM-Screen only (organism unknown)') 	then 'VIM'
																						else coalesce(x.Mechanism_of_Resistance,i.Mechanism_of_Resistance) 
			end																			as Mechanism length=30,
			x.XDRO,
			i.INEDSS,
			case when (x.XDRO AND i.INEDSS) then 3 
				 when x.XDRO				then 2 
				 when i.INEDSS				then 1 
											else .x
			end								AS	Source format=Source.,
			x.XDROFacility_Recode,
			i.Ordering_Facility_Recode,
			case when x.XDROFacility_Recode EQ 'UNKNOWN' 	then i.Ordering_Facility_Name
															else coalesce(x.XDROFacility_Recode,i.Ordering_Facility_Recode) 
			end										 		as OrderingFacility,
			case when strip(upcase(Tracheostomy))='YES' 	then 1
				 when strip(upcase(Tracheostomy))='NO' 		then 0
				 when strip(upcase(Tracheostomy))='UNKNOWN'
						OR Tracheostomy is missing 			then .u
															else .x
			end												AS Tracheostomy_Num,
			case when strip(upcase(Ventilator))='YES'		then 1
				 when strip(upcase(Ventilator))='NO' 		then 0
				 when strip(upcase(Ventilator))='UNKNOWN'
						OR Ventilator is missing			then .u
															else .x
			end												AS Ventilator_Num,
			case when calculated Source=1 then i.Investigation_Status
										  else 'NA'
			end								AS Investigation_Status_Recode,
			coalesce(mpl.key1_combined,x.key1,i.key1) as key1,
			coalesce(mpl.key2_combined,x.key2,i.key1) as key2,
			coalesce(mpl.key3_combined,x.key3,i.key2) as key3,
			coalesce(mpl.key4_combined,x.key4,i.key4) as key4,
			coalesce(mpl.key5_combined,x.key5,i.key5) as key5,
			coalesce(mpl.key6_combined,x.key6,i.key6) as key6,
			coalesce(mpl.key7_combined,x.key7,i.key7) as key7,
			coalesce(mpl.key8_combined,x.key8,i.key8) as key8,
			coalesce(mpl.key9_combined,x.key9,i.key9) as key9,
			coalesce(mpl.key10_combined,x.key10,i.key10) as key10,
			coalesce(mpl.key11_combined,x.key11,i.key11) as key11,
			coalesce(mpl.key13_combined,x.key13,i.key13) as key13,
			coalesce(mpl.key14_combined,x.key14,i.key14) as key14,
			coalesce(mpl.key15_combined,x.key15,i.key15) as key15, 
			coalesce(mpl.key16_combined,x.key16,i.key16) as key16, 
			coalesce(mpl.key17_combined,x.key17,i.key17) as key17, 
			coalesce(mpl.key18_combined,x.key18,i.key18) as key18

	from 		/*MDROPatientList*/ Match_all mpl
	full join 	XDRO_Keys		x 	ON ( mpl.key1_combined = x.key1 OR
										mpl.key2_combined = x.key2 OR
										mpl.key3_combined = x.key3 OR
										mpl.key4_combined = x.key4 OR
										mpl.key5_combined = x.key5 OR
										mpl.key6_combined = x.key6 OR
										mpl.key7_combined = x.key7 OR
										mpl.key8_combined = x.key8 OR
										mpl.key9_combined = x.key9 OR
										mpl.key10_combined = x.key10 OR
										mpl.key11_combined = x.key11 OR
/*										mpl.key12 = x.key12 OR*/
										mpl.key13_combined = x.key13 OR
										mpl.key14_combined = x.key14 OR
										mpl.key15_combined = x.key15 OR
										mpl.key16_combined = x.key16 OR
										mpl.key17_combined = x.key17 OR
										mpl.key18_combined = x.key18		)
	full join INEDSS_Keys 		i 	ON ( mpl.key1_combined = i.key1 OR
										mpl.key2_combined = i.key2 OR
										mpl.key3_combined = i.key3 OR
										mpl.key4_combined = i.key4 OR
										mpl.key5_combined = i.key5 OR
										mpl.key6_combined = i.key6 OR
										mpl.key7_combined = i.key7 OR
										mpl.key8_combined = i.key8 OR
										mpl.key9_combined = i.key9 OR
										mpl.key10_combined = i.key10 OR
										mpl.key11_combined = i.key11 OR
/*										mpl.key12 = i.key12 OR*/
										mpl.key13_combined = i.key13 OR
										mpl.key14_combined = i.key14 OR
										mpl.key15_combined = i.key15 OR
										mpl.key16_combined = i.key16 OR
										mpl.key17_combined = i.key17 OR
										mpl.key18_combined = i.key18		)  OR   

										(
											(i.Culture_Acquisition_Date=x.Culture_Acquisition_Date OR
											i.CAD1=x.CAD1 OR
											i.CAD2=x.CAD2)					
											AND
										(i.Specimen_Source_Recode=x.Specimen_Source_Recode) AND
										(i.Organism_Name_Recode=x.Organism_Name_Recode) AND
									(	x.key1 = i.key1 OR
										x.key2 = i.key2 OR
										x.key3 = i.key3 OR
										x.key4 = i.key4 OR
										x.key5 = i.key5 OR
										x.key6 = i.key6 OR
										x.key7 = i.key7 OR
										x.key8 = i.key8 OR
										x.key9 = i.key9 OR
										x.key10 = i.key10 OR
										x.key11 = i.key11 OR
/*										x.key12 = i.key12 OR*/
										x.key13 = i.key13 OR
										x.key14 = i.key14 OR
										x.key15 = i.key15 OR
										x.key16 = i.key16 OR
										x.key17 = i.key17 OR
										x.key18 = i.key18		)

									)
where ( x.key1 	is not null OR i.key1 	is not null ) 	OR
	  ( x.key2 	is not null OR i.key2 	is not null ) 	OR
	  ( x.key3 	is not null OR i.key3 	is not null )	OR
	  ( x.key4 	is not null OR i.key4 	is not null )	OR
	  ( x.key5 	is not null OR i.key5 	is not null )	OR
	  ( x.key6 	is not null OR i.key6 	is not null )	OR
	  ( x.key7 	is not null OR i.key7 	is not null )	OR
	  ( x.key8 	is not null OR i.key8 	is not null )	OR
	  ( x.key9 	is not null OR i.key9 	is not null )	OR
	  ( x.key10 is not null OR i.key10	is not null )	OR
	  ( x.key11 is not null OR i.key11 	is not null )	OR
	  ( x.key13 is not null OR i.key13 	is not null )	OR
	  ( x.key14 is not null OR i.key14 	is not null )	OR
	  ( x.key15 is not null OR i.key15 	is not null )	OR
	  ( x.key16 is not null OR i.key16 	is not null )	OR
	  ( x.key17 is not null OR i.key17 	is not null )	OR
	  ( x.key18 is not null OR i.key18 	is not null )
order by FirstName_Recoded, LastName_Recoded, DOB_Recoded, CAD_Combined				
;
quit;
/*proc sort data=MDRO_DeDup nodupkey dupout=MDRO_dupes out=MDRO_nodup;*/
/*by LastName_Recoded FirstName_Recoded DOB_Recoded Organism_Name_Combined Specimen_Source_Combined;*/
/*run;*/

/*proc freq data=MDRO_DeDup;*/
/*tables Clinical_combined*SpecimenSource_INEDSS*SpecimenSource_XDRO*Organism_XDRO*Organism_INEDSS/list missing;*/
/*run;*/
/*proc freq data=MDRO_DeDup;*/
/*tables Mechanism/list missing;*/
/*run;*/
/* De-duplicating my Culture Acquisition Date data entry errors */
%macro DeDupCAD(CAD=);
proc sort data=MDRO_DeDup nodupkey dupout=MDRO_&CAD._dup out=MDRO_&CAD._nodup;
by LastName_Recoded FirstName_Recoded DOB_Recoded Organism_Name_Combined Specimen_Source_Combined &CAD.;
run;

%mend DeDupCAD;
%DeDupCAD(CAD=CAD1_Combined);
%DeDupCAD(CAD=CAD2_Combined);
%DeDupCAD(CAD=CAD_Combined);

/*n=9,494 vs. n=9,169*/
data MDRO_DeDup_Final;
merge 	MDRO_CAD1_Combined_nodup (in=CAD1)
		MDRO_CAD2_Combined_nodup (in=CAD2)
		MDRO_CAD_Combined_nodup  (in=CAD);
by LastName_Recoded FirstName_Recoded DOB_Recoded;
if CAD1 AND CAD2 AND CAD;

AllCases=1;
run;
proc freq data=MDRO_DeDup_Final;
tables SpecimenSource_XDRO*SpecimenSource_INEDSS*Specimen_Source_Combined/list missing;
run;

proc datasets lib=work nolist ;
delete 		nodup_INEDSSXDRO_key: 
			nodup_INEDSS_key:
			nodup_XDRO_key: 
			dupes_:
			inedss_xdro_key:
			mdro_cad1:
			mdro_cad2:
			mdro_cad:
			dup_inedss_key:
			dupe_inedss:
			dupe_xdro:
			dup_xdro_key:
			nodup_:
;
run;

