/******************************************************************************
Macro:  tm_dsexist

Parameters:  Named

	data=				SAS dataset
	type=          VIEW or DATA, defaults to DATA

This macro check for the existence of a SAS dataset and returns

	0 if the dataset doesn't exist
	1 if it does exist

*******************************************************************************/
%macro tm_dsexist(data=,type=DATA);
	%local rc;
	%let rc=0;
	%if %sysfunc(exist(&data,&type))=1 %then %let rc=1;
	%str(&rc)
%mend;