/******************************************************************************
 Macro: tm_VarType

 Parameters: Positional
 	x	   Variable Name
  data     SAS data set

 This macro resolves to the type associated with variable &x
 	N= numerical
 	C= character

 Example:  %VarLabel(varname,dataset)
******************************************************************************/
%macro tm_VarType(x,data);
%local dsid i nvars name rc rv;
%let dsid = %sysfunc(open(&data,i));
%let nvars = %sysfunc(attrn(&dsid,NVARS));
%let rv=;
%do i=1 %to &nvars;
  %let name=%sysfunc(varname(&dsid,&i));
  %if %qupcase(&name)=%qupcase(&x) %then %do;
  	  %let rv=%sysfunc(vartype(&dsid,&i));
     %end;
  %end;
%let rc=%sysfunc(close(&dsid));
&rv
%mend;