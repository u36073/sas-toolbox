/******************************************************************************
 Macro: tm_VarLabel

 Parameters: Positional
 	x			Variable Name
   data     SAS data set

 This macro resolves to the label associated with variable &x

 Example:  %VarLabel(varname,dataset)
******************************************************************************/
%macro tm_VarLabel(x,data);
%local dsid i nvars name rc rv;
%let dsid = %sysfunc(open(&data,i));
%let nvars = %sysfunc(attrn(&dsid,NVARS));
%let rv=;
%do i=1 %to &nvars;
  %let name=%sysfunc(varname(&dsid,&i));
  %if %qupcase(&name)=%qupcase(&x) %then %do;
  	  %let rv=%sysfunc(varlabel(&dsid,&i));
     %end;
  %end;
%let rc=%sysfunc(close(&dsid));
&rv
%mend;