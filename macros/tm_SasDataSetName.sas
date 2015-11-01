/******************************************************************************
 Macro: tm_SasDataSetName

 Dependencies: %tm_sasname()

 Parameters: Positional
   x        SAS Variable Name

 This macro determines if the supplied entry is a valid Sas Data Set Name.
 The name can be a one or two level name. It returns a value of 0 or 1

 0      Invalid SAS Dataset Name
 1      Valid SAS Dataset Name
******************************************************************************/

%macro tm_SasDataSetName(x);
    %local null x w1 w2 rv;
    %let null=;
    %let rv=1;
    %let x=%trim(%left(%qupcase(&x)));
    %let w1=%scan(&x,1,%str(.));
    %let w2=%scan(&x,2,%str(.));
    %if %tm_sasname(&w1)=0 %then %let rv=0;
    %else %if &w2 ne &null %then %do;
        %if %tm_sasname(&w2)=0 %then %let rv=0;
        %end;
    %if %qsubstr(&x,1,1)=%quote(.) or %qsubstr(&x,%length(&x),1)=%quote(.) %then %let rv=0;
    &rv
%mend;