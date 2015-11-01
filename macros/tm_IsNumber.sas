/******************************************************************************
 Macro: tm_IsNumber

 Parameters: Positional
   x        SAS Variable Name

 This macro determines if the supplied entry is a valid base 10 real number.
 It returns a value of 0 or 1

 0      Invalid Number
 1      Valid Number

 Example:  %do i=1 %to %dsobs(work.test);  .....  %end;
******************************************************************************/

%macro tm_IsNumber(x);
    %let null=;
    %let rv=1;
    %let x=%upcase(%trim(%left(&x)));
    %if %quote(x)=%quote(&null) %then %do;
        %let rv=0;
        %end;
    %else %do;
        %do i=1 %to %length(x);
            %if &i=1 %then %do;
         %if %qsubstr(&x,&I,1) ne %quote(0) and
             %qsubstr(&x,&I,1) ne %quote(1) and
             %qsubstr(&x,&I,1) ne %quote(2) and
             %qsubstr(&x,&I,1) ne %quote(3) and
             %qsubstr(&x,&I,1) ne %quote(4) and
             %qsubstr(&x,&I,1) ne %quote(5) and
             %qsubstr(&x,&I,1) ne %quote(6) and
             %qsubstr(&x,&I,1) ne %quote(7) and
             %qsubstr(&x,&I,1) ne %quote(8) and
             %qsubstr(&x,&I,1) ne %quote(9) and
             %qsubstr(&x,&I,1) ne %quote(.) and
             %qsubstr(&x,&i,1) ne %quote(-)
         %then %let rv=0;
         %end;
      %else %do;
         %if %qsubstr(&x,&I,1) ne %quote(0) and
             %qsubstr(&x,&I,1) ne %quote(1) and
             %qsubstr(&x,&I,1) ne %quote(2) and
             %qsubstr(&x,&I,1) ne %quote(3) and
             %qsubstr(&x,&I,1) ne %quote(4) and
             %qsubstr(&x,&I,1) ne %quote(5) and
             %qsubstr(&x,&I,1) ne %quote(6) and
             %qsubstr(&x,&I,1) ne %quote(7) and
             %qsubstr(&x,&I,1) ne %quote(8) and
             %qsubstr(&x,&I,1) ne %quote(9) and
             %qsubstr(&x,&I,1) ne %quote(.)
         %then %let rv=0;
         %end;
            %end;
        %end;
&rv
%mend;