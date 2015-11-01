/**********************************************************************

This macro will identify all character variables that should be numeric, 
and converts them to numeric.  It also assigns a length statement to the
new variable based on the length of the pre-converted character variables.  
This is done to help minimize the disk space used in the new dataset.  

Note:  To overwrite the new dataset, just specify the same dataset as 
       your out= dataset.

The format of the macro is:

%chr2num ( data= , out= <,report=> );

   data=       Input dataset (required)
   out=        Output dataset (required)
   report=     Report File.  Defaults to chr2num.report if unspecified.


Macro Variables and what values the will contain 

Note:  Variable names of the form &&var&n in comments, represents the 
       macro variables &var1, var2,... var&k

&&var&n     Dataset variable names  
&&type&n    Corresponding Dataset variable type (1=numeric, 2=character)
&&lbl&n     Corresponding Dataset variable label
&&len&n     Corresponding Dataset variable length
&nobs       Number of variables in the dataset.
&dsobs      # of observations in the input dataset
&sampsize   % of dataset to use when determining if a character variable
            should be converted to a numeric or not.

**********************************************************************/

%macro chr2num(data=,out=,report=chr2num.report.doc,
			   drop=
		);


%let null=;

%if %quote(&out)=%quote(&null) %then %let out=&data;

/**********************************************************************
The proc contents outputs the input dataset information into a 
dataset, work.contents.  The data _null_ takes the information in
the work.contents dataset and stores them into macro variables.
**********************************************************************/
proc contents data=&data out=contents noprint;
run;


data _null_;
   set contents end=last;
   call symput('var'||trim(left(put(_n_,best4.))),trim(left(name)));
   call symput('type'||trim(left(put(_n_,best4.))),trim(left(type)));
   call symput('lbl'||trim(left(put(_n_,best4.))),trim(left(label)));
   if last then do;
      call symput('nobs',trim(left(_n_)));
      call symput('dsobs',trim(left(nobs)));
   	return;
   	end;
   return;
   run;

/********************************************************************** 
Take a maximum sample of 10000 from the input dataset.  The percentage
needed is computed and stored in the macro variable &sampsize.
**********************************************************************/
%if &dsobs<=10000 %then %let sampsize=1;
%else %do;
   data _null_;
      sampsize=10000/&dsobs; 
      call symput('sampsize',trim(left(put(sampsize,best6.4))));
      return;
      run;
   %end;

data rsdata;
   set &data;
   if ranuni(-1)<=&sampsize then output;
   return;
   run;

/**********************************************************************
Run a proc freq with a tables statement for each character variable in
the dataset and output the results for each into a corresponding dataset
work.tb&n   
**********************************************************************/
proc freq data=rsdata;
%do i=1 %to &nobs;
   %if &&type&i=2 %then %str(tables &&var&i / out=tb&i noprint;);
   %end;
run;

/**********************************************************************
Parse each tb&n dataset and determine if the corresponding variable
should indeed be a numeric variable.  This is done using the input()
dataset function.  &&flag&n will be set to 1 if the variable is to be
converted.  
**********************************************************************/
%do i=1 %to &nobs;
   %let flag&i=0;
   %if &&type&i=2 %then %do;
      %let flag&i=1;
      data _null_;
         set tb&i;
         call symput("len&i",length(trim(left(&&var&i))));
         if (input(&&var&i,best22.)=.) and (&&var&i ne '') then do;
            call symput("flag&i",'0');
            stop;
            end;   
         return;
         run;
      %end;
   %end;

filename flat "&report";

/**********************************************************************
The new dataset is produced here with the converted variables.  Note 
that the variable names will remain the same for the converted variables.
Also lengths for the variables are computed based on the length of the 
pre-converted character variable.  A report is also produced with a list
of variables converted. 
**********************************************************************/
data &out(compress=yes);
   set &data(rename=(
      %do i=1 %to &nobs;
         %if &&type&i=2 and &&flag&i=1 %then %str(&&var&i=z1xr&i );
         %end;
      ));
   file flat;
   %do i=1 %to &nobs;
      %if &&type&i=2 and &&flag&i=1 %then %do;

         %if &&len&i<=3 %then %do;
            length &&var&i 8;
            %end;
         %else %if &&len&i<=7 %then %do;
            length &&var&i 8;
            %end;
         %else %if &&len&i<=9 %then %do;
            length &&var&i 8;
            %end;
         %else %if &&len&i<=12 %then %do;
            length &&var&i 8;
            %end;
         %else %if &&len&i<=14 %then %do;
            length &&var&i 8;
            %end;
         %else %do;
            length &&var&i 8;
            %end;

         &&var&i=z1xr&i;
         label &&var&i="&&lbl&i";

         if _n_=1 then put "&&var&i was converted to a numeric variable.";
         drop z1xr&i &drop;
         %end;
      %end;
   return;
   run;  
   
%mend;   

   
      
         
      


