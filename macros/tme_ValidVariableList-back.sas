/*************************************************************************************************
Macro:  tme_ValidVariableList
Version: 1.00
Last Updated On:  4/26/2012

Tests all variables in a space delimited list for specified type, specified maximum name length,
and presence in a specified sas dataset.

This macro uses some base sas code for improved performance when there are many
variables to test.   It is not uncommon to have a thousand or more variables to test.  Iterating
through that many using the tme_ValidVariable macro can take several minutes to finish.  The
technique used here will run in seconds regardless of the amount of variables in the list.

The tradeoff is that this macro can't be used like a function call.  It passes a return code value
to a macro variable specified in the rcMacroVar parameter.

Dependencies: tme_ErrorMessage
              tm_ParseList

Parameters: Named

  type=         Variable type that the test variable must be to pass.
                C = Character   N = Numeric   A= Any (No test is performed for variable type)
  vars=         Space separated list of variables to test
  data=         Name of dataset to test against
  testlength=   Y = test length of variable name to be less than or equal to parameter maxlength=
                N = do not test the length of the variable name
  maxlength=    maximum length the variable name is allowed to be.  Defaults to 32.
  AppName=      Text to be used in the first line of the error message.  Intent
                is that it is the application or macro name, but can be anything.
  module=       Text to identify which module of the application or macro that
                the error occured in.  Again this can be anything including blank.
  code=         A code to assign to this error.  This can also be anything.
  parameter=    Name of the application or macro parameter that is being tested.
  rcMacroVar=   A macro variable name (does not need to previously exist) that the return code
                will be stored in on macro exit.

*****************************************************************************************************/


%macro tme_ValidVariableList(type=,   /*A=Any, C=Character, N=Numeric*/
             vars=,
             data=,
             AppName=,
             module=,
             code=,
             parameter=,
             testlength=Y,
             maxlength=32,
             rcMacroVar=
            );

%local i msg1 msg2 flag null nvars missing_count missing_vars badtype_count badtype_vars
       badlength_count badlength_vars
       ;
       
%if %symexist(&rcMacroVar)=0 %then %do;
	%global &rcMacroVar;
	%end;

%let msg1=; %let msg2=;
%let flag=0; %let null=;

%let nvars=%tm_ParseList(_vvl_var,&vars);

proc contents data=&data out=work._vvl_contents noprint;
run;

data work._vvl_contents;
    set work._vvl_contents;
    name=trim(left(upcase(name)));
    type=trim(left(type));
    namelength=length(name);
    run;

data work._vvl_list;
    length name $ 32;
    %do i=1 %to &nvars;
       name=trim(left(upcase("&&_vvl_var&i")));
       output;
       %end;
    run;

proc sort data=work._vvl_contents; by name; run;
proc sort data=work._vvl_list; by name; run;

%let missing_count=0;
%let missing_vars=0;

data _null_;
   merge work._vvl_contents(in=in1)
         work._vvl_list(in=in2) end=last
         ;
   by name;
   length z $ 4096;
   retain c 0 z "";
   if in2 and not in1 then do;
      c=c+1; z=trim(left(z))||" "||trim(left(name));
      end;
   if last then do;
      call symput("missing_count",trim(left(c)));
      call symput("missing_vars",trim(left(z)));
      end;
   run;

%let badtype_count=0;
%let badtype_vars=;

%if &missing_count=0 and %qupcase(&type)=%quote(C) or %qupcase(&type)=%quote(N) %then %do;
   data _null_;
      merge work._vvl_contents(in=in1)
            work._vvl_list(in=in2) end=last
            ;
      by name;
      length z $ 4096;
      retain c 0 z "";
      %if %qupcase(&type)=%quote(C) %then %do;
         if type ne 2 then do;
         %end;
      %else %if %qupcase(&type)=%quote(N) %then %do;
         if type ne 1 then do;
         %end;
            c=c+1; z=trim(left(z))||" "||trim(left(name));
            end;
      if last then do;
         call symput("badtype_count",trim(left(c)));
         call symput("badtype_vars",trim(left(z)));
         end;
      run;
   %end;

%let badlength_count=0;
%let badlength_vars=;

%if &badtype_count=0 and %qupcase(TestLength)=%quote(Y) %then %do;
   data _null_;
      merge work._vvl_contents(in=in1)
            work._vvl_list(in=in2) end=last
            ;
      by name;
      length z $ 4096;
      retain c 0 z "";
      if namelength>&maxlength then do;
         c=c+1; z=trim(left(z))||" "||trim(left(name));
         end;
      if last then do;
         call symput("badlength_count",trim(left(c)));
         call symput("badlength_vars",trim(left(z)));
         end;
      run;
   %end;

%if &missing_count>0 %then %do;
   %tme_ErrorMessage(AppName=&AppName,
                     module=&module,
                     code=&code.A,
                     parameter=&parameter,
                     msg1=The following &missing_count variables were not found in dataset &data ,
                     msg2=&missing_vars,
                     msg3=
                     );
   %let &rcMacroVar=&code.A;
   %end;
%else %if &badtype_count>0 %then %do;
   %tme_ErrorMessage(AppName=&AppName,
                     module=&module,
                     code=&code.B,
                     parameter=&parameter,
                     %if %qupcase(&type)=%quote(C) %then %do;
                        msg1=The following &badtype_count variables are not character variables ,
                        %end;
                     %else %if %qupcase(&type)=%quote(N) %then %do;
                        msg2=The following &badtype_count variables are not numeric variables ,
                        %end;
                     msg2=&badtype_vars,
                     msg3=
                     );
   %let &rcMacroVar=&code.B;
   %end;
%else %if &badlength_count>0 %then %do;
   %tme_ErrorMessage(AppName=&AppName,
                     module=&module,
                     code=&code.C,
                     parameter=&parameter,
                     msg1=The following &badlength_count variables have a name greater than &maxlength characters,
                     msg2=&badlength_vars,
                     msg3=
                     );
   %let &rcMacroVar=&code.C;
   %end;
%else %do;
   %let &rcMacroVar=0;
   %end;
%mend;
