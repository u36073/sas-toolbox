/*************************************************************
Macro:  tme_ValueNotNull

Dependencies: tme_ErrorMessage

Parameters: Named

   macrovar=     Macro variable to be tested for having a null value
   AppName=      Text to be used in the first line of the error message.  Intent
                 is that it is the application or macro name, but can be anything.
   module=       Text to identify which module of the application or macro that 
                 the error occured in.  Again this can be anything including blank.
   code=         A code to assign to this error.  This can also be anything.
   parameter=    Name of the application or macro parameter that is being tested.

**************************************************************/
%macro tme_ValueNotNull(value=,AppName=,module=,code=,parameter=);
   %let null=;
   %if %quote(&value)=%quote(&null) %then %do;
      %tme_ErrorMessage(module=&module,
                    code=&code,
                    parameter=&parameter,
                    msg1=Null value found. [&parameter] does not have a value assigned to it.,
                    msg2=,
                    msg3=,
                   AppName=&AppName
                    );
      %end;
   %else %do;
      %let code=0;
      %end;
   %str(&code)
%mend;