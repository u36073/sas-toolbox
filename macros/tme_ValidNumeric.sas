/******************************************************************************
 Macro: tme_ValidNumeric

 Tests for a valid numeric value and additional criteria of minimum, maximum,
 and null if specified.

 Dependencies: tme_ErrorMessage
               tm_isnumber


 Parameters: Named

  value=        The value to test
  maxvalue=     Maximum allowed value.  Leaving blank skips this test.
  minvalue=     Minimum allowed value.  Leaving blank skips this test.
  nullok=       Y = null value considered valid.  N = null value considered invalid.
  AppName=      Text to be used in the first line of the error message.  Intent
                is that it is the application or macro name, but can be anything.
  module=       Text to identify which module of the application or macro that
                the error occured in.  Again this can be anything including blank.
  code=         A code to assign to this error.  This can also be anything.
  parameter=    Name of the application or macro parameter that is being tested.
******************************************************************************/

%macro tme_ValidNumeric(value=,
                  maxvalue=,
                  minvalue=,
                  module=,
                  code=,
                  parameter=,
                  nullok=N,
                  AppName=
                  );
%local null msg2 msg3;
%let msg2=; %let msg3=;
%let null=;

%if %quote(&value)=%quote(&null) and %qupcase(&nullok)=%quote(N) %then %do;
  %let code=&code.A;
  %tme_ErrorMessage(module=&module,
            code=&code,
            parameter=&parameter,
            msg1=No value supplied for this parameter.,
            msg2=&msg2,
            msg3=&msg3,
            AppName=&AppName
            );
   %goto vnend;
  %end;

%if %tm_IsNumber(&value)=0 %then %do;
  %let code=&code.B;
  %tme_ErrorMessage(module=&module,
            code=&code,
            parameter=&parameter,
            msg1=Value entered is not a number.,
            msg2=&msg2,
            msg3=&msg3,
            AppName=&AppName
            );
   %goto vnend;
  %end;

%if %quote(&maxvalue) ne %quote(&null) %then %do;
   %if %sysevalf(&value-&maxvalue,CEIL)>=1 %then %do;
     %let code=&code.C;
     %tme_ErrorMessage(module=&module,
              code=&code,
              parameter=&parameter,
              msg1=Value entered is greater than &maxvalue.,
              msg2=&msg2,
              msg3=&msg3,
              AppName=&AppName
              );
      %goto vnend;
     %end;
  %end;

%if &minvalue ne &null %then %do;
   %if %sysevalf(&minvalue-&value,CEIL)>=1 %then %do;
     %let code=&code.D;
     %tme_ErrorMessage(module=&module,
              code=&code,
              parameter=&parameter,
              msg1=Value entered is less than &minval.,
              msg2=&msg2,
              msg3=&msg3,
              AppName=&AppName
              );
      %goto vnend;
     %end;
  %end;
%let code=0;
%vnend:
%str(&code)
%mend;