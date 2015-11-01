/******************************************************************************
 Macro: tme_ValidWindowsName

 Test for a valid Windows file name.

 Dependencies: tme_ErrorMessage
               tm_ValidWindowsName

 Parameters: Named

  name=         value to test for being a valid Windows file name
  AppName=      Text to be used in the first line of the error message.  Intent
                is that it is the application or macro name, but can be anything.
  module=       Text to identify which module of the application or macro that
                the error occured in.  Again this can be anything including blank.
  code=         A code to assign to this error.  This can also be anything.
  parameter=    Name of the application or macro parameter that is being tested.
******************************************************************************/

%macro tme_ValidWindowsName(AppName=,
                            module=,
                            code=,
                            parameter=,
                            name=
                            );

   %if %tm_ValidWindowsName(&name)=0 %then %do;
       %tme_ErrorMessage(AppName=&AppName,
                         module=&module,
                         code=&code,
                         parameter=&parameter,
                         msg1=&name is not a valid Windows file name,
                         msg2=
                         );
      %end;
   %else %do;
      %let code=0;
      %end;
%str(&code)
%mend;