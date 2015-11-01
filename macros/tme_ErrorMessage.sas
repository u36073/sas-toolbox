/*************************************************************
Macro:  tme_ErrorMessage

Outputs an error message to the SAS Log

Dependencies: None

Parameters: Named

   AppName=      Text to be used in the first line of the error message.  Intent
                 is that it is the application or macro name, but can be anything.
   module=       Text to identify which module of the application or macro that 
                 the error occured in.  Again this can be anything including blank.
   code=         A code to assign to this error.  This can also be anything.
   parameter=    Name of the application or macro parameter that is being tested.
   msg1=             Line 1 of message describing error
   msg2=            Line 2 of message describing error
   msg3=         Line 3 of message describing error

**************************************************************/

%macro tme_ErrorMessage(AppName=,module=,code=,parameter=,msg1=,msg2=,msg3=);
   %local null;
   %let null=;
   %let msg1=%trim(%left(&msg1));
   %let code=%trim(%left(&code));
   %let module=%trim(%left(&module));
   %let parameter=%trim(%left(&parameter));
   %put ************************************************************************;
   %put ** &AppName ERROR;       
   %put ** Module: &module;
   %put ** Parameter: &parameter;  
   %put ** Error Code: &code;
   %put **;
   %put ** &msg1;
   %if %quote(&msg2) ne %quote(&null) %then %do;
      %let msg2=%trim(%left(&msg2));
      %put ** &msg2;
      %end;
   %if %quote(&msg3) ne %quote(&null) %then %do;
      %let msg3=%trim(%left(&msg3));
      %put ** &msg3;
      %end;
   %put ************************************************************************;
%mend;