/*************************************************************************************************
Macro:  tme_ValidVariable

Determines if a variable name is a valid name, in the specified dataset, of the specified type,
and the name is under a specified length.

Dependencies: tme_ErrorMessage
              tm_sasname
              tm_VarInDataset
              tm_VarType

Parameters: Named

  type=         Variable type that the test variable must be to pass.
                C = Character   N = Numeric   A= Any (No test is performed for variable type)
  name=         Name of variable to test
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

*****************************************************************************************************/


%macro tme_ValidVariable(type=,   /*A=Any, C=Character, N=Numeric*/
             name=,
             data=,
             module=,
             code=,
             parameter=,
             testlength=Y,
             maxlength=32,
             AppName=
            );

%local msg1 msg2 flag null; %let msg1=; %let msg2=;
%let flag=0; %let null=;

%let name=%trim(%left(&name));

%if %tm_sasname(&name)=0 %then %do;
  %let code=&code.A;
  %let msg1=&name is not a valid SAS variable name.;
  %let flag=1;
  %goto bottom;
  %end;

%if %qupcase(&testlength)=%quote(Y) %then %do;
  %if %length(&name)>&maxlength %then %do;
    %let code=&code.B;
     %let msg1=&name is longer than &maxlength characters.;
     %let msg2=This application only accepts variable names shorter than &maxlength characters for this parameter.;
     %let flag=1;
     %goto bottom;
    %end;
  %end;

%if %tm_VarInDataset(&name,&data)=0 %then %do;
  %let code=&code.C;
  %let msg1=Variable &name not found in dataset &data.;
  %let flag=1;
  %goto bottom;
  %end;

%if %qupcase(&type)=%quote(N) and %tm_VarType(&name,&data) ne N %then %do;
  %let code=&code.D;
  %let msg1=Variable &name is not of type numeric.;
  %let flag=1;
  %goto bottom;
  %end;

%if %qupcase(&type)=%quote(C) and %tm_VarType(&name,&data) ne C %then %do;
  %let code=&code.E;
  %let msg1=Variable &name is not of type character.;
  %let flag=1;
  %goto bottom;
  %end;


%bottom:
%if &flag ne 0 %then %do;
  %tme_ErrorMessage(module=&module,
            code=&code,
            parameter=&parameter,
            msg1=&msg1,
            msg2=&msg2,
            AppName=&AppName
            );
  %end;
%else %do;
  %let code=0;
  %end;
%str(&code)
%mend;