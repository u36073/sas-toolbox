/****************************************************************************** 
 Macro: tme_ValidDataset                                                                  

 Test for a valid dataset.  If expecting a pre-existing dataset also tests 
 for it's existence.

 Dependencies: tme_ErrorMessage
               tm_SasDatasetName
               tm_dsexist               

 Parameters: Named
  
  type=         Type of dataset to test.  NEW or EXISTS
  name=         Dataset name.  This can be a two level specification 
                of <libname>.<dataset>
	AppName=			Text to be used in the first line of the error message.  Intent
	              is that it is the application or macro name, but can be anything.
	module=       Text to identify which module of the application or macro that 
	              the error occured in.  Again this can be anything including blank.
	code=         A code to assign to this error.  This can also be anything.
	parameter=    Name of the application or macro parameter that is being tested.                                                                                                                                                                
******************************************************************************/ 

%macro tme_ValidDataset(type=new,  /* New or Exists */
						name=,
						module=,
						code=,
						parameter=,
						AppName=
						);

%local msg1 msg2 flag null w1 w2; 
%let msg1=; %let msg2=; 
%let flag=0; 

%let null=;

%if %tm_SasDataSetName(&name)=0 %then %do;
	%let code=&code.A;
	%let msg1=Not a valid SAS Name;						
	%let flag=1;
	%goto bottom;		
	%end;

%let w1=%scan(&name,1,.);
%let w2=%scan(&name,2,.);
%if &w2 ne &null and %sysfunc(libref(&w1)) ne 0 %then %do;
	%let code=&code.B;
	%let msg1=Library &w1 has not been assigned or does not exist.;							
	%let flag=1;
	%goto bottom;
	%end;
	
%if %qupcase(&type)=%quote(EXISTS) and %tm_dsexist(data=&name,type=DATA)=0 %then %do;
	%let code=&code.C;
	%let msg1=Dataset &name does not exist.;
	%let flag=1;	
	%goto bottom;
	%end;
	
%bottom:
%if &flag ne 0 %then %do;
	%tme_ErrorMessage(AppName=&AppName,
	          module=&module,
					  code=&code,
					  parameter=&parameter,
					  msg1=&msg1,
					  msg2=&msg2
	   			  );
	%end;
%else %do;
	%let code=0;
	%end;
%str(&code)
%mend;