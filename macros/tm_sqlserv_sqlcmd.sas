options mprint mlogic mcompilenote=all;

%macro tm_sqlserv_sqlcmd(command=,								 
								 command_file=,
								 output_file=,								 
								 print_output=y,
								 print_row_max=250,
								 query=y,
								 out=,
								 guessingrows=10000
								 );
options xsync noxwait;
%local workpath rn null output_filex parms;
%let workpath=%sysfunc(pathname(work));
%let rn=%sysfunc(int(%sysevalf(9999999*(%sysfunc(ranuni(-1))))));
%let null=;

%if %quote(&output_file)=%quote(&null) %then %do;
	%let output_filex=&workpath.\_sqlcmd%trim(%left(&rn.)).out;
	%end;

%let parms=-S &sqlserv_protocol.:&sqlserv_server.,&sqlserv_port.;
%let parms=&parms. -U &sqlserv_user. -P &sqlserv_password.;
%let parms=&parms. -o "&output_filex" -w 65535;

%if %qupcase(&query)=%quote(Y) %then %do;
	%let parms=&parms. -s "," -W;
	%end;

%if %quote(&command) ne %quote(&null) %then %do;
	%let parms=&parms. -Q "set nocount on %quote(&command.)";
	%end;
%else %if %quote(&command_file) ne %quote(&null) %then %do;
	%let parms=&parms -i "&command_file.";
	%end;

%sysexec "&sqlserv_sqlcmd" &parms.;

%if %sysfunc(fileexist(&output_filex.)) and %qupcase(&print_output.)=%quote(Y) %then %do;
	data _null_;
		file log;
		infile "&output_filex." lrecl=15000;
		if _n_=1 then do;
			put "**************************************************************";
			put "SQL SERVER OUTPUT";
			put;
			put "    Top &print_row_max lines of output printed.";
			put "**************************************************************";
			end;
		put;
		input @1 x $1.;
		put _infile_ @;
		run;
	%end;

%if %quote(&out) ne %quote(&null) %then %do;
	proc import datafile="&output_filex."
	            out=&out dbms=csv replace;
	getnames=yes;
	datarow=3;
	guessingrows=&guessingrows;
	run; quit;
	%end;

%if %quote(&output_file)=%quote(&null) %then %do;
	data _null_;
		call system(cat('del "',"&output_filex.",'"'));
		run;
	%end;
%mend;	

%*tm_sqlserv_setenv;

%*tm_sqlserv_sqlcmd(command=select * from home.sys.columns where object_id=OBJECT_ID('home.dbo.xdata2-base'),
                   out=work.test
                   );

