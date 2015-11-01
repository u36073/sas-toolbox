%macro r_session_begin(r_session_id=,                       
                       r_executable=,
                       r_command_line_options=,
                       r_startup_script=,
                       r_workspace=,
                       r_save_code_filename=,
                       r_save_code_append=,
                       r_server=,
                       r_server_os_type=,   /* WINDOWS UNIX */
                       r_server_userid=,
                       r_server_password=,
                       r_server_work_directory=                      
                       );
                       
%let workdir=%sysfunc(getoption(work));

%*[region] - Use global defaults if no input supplied for a parameter;
%if %quote(&r_executable)=%quote(&null) %then %do;
   %if %symexist(rgs_executable) %then %do;
      %let r_executable=&rgs_executable.;
      %end;
   %end;

%if %quote(&r_command_line_options)=%quote(&null) %then %do;
   %if %symexist(rgs_command_line_options) %then %do;
      %let r_command_line_options=&rgs_command_line_options.;
      %end;
   %end;
   
%if %quote(&r_startup_script)=%quote(&null) %then %do;
   %if %symexist(rgs_startup_script) %then %do;
      %let r_startup_script=&rgs_startup_script.;
      %end;
   %end; 
   
%if %quote(&r_workspace)=%quote(&null) %then %do;
   %if %symexist(rgs_workspace) %then %do;
      %let r_workspace=&rgs_workspace.;
      %end;
   %end;       

%if %quote(&r_save_code_filename)=%quote(&null) %then %do;
   %if %symexist(rgs_save_code_filename) %then %do;
      %let r_save_code_filename=&rgs_save_code_filename.;
      %end;
   %end;   

%if %quote(&r_save_code_append)=%quote(&null) %then %do;
   %if %symexist(rgs_save_code_append) %then %do;
      %let r_save_code_append=&rgs_save_code_append.;
      %end;
   %end;  

%if %quote(&r_server)=%quote(&null) %then %do;
   %if %symexist(rgs_server) %then %do;
      %let r_server=&rgs_server.;
      %end;
   %end;
   
%if %quote(&r_server_os_type)=%quote(&null) %then %do;
   %if %symexist(rgs_server_os_type) %then %do;
      %let r_server_os_type=&rgs_server_os_type.;
      %end;
   %end;

%if %quote(&r_server_userid)=%quote(&null) %then %do;
   %if %symexist(rgs_server_userid) %then %do;
      %let r_server_userid=&rgs_server_userid.;
      %end;
   %end;
   
%if %quote(&r_server_password)=%quote(&null) %then %do;
   %if %symexist(rgs_server_password) %then %do;
      %let r_server_password=&rgs_server_password.;
      %end;
   %end;
   
%if %quote(&r_server_work_directory)=%quote(&null) %then %do;
   %if %symexist(rgs_server_work_directory) %then %do;
      %let r_server_work_directory=&rgs_server_work_directory.;
      %end;
   %end;
%*[end region];


   
%*[region] - Create global macro variables of macro parameters;
%let k=&r_session_id;

%global r_main_code_file&k
        r_executable&k
        r_command_line_options&k
        r_startup_script&k
        r_workspace&k
        r_save_code_append&k
        r_save_code_append&k
        r_server&k
        r_server_os_type&k
        r_server_userid&k
        r_server_password&k
        r_server_work_directory&k
        ;
        
data _null_;
   length rn $ 7 fn $ 1024;
   flag='N';
   do until(flag='Y');
      rn=put(int(9999999*ranuni(-1))+1,z7.);
      fn=cats("&workdir.\rsid&r_session_id._main_",rn,".r");
      if fileexist(fn)=0 then flag='Y';
      end;
   call symput("r_main_code_file&k.",strip(fn));
   run;        

%let r_executable&k=&r_executable;
%let r_command_line_options&k=&r_command_line_options;
%let r_startup_script&k=&r_startup_script;
%let r_workspace&k=&r_workspace;
%let r_save_code_append&k=&r_save_code_append;
%let r_save_code_append&k=&r_save_code_append;
%let r_server&k=&r_server;
%let r_server_os_type&k=&r_server_os_type;
%let r_server_userid&k=&r_server_userid;
%let r_server_password&k=&r_server_password;
%let r_server_work_directory&k=&r_server_work_directory;




%*[end region];   

data _null_;
   file "&&r_main_code_file&k.." lrecl=1024; 
   put;
   %if %quote(&&r_workspace&k..)=%quote(&null) %then %do;
	   put "sasr_load_workspace <- function(path) {";
	   put "   if (file.exists(path)==TRUE) {";
	   put "     load(path,envir=.GlobalEnv)";
	   put "   }";
	   put " }";
	   put 'sasr_load_workspace("' "&&r_workspace&k.." '")';
	   put;
	   %end;
	%if %quote(&&r_startup_script&k.) ne %quote(&null) %then %do;
	   put "sasr_load_startup_script <- function(path) {";
	   put "   if (file.exists(path)==TRUE) {";
	   put "     source(path,local=.GlobalEnv)";
	   put "   }";
	   put " }";
	   put 'sasr_load_startup_script("' "&&r_startup_scripte&k.." '")';
	   put;		
		%end;
   put;
   run;
%mend;
