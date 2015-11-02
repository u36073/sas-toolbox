%macro tm_oracle_upload(data=,
                        table_name=,
                        userid=mydesk,                        
                        password=1Catbird,
                        tnsname=oracle,
                        control_files_directory=,
                                    data_files_directory=,
                                    keep_control_files=N,
                                    keep_data_files=N,
                                    keep_log_files=N,                                  
                        sqlplus_path=C:\oracle\product\12.1.0\client_1\BIN\sqlplus.exe,
                        sqlldr_path=C:\oracle\product\12.1.0\client_1\BIN\sqlldr.exe                     
                        );
%local i j k null sep;
%let null=;
%let datetime_fmts="B8601DN","B8601DT","B8601DX","B8601DZ","B8601LX","DATEAMPM","DATETIME",
                   "DTDATE","DTMONYY","DTWKDATX","DTYEAR","DTYYQC","E8601DN","E8601DT",
                   "E8601DX","E8601DZ","E8601LX","MDYAMPM"
                   ;
%let time_fmts="B8601LZ","B8601TM","B8601TX","B8601TZ","E8601LZ","E8601TM","E8601TX",
               "E8601TZ","HHMM","HOUR","MMSS","TIME","TIMEAMPM","TOD"
               ;
%let date_fmts="B8601DA","DATE","DAY","DDMMYY","DDMMYYX","DOWNAME","E8601DA","JULDAY",
               "JULIAN","MMDDYY","MMDDYYX","MMYY","MMYYX","MONNAME","MONTH","MONYY",
               "PDJULG","PDJULI","QTR","QTRR","WEEKDATE","WEEKDATX","WEEKDAY","WEEKU",
               "WEEKV","WEEKW","WORDDATE","WORDDATX","YEAR","YYMM","YYMMDD","YYMMDDX",
               "YYMON","YYQ","YYQX","YYQR","YYQRX"
               ;

proc sql;
reset noprint;
select path into :worklib_path from dictionary.libnames where libname='WORK';
quit;

%if %quote(&control_files_directory.)=%quote(&null.) %then %do;
   %let control_files_directory=%trim(%left(&worklib_path.));
   %let keep_control_files=N;
   %let keep_log_files=N;
   %end;
%if %quote(&data_files_directory.)=%quote(&null.) %then %do;
   %let data_files_directory=%trim(%left(&worklib_path.));
   %let keep_data_files=N;
   %end;

proc contents data=&data out=_contents noprint;
run;

data _contents;
   set _contents;
   length xname $ 30;
   if length(name)>30 then xname=substr(name,1,29);
   else xname=name;
   run;

proc sort data=_contents;
by xname;
run;

data _contents;
   length xname2 $ 30 _dc 8;
   retain _dc 0;
   drop _dc;
   set _contents;
   by xname;
   if first.xname then do;
      _dc=0;
      end;
   if xname=lag(xname) then do;
      _dc=_dc+1;
      xname2=trim(left(xname))||trim(left(_dc));
      end;
   else do;
      xname2=xname;
      end;
   
   if substr(format,1,8) in (&datetime_fmts) and length(format)>=8 then format="DATETIME25.6";
   else if substr(format,1,8) in (&time_fmts) and length(format)>=8 then format="TOD15.6";
   else if substr(format,1,8) in (&date_fmts) and length(format)>=8 then format="MMDDYY10.";

   else if substr(format,1,7) in (&datetime_fmts) and length(format)>=7 then format="DATETIME25.6";
   else if substr(format,1,7) in (&time_fmts) and length(format)>=7 then format="TOD15.6";
   else if substr(format,1,7) in (&date_fmts) and length(format)>=7 then format="MMDDYY10.";

   else if substr(format,1,6) in (&datetime_fmts) and length(format)>=6 then format="DATETIME25.6";
   else if substr(format,1,6) in (&time_fmts) and length(format)>=6 then format="TOD15.6";
   else if substr(format,1,6) in (&date_fmts) and length(format)>=6 then format="MMDDYY10.";

   else if substr(format,1,5) in (&datetime_fmts) and length(format)>=5 then format="DATETIME25.6";
   else if substr(format,1,5) in (&time_fmts) and length(format)>=5 then format="TOD15.6";
   else if substr(format,1,5) in (&date_fmts) and length(format)>=5 then format="MMDDYY10.";

   else if substr(format,1,4) in (&datetime_fmts) and length(format)>=4 then format="DATETIME25.6";
   else if substr(format,1,4) in (&time_fmts) and length(format)>=4 then format="TOD15.6";
   else if substr(format,1,4) in (&date_fmts) and length(format)>=4 then format="MMDDYY10.";

   else if substr(format,1,3) in (&datetime_fmts) and length(format)>=3 then format="DATETIME25.6";
   else if substr(format,1,3) in (&time_fmts) and length(format)>=3 then format="TOD15.6";
   else if substr(format,1,3) in (&date_fmts) and length(format)>=3 then format="MMDDYY10.";   

   else format='';
   
   length oracle_type $ 20;
   if format="DATETIME25.6" then oracle_type='TIMESTAMP (6)';
   else if format="TOD15.6" then oracle_type='TIME (6)';
   else if format="MMDDYY10." then oracle_type='DATE';
   else if type=1 then oracle_type='NUMBER';
   else if type=2 then oracle_type='VARCHAR2(' || trim(left(length)) || ' CHAR)';
   run;

proc sort data=_contents; 
by varnum;
run;

data _null_;
   set _contents end=last;
   call symput('var'||trim(left(_n_)),trim(left(name)));
   call symput('xvar'||trim(left(_n_)),trim(left(xname2)));
   call symput('type'||trim(left(_n_)),trim(left(type)));
   call symput('len'||trim(left(_n_)),trim(left(name)));
   call symput('fmt'||trim(left(_n_)),trim(left(upcase(format))));
   call symput('otype'||trim(left(_n_)),trim(left(upcase(oracle_type))));
   if last then do;
      call symput('nvars',trim(left(_n_)));
      end;
   run;

filename dftx1 "&data_files_directory.\&data..psv" lrecl=32000;

data _null_;
   set &data;
   file dftx1 termstr=LF;
   length __xx $ 256 _cc $ 32767;
   %do i=1 %to &nvars;
      %if &&fmt&i=&null %then %do;
          %if &&type&i=1 %then %do;
             if &&var&i ne . then put &&var&i +(-1) @;
             %end;
          %else %do;
             if strip(&&var&i) ne '' then do;
                if index(&&var&i..,'"')>0 then do;
                   _cc=tranwrd(&&var&i..,'"','""');
                   put '"' _cc +(-1) '"' @;
                   end;
                else do;
                   put '"' &&var&i.. +(-1) '"' @;
                   end;
                end;
              %end;
         %end;
      %else %do;
          %if &&type&i=1 %then %do;
             if &&var&i ne . then do;
                  __xx=trim(left(put(&&var&i,&&fmt&i.)));
                  put __xx +(-1) @;
                end;                   
             %end;
          %else %do;
             if strip(&&var&i) ne '' then do;
                if index(&&var&i..,'"')>0 then do;
                   _cc=tranwrd(&&var&i..,'"','""');
                   put '"' _cc +(-1) '"' @;
                   end;
                else do;
                   put '"' &&var&i.. +(-1) '"' @;
                   end;
                end;
             %end;         
         %end;
      %if &i<=&nvars %then %do;
         put "|" @;
         %end;    
      %end;
   put;
   run;

filename dftx2 "&control_files_directory.\&data..ctl" lrecl=150;

data _null_;
   file dftx2;
   put 'LOAD DATA';
   put 'INFILE "' "&data_files_directory.\&data..psv" '" ' '" str x' "'7c0a'" '"';
   put "INSERT INTO TABLE &table_name";
   put '   FIELDS TERMINATED BY "|" OPTIONALLY ENCLOSED BY ' "'" '"' "'"; 
   put 'TRAILING NULLCOLS';  
   put '(';
   %do i=1 %to &nvars;
      %if &i ne &nvars %then %let sep=,;
      %else %let sep=&null;

      %if &&type&i=2 %then %do;
         put " &&xvar&i..&sep";
         %end;
      %else %do;
         %if &&fmt&i=&null %then %do;
            put " &&xvar&i..&sep";
            %end;
         %else %if %quote(&&fmt&i)=%quote(DATETIME25.6) %then %do;
            put " &&xvar&i.. TIMESTAMP(6) " '"DDMONYYYY:HH24:MI:SS.FF6"' "&sep. ";
            %end;
         %else %if %quote(&&fmt&i)=%quote(TIME9.) %then %do;
            put " &&xvar&i.. TIME(6)" '"HH24:MI:SS.FF6"' "&sep. "; 
           
            %end; 
         %else %do;
            put " &&xvar&i.. DATE" '"MM/DD/YYYY"' "&sep. ";
            %end;
         %end;
      %end;
      put ' )';
      run;

filename dftx3 "&control_files_directory.\&data..sql" lrecl=150;

data _null_;
   file dftx3;
   put 'DECLARE';
   put "tc_sql VARCHAR(25000) := 'CREATE TABLE &table_name (";  
   %do i=1 %to &nvars;
      %if &i ne &nvars %then %let sep=,;
      %else %let sep=&null;
      put '                             ' "&&xvar&i" ' ' "&&otype&i..&sep";
      %end;
   put "                                ) SEGMENT CREATION DEFERRED";
   put "                                  PCTFREE 10 PCTUSED 40 INITRANS 1 MAXTRANS 255";
   put "                                  ';";
/*   put "                                  COMPRESS FOR OLTP LOGGING';";*/
   put "BEGIN";
   put "   BEGIN";
   put "      EXECUTE IMMEDIATE 'DROP TABLE &table_name';";
   put "   EXCEPTION";
   put "      WHEN OTHERS THEN";
   put "         IF SQLCODE != -942 THEN";
   put "            RAISE;";
   put "         END IF;";
   put "   END;";
   put "   EXECUTE IMMEDIATE tc_sql;";
   put "END;";
   put "/";
   put "EXIT";
   run;
   
proc sql;
reset noprint;
select trim(left(upcase(setting))) into :xwait_setting from dictionary.options
where trim(left(upcase(optname)))='XWAIT'
;
quit;

%if &xwait_setting=XWAIT %then %do;
   options noxwait;
   %end;


%*let cmd1=cmd.exe /C "&sqlplus_path." &userid/&password@&tnsname @"&control_files_directory.\&data..sql";
%*let cmd2=cmd.exe /C "&sqlldr_path." USERID=&userid/&password@&tnsname, CONTROL=%str(%')&control_files_directory.\&data..ctl%str(%'), LOG=%str(%')&control_files_directory.\&data..log%str(%'), BAD=%str(%')&control_files_directory.\&data..bad%str(%'), DIRECT=TRUE;

filename dftx4 "&control_files_directory.\&data..bat" lrecl=2500;

data _null_;
   file dftx4;
   put 'cd "' "&control_files_directory." '"';
   put 'cmd.exe /C "' "&sqlplus_path. &userid/&password@&tnsname @" '"' "&control_files_directory.\&data..sql" '"';
   put 'cmd.exe /C "' "&sqlldr_path. USERID=&userid/&password@&tnsname, CONTROL='&control_files_directory.\&data..ctl', LOG='&control_files_directory.\&data..log', BAD='&control_files_directory.\&data..bad', DIRECT=TRUE";
  run;
  
%let cmd=cmd.exe /C "&control_files_directory.\&data..bat";
%sysexec &cmd;
  
%if &xwait_setting=XWAIT %then %do;
   options xwait;
   %end;

filename dftx1 clear;
filename dftx2 clear;
filename dftx3 clear;
filename dftx4 clear;


%macro delete_file(x);
data _null_;
   rc=filename(fn,"&x");
   if rc=0 and fexist(fn) then rc=fdelete(fname);
   rc=filename(fname);
   run;
%mend;
 
%if %qupcase(&keep_control_files.)=%quote(N) %then %do;
   %delete_file(&control_files_directory.\&data..ctl);
   %delete_file(&control_files_directory.\&data..sql);
   %end;
   
%if %qupcase(&keep_log_files.)=%quote(N) %then %do;
   %delete_file(&control_files_directory.\&data..log);
   %delete_file(&control_files_directory.\&data..bad);
   %end;
   
%if %qupcase(&keep_data_files.)=%quote(N) %then %do;
   %delete_file(&data_files_directory.\&data..psv);
   %end;
%mend;


