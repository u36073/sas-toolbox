%macro tm_postgresql_upload(data=,
                        table_name=,
                        tablespace=,
                        userid=,
                        database=,
                        server=localhost,
                        server_port=5432,
                        psql_path=C:\Program Files\PostgreSQL\9.3\bin 
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
%global step_end step_duration step_start;

%macro step_start;
%let step_end=;
%let step_duration=;
data _null_;
   dt=datetime();
   call symput('step_start',strip(dt));
   run;
%mend;

%macro step_end(d1,d2,d3,d4,d5);
%let null=;
data _null_;
   dt=datetime();
   duration=dt-&step_start;

   hours=int(duration/3600);
   minutes=int(mod(duration,3600)/60);
   seconds=mod(mod(duration,3600),60);

   call symput('step_start',strip(put(&step_start,datetime23.4)));
   call symput('step_end',strip(put(dt,datetime23.4)));      
   call symput('step_duration',catx(' ',hours,'Hours ',minutes,'Minutes ',seconds,'Seconds '));
   run;

data _null_;
   put;
   put "-----------------------------------------------------------------------";
   %do i=1 %to 5;
      %if %quote(&&d&i) ne &null %then %do;
         put " &&d&i..";
         %end;
      %end;
   put;
   put "     START:  &step_start.";
   put "       END:  &step_end.";
   put "  DURATION:  &step_duration.";
   put "-----------------------------------------------------------------------";
   put;
   run;
%mend;

%step_start;

proc sql;
reset noprint;
select path into :worklib_path from dictionary.libnames where libname='WORK';
quit;

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
   
   if substr(format,1,8) in (&datetime_fmts) and length(format)>=8 then format="_DT_";
   else if substr(format,1,8) in (&time_fmts) and length(format)>=8 then format="e8601tm15.6";
   else if substr(format,1,8) in (&date_fmts) and length(format)>=8 then format="e8601da10.";

   else if substr(format,1,7) in (&datetime_fmts) and length(format)>=7 then format="_DT_";
   else if substr(format,1,7) in (&time_fmts) and length(format)>=7 then format="e8601tm15.6";
   else if substr(format,1,7) in (&date_fmts) and length(format)>=7 then format="e8601da10.";

   else if substr(format,1,6) in (&datetime_fmts) and length(format)>=6 then format="_DT_";
   else if substr(format,1,6) in (&time_fmts) and length(format)>=6 then format="e8601tm15.6";
   else if substr(format,1,6) in (&date_fmts) and length(format)>=6 then format="e8601da10.";

   else if substr(format,1,5) in (&datetime_fmts) and length(format)>=5 then format="_DT_";
   else if substr(format,1,5) in (&time_fmts) and length(format)>=5 then format="e8601tm15.6";
   else if substr(format,1,5) in (&date_fmts) and length(format)>=5 then format="e8601da10.";

   else if substr(format,1,4) in (&datetime_fmts) and length(format)>=4 then format="_DT_";
   else if substr(format,1,4) in (&time_fmts) and length(format)>=4 then format="e8601tm15.6";
   else if substr(format,1,4) in (&date_fmts) and length(format)>=4 then format="e8601da10.";

   else if substr(format,1,3) in (&datetime_fmts) and length(format)>=3 then format="_DT_";
   else if substr(format,1,3) in (&time_fmts) and length(format)>=3 then format="e8601tm15.6";
   else if substr(format,1,3) in (&date_fmts) and length(format)>=3 then format="e8601da10.";

   else format='';
   
   length db_type $ 64;
   if format="_DT_" then db_type='TIMESTAMP WITHOUT TIME ZONE';
   else if format="e8601tm15.6" then db_type='TIME WITHOUT TIME ZONE';
   else if format="e8601da10." then db_type='DATE';
   else if type=1 then db_type='NUMERIC';
   else if type=2 then db_type=cats('VARCHAR(',length,')');
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
   call symput('otype'||trim(left(_n_)),trim(left(upcase(db_type))));
   if last then do;
      call symput('nvars',trim(left(_n_)));
      end;
   run;

filename dftx1 "%trim(%left(&worklib_path))\&data..csv" lrecl=32000;

data _null_;
   set &data;
   file dftx1;
   length __xx $ 5000;
   %do i=1 %to &nvars;
      %if &&fmt&i=&null %then %do;
         %if &&type&i=1 %then %do;
            if &&var&i ne . then put &&var&i +(-1) @;
            *else put '\N' @;
            %end;
         %else %do;
            if index(&&var&i,'"')>0 then __xx=tranwrd(&&var&i,'"','\"');
            else __x=&&var&i;
            put '"' __x +(-1) '"' @;
            %end;
         %end;
      %else %if %qupcase(&&fmt&i)=%quote(_DT_) %then %do;
         if &&var&i ne . then do;
            __xx=strip(catx(' ',put(datepart(&&var&i),e8601da10.),put(timepart(&&var&i),e8601tm15.6)));
            put __xx @;
            end;
         else do;
            *put '\N' @;
            end;
         %end;
      %else %do;
         if &&var&i ne . then do;
            __xx=strip(put(&&var&i,&&fmt&i.));
            put __xx @;
            end;
         else do;
            *put '\N' @;
            end;
         %end;
      %if &i<&nvars %then %do;
         put "|" @;
         %end;
      %end;
   put;
   run;

%step_end(Create Temporary CVS File,%trim(%left(&worklib_path))\&data..csv,,,);


%step_start;
filename dftx2 "%trim(%left(&worklib_path))\&data..sql" lrecl=150;

data _null_;
   file dftx2;
   put "DROP TABLE IF EXISTS &table_name CASCADE;";
   put "CREATE TABLE &table_name (";  
   %do i=1 %to &nvars;
      %if &i ne &nvars %then %let sep=,;
      %else %let sep=&null;
      put "   &&xvar&i &&otype&i..&sep";
      %end;
   put "   )";
   %if %quote(&tablespace) ne %quote(&null) %then %do;
      put "     TABLESPACE &tablespace";
      %end;
   put ";";
   put;
   put "\COPY &table_name FROM '%trim(%left(&worklib_path))\&data..csv' DELIMITERS '|' CSV";
   run;

proc sql;
reset noprint;
select trim(left(upcase(setting))) into :xwait_setting from dictionary.options
where trim(left(upcase(optname)))='XWAIT'
;
quit;

%if &xwait_setting=NOXWAIT %then %do;
   options xwait;
   %end;

%local load_start load_end;
data _null_;
    fname='__dfn';
    rc=filename(fname, "%trim(%left(&worklib_path))\&data..log");
    if rc = 0 and fexist(fname) then
       rc=fdelete(fname);
    rc=filename(fname);
run;
   
%let cmd="&psql_path.\psql.exe" -a -e -E -h &server -p &server_port -d &database -U &userid -f "%trim(%left(&worklib_path))\&data..sql" -L "%trim(%left(&worklib_path))\&data..log";         
%sysexec &cmd;

%step_end(Upload &data to PostgreSQL,
          %str(   SERVER=&server),
          %str(   PORT=&server_port),
          %str(   DATABASE=&database),
          %str(   TABLE=&table_name)
          );

%if &xwait_setting=NOXWAIT %then %do;
   options noxwait;
   %end;
%mend;


