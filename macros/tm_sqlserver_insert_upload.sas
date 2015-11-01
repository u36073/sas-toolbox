%macro tm_sqlserver_insert_upload(data=,
                                  table_name=,
                                  outfile=     /* Specify a file reference from a filename statement.  */
                                               /* LRECL should be set to 15000 or higher.  To append   */
                                               /* specify MOD after the file reference separated by    */
                                               /* a space                                              */
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

proc contents data=&data out=_contents noprint;
run;

data _contents;
   set _contents;
   length xname $ 32;
   if length(name)>32 then xname=substr(name,1,32);
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
   
   if substr(format,1,8) in (&datetime_fmts) and length(format)>=8 then format="DATETIME18.";
   else if substr(format,1,8) in (&time_fmts) and length(format)>=8 then format="TIME8.";
   else if substr(format,1,8) in (&date_fmts) and length(format)>=8 then format="YYMMDDD10.";

   else if substr(format,1,7) in (&datetime_fmts) and length(format)>=7 then format="DATETIME18.";
   else if substr(format,1,7) in (&time_fmts) and length(format)>=7 then format="TIME8.";
   else if substr(format,1,7) in (&date_fmts) and length(format)>=7 then format="YYMMDDD10.";

   else if substr(format,1,6) in (&datetime_fmts) and length(format)>=6 then format="DATETIME18.";
   else if substr(format,1,6) in (&time_fmts) and length(format)>=6 then format="TIME8.";
   else if substr(format,1,6) in (&date_fmts) and length(format)>=6 then format="YYMMDDD10.";

   else if substr(format,1,5) in (&datetime_fmts) and length(format)>=5 then format="DATETIME18.";
   else if substr(format,1,5) in (&time_fmts) and length(format)>=5 then format="TIME8.";
   else if substr(format,1,5) in (&date_fmts) and length(format)>=5 then format="YYMMDDD10.";

   else if substr(format,1,4) in (&datetime_fmts) and length(format)>=4 then format="DATETIME18.";
   else if substr(format,1,4) in (&time_fmts) and length(format)>=4 then format="TIME8.";
   else if substr(format,1,4) in (&date_fmts) and length(format)>=4 then format="YYMMDDD10.";

   else if substr(format,1,3) in (&datetime_fmts) and length(format)>=3 then format="DATETIME18.";
   else if substr(format,1,3) in (&time_fmts) and length(format)>=3 then format="TIME8.";
   else if substr(format,1,3) in (&date_fmts) and length(format)>=3 then format="YYMMDDD10.";

   else format='';
   
   length oracle_type $ 20;
   if format="DATETIME18." then oracle_type='DATETIME';
   else if format="TIME8." then oracle_type='TIME';
   else if format="YYMMDDD10." then oracle_type='DATE';
   else if type=1 then oracle_type='NUMERIC(21,9)';
   else if type=2 then oracle_type='NVARCHAR(' || trim(left(length)) || ')';
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

data _null_;
   file &outfile;
   put "CREATE TABLE &table_name (";  
   put '   ' "&table_name._id" ' ' "INTEGER PRIMARY KEY,";
   %do i=1 %to &nvars;
      %if &i ne &nvars %then %let sep=,;
      %else %let sep=&null;
      %if %qupcase(&&otype&i)=%quote(DATE) %then %do;
      	put '   ' "&&xvar&i" ' ' "DATETIME NULL&sep.";
      	%end;
      %else %if %qupcase(&&otype&i)=%quote(DATETIME) %then %do;
      	put '   ' "&&xvar&i" ' ' "DATETIME NULL&sep.";
      	%end;
      %else %do;
      	put '   ' "&&xvar&i" ' ' "&&otype&i.. NULL&sep.";
      	%end;      
      %end;
   put ")";
   put "GO";
   run;


data _null_;
	set &data;   
   file %scan(&outfile,1,%str( )) mod;
   length _ts $ 256;
   put "INSERT INTO &table_name VALUES (" _n_ "," @;  
   %do i=1 %to &nvars;
      %if &i ne &nvars %then %let sep=,;
      %else %let sep=&null;
      %if %qupcase(&&otype&i)=%quote(DATE) %then %do;
      	if &&xvar&i=. then do;
      		put "NULL&sep." @;
      		end;
      	else do;
      		_ts=cat("'",strip(put(&&xvar&i,&&fmt&i))," 00:00:00'&sep.");
      		_ts=strip(_ts);
      		put _ts +(-1) @;
      	   end;
      	%end;
      %else %if %qupcase(&&otype&i)=%quote(DATETIME) %then %do;
      	if &&xvar&i=. then do;
      		put "NULL&sep." @;
      		end;
      	else do;
      		_ts=cat("'",strip(put(datepart(&&xvar&i),YYMMDDD10.))," ",strip(put(timepart(&&xvar&i),tod9.)),"&sep.");
      		_ts=strip(_ts);
      		put _ts +(-1) @;
      	   end;
      	%end;
      %else %if &&type&i=2 %then %do;
      	_ts=cats("'",&&xvar&i,"'&sep.");
      	_ts=strip(_ts);
      	put _ts +(-1) @;
      	%end;
      %else %do;
      	if &&xvar&i=. then do;
      		put "NULL&sep." @;
      		end;
      	else do;
      		_ts=cats(&&xvar&i,"0&sep.");
      		put _ts +(-1) @;
      		end;
      	%end;
      %end;
	put ")";
	put "GO";
   run;
   
/*data _null_;*/
/*	file ssof mod;*/
/*	put;*/
/*	put "create index [idx01] on [summary] (test_group ASC, adjuster_title asc, cur_adjustor_no asc, pending_date asc)";*/
/*	put "GO";*/
/*	put "create index [idx02] on [summary] (test_group ASC, cur_adjustor_no asc, pending_date asc)";*/
/*	put "GO";*/
/*	put "create index [idx03] on [summary] (test_group ASC, adjuster_title asc, pending_date asc)";*/
/*	put "GO";*/
/*	put "create index [idx04] on [summary] (test_group ASC, adjuster_title asc)";*/
/*	put "GO";*/
/*	put "create index [idx05] on [summary] (cur_adjustor_no asc, pending_date asc)";*/
/*	put "GO";*/
/*	run;*/
	
%mend;

