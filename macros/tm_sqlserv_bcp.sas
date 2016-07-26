%macro tm_oracle_upload(data=,
								db=,
								schema=,
                        table=,                        
                        append=N,
                        db_keyvar=,
                        db_int_vars=,
                        db_bigint_vars=,
                        db_notnull_vars=,
                        db_numeric_type=FLOAT,
                        bcp_files_directory=,
                        bcp_keep_data_file=N,
                        bcp_keep_format_file=N,
                        );
%local i j k null sep worklib_path rn datetime_fmts time_fmts date_fmts nvars sql;
%let null=;
%let rn=%sysfunc(int(%sysevalf(9999999*(%sysfunc(ranuni(-1))))));
%let worklib_path=%sysfunc(pathname(work));

%if %quote(&bcp_files_directory)=%quote(&null) %then %do;
	%let bcp_files_directory=&worklib_path.;
	%end;

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
               
%macro tm_ParseList(prefix,list);
    %local n word flag null i nwords list prefix;
    %let null=;        
    
    %let list=%trim(%left(&list));

    %if %quote(&list) ne %quote(&null) %then %do;
      %let flag=0; %let n=1; %let word=;
      %global &prefix.1;
      %let &prefix.1=%scan(&list,1,%str( ));
       %do %while (&flag=0);
        %let n=%eval(&n+1);
        %let word=%scan(&list,&n,%str( ));
        %if &word=&null %then %do;
            %let nwords=%eval(&n-1);
            %let flag=1;
            %end;
        %else %do;
            %global &prefix.&n;
            %let &prefix.&n=&word;
            %end;
        %end;
       %end;
    %else %do;
        %let nwords=0;
        %end;

     %if &nwords>0 %then %do;
        %global &prefix._csv;
        %let &prefix._csv=&prefix.1;
        %if &nwords>1 %then %do;
            %do i=2 %to &nwords;
                %let &prefix._csv=&&&prefix._csv,&&&prefix.&i;
                %end;
            %end;
        %end;
&nwords
%mend;                        

/* Parse list of variables in the DB_*_VARS parameters */
%let ndb_int=%tm_parselist(db_int,&db_int_vars.);
%let ndb_notnull=%tm_parselist(db_notnull,&db_notnull_vars.);
%let ndb_bigint=%tm_parselist(db_bigint,&db_bigint_vars.);

proc contents data=&data out=_contents noprint;
run;

proc sort data=_contents; 
by name;
run;

data _contents;
	 set _contents;
   
   if substr(format,1,8) in (&datetime_fmts) and length(format)>=8 then format="E8601DT25.3";
   else if substr(format,1,8) in (&time_fmts) and length(format)>=8 then format="TIME8.";
   else if substr(format,1,8) in (&date_fmts) and length(format)>=8 then format="YYMMDDD10.";

   else if substr(format,1,7) in (&datetime_fmts) and length(format)>=7 then format="E8601DT25.3";
   else if substr(format,1,7) in (&time_fmts) and length(format)>=7 then format="TIME8.";
   else if substr(format,1,7) in (&date_fmts) and length(format)>=7 then format="YYMMDDD10.";

   else if substr(format,1,6) in (&datetime_fmts) and length(format)>=6 then format="E8601DT25.3";
   else if substr(format,1,6) in (&time_fmts) and length(format)>=6 then format="TIME8.";
   else if substr(format,1,6) in (&date_fmts) and length(format)>=6 then format="YYMMDDD10.";

   else if substr(format,1,5) in (&datetime_fmts) and length(format)>=5 then format="E8601DT25.3";
   else if substr(format,1,5) in (&time_fmts) and length(format)>=5 then format="TIME8.";
   else if substr(format,1,5) in (&date_fmts) and length(format)>=5 then format="YYMMDDD10.";

   else if substr(format,1,4) in (&datetime_fmts) and length(format)>=4 then format="E8601DT25.3";
   else if substr(format,1,4) in (&time_fmts) and length(format)>=4 then format="TIME8.";
   else if substr(format,1,4) in (&date_fmts) and length(format)>=4 then format="YYMMDDD10.";

   else if substr(format,1,3) in (&datetime_fmts) and length(format)>=3 then format="E8601DT25.3";
   else if substr(format,1,3) in (&time_fmts) and length(format)>=3 then format="TIME8.";
   else if substr(format,1,3) in (&date_fmts) and length(format)>=3 then format="YYMMDDD10.";   

   else format='';
   
   if format="E8601DT25.3" then dbtype='DATETIME2(3)';
   else if format="TIME8." then dbtype='TIME';
   else if format="YYMMDDD10." then dbtype='DATE';
   else if type=1 then dbtype="&db_numeric_type.";
   else if type=2 then dbtype='VARCHAR(' || trim(left(length)) || ')';
   
   notnull=0;
   
   %if &ndb_int.>0 %then %do;
	   %do i=1 %to &ndb_int.;
	   	if strip(upcase(name))=strip(upcase("&&db_int&i..")) then dbtype='INT';
	   	%end;
	   %end;
	   
   %if &ndb_bigint.>0 %then %do;
	   %do i=1 %to &ndb_bigint.;
	   	if strip(upcase(name))=strip(upcase("&&db_bigint&i..")) then dbtype='BIGINT';
	   	%end;
	   %end;	   
	   
	%if &ndb_notnull.>0 %then %do;
	   %do i=1 %to &ndb_notnull.;
	   	if strip(upcase(name))=strip(upcase("&&db_notnull&i..")) then notnull=1;
	   	%end;
	   %end;
   
   run;

%tm_sqlserv_table_exists(db=&db.,schema=&schema.,table=&table.,rv=bcp_exist_flag);

%if &bcp_exist_flag.=1 %then %do;
	/*******************************************************************************************/
	/*  Get information about existing table which we will be appending to.                    */
	/*******************************************************************************************/
	%if %qupcase(&append.)=%quote(Y) %then %do;
		%let sql=select   a.column_id as dbc_column_id,
								a.name as dbc_name,
							   b.name as dbc_type,
							   a.collation_name as dbc_collation_name,
							   a.max_length as dbc_max_length,
							   a.scale as dbc_scale,
							   a.precision as dbc_precision,
							   a.is_computed as dbc_is_computed,
							   a.is_identity as dbc_is_identity,
							   a.is_nullable as dbc_is_nullable
						from home.sys.columns a
						join home.sys.types b
						  on a.system_type_id=b.system_type_id
						where a.object_id=OBJECT_ID(%bquote(')&db..&schema..&table.%bquote(') and a.is_identity=0 and a.is_computed=0						
						;		
		%tm_sqlserv_sqlcmd(command=%bquote(sql.),out=__dbcols&rn.);
		
		proc sql;
		create table _contents2 as
		select a.*,
		       b.*
		from _contents a
		join __dbcols&rn. b
		  on strip(upcase(a.name))=strip(upcase(b.dbc_name))
		order by b.dbc_column_id
		;quit;
		%end;
	%else %do;
		%tm_sqlserv_sqlcmd(command=drop table &db..&schema..&table.);
		%end;
	%end;

%if &bcp_exist_flag.=0 or %qupcase(&append.) ne %quote(Y) %then %do;
	proc sort data=_contents out=_contents2;
	by name;
	run;
	%end;
	
proc sql;
reset noprint;
select count(*) into :nvars from _contents2
;quit;

%do i=1 %to &nvars;
	%local var&i type&i len&i fmt&i dbtype&i dbc_name&i dbc_type&i notnull&i
	       dbc_length&i dbc_scale&i dbc_precision&i dbc_nullable&i dbc_collation&i;
	%end;
	
data _null_;
   set _contents end=last;
   call symput('var'||trim(left(_n_)),trim(left(name)));
   call symput('type'||trim(left(_n_)),trim(left(type)));
   call symput('len'||trim(left(_n_)),trim(left(name)));
   call symput('fmt'||trim(left(_n_)),trim(left(upcase(format))));
   call symput('dbtype'||trim(left(_n_)),trim(left(upcase(dbtype))));
   call symput('notnull'||trim(left(_n_)),trim(left(notnull)));
   %if &bcp_exist_flag.=1 and %qupcase(&append.)=%quote(Y) %then %do;
	  	call symput(cats('dbc_name',_n_),strip(upcase(name)));
	  	call symput(cats('dbc_type',_n_),strip(type));
	  	call symput(cats('dbc_length',_n_),strip(upcase(max_length)));
	  	call symput(cats('dbc_scale',_n_),strip(upcase(scale)));
	  	call symput(cats('dbc_precision',_n_),strip(upcase(precision)));
	  	call symput(cats('dbc_nullable',_n_),strip(upcase(is_nullable)));
	  	call symput(cats('dbc_collation',_n_),strip((collation_name));  
	  	%end; 
   run;
   
/*******************************************************************************************/
/*  Create Table In Database                                                               */
/*******************************************************************************************/
%if &bcp_exist_flag.=0 or %qupcase(&append.)=%quote(N) %then %do;
	filename dftx0 "&bcp_files_directory.\&data..sql";
	
	data _null_;
		file "&bcp_files_directory.\&data..sql" lrecl=512;
		put "CREATE TABLE [&db.].[&schema.].[&table.](" @;
		%do i=1 %to &nvars;
			put;
			put "   [&&var&i..] &&dbtype&i.." @;
			%if &&notnull&i.=1 %then %do;
				put " NOT" @;
				%end;
			put " NULL" @;	
			%if &i. < &nvars. %then %do;
				put "," @;
				%end;
			%end;
	   put;
		put "   ) ON [PRIMARY]";
		
	%tm_sqlserv_sqlcmd(command_file=&bcp_files_directory.\&data..sql,query=N);	
	%end;


/*******************************************************************************************/
/*  Create CSV File to be imported with the SQL Server BCP Tool                            */
/*******************************************************************************************/
filename dftx1 "&bcp_files_directory.\&data..csv" lrecl=32000;

data _null_;
   set &data;
   file dftx1;
   length __xx $ 256 _cc $ 32767;
   %do i=1 %to &nvars;
      %if &&fmt&i=&null %then %do;
          %if &&type&i=1 %then %do;
             if &&var&i ne . then put &&var&i +(-1) @;
             %end;
          %else %do;
             if strip(&&var&i) ne '' then do;
                if index(&&var&i..,'09'x)>0 then do;
                   _cc=tranwrd(&&var&i..,'09'x,' ');
                   put _cc +(-1) @;
                   end;
                else do;
                   put &&var&i.. @;
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
                if index(&&var&i..,'09'x)>0 then do;
                   _cc=tranwrd(&&var&i..,'09'x,' ');
                   put _cc +(-1) @;
                   end;
                else do;
                   put &&var&i.. @;
                   end;
                end;
             %end;         
         %end;
      %if &i<=&nvars %then %do;
         put '09'x @;
         %end;
      %end;
   put;
   run;


/*******************************************************************************************/
/*  Create BCP Format File                                                                 */
/*******************************************************************************************/
filename dftx2 "&bcp_files_directory.\&data..xml" lrecl=150;

data _null_;
   file dftx2;
   put '<?xml version="1.0"?>';
   put '<BCPFORMAT'; 
   put 'xmlns="http://schemas.microsoft.com/sqlserver/2004/bulkload/format"'; 
   put 'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';
   put '   <RECORD>';
   %do i=1 %to &nvars;
   	%if &i. < &nvars. %then %do;
			put '      <FIELD ID="' "&i." '" xsi:type="CharTerm" TERMINATOR="\t"/>';
			%end;
		%else %do;
			put '      <FIELD ID="' "&i." '" xsi:type="CharTerm" TERMINATOR="\r\n"/>';
			%end;
		%end;
	put '   </RECORD>';
	put '   <ROW>';
   %do i=1 %to &nvars;
		put '      <COLUMN SOURCE="' "&i." '" NAME="' "&&var&i.." @;		
		%if %quote(&&dbtype&i..)=%quote(&db_numeric_type.) %then %do;
			put '" xsi:type="SQLFLT8"/>';
			%end;
		%else %if %quote(&&dbtype&i..)=%quote(DATETIME2(3)) %then %do;
			put '" xsi:type="SQLDATETIME"/>';
			%end;
		%else %if %quote(&&dbtype&i..)=%quote(TIME) %then %do;
			put '" xsi:type="SQLDATETIME"/>';
			%end;
		%else %if %quote(&&dbtype&i..)=%quote(DATE) %then %do;
			put '" xsi:type="SQLDATETIME"/>';
			%end;
		%else %if %quote(&&dbtype&i..)=%quote(INT) %then %do;
			put '" xsi:type="SQLINT"/>';
			%end;
		%else %do;
			put '" xsi:type="SQLVARYCHAR"/>';
			%end;
		%end;	
	put '   </ROW>';
	put '</BCPFORMAT>';
	run;

filename dftx0 clear;
filename dftx1 clear;
filename dftx2 clear;

%let parm="&db..&schema..&table" in "&bcp_files_directory.\&data..csv";
%let parm=&parm. -f "&bcp_files_directory.\&data..xml" -e "&bcp_files_directory.\&data.err";
%let parm=&parm. -U &sqlserv_user. -P &sqlserv_password. -S "&sqlserv_server.,&sqlserv_port.";

%put "&sqlserv_bcp." &parm.;
  
%sysexec "&sqlserv_bcp." &parm.;
  
%macro delete_file(x);
data _null_;
   rc=filename(fn,"&x");
   if rc=0 and fexist(fn) then rc=fdelete(fname);
   rc=filename(fname);
   run;
%mend;
 
%if %qupcase(&bcp_keep_data_file.)=%quote(N) %then %do;
   %delete_file(&bcp_files_directory./&data..csv);
   %end;
   
%if %qupcase(&bcp_keep_format_file.)=%quote(N) %then %do;
   %delete_file(&bcp_files_directory.\&data..xml);
   %end;

%mend;


