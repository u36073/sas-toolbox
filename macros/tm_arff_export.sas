

%macro tm_arff_export(data=,
							 file=,
							 relation=,
							 worklib=work,
							 min_unique_values_for_nvar=5,
							 max_unique_values_excl_cvar=20
							 );
	
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
	
	%let oparen=(;
   %let cparen=);
   	
	%let paren_pos=%index(&data,%bquote(&oparen));
	
	%if &paren_pos>0 %then %let dsn=%substr(&data,1,%eval(&paren_pos-1));
	%else %let dsn=&data;
	
					
   proc contents data=&dsn out=_contents noprint;
   run;
   
   data work._null_;
   	set _contents end=last;
   	
   	if      strip(upcase(format)) in (&datetime_fmts) and length(strip(format))=8 then  format="_DATETIME_";
      else if strip(upcase(format)) in (&time_fmts) and length(strip(format))=8 then format="_TIME_";
      else if strip(upcase(format)) in (&date_fmts) and length(strip(format))=8 then format="_DATE_";

      else if strip(upcase(format)) in (&datetime_fmts) and length(strip(format))=7 then format="_DATETIME_";    
      else if strip(upcase(format)) in (&time_fmts) and length(strip(format))=7 then     format="_TIME_";        
      else if strip(upcase(format)) in (&date_fmts) and length(strip(format))=7 then     format="_DATE_";        

      else if strip(upcase(format)) in (&datetime_fmts) and length(strip(format))=6 then format="_DATETIME_";    
      else if strip(upcase(format)) in (&time_fmts) and length(strip(format))=6 then     format="_TIME_";        
      else if strip(upcase(format)) in (&date_fmts) and length(strip(format))=6 then     format="_DATE_";        

      else if strip(upcase(format)) in (&datetime_fmts) and length(strip(format))=5 then format="_DATETIME_";    
      else if strip(upcase(format)) in (&time_fmts) and length(strip(format))=5 then     format="_TIME_";        
      else if strip(upcase(format)) in (&date_fmts) and length(strip(format))=5 then     format="_DATE_";        

      else if strip(upcase(format)) in (&datetime_fmts) and length(strip(format))=4 then format="_DATETIME_";    
      else if strip(upcase(format)) in (&time_fmts) and length(strip(format))=4 then     format="_TIME_";  
      else if strip(upcase(format)) in (&date_fmts) and length(strip(format))=4 then     format="_DATE_";        

      else if strip(upcase(format)) in (&datetime_fmts) and length(strip(format))=3 then format="_DATETIME_";    
      else if strip(upcase(format)) in (&time_fmts) and length(strip(format))=3 then     format="_TIME_"; 
      else if strip(upcase(format)) in (&date_fmts) and length(strip(format))=3 then     format="_DATE_";        
   	
   	call symput(cats('var',_n_),strip(lowcase(name)));
   	call symput(cats('type',_n_),strip(lowcase(type)));
   	call symput(cats('format',_n_),strip(upcase(format)));
   	call symput(cats('label',_n_),strip(label));
   	if last then do;
   		call symput('nvars',_n_);
   		end;
   	run;   	   
								
	proc sql;
	create table &worklib..unique_values1 as
	select 	%do i=1 %to &nvars;
					count(distinct &&var&i) as dv&i,					
					%end;
				count(*) as nobs
	from &data
	;quit;
		
	proc transpose data=&worklib..unique_values1(drop=nobs) out=&worklib..unique_values2 name=variable prefix=unique_values;
	run;
	
	data _null_;
		set &worklib..unique_values2;
		length n $ 5;
		variable=trim(left(variable));
		n=substr(variable,3,length(variable)-2);
		call symput('dvalues'||trim(left(n)),trim(left(unique_values1)));
		run;
		
	%do i=1 %to &nvars;
		%if &&type&i=1 %then %do;	
			%if %quote(&&format&i)=%quote(_DATETIME_) %then %do;
				%let arfftype&i=DATETIME;
				%let format&i=b8601dt15.;
				%end;
			%else %if %quote(&&format&i)=%quote(_TIME_) %then %do;				
				%let arfftype&i=TIME;
				%let format&i=b8601tm6.;
				%end;
			%else %if %quote(&&format&i)=%quote(_DATE_) %then %do;
				%let arfftype&i=DATE;
				%let format&i=b8601da8.;
				%end;
			%else %if &&dvalues&i < &min_unique_values_for_nvar %then %let arfftype&i=CLASS;
			%else %let arfftype&i=NUMERIC;
			%end;
		%else %do;
			%if &&dvalues&i <= &max_unique_values_excl_cvar %then %let arfftype&i=CLASS;
			%else %let arfftype&i=STRING;
			%end;
		%end;
   		
	proc freq data=&data;
	%do i=1 %to &nvars;
		%if &&arfftype&i=CLASS %then %do;
			tables &&var&i / out=&worklib..freq&i noprint;
			%end;
		%end;
	run;
	
	%do i=1 %to &nvars;
		%if &&arfftype&i=CLASS %then %do;
			data _null_;
				set &worklib..freq&i end=last;
				call symput("values&i._"||trim(left(_n_)),trim(left(&&var&i)));
				if last then do;
					call symput("nvalues&i",trim(left(_n_)));
					end;
				run;
			%end;
		%end;
	
	filename tmarff "&file" lrecl=20000;
	
	data _null_;	
		set &data;
		file tmarff;
		if _n_=1 then do;
			put "@RELATION &relation";
			%do i=1 %to &nvars;
				%if &&arfftype&i=DATE %then %do;
				   put "@ATTRIBUTE &&var&i date " '"yyyyMMdd"';
				   %end;
				%else %if &&arfftype&i=TIME %then %do;
				   put "@ATTRIBUTE &&var&i date " '"HHmmss"';
				   %end;
				%else %if &&arfftype&i=DATETIME %then %do;
				   put "@ATTRIBUTE &&var&i date " '"yyyyMMdd' "'T'" 'HHmmss"';
				   %end;
				%else %if &&arfftype&i=NUMERIC %then %do;
				   put "@ATTRIBUTE &&var&i NUMERIC";
				   %end;
				%else %if &&arfftype&i=CLASS %then %do;
					put "@ATTRIBUTE &&var&i {'&&values&i._1'" @;
					%do k=2 %to &&nvalues&i;
						put ",'&&values&i._&k.'" @;
						%end;
					put "}";
					%end;
				%else %if &&arfftype&i=STRING %then %do;
					put "@ATTRIBUTE &&var&i STRING";
				   %end;
				%end;
			put "@DATA";
			end;
		
		%do i=1 %to &nvars;
			%if &i>1 %then %do;
				put "," @;
				%end;
			%if &&arfftype&i=DATE or &&arfftype&i=TIME or &&arfftype&i=DATETIME %then %do;
				put '"' &&var&i &&format&i '"' @;
			   %end;
			%else %if &&arfftype&i=CLASS and &&type&i=2 %then %do;
				put '"' &&var&i +(-1) '"' @;
				%end;
			%else %do;
				put &&var&i +(-1) @;
				%end;
			%end;
			put;			
		run;
		
%exit:
%mend; 