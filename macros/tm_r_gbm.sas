%macro tm_r_xgboost(data=,
                    out=,
                    depvar=,
                    xvars=,
                    key_vars=,
                    train=,
                    score_vars_prefix=,
                    xgb_subsample=1,
                    xgb_nrounds=250,
                    xgb_eta=.05,
                    xgb_objective=reg:linear,    /*reg:linear or binary:logistic*/
                    xgb_missing=NaN,
                    xgb_max_depth=3,
                    tempdir=,
                    keep_temp_files=n,                    
                    r_executable=C:\Program Files\Microsoft\MRO\R-3.2.3\bin\x64\R.exe
                    );

options xsync noxwait;
%local null rn templib rwd i j k q max_xround;
%let null=;
%let max_xround=.;

/* Get Random Number to use in naming temp files and datasets */
data _null_;
   x=int(99998*ranuni(-1))+1;
   call symput('rn',strip(x));
   run;

/* Set temp directory to SAS Work directory if not specified in the macro call */
%if %quote(&tempdir.)=%quote(&null) %then %do;
    %let tempdir="%trim(%left(%sysfunc(getoption(work))))";
    %let templib=work;
    %end;
%else %do;
    libname w&rn. "&tempdir.";
    %let templib=w&rn.;
    %end;
    
/* Derive the R compatible path string for the temp directory. */
data _null_;
	length x $ 2000;
	x=translate("&tempdir.","/","\");
	call symput('rwd',strip(x));
	run;
	
   
%* [region] tm_ParseList;
%macro tm_ParseList(prefix,list);
    %local n word flag null i nwords;
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
%* [end region];


/* Get a list of the x variables and store in macro variables.  These steps allow for */
/* variable lists wild card notations to be used in the parameter.                    */
data &templib..xvars&rn.(keep=&xvars.);
   set &data;
   
   if _n_>=1 then do;
      output;
      stop;
      end;
   run;
   
proc contents data=&templib..xvars&rn. out=&templib..xvar_contents&rn. noprint;
run;

data _null_;
   set &templib..xvar_contents&rn. end=last;
   call symput(cats('xvar',_n_),strip(name));
   
   if last then do;
      call symput('nxvars',strip(_n_));
      end;
   run;
   
   
%let nkvars=%tm_parselist(kvar,&key_vars);
%let nxsample=%tm_parselist(xsample,&xgb_subsample);
%let nxround=%tm_parselist(xround,&xgb_nrounds);
%let nxeta=%tm_parselist(xeta,&xgb_eta);
%let nxdepth=%tm_parselist(xdepth,&xgb_max_depth);


%if &nxround>0 %then %do;
	data &templib..xround&rn.;
		%do i=1 %to &nxround;
			xround=&&xround&i..;
			output;
			%end;
		run;
		
	proc sql;
	reset noprint;
	select max(xround) into :max_xround from &templib..xround&rn.
	;quit;
	%end;

%if %quote(&max_xround.)=%quote(.) %then %do;
	%let xround1=150;
	%let max_xround=150;
	%let nxround=1;
	%end;
	
%let max_xround=%trim(%left(&max_xround.));

data &templib..data_view&rn.(keep=&key_vars &depvar. &xvars.) / view=&templib..data_view&rn.;
   set &data;
   run;

proc export data=&templib..data_view&rn.
                  outfile="&tempdir.\xgboost_data&rn..csv" 
                  dbms=csv replace;
putnames=YES;
run;

filename cf&rn. "&tempdir.\xgboost&rn..r" lrecl=2500;

data _null_;
	file cf&rn.;
	put "require(xgboost)";
	put 'setwd("' "&rwd." '")';
	put 'df <- read.csv("' "xgboost_data&rn..csv" '")';
	put "x_cols <- c('" "&xvar1." "'";
	put '             ' @;
	%do i=2 %to &nxvars;
		put ",'" "&&xvar&i.." "'" @;
		if mod(&i,5)=0 then do;
			put;
			put '             ' @;
			end;		
		%end;
	put ')';
	%if %quote(&train) ne %quote(&null) %then %do;				
		put "df_train <- subset(df,&train)";
		%end;
	%else %do;
		put "df_train <- df";
		%end;
	put;
	%do i=1 %to &nxsample;
		%let fsample=%sysfunc(translate(&&xsample&i..,d,.));
		%do j=1 %to &nxeta;
			%let feta=%sysfunc(translate(&&xeta&j..,d,.));
			%do k=1 %to &nxdepth;	
		
				put "gbm_s&fsample._e&feta._m&&xdepth&k.. <- xgboost(data=data.matrix(df_train[,x_cols]),";
				put "                              label = df_train$&depvar.,";
				put "                              nrounds = &max_xround.,";
				put "                              subsample = &&xsample&i..,";
				put "                              eta = &&xeta&j..,";
				put '                              objective = "' "&xgb_objective" '",';
				put "                              missing = NaN,";
				put "                              max_depth = &&xdepth&k..)";
				put;
				%end;
			%end;
		%end;
		
	put "csv_cols <- c()";
	%do i=1 %to &nkvars;
		put "csv_cols <- c(csv_cols, '" "&&kvar&i.." "')";
		%end;
						
	%do i=1 %to &nxsample;
		%let fsample=%sysfunc(translate(&&xsample&i..,d,.));
		%do j=1 %to &nxeta;
			%let feta=%sysfunc(translate(&&xeta&j..,d,.));
			%do k=1 %to &nxdepth;	
				%do q=1 %to &nxround;
					put "df$&score_vars_prefix.s&fsample._e&feta._m&&xdepth&k.._r&&xround&q.. <- predict(gbm_s&fsample._e&feta._m&&xdepth&k.., data.matrix(df[,x_cols]), ntreelimit=&&xround&q..)";
					put "csv_cols <- c(csv_cols, '" "&score_vars_prefix.s&fsample._e&feta._m&&xdepth&k.._r&&xround&q.." "')";
					%end;
				%end;
			%end;
		%end;
			
	put 'write.csv(df[,csv_cols],	"' "xgboost_scores&rn..csv" '", row.names=FALSE)';		
	run;
	
data _null_;
	length cmd $ 5000;
	cmd=cat('"', "&r_executable.", '" --no-save --no-restore < ',
	        '"',"&tempdir.\xgboost&rn..r",'" > ', 
	        '"', "&tempdir.\xgboost&rn..rout", '"'
	        );
	put cmd;
	call system(cmd);
	run;

filename ro&rn. "&tempdir.\xgboost&rn..rout" lrecl=5000;

data _null_;
	file log;
	infile ro&rn. end=last;	
	if _n_=1 then do;
		put "*****************************************************";
		put '%tm_r_gbm() Macro';
		put "Begin: R Output from &tempdir.\xgboost&rn..r";		
		put "*****************************************************";
		put;
		end;
	input @1 x $1. @;
	put _infile_;
	if last then do;
		put;
		put "*****************************************************";
		put '%tm_r_gbm() Macro';
		put "End: R Output from &tempdir.\xgboost&rn..r";
		put "*****************************************************";
		end;
	run;

proc import datafile="&tempdir.\xgboost_scores&rn..csv"
            out=&out
            dbms=csv replace;
getnames=yes;
guessingrows=max;
run;

filename cf&rn. clear;
filename ro&rn. clear;

%if %qupcase(&keep_temp_files) ne %quote(N) %then %do;
	data _null_;
		call system(cat('del "',"&tempdir.\xgboost_data&rn..csv",'"');
		call system(cat('del "',"&tempdir.\xgboost_scores&rn..csv",'"');
		call system(cat('del "',"&tempdir.\xgboost&rn..rout",'"');
		call system(cat('del "',"&tempdir.\xgboost&rn..r",'"');
		run;
		
	proc datasets lib=&templib. nolist nowarn;
	delete xvars&rn. xvar_contents&rn. data_view&rn.;
	run;quit;
	%end;

%if %qupcase(&templib.) ne %quote(WORK) %then %do;
	libname &templib. clear;
	%end;
%mend;