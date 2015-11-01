/******************************************************************************
 Macro: tm_univariate_stats
 
 Dependencies:  tm_Parselist
                tm_ListDedupe
                
 Global Macros Variables Used:  _tus_var1 - _tus_var[items in vars= list]
                                _tus_stat1 - _tus_stat[items in statistics= list]     
                                _tus_bvar1 - _tus_bvar[items in by= list]          
 
 Parameters: Named

    data=		input dataset
    out=    	Output dataset. Leave blank for no output dataset.
    where=    
    report=   Y - to print the summary statistics table to the active output destination(s)
              N - do not print a report tot he default active destination(s).
    format=   1 - All statics in one column called VALUE where the statistic is 
                  identified by the string in the STATISTIC column.
              2 - Separate column for each statistic.  Column name will be the name 
                  of the statistic.              
    vars=   	numeric variables to calculate statistics for, if left blank then all 
            	numeric variables from the input dataset will be used
    by=     	variable to use for by group analysis, if blank then no by group 
            	processing will occur and the summary statistics will be for all
            	records in the input dataset.
    statstics=  space separated list of statistics to calculate.  Valid values are 
                listed below
    pctldef=    specifies the definition that Proc Univariate uses to calculate percentiles
                1  weighted average
                2  observation numbered closest to np
                3  empirical distribution function
                4  weighted average aimed at X[(n+1)p]
                5  emperical distribution function with averaging (default)
    pctlpts=    specifies one or more percentiles that are not automatically computed
                by the UNIVARAIATE procedure.  You can list specific percentiles separated
                by spaces.  You can also specify percentiles with an expression of the 
                form start TO stop BY increment where start is a starting number, stop is 
                an ending number, and increment is a number to increment by.  They can be 
                fractional as well (eg 33.33, 66.67, 99.5).
                
                CSS	 
                CV	 
                GINI
                KURTOSIS  
                MAD
                MAX	 
                MEAN 
                MIN	 
                MODE
                MSIGN	 
                N	 
                NMISS
                NORMALTEST	
                P1
                P5
                P10
                Q1
                MEDIAN
                Q3
                P90
                P95
                P99
                PROBM
                PROBN
                PROBS
                PROBT	
                QN
                QRANGE
                RANGE
                SIGNRANK
                SN
                SKEWNESS
                STD
                STD_GINI
                STD_MAD
                STD_QN
                STD_QRANGE
                STD_SN
                STDERR
                SUM
                SUMWGT
                T   
                USS
                VAR           
    
******************************************************************************/

%macro tm_univariate_stats(data=,
                           out=,
                           where=,
                           report=Y,
                           format=2,
                           vars=,
                           by=,
                           statistics=n nmiss mean std min p1 p5 p10 q1 median q3 p90 p95 p99 max,
                           pctlpts=,
                           pctldef=5
                           );
%local i k j null errcode dvars dstats dby nbvars nvars nstats nskw
       missing_count missing_stats
     ;
                           
%let null=;

%if &out=&null %then %let &out=work.__tmus_out;

%do i=1 %to 44;
   %local skw&i;
   %end;

%let skw1=CSS;
%let skw2=CV;
%let skw3=GINI;
%let skw4=KURTOSIS;
%let skw5=MAD;
%let skw6=MAX;
%let skw7=MEAN;
%let skw8=MEDIAN;
%let skw9=MIN;
%let skw10=MODE;
%let skw11=MSIGN;
%let skw12=N;
%let skw13=NMISS;
%let skw14=NORMALTEST;
%let skw15=PROBM;
%let skw16=PROBN;
%let skw17=PROBS;
%let skw18=PROBT;
%let skw19=QN;
%let skw20=QRANGE;
%let skw21=RANGE;
%let skw22=SIGNRANK;
%let skw23=SN;
%let skw24=SKEWNESS;
%let skw25=STD;
%let skw26=STD_GINI;
%let skw27=STD_MAD;
%let skw28=STD_QN;
%let skw29=STD_QRANGE;
%let skw30=STD_SN;
%let skw31=STDERR;
%let skw32=SUM;
%let skw33=SUMWGT;
%let skw34=T;
%let skw35=USS;
%let skw36=VAR;
%let skw37=P1;
%let skw38=P5;
%let skw39=P10;
%let skw40=Q1;
%let skw41=Q3;
%let skw42=P90;
%let skw43=P95;
%let skw44=P99;

%let nskw=44;

%if %quote(&statistics)=%quote(&null) %then 
    %let statistics=n nmiss mean std min p1 p5 p10 p25 p50 p75 p90 p95 p99 max;

%let dby=%tm_ListDedupe(&by);
%let dvars=%tm_ListDedupe(&vars);
%let dstats=%tm_ListDedupe(&statistics);

%let nbvars=%tm_ParseList(_tus_bvar,&dby);
%let nvars=%tm_ParseList(_tus_var,&dvars);
%let nstats=%tm_ParseList(_tus_stat,&dstats);

%goto ErrorHandling;
%main:

%if %quote(&vars)=%quote(&null) %then %do;
   proc contents data=&data out=work._tus_contents noprint;
   run;
   
   data _null_;
      set work._tus_contents end=last;
      retain vcount 0;
      if type=1 then do;
         vcount=vcount+1;
         call symput('_tus_var'||trim(left(vcount)),trim(left(upcase(name))));
         end;
      if last then do;
         call symput('nvars',trim(left(vcount)));
         end;
      run;
   %end;

%if &nbvars>0 %then %do;
   proc sort data=&data;
   by &dby;
   run;
   %end;

proc univariate data=&data noprint;
%if %quote(&where) ne %quote(&null) %then %do;
   where &where;
   %end;
%if &nbvars>0 %then %do;
   by &dby;
   %end;
var %do i=1 %to &nvars; &&_tus_var&i %end;;
output out=__tmus1
   %do k=1 %to &nstats;
      &&_tus_stat&k=%do i=1 %to &nvars;  v&i._&&_tus_stat&k.. %end;
      %end;
   %if %quote(&pctlpts) ne %quote(&null) %then %do;
      pctlpts=&pctlpts
      pctlpre=%do i=1 %to &nvars; v&i._ %end;
      %end;
   ;
run;
     
proc transpose data=__tmus1 out=__tmus2 name=name label=label;
%if &nbvars>0 %then %do;
   by &dby;
   %end;

%if %quote(&format)=%quote(1) %then %do;
	data &out;
	%end;
%else %do;	
	data __tmus3;
	%end;
	
	set __tmus2(rename=(col1=value));
	length varnum $ 12 variable statistic $ 32;
	drop name varnumx varnum label;
	varnumx=trim(left(scan(name,1,'_')));
	varnum=substr(varnumx,2,length(varnumx)-1);	
	variable=symget("_tus_var"||trim(left(varnum)));		
	statistic=substr(name,indexc(name,'_')+1,length(name)-indexc(name,'_'));		
	%if %quote(&format)=%quote(1) %then %do;
		statistic=translate(statistic,'.','_');
		%end;		
	if substr(statistic,1,1) in ('1','2','3','4','5','6','7','8','9','0','-') then 
	   statistic='P'||trim(left(statistic));
	run;
	
%if %quote(&format)=%quote(1) %then %do;
	proc sort data=&out;
	by &dby statistic variable;
	run;
	%end;
	
%if %quote(&format) ne %quote(1) %then %do;
	proc sql;
	create table __tmus4 as 
	select distinct statistic from __tmus3
	order by statistic
	;quit;
	
	data _null_;
		set __tmus4 end=last;
		call symput('__stat'||trim(left(_n_)),statistic);
		if last then do;
			call symput('__nstat',trim(left(_n_)));
			end;
		run;
	
	proc sort data=__tmus3;
	by &by variable statistic;
	run;
	
	data &out(keep=variable &by %do i=1 %to &__nstat; &&__stat&i %end;);
		set __tmus3;
		by &dby variable statistic;
		
		retain %do i=1 %to &__nstat; &&__stat&i %end;
		;
		
		if first.variable then do;
			%do i=1 %to &__nstat; &&__stat&i=.; %end;
			end;
			
		%do i=1 %to &__nstat; 
			if statistic="&&__stat&i" then &&__stat&i=value; 
			%end;
		
		if last.variable then do;
			output;
			end;
		run;		
	%end;
	
%if %qupcase(&report)=%quote(Y) %then %do;
	proc print data=&out noobs;
	%if %quote(&format)=%quote(1) %then %do;
		var &dby statistic variable value;
		%end;
	run;
	%end;
	
	     
%goto Exit;
%ErrorHandling:

%let errcode=%tme_ValueNotNull(
                  value=&data,
                  AppName=Macro TM_UNIVARIATE_STATS,
                  module=,
                  code=1,
                  parameter=DATA
                  );
%if %quote(%trim(%left(&errcode))) ne %quote(0) %then %goto Exit;                  

%let errcode=%tme_ValidDataset(
                  type=EXISTS,
                  name=&data,
                  AppName=Macro TM_UNIVARIATE_STATS,
                  module=,
                  code=2,
                  parameter=DATA
                  );
%if %quote(%trim(%left(&errcode))) ne %quote(0) %then %goto Exit;


%let errcode=%tme_ValueNotNull(
                  value=&out,
                  AppName=Macro TM_UNIVARIATE_STATS,
                  module=,
                  code=3,
                  parameter=OUT
                  );
%if %quote(%trim(%left(&errcode))) ne %quote(0) %then %goto Exit; 
                  
%let errcode=%tme_ValidDataset(type=NEW,
                  name=&out,
                  AppName=Macro TM_UNIVARIATE_STATS,
                  module=,
                  code=4,
                  parameter=OUT
                  );
%if %quote(%trim(%left(&errcode))) ne %quote(0) %then %goto Exit;


%if %quote(&by) ne %quote(&null) %then %do;
   %tme_ValidVariableList(type=A,
                          vars=&dby,
                          data=&data,
                          testlength=Y,
                          maxlength=32,
                          rcMacroVar=errcode,
                          AppName=Macro TM_UNIVARIATE_STATS,
                          module=,
                          code=5,
                          parameter=BY
                          );
                          
   %if %quote(%trim(%left(&errcode))) ne %quote(0) %then %goto Exit;                    
   %end;
   

%if %quote(&vars) ne %quote(&null) %then %do;   
   %tme_ValidVariableList(type=N,
                          vars=&dvars,
                          data=&data,
                          testlength=Y,
                          maxlength=32,
                          rcMacroVar=errcode,
                          AppName=Macro TM_UNIVARIATE_STATS,
                          module=,
                          code=7,
                          parameter=VARS                    
                          );
                          
   %if %quote(%trim(%left(&errcode))) ne %quote(0) %then %goto Exit; 
   %end;


data work._tus_stats_all(keep=stat);  
   length stat $ 10;
   %do i=1 %to &nskw;
      stat="&&skw&i"; output;
      %end;
/*   %do i=1 %to 100;*/
/*      %do k=0 %to 9;*/
/*         %do j=0 %to 9;*/
/*            %if &k=0 and &j=0 %then %do;*/
/*               stat="p&i"; output;*/
/*               %end;*/
/*            %else %if &j=0 %then %do;*/
/*               stat="p&i..&k"; output;*/
/*               %end;*/
/*            %else %do;*/
/*               stat="p&i..&k.&j"; output;*/
/*               %end;*/
/*            %end;*/
/*         %end;*/
/*      %end;  */                
   run;
   
data work._tus_stats;  
   length stat $ 10;
   %do i=1 %to &nstats;
      stat="&&_tus_stat&i"; output;
      %end;
   run;
   
proc sort data=work._tus_stats; by stat; run;
proc sort data=work._tus_stats_all; by stat; run;

%let missing_count=0;
%let missing_stats=;
   
data _null_;
	merge work._tus_stats(in=in1)
	      work._tus_stats_all(in=in2) end=last
	      ;
	by stat;
    length z $ 4096;
    retain c 0 z "";
    if in1 and not in2 then do;
       c=c+1; z=trim(left(z))||" "||trim(left(stat));
       end;
    if last then do;
       call symput("missing_count",trim(left(c)));
       call symput("missing_stats",trim(left(z)));
       end;
    run;
    
%if &missing_count>0 %then %do;
   %tme_ErrorMessage(AppName=TM_UNIVARIATE_STATS,
                     module=,
                     code=8,
                     parameter=STATISTICS,
                     msg1=The following &missing_count values in the STATISTICS parameter were not valid ,
                     msg2=&missing_stats,
                     msg3=
                     );
   %goto Exit;
   %end;	


%if %quote(&pctldef) ne %quote(1) and
    %quote(&pctldef) ne %quote(2) and
    %quote(&pctldef) ne %quote(3) and
    %quote(&pctldef) ne %quote(4) and
    %quote(&pctldef) ne %quote(5) %then %do;
    %tme_ErrorMessage(AppName=TM_UNIVARIATE_STATS,
                     module=,
                     code=9,
                     parameter=PCTLDEF,
                     msg1=&pctldef is an invalid value.,
                     msg2=Valid values are 1 2 3 4 or 5,
                     msg3=
                     );
   %goto Exit;
   %end;
     
%goto main;
%Exit:                        
%mend;                       