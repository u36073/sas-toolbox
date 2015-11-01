%macro tm_percentiles(data=,
							 out=,
							 codefile=,
							 vars=,
							 by=,
							 build=,
							 groups=10,
							 rank_var_name_method=1   /* 1 = Prefix variable name with R_                   */
							                          /* 2 = Replace the first 2 chars of the variable name */
							                          /*     with R_.  All vars must be longer than 2 chars */		);
%local i j k;
%let null=;
options compress=yes;
data _temp;
	set &data(obs=1 keep=&vars);
	run;
	
proc contents data=_temp out=_contents noprint;
run;

data _null_;
	set _contents end=last;
	call symput('var'||strip(_n_),strip(name));
	if last then do;
		call symput('nvars',strip(_n_));
		end;
	run;
								
%let nbvars=%tm_parselist(bvar,&by);

%do i=1 %to &nbvars;
	%let bvar_type&i=%tm_VarType(&&bvar&i..,&data);
	%let bvar_type&i=%trim(%left(%upcase(&&bvar_type&i)));
	%end;

data _temp(keep=&vars &by __build);
	set &data;
	__build=&build;
	run;
	
proc sort data=_temp;
%if &nbvars>0 %then %do;
	by __build
	%do i=1 %to &nbvars;
      &&bvar&i
		%end;
	;
   %end;
%else %do;
	by __build;
	%end;
run;

proc rank data=_temp out=_temp2 groups=&groups;
where __build=1;
%if &nbvars>0 %then %do;
	by %do i=1 %to &nbvars;
      &&bvar&i
		%end;
	   ;
   %end;
var %do i=1 %to &nvars; &&var&i %end; ;
ranks %do i=1 %to &nvars; __rank&i %end; ;
run;

proc sql;
create table _temp3 as 
select
%if &nbvars>0 %then %do;
	%do i=1 %to &nbvars;
	   %if &i=1 %then %do;
        	&&bvar&i	
        	%end;
      %else %do;
        	,&&bvar&i	
        	%end;
		%end;	      
   %end;	
%do i=1 %to &nvars;
	%do k=1 %to %eval(&groups-1);
		%if &nbvars=0 and &i=1 and &k=1 %then %do;
			max(case when __rank&i=%eval(&k-1) then &&var&i else -9999999 end) as cut&i._p&k
			%end;		
		%else %do;
		   ,max(case when __rank&i=%eval(&k-1) then &&var&i else -9999999 end) as cut&i._p&k
			%end;
		%end;
	%end;
from _temp2
%if &nbvars>0 %then %do;
	group by %do i=1 %to &nbvars;
		         %if &i=1 %then %do;
               	&&bvar&i	
               	%end;
               %else %do;
               	,&&bvar&i	
               	%end;
		         %end;	            
	order by %do i=1 %to &nbvars;
		         %if &i=1 %then %do;
               	&&bvar&i	
               	%end;
               %else %do;
               	,&&bvar&i	
               	%end;
		         %end;	            
   %end;
;quit;

filename cf "&codefile";


data _null_;
	file cf;
	set _temp3;
	
	%if &nbvars>0 %then %do;
		%do i=1 %to &nbvars;			
			%if &i=1 %then %do;
				%if &&bvar_type&i=C %then %do;
					put "   if &&bvar&i..='" &&bvar&i.. +(-1) "'" @;
   				%end;
   			%else %do;
   			   put "   if &&bvar&i..=" &&bvar&i.. +(-1) @;
   				%end;
   			%end;
   		%else %do;
   			%if &&bvar_type&i=C %then %do;
					put " and &&bvar&i..='" &&bvar&i.. +(-1) "'" @;
   				%end;
   			%else %do;
   			   put " and &&bvar&i..=" &&bvar&i.. +(-1) @;
   				%end;   		
   			%end; 
   		%end;
   		put " then do;";
   	%end;
   
   %if &nbvars>0 %then %do;
   	indent=6;
   	%end;
   %else %do;
   	indent=3;
   	%end;
   	
   %do i=1 %to &nvars;
   	%if &rank_var_name_method=1 %then %let rvar=r_&&var&i;
   	%else %if &rank_var_name_method=2 %then %let rvar=r_%substr(&&var&i,3,%eval(%length(&&var&i)-2));
   	%else %let rvar=r_&&var&i;
   	
   	%do k=1 %to &groups;
   		%if &k=1 %then %do;   
   		   put;			
   			put @indent "if &&var&i<=" cut&i._p&k. +(-1) " then &rvar.=&k;";
   			%end;
   		%else %if &k<&groups %then %do;
   			put @indent "else if &&var&i<=" cut&i._p&k. +(-1) " then &rvar.=&k;";
   			%end; 
   		%else %do;
   			put @indent "else &rvar.=&k;";
   			%end;
   		%end;
   	%end;
   	
   %if &nbvars>0 %then %do;
   	put "     end;";
   	put;
     	%end;
	run; 
	
data &out;
	set &data;
	%include cf;
	run;

%mend;  
   	
   
   	
   
   
  	
   
   









						
							 