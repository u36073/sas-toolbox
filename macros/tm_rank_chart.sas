%macro tm_rank_chart(data=,
	    	            out=,
	    	            print=y,
			            where=,
			            wherelabel=,
			            by=,			           
			            sumvars=,
			            scorevar=,
			            scorelabel=,
			            scoreformat=10.5,
			            groups=10,
			            bylabel=,			            
			            code=,
			            codevar=,
			            code_value_var=,
			            keepvars=,
			            );
	%local i j k null flag;
	%let null=;

  /******************************************************************************
   Macro: VarType

   Parameters: Positional
   	x			   Variable Name
    data     SAS data set

   This macro resolves to the type associated with variable &x
   	N= numerical
   	C= character

   Example:  %VarLabel(varname,dataset)
  ******************************************************************************/
  %macro VarType(x,data);
  %local dsid i nvars name rc rv;
  %let dsid = %sysfunc(open(&data,i));
  %let nvars = %sysfunc(attrn(&dsid,NVARS));
  %let rv=;
  %do i=1 %to &nvars;
    %let name=%sysfunc(varname(&dsid,&i));
    %if %qupcase(&name)=%qupcase(&x) %then %do;
    	  %let rv=%sysfunc(vartype(&dsid,&i));
       %end;
    %end;
  %let rc=%sysfunc(close(&dsid));
  &rv
  %mend;
	/****************************************************************************/

/******************************************************************************
Macro:  ParseList

Parameters:  positional

	prefix	Prefix to use for macro variables containing the name of each
	         member in the list

	list		List of values separated by spaces

Parses a list of text or numeric values separated by a space and assigns each
value to a macro variable with the prefix specified in the [prefix] parameter
and an integer suffix corresponding to its position in the list.

The macro itself resolves to the number of values in the list. An additional macro
variable is also created in the global environment of the form &prefix._csv.
It contains the list of values separated by a comma.

Example:

	%let varlist=x y a1 model_score utilization debt_to_income;
	%let num_vars=%ParseList(var,&varlist);

	data .........;
		%do i=1 %to &num_vars;
			&&var&i=round(&&var&i,.00001);
			%end;
		run;
*******************************************************************************/

%macro ParseList(prefix,list);
	%local n word flag null;
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
/******************************************************************************/

	%if %quote(&wherelabel)=%quote(&null) %then %let wherelabel=WHERE: &where;

	data __temp0(compress=yes);
		set &data(keep=&keepvars &scorevar &by);
		%if %quote(&where) ne %quote(&null) %then %do;
		   if &where;
		   %end;
		run;

	%let nbv=0;

	%if %quote(&by) ne %quote(&null) %then %do;
		proc sort data=__temp0;
		by &by;
		run;

	  %let nbv=%ParseList(byvar,&by);

		%let sqlby=&byvar1;
		%if &nbv>1 %then %do;
			%do i=2 %to &nbv;
			  %let sqlby=&sqlby.,&&byvar&i;
				%end;
			%end;
		%end;

	proc rank data=__temp0 out=__temp ties=low groups=&groups;
	%if %quote(&by) ne %quote(&null) %then %do;
		by &by;
		%end;
	var &scorevar;
	ranks rank;
	run;

	%let null=;
	
	%let flag=0;
	%let k=0;
	%do %until (&flag=1);
	   %let k=%eval(&k+1);
	   %let word=%scan(%bquote(&sumvars.),&k.,@);
	   %if %quote(&word.) ne %quote(&null.) %then %do;
	      %let sumvar&k.=%quote(&word.);
	      %end;
	   %else %do;
	      %let nsumvars=%eval(&k.-1);
	      %let flag=1;
	      %end;
	   %end;

	%if %quote(&scorelabel)=%quote(&null) %then %let scorelabel=&scorevar;
	%if %quote(&bylabel)=%quote(&null) %then %let bylabel=&by;
   
   %if %qupcase(&print.)=%quote(Y) %then %do;
   	TITLE h=2 "&scorelabel. Data Table";
   	%if %quote(&where) ne %quote(&null) %then %do;
   		title2 h=1.5 "&wherelabel";
   		%end;
   			
   	proc sql;
   	select %if %quote(&by) ne %quote(&null) %then %do;
   			   &sqlby,
   			   %end;
   			rank+1 as rank label="Score Rank",
   			%do i=1 %to &nsumvars.;
   			   &&sumvar&i..,
               %end;
   			count(*)/(select count(*) from __temp) as pct_obs format=percent7.2,
   			mean(&scorevar) as avg_score label="AVG Score" format=&scoreformat.,
   			min(&scorevar) as min_score label="MIN Score" format=&scoreformat.,
   			max(&scorevar) as max_score label="MAX Score" format=&scoreformat.,
   			count(*) as nobs format=comma12.
   	from __temp
   	%if %quote(&by) ne %quote(&null) %then %do;
   		group by &sqlby, rank
   		order by &sqlby, rank
   		%end;
   	%else %do;
   		group by rank
   		order by rank
   		%end;
   	;quit;
   	title;
   	%end;

  
  %if %quote(&out) eq %quote(&null) %then %let out=work._rankx_temp;
  
  proc sql;
  create table &out as
  select %if %quote(&by) ne %quote(&null) %then %do;
  		      &sqlby,
  		      %end;
  		rank+1 as rank label="Score Rank",
  		%do i=1 %to &nsumvars.;
		   &&sumvar&i..,
         %end;
  		count(*)/(select count(*) from __temp) as pct_obs,
  		mean(&scorevar) as avg_score label="AVG Score",
  		min(&scorevar) as min_score label="MIN Score",
  		max(&scorevar) as max_score label="MAX Score",
  		count(*) as nobs
  from __temp
  %if %quote(&by) ne %quote(&null) %then %do;
  	group by &sqlby, rank
  	order by &sqlby, rank
  	%end;
  %else %do;
  	group by rank
  	order by rank
  	%end;
  ;quit;

   
  %if %quote(&codevar)=%quote(&null) %then %let codevar=&scorevar._g&groups;

  %if %quote(&by) ne %quote(&null) %then %do;
     data _null_;
     	set &out end=last;
     	by &by;
     	%if %quote(&code)=%quote(&null) %then %do;
     	   file log;
     	   %end;
     	%else %do;
     	   file "&code" lrecl=10000;
     	   %end;     	
     	if first.&&byvar&nbv then do;
     		%if %VarType(&by,&data)=C %then %do;
     			put "if &by='" &by "' then do;";
     			%end;
     		%else %do;
     		  put "if &by=" &by " then do;";
     		  %end;
     		put "   if &scorevar<=" max_score " then do;";
     		put "      &codevar.=" rank ";";
     		%if %quote(&code_value_var) ne %quote(&null.) %then %do;
     		   put "      &codevar.v=" &code_value_var ";";
     		   %end;
     		put "   end;";
     		end;
     	else if not last.&&byvar&nbv then do;
   		put "   else if &scorevar<=" max_score " then do;";
     		put "      &codevar.=" rank ";";
     		%if %quote(&code_value_var) ne %quote(&null.) %then %do;
     		   put "      &codevar.v=" &code_value_var ";";
     		   %end;
     		put "      end;";
     		end;
     	else do;
     		put "   else do;";
     		put "      &codevar.=" rank ";";
     		%if %quote(&code_value_var) ne %quote(&null.) %then %do;
     		   put "      &codevar.v=" &code_value_var ";";
     		   %end;
     		put "      end;";
     		put "   end;";
     		end;
     	run;
     %end;
  %else %do;
     data _null_;
     	set &out end=last;
     	%if %quote(&code)=%quote(&null) %then %do;
     	   file log;
     	   %end;
     	%else %do;
     	   file "&code" lrecl=10000;
     	   %end;    
     	if _n_=1 then do;
     		put "if &scorevar<=" max_score " then do;";
     		put "   &codevar.=" rank ";";
     		%if %quote(&code_value_var) ne %quote(&null.) %then %do;
     		   put "      &codevar.v=" &code_value_var ";";
     		   %end;
     		put "   end;";
     		end;
     	else if not last then do;
   		put "else if &scorevar<=" max_score " then do;";
     		put "   &codevar.=" rank ";";
     		%if %quote(&code_value_var) ne %quote(&null.) %then %do;
     		   put "      &codevar.v=" &code_value_var ";";
     		   %end;
     		put "   end;";
     		end;
     	else do;
     		put "else do;";
     		put "   &codevar.=" rank ";";
     		%if %quote(&code_value_var) ne %quote(&null.) %then %do;
     		   put "      &codevar.v=" &code_value_var ";";
     		   %end;
     		put "   end;";
     		end;
     	run;
     %end;

%mend;
