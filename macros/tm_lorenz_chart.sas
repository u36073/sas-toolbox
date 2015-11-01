%macro lorenz_chart(data=,						
                    by=,
                    model_var=,
                    model_label=,
                    actual_var=,
                    actual_label=,
                    groups=20
                    );

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
/*********************************************************************************/

%if %quote(&model_label)=%quote(&null) %then %let model_label=&model_var;
%if %quote(&actual_label)=%quote(&null) %then %let actual_label=&actual_var;
                    
data _to_lctemp1;
	set &data(keep=&by &model_var &actual_var);
	run;
	
%let nbv=0;

%if %quote(&by) ne %quote(&null) %then %do;
	proc sort data=_to_lctemp1;
	by &by;
	run;

  %let nbv=%ParseList(byvar,&by);

	%let sqlby=&byvar1;
	%let procby=&byvar1;
	%if &nbv>1 %then %do;
		%do i=2 %to &nbv;
		  %let sqlby=&sqlby.,&&byvar&i;
		  %let procby=&procby. &&byvar&i;
			%end;
		%end;
	%end;
	

%if &nbv>0 %then %do;
	proc sql;
	create table _to_lctemp1b as 
	select *, count(*) as __nobs
	from _to_lctemp1
	group by &sqlby.
	order by &sqlby., &model_var
	;quit;
	
	data _to_lctemp1c;
		set _to_lctemp1b;
		by &procby. &model_var;
		retain __n; drop __n;
		
		if first.&&byvar&nbv.. then do;
			__n=0;
			end;
			
		__n=__n+1;

		%do k=1 %to &groups;
			%if &k=1 %then %do;
				if __n<=ceil(&k*(__nobs/&groups)) then rank_model=&k;
				%end;
			%else %if &k<&groups %then %do;
				else if __n<=ceil(&k*(__nobs/&groups)) then rank_model=&k;
				%end;
			%else %do;
				else rank_model=&k;
				%end;		 	
		 	%end;
		run;
	
	proc sort data=_to_lctemp1c;
	by &procby. &actual_var;
	run;
		
	data _to_lctemp2;
		set _to_lctemp1c;
		by &procby. &actual_var;
		retain __n;  drop __n;
		
		if first.&&byvar&nbv.. then do;
			__n=0;
			end;
			
		__n=__n+1;

		%do k=1 %to &groups;
			%if &k=1 %then %do;
				if __n<=ceil(&k*(__nobs/&groups)) then rank_actual=&k;
				%end;
			%else %if &k<&groups %then %do;
				else if __n<=ceil(&k*(__nobs/&groups)) then rank_actual=&k;
				%end;
			%else %do;
				else rank_actual=&k;
				%end;		 	
		 	%end;
		run;		
	%end;  
%else %do;
	proc sort data=_to_lctemp1;
	by &model_var;
	run;
	
	data _to_lctemp1b;
		set _to_lctemp1 nobs=__nobs;
		by &model_var;

		%do k=1 %to &groups;
			%if &k=1 %then %do;
				if _n_<=ceil(&k*(__nobs/&groups)) then rank_model=&k;
				%end;
			%else %if &k<&groups %then %do;
				else if _n_<=ceil(&k*(__nobs/&groups)) then rank_model=&k;
				%end;
			%else %do;
				else rank_model=&k;
				%end;		 	
		 	%end;
		run;	
		
	proc sort data=_to_lctemp1b;
	by &actual_var;
	run;
	
	data _to_lctemp2;
		set _to_lctemp1b nobs=__nobs;
		by &actual_var;

		%do k=1 %to &groups;
			%if &k=1 %then %do;
				if _n_<=ceil(&k*(__nobs/&groups)) then rank_actual=&k;
				%end;
			%else %if &k<&groups %then %do;
				else if _n_<=ceil(&k*(__nobs/&groups)) then rank_actual=&k;
				%end;
			%else %do;
				else rank_actual=&k;
				%end;		 	
		 	%end;
		run;									
	%end;                              

%if &nbv>0 %then %do;
	proc sql;
	create table _to_lctemp3 as
	select a.*, b.total_obs, b.total_actual, c.best_model
	from (select &sqlby., rank_model,
	             sum(&actual_var) as model,
	             count(*) as obs
	      from _to_lctemp2
	      group by &sqlby., rank_model
	      ) a
	left join (select &sqlby., count(*) as total_obs, sum(&actual_var) as total_actual
	           from _to_lctemp2 group by &sqlby.) b
	  on %do i=1 %to &nbv;
				a.&&byvar&i..=b.&&byvar&i..
				%if &i<&nbv %then %do;
					and
					%end;
				%end;				  
	left join (select &sqlby., rank_actual,
	                  sum(&actual_var) as best_model
	           from _to_lctemp2
	           group by &sqlby., rank_actual
	           ) c
	  on %do i=1 %to &nbv;
				a.&&byvar&i..=c.&&byvar&i.. and
				%end;	
		  a.rank_model=c.rank_actual				
	order by &sqlby., rank_model
	;quit;
	%end;
%else %do;
	proc sql;
	create table _to_lctemp3 as
	select *, 
	       count(*) as total_obs,
	       sum(&actual_var) as total_actual
	from (select a.*, b.best_model
			from (select rank_model,
			             sum(&actual_var) as model,
			             count(*) as obs
			      from _to_lctemp2
			      group by rank_model
			      ) a	  
			left join (select rank_actual,
			                  sum(&actual_var) as best_model
			           from _to_lctemp2
			           group by 1,2
			           ) b
			  on a.rank_model=b.rank_actual	
		  )			
	order by rank_model
	;quit;
	%end;

%if &nbv>0 %then %do;
	data _to_lctemp4;
	   set _to_lctemp3;
	   by &procby. rank_model;
	
	   retain cum_obs cum_model cum_best;
	
	   if first.&&byvar&nbv.. then do;
	      cum_obs=0;
	      cum_model=0;
	      cum_best=0;
	      random=0;
	      pct_obs=0;
	      random=0;
	      pct_model=0;
	      pct_best=0;
	      output;
	      end;
	   
	   cum_obs=cum_obs+obs;
	   cum_model=cum_model+model;
	   cum_best=cum_best+best_model;
	
	   pct_obs=cum_obs/total_obs;
	   random=pct_obs;   
	   pct_model=cum_model/total_actual;
	   pct_best=cum_best/total_actual;
	   format pct_obs random pct_model pct_best percent8.1;
	   output;
	   run;
	%end;
%else %do;
	data _to_lctemp4;
	   set _to_lctemp3;
	
	   retain cum_obs cum_model cum_best;
	
	   if _n_=1 then do;
	      cum_obs=0;
	      cum_model=0;
	      cum_best=0;
	      random=0;
	      pct_obs=0;
	      random=0;
	      pct_model=0;
	      pct_best=0;
	      output;
	      end;
	   
	   cum_obs=cum_obs+obs;
	   cum_model=cum_model+model;
	   cum_best=cum_best+best_model;
	
	   pct_obs=cum_obs/total_obs;
	   random=pct_obs;   
	   pct_model=cum_model/total_actual;
	   pct_best=cum_best/total_actual;
	   format pct_obs random pct_model pct_best percent8.1;
	   output;
	   run;
	%end;


/*proc print data=_temp2;*/
/*run;*/

%if &nbv>0 %then %do;
	proc sort data=_to_lctemp1;
	by &procby. &model_var.;
	run;
	
	data _to_gini1(keep=&procby. gini auc);
		set _to_lctemp1;
		by &procby. &model_var;
		
		retain wsum 0 sum 0 nobs 0;
		
		if first.&&byvar&nbv. then do;
			wsum=0;
			sum=0;
			nobs=0;
			end;
		
		nobs=nobs+1;
		sum=sum+&actual_var;
		wsum=wsum+(nobs*&actual_var.);
						
		if last.&&byvar&nbv. then do;
			gini=1 - ((2/(nobs-1))*(nobs-(wsum/sum)));
			auc=(gini+1)/2;
			label gini='Gini'
			      auc='AUC'
			      ;
			output;
			end;
		run;
		
	proc sort data=_to_lctemp1;
	by &procby. &actual_var.;
	run;
	
	data _to_gini2(keep=&procby. perfect_gini perfect_auc);
		set _to_lctemp1;
		by &procby. &actual_var;
		
		retain wsum 0 sum 0 nobs 0;
		
		if first.&&byvar&nbv. then do;
			wsum=0;
			sum=0;
			nobs=0;
			end;
		
		nobs=nobs+1;
		sum=sum+&actual_var;
		wsum=wsum+(nobs*&actual_var.);
		
		if last.&&byvar&nbv. then do;
			perfect_gini=1 - ((2/(nobs-1))*(nobs-(wsum/sum)));
			perfect_auc=(perfect_gini+1)/2;
			output;
			end;
		run;
		
	proc sql;
	create table _to_gini3 as
	select a.*, 
			 a.gini/b.perfect_gini as pop_gini format=percent8.2 label='% of Perfect Model Gini',
			 a.auc/b.perfect_auc as pop_auc format=percent8.2 label='% of Perfect Model AUC',
			 b.perfect_gini label='Perfect Model Gini', 
			 b.perfect_auc label='Perfect Model AUC'
	from _to_gini1 a
	left join _to_gini2 b
	  on %do i=1 %to &nbv;
				a.&&byvar&i..=b.&&byvar&i..
				%if &i<&nbv %then %do;
					and
					%end;
				%end;	
	order by &sqlby.
	;quit;
	
	title "&model_label. Gini Coefficient and AUC Statistic";
	proc print data=_to_gini3 noobs label;
	var &procby. gini pop_gini perfect_gini auc pop_auc perfect_auc;
	run;
	
/*	proc print data=_to_lctemp3;*/
/*	run;*/
/*	*/
/*	proc print data=_to_lctemp4;*/
/*	run;*/
	
	title "&model_label. Lorenz Curve";	
	proc sgpanel data=_to_lctemp4;
	%if &nbv>1 %then %do;
		by %do i=1 %to %eval(&nbv-1); &&byvar&i %end; ;
		%end;		
	panelby &&byvar&nbv.. / layout=rowlattice;
	series x=pct_obs y=pct_model / legendlabel="&model_label." lineattrs=(pattern=1);
	series x=pct_obs y=pct_best / legendlabel="Perfect Model" lineattrs=(pattern=2);
	series x=pct_obs y=random / legendlabel="Random Model" lineattrs=(pattern=4);
	refline 1 / axis=x;
	label pct_obs='% Of Observations Sorted By Model Score (Ascending)';
	label pct_model="Cumulative % Of &actual_label. Captured";
	run;quit;
	%end;
%else %do;
	proc sort data=_to_lctemp1;
	by &model_var.;
	run;
	
	data _to_gini1(keep=gini auc);
		set _to_lctemp1 end=last;
		by &model_var;
		
		retain wsum 0 sum 0 nobs 0;
		
		if _n_=1 then do;
			wsum=0;
			sum=0;
			nobs=0;
			end;
		
		nobs=nobs+1;
		sum=sum+&actual_var;
		wsum=wsum+(nobs*&actual_var.);
						
		if last then do;
			gini=1 - ((2/(nobs-1))*(nobs-(wsum/sum)));
			auc=(gini+1)/2;
			label gini='Gini'
			      auc='AUC'
			      ;
			output;
			end;
		run;
		
	proc sort data=_to_lctemp1;
	by &actual_var.;
	run;
	
	data _to_gini2(keep=perfect_gini perfect_auc);
		set _to_lctemp1;
		by &model_var;
		
		retain wsum 0 sum 0 nobs 0;
		
		if _n_=1 then do;
			wsum=0;
			sum=0;
			nobs=0;
			end;
		
		nobs=nobs+1;
		sum=sum+&actual_var;
		wsum=wsum+(nobs*&actual_var.);
		
		if last then do;
			perfect_gini=1 - ((2/(nobs-1))*(nobs-(wsum/sum)));
			perfect_auc=(perfect_gini+1)/2;
			output;
			end;
		run;
		
	data _to_gini3;
		if _n_=1 then set _to_gini1;
		set_to_gini2;
      pop_gini=gini/perfect_gini;
      pop_auc=auc/perfect_auc;
      
      format pop_gini pop_auc percent8.2;
      label pop_gini='% of Perfect Model Gini'
            pop_auc='% of Perfect Model AUC'
            perfect_gini='Perfect Model Gini'
            perfect_auc='Perfect Model AUC'
            ;
      run;
      
   title "&model_label. Gini Coefficient and AUC Statistic";
	proc print data=_to_gini3 noobs label;
	var gini pop_gini perfect_gini auc pop_auc perfect_auc;
	run;
	
	title "&model_label. Lorenz Curve";	
	proc sgplot data=_to_lctemp4;
	series x=pct_obs y=pct_model / smoothconnect legendlabel="&model_label." lineattrs=(pattern=1);
	series x=pct_obs y=pct_best / smoothconnect legendlabel="Perfect Model" lineattrs=(pattern=2);
	series x=pct_obs y=random / smoothconnect legendlabel="Random Model" lineattrs=(pattern=4);
	refline 1 / axis=x;
	label pct_obs='% Of Observations Sorted By Model Score (Ascending)';
	label pct_model="Cumulative % Of &actual_label. Captured";
	run;quit;	
	%end;
%mend;


            