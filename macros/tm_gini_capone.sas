%macro tm_gini(data=,
			depvar=,	    /* Dependent Variable */
			modelvar=,   /* Model Score Variable */
			mpredicts=1, /* Value of dependent variable that model predicts.  Can only be 0 or 1 */
			where=,      /* Where clause */
			byvar=,		 /* By Group Processing:  Only one variable allowed here */
			freqgrps=20, /* Number of groups for use in the proc freq calculation of gini */
			ganalysis=y, /* Partial GINI coefficients and data points for charting */
			groups=10,   /* Number of groupings */
			curve=lorenz  /* roc is the other option */
			);

	%let null=;

	%if %quote(&mpredicts)=%quote(1) and %qupcase(&curve)=%quote(ROC) %then %let sort_order=descending;
	%else %if %quote(&mpredicts)=%quote(0) and %qupcase(&curve)=%quote(LORENZ) %then %let sort_order=descending;
	%else %let sort_order=;

	%if %quote(&where)=%quote(&null) %then %let wc=;
	%else %let wc=where=(&where);

	data _gini_t1;
		set &data(&wc keep=&byvar &depvar &modelvar);
		if &modelvar ne .;
		run;

	%if %quote(&null) ne %quote(&byvar) %then %do;
		proc sql;
		create table _gini_summary as 
		select &byvar, mean(&depvar) as dmean, count(*) as nobs
		from _gini_t1
		group by &byvar
		order by &byvar
		;quit;
		%end;
	%else %do;
		proc sql;
		reset noprint;
		select mean(&depvar) into :dmean from _gini_t1;
		select count(*) into :nobs from _gini_t1;
		;quit;
		%end;
	
	%if %quote(&null) ne %quote(&byvar) %then %do;
		proc sort data=_gini_t1;
		by &byvar;
		run;
		%end;

	proc rank data=_gini_t1 out=_gini_t2 ties=low &sort_order;
	%if %quote(&null) ne %quote(&byvar) %then %do;
		by &byvar;
		%end;
	var &modelvar;
	ranks rank;
	run;

	%if %qupcase(&ganalysis)=%qupcase(Y) %then %do;
		proc rank data=_gini_t2 out=_gini_t2 ties=low groups=&groups &sort_order;
		%if %quote(&null) ne %quote(&byvar) %then %do;
			by &byvar;
			%end;
		var &modelvar;
		ranks group;
		run;
		%end;
	
	proc rank data=_gini_t2 out=_gini_t2b ties=low groups=&freqgrps;
	%if %quote(&null) ne %quote(&byvar) %then %do;
			by &byvar;
			%end;
	var &modelvar;
	ranks fgroup;
	run;
	 
	data _gini_t2b;
		set _gini_t2b;
		percentile=((fgroup+1)/&freqgrps);
		run;
	
	%if %quote(&null) ne %quote(&byvar) %then %do;
		proc sort data=_gini_t2b;
		by &byvar;
		run;
		%end; 
	
	title "Gini (Somers' D R|C) with 95% Confidence Intervals";
	title2 "Calculated from twentiles based on the Model Score Variable &modelvar";	
	proc freq data=_gini_t2b;
	%if %quote(&null) ne %quote(&byvar) %then %do;
			by &byvar;
			%end;
	tables percentile*&depvar / measures cl;
	label percentile='Percentile';
	format percentile percent8.;
	run;

	title1 "Gini and the C Statistics calculated using the mean difference formula";
	title2 "UnBiased Gini = Gini*(n/(n+1))";
	title3 "C or UAC Statistic = .5*(1+Gini)";
	%if %quote(&null) ne %quote(&byvar) %then %do;
		proc sort data=_gini_t2;
		by &byvar;
		run;

		data _gini_t3;
			merge _gini_t2 _gini_summary;
			by &byvar;
			run;

		proc sql;
		select	&byvar, 
				abs(sum((2*rank-nobs-1)*&depvar)/(sum(nobs*&depvar))) as gini label='Gini Coefficient' format=8.6,
				(calculated gini)*(count(*)/(count(*)-1)) 
                  as ubgini label='Unbiased Gini Coefficient' format=8.6,
				.5*((calculated gini)+1) as cstat label='C or AUC Statistic (Area under ROC Curve)' format=8.6

		from _gini_t3
		group by &byvar
		order by &byvar
		;quit;
		%end;
	%else %do;
		proc sql;
		select 	abs(sum((2*rank-&nobs-1)*&depvar)/(&nobs*sum(&depvar))) as gini label='Gini Coefficient' format=8.6,
				(calculated gini)*(count(*)/(count(*)-1)) 
			as ubgini label='Unbiased Gini Coefficient' format=8.6,
			.5*((calculated gini)+1) as cstat label='C or AUC Statistic (Area under ROC Curve)' format=8.6
		from _gini_t2
		;quit;
		%end;
	
	title;
	title2;
	title3;

	%if %qupcase(&ganalysis)=%qupcase(Y) and %qupcase(&byvar)=%quote(&null) %then %do;
		%do i=0 %to %eval(&groups-1);
			%let sobs=(select count(*) from _gini_t2 where group<=&i and group ne .);
			%let dcount=(select sum(&depvar) from _gini_t2 where &modelvar ne .);
			%if &i=0 %then %do;
				proc sql;
				create table _gini_t4 as 
				select %eval(&i+1) as cum_group,	
				abs(sum((2*rank-&sobs-1)*&depvar)/(&sobs*sum(&depvar))) as gini label='Gini Coefficient' format=8.6,
				(calculated gini)*(count(*)/(count(*)-1)) 
				 as ubgini label='Unbiased Gini Coefficient' format=8.6,
				 .5*((calculated gini)+1) as cstat label='C or AUC Statistic (Area under ROC Curve)' format=8.6,
				sum(&depvar)/&dcount as cum_dep_var label="Cumulative % &depvar Captured" format=percent8.2,
				mean(&depvar) as pct_dep_var label="Cumulative mean &depvar" format=8.5,
				count(*) as cobs label="Cumulative Observations" format=comma10.
				from _gini_t2
				where group<=&i and group ne .
				;quit;
				%end;
			%else %do;
				proc sql;
				insert into _gini_t4  
				select %eval(&i+1) as cum_group,	
				abs(sum((2*rank-&sobs-1)*&depvar)/(&sobs*sum(&depvar))) as gini label='Gini Coefficient' format=8.6,
				(calculated gini)*(count(*)/(count(*)-1)) 
				 as ubgini label='Unbiased Gini Coefficient' format=8.6,
				 .5*((calculated gini)+1) as cstat label='C or AUC Statistic (Area under ROC Curve)' format=8.6,
				 sum(&depvar)/&dcount as cum_dep_var label="Cumulative % &depvar Captured" format=percent8.2,
				 mean(&depvar) as pct_dep_var label="Cumulative mean &depvar" format=8.5,
				 count(*) as cobs label="Cumulative Observations" format=comma10.
				from _gini_t2
				where group<=&i and group ne .
				;quit;
				%end;
			%end;
		
		%if %quote(&sort_order)=%quote(&null) %then %let torder=ascending;
		%else %let torder=descending;

		title "Cumulative Gini Table";
		title2 "Ranked in &torder order by model variable &modelvar";
		title3 "Model Predicts &depvar.=&mpredicts";
		title4 "Cumulative % &depvar Captured can be used to plot the &curve curve";

		proc sql;
		select 
				int(cum_group*(100/&groups)) as cum_group label='Nth Percentile', 
				gini, ubgini, cstat, cum_dep_var, pct_dep_var, cobs
		from _gini_t4
		order by cum_group
		;quit;

		title;
		title2;
		title3;
		%end;
%mend;