%macro tm_create_groups(data=,
								where=,
								out=,
								filename=,
								nominal_vars=,
								ordinal_vars=,
								numeric_vars=,
								var_prefix=,
								by=,
								depvar=,
								nominal_min_group_size=.05,
								nominal_group_other_threshold=.01,
								nominal_grouping_threshold=.20,  /* Proportion (of the group to join's &depvar mean) */
								nominal_type_of_grouping=,       /* n=nominal, o = ordinal, m = group projected means, i=indicator */
								ordinal_min_group_size=.05,
								ordinal_grouping_threshold=.20,	/* Proportion (of the group to join's &depvar mean) */
								ordinal_grouping_type=,          /* n=nominal, o = ordinal, m = group projected means, i=indicator */
								numeric_min_group_size=.10,
								numeric_grouping_threshold=.20,  /* Proportion (of the group to join's &depvar mean) */
								numeric_group_type=,             /* n=nominal, o = ordinal, m = group projected means, i=indicator */								
								tm_macro_lib_directory=D:/My Documents/My SAS Files
							  );
%local i j k null;

%let null=;

%include "&tm_macro_lib_directory./macro-development/tm_VarType.sas";
%include "&tm_macro_lib_directory./macro-development/tm_ParseList.sas";
%include "&tm_macro_lib_directory./macro-development/tm_ListItemCount.sas";
%include "&tm_macro_lib_directory./macro-development/tm_ListFormat.sas";
%include "&tm_macro_lib_directory./macro-development/tm_ListDedupe.sas";
%include "&tm_macro_lib_directory./macro-development/tm_dsobs.sas";

%let vars=%trim(%left(%tm_ListDedupe(&vars.)));
%let n_vars=%tm_ParseList(var,&vars.);

%let numeric_vars=%trim(%left(%tm_ListDedupe(&numeric_vars.)));
%let n_numvars=%tm_ParseList(numvar,&numeric_vars.);

%let ordinal_vars=%trim(%left(%tm_ListDedupe(&ordinal_vars.)));
%let n_ordvars=%tm_ParseList(ordvar,&ordinal_vars.);

%let nominal_vars=%trim(%left(%tm_ListDedupe(&nominal_vars.)));
%let n_nomvars=%tm_ParseList(nomvar,&numeric_vars.);

%let by=%trim(%left(%tm_ListDedupe(&by.)));
%let nbyvars=%tm_ParseList(byvar,&by.);

%let numeric_proc_rank_groups=%sysevalf(1/&numeric_min_group_size,floor);

data _to_tmcg1(compress=yes keep=&depvar &by &nominal_vars &ordinal_vars &numeric_vars);
	set &data;
	%if %quote(&where) ne %quote(&null) %then %do;
		where &where;
		%end;
	run;

%if &nbyvars=0 %then %do;
	%if &n_nomvars>0 or &n_ordvars>0 %then %do;
		%if &n_nomvars>0 %then %do;
			%do i=1 %to &n_nomvars.;
				proc means data=_to_tmcg1 noprint nway;
				class &&nomvar&i;
				var &depvar;
				output out=_to_nom_means&i. n=(&depvar)=_n_ nmiss(&depvar)=_nmiss_
				                            mean(&depvar)=_mean_ median(&depvar=_median);
				run;
				%end;
			%end;
		%if &n_ordvars>0 %then %do;
			%do i=1 %to &n_ordvars.;
				proc means data=_to_tmcg1 noprint nway;
				class &&ordvar&i;
				var &depvar;
				output out=_to_ord_means&i. n=(&depvar)=_n_ nmiss(&depvar)=_nmiss_
				                            mean(&depvar)=_mean_ median(&depvar=_median);
				run;
				%end;
			%end;
		run;

		%if &n_nummvars>0 %then %do;
			%do i=1 %to &n_numvars.;
				proc rank data=_to_tmcg1 out=_to_num_rank&i. groups=&numeric_proc_rank_groups.;
				var &&numvar&i..;
				ranks _rank_;
				run;
				%end;
			%end;
		%end;


	%end;



%mend;
