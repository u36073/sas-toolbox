%macro tm_rank_chart(data=,
	    	            out=,
			            where=,
			            wherelabel=,
			            by=,
			            depvar1=,
			            depvar2=,
			            depvar3=,
			            indvar=,
			            indlabel=,
			            indformat=,
			            groups=10,
			            format1=,
			            format2=,
			            format3=,
			            depname1=,
			            depname2=,
			            depname3=,
			            deplabel1=,
			            deplabel2=,
			            deplabel3=,
			            bylabel=,
			            graph=,
			            graph_type=bar,
			            print_table=y,
			            graph_data_table=y,
			            keepvars=,
			            TransformVarFilename=,
			            TransformVarName=,
			            TransformVarType=1   /* 1 ===> Rank Ordered Groups from Lowest to Highest (1st Group=1, 2nd Group=2, ...) */
			                                 /* 2 ===> Rank Ordered Groups from Lowest to Highest (1st Group=mean(depvar1), 2nd Group=mean(depvar1), ...) */
			            );
	%local i j k null;
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
		set &data(keep=&keepvars &indvar &by);
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
	var &indvar;
	ranks rank;
	run;

	%let null=;

	%if %quote(&depname1)=%quote(&null) %then %let depname1=__v1;
	%if %quote(&depname2)=%quote(&null) %then %let depname2=__v2;
	%if %quote(&depname3)=%quote(&null) %then %let depname3=__v3;

	%if %quote(&deplabel1)=%quote(&null) %then %let deplabel1=&depvar1;
	%if %quote(&deplabel2)=%quote(&null) %then %let deplabel2=&depvar2;
	%if %quote(&deplabel3)=%quote(&null) %then %let deplabel3=&depvar3;

	%if %quote(&indlabel)=%quote(&null) %then %let indlabel=&indvar;
	%if %quote(&bylabel)=%quote(&null) %then %let bylabel=&by;

	TITLE h=2 "&indlabel Data Table";
	%if %quote(&where) ne %quote(&null) %then %do;
		title2 h=1.5 "&wherelabel";
		%end;
	proc sql;
	%if %qupcase(&print_table)=%quote(N) %then %do;
		reset noprint;
		%end;
	select %if %quote(&by) ne %quote(&null) %then %do;
			&sqlby,
			%end;
			rank,
			&depvar1 as &depname1
			                  label="&deplabel1"
			                  %if %quote(&format1) ne %quote(&null) %then %do;
												format=&format1
												%end;
												,
			%if %quote(&depvar2) ne %quote(&null) %then %do;
				&depvar2 as &depname2
													label="&deplabel2"
				                  %if %quote(&format2) ne %quote(&null) %then %do;
													format=&format2
													%end;
													,
				%end;
			%if %quote(&depvar3) ne %quote(&null) %then %do;
				&depvar3 as &depname3
													label="&deplabel3"
													%if %quote(&format3) ne %quote(&null) %then %do;
													format=&format3
													%end;
													,
				%end;
			count(*)/(select count(*) from __temp) as pct_obs format=percent7.2,
			mean(&indvar) as _mindvar label="Predicted &indvar",
			min(&indvar) as _min label="MIN(&indvar)",
			max(&indvar) as _max label="MAX(&indvar)",
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

  %if %quote(&out) eq %quote(&null) %then %let out=work._rankx_temp;

  proc sql;
  create table &out as
  select %if %quote(&by) ne %quote(&null) %then %do;
  		&sqlby,
  		%end;
  		rank,
  		&depvar1 as &depname1 %if %quote(&format1) ne %quote(&null) %then %do;
  											label="&deplabel1"
  											%end;
  											,
  		%if %quote(&depvar2) ne %quote(&null) %then %do;
  			&depvar2 as &depname2 %if %quote(&format2) ne %quote(&null) %then %do;
  												label="&deplabel2"
  												%end;
  												,
  			%end;
  		%if %quote(&depvar3) ne %quote(&null) %then %do;
  			&depvar3 as &depname3 %if %quote(&format3) ne %quote(&null) %then %do;
  												label="&deplabel3"
  												%end;
  												,
  			%end;
  		count(*)/(select count(*) from __temp) as pct_obs,
  		min(&indvar) as _min label="MIN(&indvar)",
  		max(&indvar) as _max label="MAX(&indvar)",
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

  %if %quote(&TransformVarFilename)=%quote(&null) %then %let TransformVarFilename=log;
  %if %quote(&TransformVarName)=%quote(&null) %then %let TransformVarName=&indvar._g&groups;

  %if %quote(&by) ne %quote(&null) %then %do;
     data _null_;
     	set &out end=last;
     	by &by;
     	file &TransformVarFilename;
			rank=rank+1;
     	if first.&&byvar&nbv then do;
     		%if %VarType(&by,&data)=C %then %do;
     			put "if &by='" &by "' then do;";
     			%end;
     		%else %do;
     		  put "if &by=" &by " then do;";
     		  %end;
     		put "   if &indvar<=" _max " then do;";
     		put "      &TransformVarName.=" rank ";";
     		put "   end;";
     		end;
     	else if not last.&&byvar&nbv then do;
   		put "   else if &indvar<=" _max " then do;";
     		put "      &TransformVarName.=" rank ";";
     		put "      end;";
     		end;
     	else do;
     		put "   else do;";
     		put "      &TransformVarName.=" rank ";";
     		put "      end;";
     		put "   end;";
     		end;
     	run;
     %end;
  %else %do;
     data _null_;
     	set &out end=last;
     	file &TransformVarFilename;
     	%if %qupcase(&TransformVarType)=%quote(1) %then %do;
     		assigned_value=rank+1;
     		%end;
     	%else %if %qupcase(&TransformVarType)=%quote(2) %then %do;
     	  assigned_value=&depname1;
     	  %end;
     	%else %do;
     	  assigned_value=rank+1;
     	  %end;
     	if _n_=1 then do;
     		put "if &indvar<=" _max " then do;";
     		put "   &TransformVarName.=" rank ";";
     		put "   end;";
     		end;
     	else if not last then do;
   		put "else if &indvar<=" _max " then do;";
     		put "   &TransformVarName.=" rank ";";
     		put "   end;";
     		end;
     	else do;
     		put "else do;";
     		put "   &TransformVarName.=" rank ";";
     		put "   end;";
     		end;
     	run;
     %end;

	proc sql;
	create table __graph1 as
	     select %if %quote(&by) ne %quote(&null) %then %do;
	     		&sqlby,
	     		%end;
	     		rank,
	     		&depvar1 as &depname1 %if %quote(&format1) ne %quote(&null) %then %do;
	     											format=&format1
	     											label="&deplabel1"
	     											%end;
	     											,
	     		%if %quote(&depvar2) ne %quote(&null) %then %do;
	     			&depvar2 as &depname2 %if %quote(&format2) ne %quote(&null) %then %do;
	     												format=&format2
	     												label="&deplabel2"
	     												%end;
	     												,
	     			%end;
	     		%if %quote(&depvar3) ne %quote(&null) %then %do;
	     			&depvar3 as &depname3 %if %quote(&format3) ne %quote(&null) %then %do;
	     												format=&format3
	     												label="&deplabel3"
	     												%end;
	     												,
	     			%end;
	     		count(*)/(select count(*) from __temp) as pct_obs format=percent7.2,
	     		min(&indvar) as _min label="MIN(&indvar)",
	     		max(&indvar) as _max label="MAX(&indvar)",
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
title;
%if %qupcase(&graph)=%quote(Y) %then %do;
	ods path work.templat(update) sashelp.tmplmst(read);
	%do i=1 %to 3;
		%if %quote(&&depvar&i) ne %quote(&null) %then %do;

			proc sql;
			reset noprint;
			select max(1.05*&&depname&i) into :maxy from __graph1 where rank ne .;
			select min(.95*&&depname&i) into :miny from __graph1 where rank ne .;
			quit;

			data _null_;
				if &miny<0 and &maxy>0 then do;
				   call symput('zeroref',strip("Y"));
					end;
				else do;
					call symput('zeroref',strip("N"));
					end;
				run;


			%if %qupcase(&graph_type)=%quote(BAR) %then %do;
			   proc template;
	         define statgraph sgdesign;
            begingraph / border=false designwidth=9in designheight=7in;
            entrytitle halign=center "%upcase(&indlabel) &groups.-Quantiles";
			   entrytitle halign=center "&&deplabel&i";
               layout overlay / walldisplay=ALL
                                xaxisopts=( label=("%upcase(&indlabel) &groups.-Quantiles") type=discrete)
                                yaxisopts=( label=("&&deplabel&i.") /*linearopts=( viewmax=&maxy. viewmin=&miny.)*/)
                                ;

                  barchart category=rank response=&&depname&i / %if &nbv>=2 %then %do;	group=&byvar2. %end;
                  										                %else %if &nbv=1 %then %do;	group=&byvar1. %end;
                                                                name='series' xaxis=X stat=mean barlabel=false
                                                                barwidth=1.0 groupdisplay=Cluster BARLABELFITPOLICY=NONE
                                                                clusterwidth=0.85 grouporder=ascending;
						%if %qupcase(&zeroref)=%quote(Y) %then %do;
                  	referenceline y=0 / name='hzeroref' xaxis=X lineattrs=(pattern=SHORTDASH thickness=1 color=darkblue)
                     	                 curvelabel="Zero" curvelabellocation=outside curvelabelposition=max
                        	              ;
                     %end;

                  %if %qupcase(&graph_data_table)=%quote(Y) %then %do;
              		   innermargin / align=bottom opaque=true backgroundcolor=lightgray separator=true;
              		   	axistable x=rank value=_max / stat=sum
              		   	                              %if &nbv>=2 %then %do;	class=&byvar2. %end;
                     										   %else %if &nbv=1 %then %do;	class=&byvar1. %end;
              		   	                              display=all
              		      	                           headerlabel="&groups.-Quantile Maximum Value of &indlabel."
              		        		                        ;
              		   endinnermargin;


              		   innermargin / align=top opaque=true backgroundcolor=lightgray separator=true;              		        		                        ;
                       axistable x=rank value=&&depname&i / stat=sum
              		   	                              %if &nbv>=2 %then %do;	class=&byvar2. %end;
                     										   %else %if &nbv=1 %then %do;	class=&byvar1. %end;
              		   	                              display=all
              		      	                           headerlabel="&&deplabel&i.."
              		        		                        ;
              		   endinnermargin;
              		   %end;

              		discretelegend 'series' / opaque=false border=true
                                 halign=center valign=bottom displayclipped=true
                                 order=rowmajor location=outside;
               endlayout;
            endgraph;
            end;
            run;
				%end;
			%else %do;
			   proc template;
	         define statgraph sgdesign;
            begingraph / border=false designwidth=9in designheight=7in;
            entrytitle halign=center "%upcase(&indlabel) &groups.-Quantiles";
			   entrytitle halign=center "&&deplabel&i";
               layout overlay / walldisplay=ALL
                                xaxisopts=( label=("%upcase(&indlabel) &groups.-Quantiles") type=discrete discreteopts=( tickvaluefitpolicy=splitrotate))
                                yaxisopts=( label=("&&deplabel&i."))
                                ;
                  seriesplot x=rank y=&&depname&i / %if &nbv>=2 %then %do;	group=&byvar2. %end;
                  										    %else %if &nbv=1 %then %do;	group=&byvar1. %end;
                                                    name='series'
                                                    display=(markers)
                                                    clusterwidth=0.5
                                                    connectorder=xaxis
                                                    grouporder=ascending;

						%if %qupcase(&zeroref)=%quote(Y) %then %do;
                  	referenceline y=0 / name='hzeroref' xaxis=X lineattrs=(pattern=SHORTDASH thickness=1 color=darkblue)
                     	                 curvelabel="Zero" curvelabellocation=outside curvelabelposition=max
                        	              ;
                     %end;

                  %if %qupcase(&graph_data_table)=%quote(Y) %then %do;
              		   innermargin / align=bottom opaque=true backgroundcolor=lightgray separator=true;
              		   	axistable x=rank value=_max / stat=sum
              		   	                              %if &nbv>=2 %then %do;	class=&byvar2. %end;
                     										   %else %if &nbv=1 %then %do;	class=&byvar1. %end;
              		   	                              display=all
              		      	                           headerlabel="&groups.-Quantile Maximum Value of &indlabel."
              		        		                        ;
              		   endinnermargin;

              		   innermargin / align=top opaque=true backgroundcolor=lightgray separator=true;              		        		                        ;
                       axistable x=rank value=&&depname&i / stat=sum
              		   	                              %if &nbv>=2 %then %do;	class=&byvar2. %end;
                     										   %else %if &nbv=1 %then %do;	class=&byvar1. %end;
              		   	                              display=all
              		      	                           headerlabel="&groups.-Quantile Maximum Value of &indlabel."
              		        		                        ;
              		   endinnermargin;
              		   %end;

              		discretelegend 'series' / opaque=false border=true
                                 halign=center valign=bottom displayclipped=true
                                 order=rowmajor location=outside;
               endlayout;
            endgraph;
            end;
            run;
				%end;


         proc sgrender data=__graph1 template=sgdesign;
         %if &nbv>=2 %then %do; by &byvar1.; %end;
         format _max &indformat.;
	      run;
			%end;
		%end;
	%end;

%mend;
