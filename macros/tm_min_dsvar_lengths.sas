%macro tm_min_dsvar_lengths(data=,
									 out=,
									 drop=,
			                   data2=,data3=,data4=,data5=,data6=,data7=,data8=,data9=,data10=,data11=,data12=,data13=);
			                   
	proc contents data=&data out=contents noprint;
	run;

	proc sort data=contents;
	where type=1;
	by name;
	run;

	data _null_;
		set contents end=last;
		call symput('var'||trim(left(_n_)),trim(left(name)));
		if last then do;
			call symput('nvar',trim(left(_n_)));
			end;
		run;
	
	proc sql;
	create table _max as 
	select max(length(trim(left(put(&var1,best20.2))))) as &var1
			%do i=2 %to &nvar;
				,max(length(trim(left(put(&&var&i,best20.2))))) as &&var&i
				%end;
	from &data
	;quit;

	proc sql;
	reset noprint; 
	%do i=1 %to &nvar;
		select max(&&var&i) into :max&i from _max;
		%end;
	;quit;
	
	%let null=;
	data &out(compress=yes);
		set &data(rename=(%do i=1 %to &nvar;
							&&var&i=_&&var&i
							%end;
							))
			%do k=2 %to 13;
				%if %quote(&&data&k) ne %quote(&null) %then %do;
					&&data&k(rename=(%do i=1 %to &nvar;
								&&var&i=_&&var&i
								%end;
								))
					%end;
				%end;
				;

		drop %do i=1 %to &nvar;
				_&&var&i
				%end;
			 &drop
				;

		length %do i=1 %to &nvar;
					%if &&max&i<4 %then %do;
						&&var&i	3
						%end;
					%else %if &&max&i<6 %then %do;
						&&var&i 4
						%end;
					%else %if &&max&i<9 %then %do;
						&&var&i 5
						%end;
					%else %if &&max&i<12 %then %do;
						&&var&i 6
						%end;
					%else %if &&max&i<14 %then %do;
						&&var&i 7
						%end;
					%else %do;
						&&var&i 8
						%end;
					%end;
					;
		%do i=1 %to &nvar;
			&&var&i=_&&var&i;
			%end;
		run;
%mend;



	