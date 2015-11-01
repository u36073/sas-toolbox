%macro tm_impute(data=,OutCode=);
	
	%local i;
	
	proc contents data=&data noprint out=contents;
	run;

	data _null_;
		set contents end=last;
		call symput('var'||trim(left(_n_)),trim(left(name)));
		call symput('typ'||trim(left(_n_)),trim(left(type)));
		if last then do;
			call symput('nvars',trim(left(_n_)));
			end;
		run;
	
	%macro vlist;
		%do i=1 %to &nvars;
			%if &&typ&i=1 %then %do;
				&&var&i
				%end;
			%end;
			;
	%mend;

	proc means data=&data noprint;
	var %vlist;
	output out=__means mean=%do i=1 %to &nvars;
								%if &&typ&i=1 %then %do;
									mean&i
									%end;
								%end;
								;
	run;

	data _null_;
		set __means;
		file &OutCode;
		%do i=1 %to &nvars;
			%if &&typ&i=1 %then %do;
				if mean&i=. then mean&i=0;
				put "if &&var&i=. then &&var&i=" mean&i best12.4 ";";
				%end;
			%end;
		run;		
%mend;
