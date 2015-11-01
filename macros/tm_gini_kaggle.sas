%macro tm_gini_kaggle(data=,
							 out=,
							 depvar=,
                      scorevar=,
                      weightvar=,
                      by=
                      );

%let null=;

%if %quote(&by) ne %quote(&null) %then %do;
    %let nbv=%tm_ParseList(byvar,&by);
	 %end;
%else %do;
	%let nbv=0;
	%end;

%if %quote(&by) ne %quote(&null) %then %do;
    %let sqlbylist=%tm_ListFormat(&by,comma);
    %let databylist=%tm_ListFormat(&by,space);
    %let lastbyvar=&&byvar&nbv;
    %end;
%else %do;
    %let sqlbylist=;
    %end;


%if &nbv=0 %then %do;
	proc sort data=&data(keep=&depvar &scorevar &weightvar) out=_tgk1(compress=yes);
	by descending &scorevar;
	run;
	
	proc sort data=&data(keep=&depvar &scorevar &weightvar) out=_tgk1b(compress=yes);
	by descending &depvar;
	run;
	%end;
%else %do;
	proc sort data=&data(keep=&depvar &by. &scorevar &weightvar) out=_tgk1(compress=yes);
	by &databylist descending &scorevar;
	run;
	
	proc sort data=&data(keep=&depvar &by. &scorevar &weightvar) out=_tgk1b(compress=yes);
	by &databylist descending &depvar;
	run;
	%end;
	

%if &nbv=0 %then %do;
	proc sql;
	create table _tgk2 as
	select *,
	       sum(&weightvar) as _weight_sum,
	       sum(&depvar*&weightvar) as _total_positive,
	       count(*) as _nobs
	from _tgk1
	%if &nbv>0 %then %do;
		group by &sqlbylist
		order by &sqlbylist.,&scorevar desc
		%end;
	%else %do;
		order by &scorevar desc
		%end;
	;quit;
	
	data _tgk3_m1 _tgk3_mn;
		set _tgk2 end=last;
		retain _random _cum_pos_found _lorentz rowindex index 0;	
		
		rowindex=rowindex+1;
			
		_random=sum(_random,(&weightvar/_weight_sum));
		_cum_pos_found=sum(_cum_pos_found,&depvar*&weightvar);
		_lorentz=_cum_pos_found/_total_positive;
		
		if rowindex>1 then do;
			index=rowindex-1;
			output _tgk3_m1;
			end;
		if rowindex<_nobs then do;
			index=rowindex;
			output _tgk3_mn;
			end;
		run;
		
	proc sql;
	create table _tgk4 as
	select a.index as rowid,
			 a._lorentz as lorentz_m1,
	       a._random as random_m1,
	       b._lorentz as lorentz_mn,
	       b._random as random_mn,
	       a._lorentz*b._random as term1,                     
	       b._lorentz*a._random as term2	       
	from _tgk3_m1 as a
	join _tgk3_mn as b 
	on a.index=b.index
	order by rowid
	;quit;
	
	data _tgk4_test;
		set _tgk4;
		by rowid;		
		retain term1_sum term2_sum 0;		
		term1_sum=sum(term1_sum,term1);
		term2_sum=sum(term2_sum,term2);
		pgini=term1_sum-term2_sum;
		run;
	
	proc sql;
	create table _tgk5 as
	select max(1) as index, (sum(lorentz_m1*random_mn)-sum(lorentz_mn*random_m1)) as gini   
	from _tgk4
	;quit;
		  
	
	
	
	proc sql;
	create table _tgk2b as
	select *,
	       sum(&weightvar) as _weight_sum,
	       sum(&depvar*&weightvar) as _total_positive,
	       count(*) as _nobs
	from _tgk1b
	%if &nbv>0 %then %do;
		group by &sqlbylist
		order by &sqlbylist.,&depvar desc
		%end;
	%else %do;
		order by &depvar desc
		%end;
	;quit;
	
	data _tgk3b_m1 _tgk3b_mn;
		set _tgk2b end=last;
		retain _random _cum_pos_found _lorentz rowindex index 0;	
		
		rowindex=rowindex+1;
			
		_random=sum(_random,(&weightvar/_weight_sum));
		_cum_pos_found=sum(_cum_pos_found,&depvar*&weightvar);
		_lorentz=_cum_pos_found/_total_positive;
		
		if rowindex>1 then do;
			index=rowindex-1;
			output _tgk3b_m1;
			end;
		if rowindex<_nobs then do;
			index=rowindex;
			output _tgk3b_mn;
			end;
		run;
		
	proc sql;
	create table _tgk4b as
	select a._lorentz as lorentz_m1,
	       a._random as random_m1,
	       b._lorentz as lorentz_mn,
	       b._random as random_mn
	from _tgk3b_m1 as a
	join _tgk3b_mn as b
	on a.index=b.index
	;quit;
	
	proc sql;
	create table _tgk5b as
	select max(1) as index, (sum(lorentz_m1*random_mn)-sum(lorentz_mn*random_m1)) as ginib    
	from _tgk4b
	;quit;
	
	proc sql;
	create table &out as
	select a.gini as gini,
			 a.gini/b.ginib as gini_normalized 
	from _tgk5 as a
	join _tgk5b as b
	on a.index=b.index
	;quit;
	
	proc print data=&out noobs;
	run; 
	%end;
%else %do;
	proc sql;
	create table _tgk2 as
	select *,
	       sum(&weightvar) as _weight_sum,
	       sum(&depvar*&weightvar) as _total_positive,
	       count(*) as _nobs
	from _tgk1
	%if &nbv>0 %then %do;
		group by &sqlbylist
		order by &sqlbylist.,&scorevar desc
		%end;
	%else %do;
		order by &scorevar desc
		%end;
	;quit;
	
	data _tgk3_m1 _tgk3_mn;
		set _tgk2 end=last;
		by &databylist descending &scorevar;
				
		retain _random _cum_pos_found _lorentz rowindex index 0;	
		
		if first.&lastbyvar then do;
			_random=0; _cum_pos_found=0; _lorentz=0; rowindex=0; index=0;
			end;
			
		rowindex=rowindex+1;
			
		_random=sum(_random,(&weightvar/_weight_sum));
		_cum_pos_found=sum(_cum_pos_found,&depvar*&weightvar);
		_lorentz=_cum_pos_found/_total_positive;
		
		if rowindex>1 then do;
			index=rowindex-1;
			output _tgk3_m1;
			end;
		if rowindex<_nobs then do;
			index=rowindex;
			output _tgk3_mn;
			end;
		run;
		
	proc sql;
	create table _tgk4 as
	select %do i=1 %to &nbv;
	   		a.&&byvar&i..,
	   		%end;
			 a._lorentz as lorentz_m1,
	       a._random as random_m1,
	       b._lorentz as lorentz_mn,
	       b._random as random_mn
	from _tgk3_m1 as a
	join _tgk3_mn as b 
	on %do i=1 %to &nbv;
	   	a.&&byvar&i..=b.&&byvar&i.. and
	   	%end;
		a.index=b.index
	;quit;
	
	proc sql;
	create table _tgk5 as
	select &sqlbylist., (sum(lorentz_m1*random_mn)-sum(lorentz_mn*random_m1)) as gini   
	from _tgk4
	group by &sqlbylist
	;quit;
		  
	
	proc sql;
	create table _tgk2b as
	select *,
	       sum(&weightvar) as _weight_sum,
	       sum(&depvar*&weightvar) as _total_positive,
	       count(*) as _nobs
	from _tgk1b
	%if &nbv>0 %then %do;
		group by &sqlbylist
		order by &sqlbylist.,&depvar desc
		%end;
	%else %do;
		order by &depvar desc
		%end;
	;quit;
	
	data _tgk3b_m1 _tgk3b_mn;
		set _tgk2b end=last;
		by &databylist descending &depvar.;
				
		retain _random _cum_pos_found _lorentz rowindex index 0;		
		
		if first.&lastbyvar then do;
			_random=0; _cum_pos_found=0; _lorentz=0; rowindex=0; index=0;
			end;
			
		rowindex=rowindex+1;
			
		_random=sum(_random,(&weightvar/_weight_sum));
		_cum_pos_found=sum(_cum_pos_found,&depvar*&weightvar);
		_lorentz=_cum_pos_found/_total_positive;
		
		if rowindex>1 then do;
			index=rowindex-1;
			output _tgk3b_m1;
			end;
		if rowindex<_nobs then do;
			index=rowindex;
			output _tgk3b_mn;
			end;
		run;
		
	proc sql;
	create table _tgk4b as
	select %do i=1 %to &nbv;
	   		a.&&byvar&i..,
	   		%end;
			 a._lorentz as lorentz_m1,
	       a._random as random_m1,
	       b._lorentz as lorentz_mn,
	       b._random as random_mn
	from _tgk3b_m1 as a
	join _tgk3b_mn as b
	on %do i=1 %to &nbv;
	   	a.&&byvar&i..=b.&&byvar&i.. and
	   	%end;
		a.index=b.index
	;quit;
	
	proc sql;
	create table _tgk5b as
	select &sqlbylist., (sum(lorentz_m1*random_mn)-sum(lorentz_mn*random_m1)) as ginib    
	from _tgk4b
	group by &sqlbylist
	;quit;
	
	proc sql;
	create table &out as
	select %do i=1 %to &nbv;
	   		a.&&byvar&i..,
	   		%end;
			 a.gini as gini,
			 a.gini/b.ginib as gini_normalized 
	from _tgk5 as a
	join _tgk5b as b
	on %do i=1 %to &nbv;
	   	a.&&byvar&i..=b.&&byvar&i.. %if &i ne &nbv %then %do; %str(and) %end;
	   	%end;
	;quit;
	
	proc print data=&out noobs;
	run; 
	%end;                
%mend;
                      