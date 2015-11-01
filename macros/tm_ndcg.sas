/********************************************************************************/
/*  Calculates the Normalized Discounted Cumulative Gain (NDCG) based on        */
/*  a model score for ordering and a relevance value (actual).                  */
/*                                                                              */
/*  See http://en.wikipedia.org/wiki/Discounted_cumulative_gain#Normalized_DCG  */
/*  for more information on NDCG.                                               */
/********************************************************************************/

%macro tm_ndcg(data=,
               out=,
               report=y,     /* y or n */
               by=,
               average_by_groups=y,
               score=,
               sort_order=a,  /* a=ascending, d=descending */
               actual=
               );

%let null=;
%let nbv=0;

%if %quote(&out) = %quote(&null) %then %let out=work.tm_ndcg_results;

%if %qupcase(&sort_order)=%quote(D) %then %do;
    %let sql_order=desc;
    %let ds_order=descending;
    %end;
%else %do;
    %let sql_order=asc;
    %let ds_order=;
    %end;

%if %quote(&by) ne %quote(&null) %then %do;
    %let flag=0; %let word=&null; %let i=0;
    %do %until(&flag=1);
        %let i=%eval(&i+1);
        %let word=%scan(&by,&i,%str( ));
        %if &word ne &null %then %do;
            %let bvar&i=&word;            
            %end;
        %else %do;
            %let nbv=%eval(&i-1);
            %let flag=1;
            %end;
        %end;
    
    %do k=1 %to &nbv;
    	%if &k=1 %then %let sqlby=&&bvar&k..;
        %else %let sqlby=&sqlby.,&&bvar&k;
        %end;
    %end;

proc sql;
create view work.tm_ndcg_model as
select * from &data
%if &nbv>0 %then %do;
    order by &sqlby., &score &sql_order
    %end;
%else %do;
    order by &score &sql_order
    %end;
;quit;

proc sql;
create view work.tm_ndcg_ideal as
select * from &data
%if &nbv>0 %then %do;
    order by &sqlby., &actual desc
    %end;
%else %do;
    order by &actual desc
    %end;
;quit;

data work.tm_ndcg_ideal_stat(keep=&by rank dcg_ideal);
    set work.tm_ndcg_ideal end=last;
    retain rank dcg_ideal 0;
    %if &nbv>0 %then %do;
        by &by. descending &actual;
        if first.&&bvar&nbv then do;
        %end;
    %else %do;
        by descending &actual;
        if _n_=1 then do;
        %end;

        rank=0; dcg_ideal=0;
        end;

    rank=rank+1;

    if rank=1 then dcg_ideal=&actual;
    else dcg_ideal =dcg_ideal +(&actual/log2(rank));

    %if &nbv>0 %then %do;
        if last.&&bvar&nbv then do;
            output;
            end;
        %end;
    %else %do;
        if last then do;
            output;
            end;
        %end;
run;


data work.tm_ndcg_model_stat(keep=&by rank dcg_model);
    set work.tm_ndcg_model end=last;
    retain rank dcg_model 0;
    %if &nbv>0 %then %do;
        by &by. &ds_order &score;
        if first.&&bvar&nbv then do;
        %end;
    %else %do;
        by &ds_order &score;
        if _n_=1 then do;
        %end;

        rank=0; dcg_model=0;
        end;

    rank=rank+1;

    if rank=1 then dcg_model=&actual;
    else dcg_model=dcg_model+(&actual/log2(rank));

    %if &nbv>0 %then %do;
        if last.&&bvar&nbv then do;
            output;
            end;
        %end;
    %else %do;
        if last then do;
            output;
            end;
        %end;
run;


proc sql;
create table &out as
select a.rank label='# Records'
       %if &nbv>0 %then %do;
            %do i=1 %to &nbv;
                ,a.&&bvar&i
                %end;
            %end;
      ,a.dcg_model label='Model - Discounted Cumulative Gain (DCG)',
       b.dcg_ideal label='Ideal - Discounted Cumulative Gain (DCG)',
       (a.dcg_model/b.dcg_ideal) as ndcg label='Normalized Discounted Cumulative Gain (NDCG)'
from work.tm_ndcg_model_stat a,
     work.tm_ndcg_ideal_stat b
where a.rank=b.rank
      %if &nbv>0 %then %do;
            %do i=1 %to &nbv;
                 and a.&&bvar&i=b.&&bvar&i
                %end;
            %end;
;quit;

%if &nbv>0 and %qupcase(&average_by_groups)=%quote(Y) %then %do;
	proc sql;
	create table &out as 
	select sum(rank) as rank label='# Records',
	       count(*) as bygroups label='# By Groups',
		   mean(dcg_model) as dcg_model label='Model - Discounted Cumulative Gain (DCG)',
	       mean(dcg_ideal) as dcg_ideal label='Ideal - Discounted Cumulative Gain (DCG)',
	       mean(ndcg) as ndcg label='Normalized Discounted Cumulative Gain (NDCG)'
	from &out;
	quit;
	
	%let by=;	
    %end;

%if %qupcase(&report)=%quote(Y) %then %do;
    title4 'Normalized Discounted Cumulative Gain Report';
    title5 "Model Score Variable = &score";
    title6 "Actual Variable = &actual";
    proc print data=&out noobs label;
    var rank &by dcg_model dcg_ideal ndcg;
    run;
    %end;
%mend;






















