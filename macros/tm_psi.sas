%macro tm_psi(data=,
              by=,
              var=,
              time_var=,              
              expected1=UM DM 23.5 24.73 23.86 17.12 6.33 2.73 1.09 0.32 0.32 0,
              expected2=UM IB 32.32 17.29 13.39 12.17 10.21 7.4 4.37 1.77 0.73 0.36,
              expected3=UM UA 50 0 0 0 25 0 0 25 0 0,
              expected4=USM DM 19.44 18.64 16.87 14.1 11.47 8.21 5.78 3.41 1.41 0.64,
              expected5=USM IB 17.24 14.81 14.31 13.01 11.82 10.84 8.24 5.48 2.5 1.74,
              expected6=USM TM 18.81 18.78 17.37 15.64 11.27 8.74 5.46 2.74 0.91 0.27,
              expected7=USM UA 20.27 18.06 18.23 15.84 14.82 7.33 4.26 0.85 0 0.34,
              expected8=,
              expected9=,
              expected10=,
              expected12=,
              expected13=,
              expected14=,
              expected15=,
              expected16=,
              expected17=,
              expected18=,
              expected19=,
              expected20=,
              expected22=,
              expected23=,
              expected24=,
              expected25=,
              expected26=,
              expected27=,
              expected28=,
              expected29=,
              expected30=
             );
%local null i k j nevgroups bylist bylist_wt sqlbylist sqlbylist_wt flag total_obs;
%let null=;

%if %quote(&time_var) ne %quote(&null) %then %let time_var_type=%tm_vartype(&time_var,&data);
%else %let time_var_type=;  

%let var_type=%tm_vartype(&var,&data);   
%put &var_type;

%if %quote(&by) ne %quote(&null) %then %do;
    %let nbv=%tm_ParseList(byvar,&by);
    %do i=1 %to &nbv;
        %local bylab&i bytype&i;
        %let bylab&i=%tm_VarLabel(&&byvar&i,&data);
        %let bytype&i=%tm_VarType(&&byvar&i,&data);
        %if %quote(&&bylab&i)=%quote(&null) %then %let bylab&i=&&byvar&i;
        
        %if %tm_VarFormat(&&byvar&i..,&data) ne %quote(&null) %then %let byfmt&i=%tm_VarFormat(&&byvar&i..,&data);
        %else %do;
            %if %tm_vartype(&&byvar&i,&data)=N %then %let byfmt&i=best18.9;
            %else %let byfmt&i=;
            %end;
        %end;
        %let bylist=&by; 
        %let bylist_wt=&by &time_var;
        %let sqlbylist=%tm_ListFormat(&bylist,comma);
        %let sqlbylist_wt=%tm_ListFormat(&bylist_wt,comma);
    %end;
%else %do;
    %let nbv=0;
    %let bylist=&time_var;
    %let sqlbylist=&time_var;
    %let bylist_wt=&time_var;
    %let sqlbylist_wt=&time_var;
    %end;

%if &nbv>0 %then %do;
    %let flag=0; %let nevgroups=0;
    %do %until(&flag=1);
        %let nevgroups=%eval(&nevgroups+1);
        %if %quote(&&expected&nevgroups..)=%quote(&null) %then %do;
            %let flag=1;
            %let nevgroups=%eval(&nevgroups-1);
            %end;
        %end;
    %end;
%else %do;
    %let nevgroups=1;
    %end;
    
%do i=1 %to &nevgroups;    
    %let nvg_items&i=%tm_ParseList(ev&i._item,&&expected&i);
    %end;    
  
proc sql;
create table work.__psi_t1 as 
select 
    %if &nbv>0 or %quote(&time_var) ne %quote(&null) %then %do;
       &sqlbylist_wt.,
       %end;
    &var,
    count(*) as nobs
from &data
group by 
    %if &nbv>0 or %quote(&time_var) ne %quote(&null) %then %do;
       &sqlbylist_wt,
       %end;
    &var
order by 
    %if &nbv>0 or %quote(&time_var) ne %quote(&null) %then %do;
       &sqlbylist_wt,
       %end;
    &var
;quit; 

%if &nbv>0 or %quote(&time_var) ne %quote(&null) %then %do;   
	%if %quote(&time_var) ne %quote(&null) %then %let lastbv=&time_var;
	%else %let lastbv=&&byvar&nbv..; 
	 
    data work.__psi_t2(drop=nobs &var);
       set work.__psi_t1;
       by &bylist_wt;
       retain total_obs 0;
      
       if first.&lastbv then do;
          total_obs=0;
          end;
       total_obs=total_obs+nobs;
       if last.&lastbv then output; else delete;
       run;

    proc sort data=work.__psi_t2;
    by &bylist_wt;
    run;
    
    proc sql;
    create table work.__allsc as
    select &sqlbylist_wt., count(*) as nobs  
    from &data
    group by &sqlbylist_wt
    order by &sqlbylist_wt
    ;quit;
    
    data work.__allsc(drop=i nobs);
    	set work.__allsc;
    	by &bylist_wt;
    	%if %qupcase(&var_type)=%quote(N) %then %do; length &var 8; %end;
    	%else %do; length &var $ 8; %end;
    	if first.&lastbv then do;
    		do i=1 to &nvg_items1;
    			&var=i;
    			output;
    			end;
    		end;
    	run;
    	
    proc sort data=work.__allsc;
    by &bylist_wt. &var;
    run;
    
    data work.__allsc;
    	merge work.__allsc work.__psi_t1;
    	by &bylist_wt. &var;
    	run;	    			    			
    
    data work.__psi_t3;
       merge work.__psi_t1 work.__psi_t2 work.__allsc;
       by &bylist_wt;
       pct_obs=100*(max(0,nobs)/total_obs);
       pct_obs=round(pct_obs,.01);         
       %do i=1 %to &nevgroups;
          %do k=%eval(&nbv+1) %to &&nvg_items&i..;
             if (1=1)
             %if &nbv>0 %then %do;
                %do j=1 %to &nbv;                           
                   %if &&bytype&j=N %then %do; and (&&byvar&j=&&ev&i._item&j..) %end;
                   %else %do; and (trim(left(upcase(&&byvar&j)))=trim(left(upcase("&&ev&i._item&j..")))) %end;
                   %end;
                %end;
             and &var.=%eval(&k-&nbv) then expected=&&ev&i._item&k..;                                 
             %end;
          %end;
       if expected ne .;
       if expected>0 then psi=(pct_obs-expected)*(log(pct_obs/expected));       
       else psi=0;
       run;     
    %end;
%else %do;    
    data work.__allsc(drop=i);
    	%if %qupcase(&var_type)=%quote(N) %then %do; length &var 8; %end;
    	%else %do; length &var $ 8; %end;
        do i=1 to &nvg_items1;
    	   &var=i;
    	   output;
    	   end;
    	run;
    	
    proc sort data=work.__allsc;
    by &var;
    run;
    
    proc sort data=work.__psi_t1;
    by &var;
    run;
    
	proc sql;
	reset noprint;
	select sum(nobs) into :total_obs from work.__psi_t1
	;quit;
	
	data work.__psi_t3;
		merge work.__psi_t1 work.__allsc;
		by &var.;		
		pct_obs=100*(max(0,nobs)/&total_obs);
		pct_obs=round(pct_obs,.01);
		%do i=1 %to &nevgroups;
          %do k=%eval(&nbv+1) %to &&nvg_items&i;
             if &var.=%eval(&k-&nbv) then expected=&&ev&i._item&k..;                                 
             %end;
          %end;
       if expected>0 then psi=(pct_obs-expected)*(log(pct_obs/expected));       
       else psi=0;
	   run;
	%end;

proc sql;
select 
    %if &nbv>0 or %quote(&time_var) ne %quote(&null) %then %do;
       &sqlbylist_wt.,
       %end;
    sum(psi/100) as psi label="Population Stability Index (PSI)" format=8.4
from work.__psi_t3
%if &nbv>0 or %quote(&time_var) ne %quote(&null) %then %do;
   group by &sqlbylist_wt.
   %end;
%if &nbv>0 or %quote(&time_var) ne %quote(&null) %then %do;
   order by &sqlbylist_wt.
   %end;
;quit;  

proc sql;
drop table work.__allsc;
drop table work.__psi_t1; 
drop table work.__psi_t2;                   
drop table work.__psi_t3;   
;quit;
%mend;
