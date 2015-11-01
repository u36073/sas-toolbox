%macro tm_PerfChart(data=,
                    by=,
                    group_var=,
                    group_var_desc=,
                    group_var_format=,
                    xvar=,
                    xvar_desc=,
                    xvar_format=,
                    xvar_rank=N,
                    xvar_rank_order=A,
                    xvar_rank_groups=10,
                    yvar=,
                    yvar_function=mean,
                    yvar_format=,
                    yvar_desc=,
                    ods_style_template=styles.printer,
                    plot_area_border=N,
                    sgplot_options=uniform=all,
                    graph_frame=N,
                    graph_width=800,
                    graph_height=600,
                    plot_options=markers
                                 groupdisplay=overlay
                                 grouporder=ascending
                                 ,
                    xaxis_label=,
                    xaxis_options= type=DISCRETE
                                   fitpolicy=staggerrotate
                                   ,
                    yaxis_label=,
                    yaxis_options=grid,
                    histogram=Y,
                    histogram_xaxis_label=,
                    histogram_xaxis_options=,
                    histogram_yaxis_label=,
                    histogram_yaxis_options=,
                    histogram_plot_options=missing
                                           stat=sum
                                           response=nobs
                                           groupdisplay=cluster
                                           grouporder=ascending
                                           datalabel=pct_obs
                                           datalabelpos=bottom
                                           dataskin=pressed
                    );

%local null i j k mdnyvars application_name rename pc
       version order keeplist bylist sqlbylist inbylist
       ;
%let null=;
%let mdnyvars=5;
%let application_name=tm_PerfChart;
%let version=1.0;
%let keeplist=&xvar &yvar &by &group_var;

%goto ErrorHandling;
%NoErrors:

%if %quote(&by) ne %quote(&null) %then %do;
    %let nbv=%tm_ParseList(byvar,&by);
    %do i=1 %to &nbv;
        %local bylab&i;
        %let bylab&i=%tm_VarLabel(&&byvar&i,&data);
        %if %quote(&&bylab&i)=%quote(&null) %then %let bylab&i=&&byvar&i;

        %if %tm_VarFormat(&&byvar&i..,&data) ne %quote(&null) %then %let byfmt&i=%tm_VarFormat(&&byvar&i..,&data);
        %else %do;
        %if %tm_vartype(&&byvar&i,&data)=N %then %let byfmt&i=best18.9;
        %else %let byfmt&i=;
        %end;
        %end;
    %end;
%else %let nbv=0;

%if %quote(&xvar_desc)=%quote(&null) %then %do;
    %let xvar_desc=%tm_VarLabel(&xvar,&data);
    %if %quote(&xvar_desc)=%quote(&null) %then %do;
        %let xvar_desc=&xvar;
        %end;
    %end;

%if %quote(&yvar_desc)=%quote(&null) %then %do;
    %let yvar_desc=%tm_VarLabel(&yvar,&data);
    %if %quote(&yvar_desc)=%quote(&null) %then %do;
        %let yvar_desc=&yvar;
        %end;
    %end;

%if %quote(&group_var_desc)=%quote(&null) %then %do;
    %let group_var_desc=%tm_VarLabel(&group_var,&data);
    %if %quote(&group_var_desc)=%quote(&null) %then %do;
        %let group_var_desc=&group_var;
        %end;
    %end;

%if %quote(&by) ne %quote(&null) or %quote(&group_var) ne %quote(&null) %then %do;
    %let bylist=&by &group_var;
    %let sqlbylist=%tm_ListFormat(&bylist,comma),;
    %let sqlbylist_nc=%tm_ListFormat(&bylist,comma);
    %let inbylist=%tm_ListFormat(&bylist,qcomma);
    %end;
%else %do;
    %let bylist=;
    %let sqlbylist=;
    %let inbylist=;
    %end;

%if %quote(&xvar_format) ne %quote(&null) %then %let xvar_format=&xvar_format;
%else %if %tm_VarFormat(&xvar,&data) ne %quote(&null) %then %let xvar_format=%tm_VarFormat(&xvar,&data);
%else %do;
    %if %tm_vartype(&xvar,&data)=N %then %let xvar_format=best18.9;
    %else %let xvar_format=;
    %end;

%if %quote(&yvar_format) ne %quote(&null) %then %let yvar_format=&yvar_format;
%else %if %tm_VarFormat(&yvar,&data) ne %quote(&null) %then %let yvar_format=%tm_VarFormat(&yvar,&data);
%else %let yvar_format=best18.9;

%if %quote(&group_var) ne %quote(&null) %then %do;
   %if %quote(&group_var_format) ne %quote(&null) %then %let group_var_format=&group_var_format;
   %else %if %tm_VarFormat(&group_var,&data) ne %quote(&null) %then %let group_var_format=%tm_VarFormat(&group_var,&data);
   %else %do;
       %if %tm_vartype(&group_var,&data)=N %then %let xvar_format=best18.9;
       %else %let group_var_format=;
       %end;
   %end;

%if %qupcase(&yvar_function)=%quote(MEAN)   %then %let yvar_func_desc=Mean;
%if %qupcase(&yvar_function)=%quote(COUNT)  %then %let yvar_func_desc=# of non-missing values;
%if %qupcase(&yvar_function)=%quote(CSS)    %then %let yvar_func_desc=Corrected sum of squares;
%if %qupcase(&yvar_function)=%quote(CV)     %then %let yvar_func_desc=Coefficient of variation;
%if %qupcase(&yvar_function)=%quote(MAX)    %then %let yvar_func_desc=Maximum;
%if %qupcase(&yvar_function)=%quote(MIN)    %then %let yvar_func_desc=Minimum;
%if %qupcase(&yvar_function)=%quote(NMISS)  %then %let yvar_func_desc=# of missing values;
%if %qupcase(&yvar_function)=%quote(PRT)    %then %let yvar_func_desc=Two tailed p-value for Students T;
%if %qupcase(&yvar_function)=%quote(RANGE)  %then %let yvar_func_desc=Range;
%if %qupcase(&yvar_function)=%quote(STD)    %then %let yvar_func_desc=Standard Deviation;
%if %qupcase(&yvar_function)=%quote(STDERR) %then %let yvar_func_desc=Standard Error;
%if %qupcase(&yvar_function)=%quote(SUM)    %then %let yvar_func_desc=Sum;
%if %qupcase(&yvar_function)=%quote(T)      %then %let yvar_func_desc=Students T value;
%if %qupcase(&yvar_function)=%quote(USS)    %then %let yvar_func_desc=Uncorrected sum of squares;
%if %qupcase(&yvar_function)=%quote(VAR)    %then %let yvar_func_desc=Variance;

%if %quote(&xvar_rank)=%quote(Y) %then %do;
    proc sql;
    create view work.__tmpc_view as
    select * from &data
    %if %quote(&by) ne %quote(&null) or %quote(&group_var) ne %quote(&null) %then %do;
        order by &sqlbylist_nc
        %end;
    ;quit;

    %if %qupcase(&xvar_rank_order)=%quote(N) %then %let order=descending;
    %else %let order=;

    proc rank data=work.__tmpc_view(keep=&keeplist)
              out=work.__tmpc1 groups=&xvar_rank_groups ties=low &order;
    %if %quote(&by) ne %quote(&null) or %quote(&group_var) ne %quote(&null) %then %do;
        by &by &group_var;
        %end;
    var &xvar;
    ranks __rank;
    run;

    %let pc=;
    proc sql;
    create table work.__tmpc2 as
    select %if &nbv>0 %then %do;
               %do i=1 %to &nbv;
                  %if %tm_vartype(&&byvar&i..,&data)=N %then %do;
                     &pc.(&&byvar&i..+0) as &&byvar&i.. label="&&bylab&i"
                     %end;
                  %else %do;
                     &pc.trim(&&byvar&i..) as &&byvar&i.. label="&&bylab&i"
                     %end;
                  %let pc=,;
                  %end;
              %end;
           %if %quote(&group_var) ne %quote(&null) %then %do;
              %if %tm_vartype(&group_var,&data)=N %then %do;
                 &pc.(&group_var.+0) as &group_var label="&group_var_desc"
                 %end;
              %else %do;
                 &pc.trim(&group_var) as &group_var label="&group_var_desc"
                 %end;
              %let pc=,;
              %end;
           &pc.(__rank+1) as &xvar label="&xvar_desc",
           &yvar_function(___yvar) as &yvar label="&yvar_desc",
           count(*) as nobs label="# of Observations"
    from work.__tmpc1(drop=&xvar rename=(&yvar=___yvar))
    group by &sqlbylist.__rank
    order by &xvar
    ;quit;

    %end;
%else %do;
    %let pc=;
    proc sql;
    create table work.__tmpc2 as
    select %if &nbv>0 %then %do;
               %do i=1 %to &nbv;
                  %if %tm_vartype(&&byvar&i..,&data)=N %then %do;
                     &pc.(&&byvar&i..+0) as &&byvar&i.. label="&&bylab&i"
                     %end;
                  %else %do;
                     &pc.trim(&&byvar&i..) as &&byvar&i.. label="&&bylab&i"
                     %end;
                  %let pc=,;
                  %end;
              %end;
           %if %quote(&group_var) ne %quote(&null) %then %do;
              %if %tm_vartype(&group_var,&data)=N %then %do;
                 &pc.(&group_var.+0) as &group_var label="&group_var_desc"
                 %end;
              %else %do;
                 &pc.trim(&group_var) as &group_var label="&group_var_desc"
                 %end;
              %let pc=,;
              %end;
           %if %tm_vartype(&xvar,&data)=N %then %do;
              &pc.(___xvar+0) as &xvar label="&xvar_desc"
              %end;
           %else %do;
              &pc.trim(___xvar) as &xvar label="&xvar_desc"
              %end;
           ,&yvar_function(___yvar) as &yvar label="&yvar_desc"
           ,count(*) as nobs label="# of Observations"
    from &data(rename=(&yvar=___yvar &xvar=___xvar))
    group by &sqlbylist.___xvar
    order by &xvar
    ;quit;
    %end;

%let rename=(rename=(&yvar=__yvar nobs=__nobs;

%if &nbv>0 %then %do;
    %do i=1 %to &nbv;
        %let rename=&rename. &&byvar&i=__bv&i;
        %end;
    %end;
%if %quote(&group_var) ne %quote(&null) %then %let rename=&rename. &group_var=__gvar;

%let rename=&rename.));


data work.__tmpc2b(drop=__:);
    set work.__tmpc2&rename.;
    %if &nbv>0 %then %do;
        %do i=1 %to &nbv;
          length &&byvar&i $ 32;
          %if %quote(&&byfmt&i)=%quote(&null) %then %do;
            &&byvar&i=trim(left(__bv&i));
            %end;
          %else %do;
              &&byvar&i=trim(left(put(__bv&i,&&byfmt&i..)));
              %end;
            %end;
        %end;

   %if %quote(&group_var) ne %quote(&null) %then %do;
      length &group_var $ 32;
      %if %quote(&group_var_format) ne %quote(&null) %then %do;
         &group_var=trim(left(put(__gvar,&group_var_format)));
         %end;
      %else %do;
         &group_var=trim(left(__gvar));
         %end;
      %end;

  length &yvar $ 32;
  &yvar=trim(left(put(__yvar,&yvar_format)));

  length nobs $ 32;
  nobs=put(__nobs,comma10.);

  run;

proc sort data=work.__tmpc2b;
by &xvar &bylist;
run;

proc transpose data=work.__tmpc2b out=work.__tmpc3;
by &xvar;
var &bylist &yvar nobs;
run;

data work.__tmpc4;
    set work.__tmpc3;

    %if %quote(&by) ne %quote(&null) or %quote(&group_var) ne %quote(&null) %then %do;
        if (&xvar=1 and trim(left(upcase(_name_))) in (%upcase(&inbylist)))
           or trim(left(upcase(_name_))) in ("%qupcase(&&yvar)","NOBS");

        %if &nbv>0 %then %do;
            %do i=1 %to &nbv;
                if trim(left(upcase(_name_)))="%qupcase(&&byvar&i..)" then do;
                    _order_=&i-100;
                    end;
                %end;
            %end;

        if trim(left(upcase(_name_)))="%qupcase(&group_var)" then _order_=-10;
        %end;
    %else %do;
        if trim(left(upcase(_name_))) in ("%qupcase(&&yvar)","NOBS");
        %end;

    if trim(left(upcase(_name_)))="%qupcase(&&yvar)" then _order_=&xvar;
    if trim(left(upcase(_name_)))="NOBS" then _order_=500+&xvar;
    run;

proc contents data=work.__tmpc4(keep=col:) out=__contents noprint;
run;

proc sql;
reset noprint;
select max(input(substr(name,4,length(name)-3),3.)) into :nbg from __contents
;quit;

data work.__tmpc5;
    set work.__tmpc4 end=last;
    length variable $ 32 description $ 200 type $ 20 __clab1-__clab%trim(%left(&nbg)) $ 256;
    length xcol1-xcol%trim(%left(&nbg)) $ 512;
    retain __clab1-__clab%trim(%left(&nbg));
    label variable="Variable or Expression";
    label description="Description";
    label type="Record Type";
    %do i=1 %to &nbg;
        label xcol&i.="Group &i";
        %end;
    %if &nbv>0 %then %do;
        %do i=1 %to &nbv;
            if trim(left(upcase(_name_)))="%upcase(&&byvar&i)" then do;
                variable="&&byvar&i..";
                description="&&bylab&i";
                type="Group";
                %do k=1 %to &nbg;
                  %if &i=1 %then %do;
                 __clab&k="[&&bylab&i..]="||trim(left(col&k));
                   %end;
                %else %do;
                   __clab&k=trim(left(__clab&k))||" [&&bylab&i..]="||trim(left(col&k));
                   %end;
              %end;
                end;
            %end;
        %end;
    %if %quote(&group_var) ne %quote(&null) %then %do;
        if trim(left(upcase(_name_)))="%upcase(&group_var)" then do;
            variable="&group_var";
            description="&group_var_desc";
            type="Group";
            %do k=1 %to &nbg;
              %if &nbv=0 %then %do;
                __clab&k="[&group_var_desc]="||trim(left(col&k));
                %end;
            %else %do;
                __clab&k=trim(left(__clab&k))||" [&group_var_desc]="||trim(left(col&k));
                %end;
            %end;
            end;
        %end;

    if trim(left(upcase(_name_)))="%upcase(&yvar)" then do;
        variable="&yvar_function.(&yvar)";
        description="&yvar_desc (&yvar_func_desc.)";
        type="Statistic";
        end;

    if trim(left(upcase(_name_)))="NOBS" then do;
        variable="count(*)";
        description="# of Records";
        type="Measure";
        end;
    %do i=1 %to &nbg;
      xcol&i=col&i;
      %end;
    output;
    if last then do;
      %if &nbg>1 %then %do;
        type='Header';
        variable='';
        description='';
        %if %tm_vartype(&xvar,&data)=N %then %do;
          &xvar=.;
          %end;
        %else %do;
            &xvar='';
            %end;
        %do k=1 %to &nbg;
          xcol&k=trim(left(__clab&k));
          %end;
        _order_=-9999;
        output;
        %end;
      end;
    run;

proc sort data=work.__tmpc5;
by _order_;
run;

title3 "&yvar_func_desc.(&yvar_desc) by &xvar_desc";
proc print data=work.__tmpc5 noobs label;
where variable ne "count(*)";
var type description variable &xvar xcol:;
run;

title3 "Record Counts by &xvar_desc";
proc print data=work.__tmpc5 noobs label;
where variable ne "&yvar_function.(&yvar.)";
var type description variable &xvar xcol:;
run;

proc template;
   define style mystyle;
   parent=&ods_style_template;
      class graphwalls /
            %if %qupcase(&plot_area_border)=%quote(Y) %then %do;
              frameborder=on;
              %end;
            %else %do;
              frameborder=off;
              %end;
      class graphbackground /
            color=white;
   end;
run;

ods html style=mystyle;
ods graphics / reset=all width=&graph_width.px height=&graph_height.px
               %if %qupcase(&graph_frame)=%quote(Y) %then %do;
                  border=on
                  %end;
               %else %do;
                  border=off
                  %end;
               ;

%if %quote(&by) ne %quote(&null) %then %do;
  proc sort data=work.__tmpc2;
  by &by;
  run;
  %end;

%if %quote(&xaxis_label)=%quote(&null) %then %let xaxis_label=&xvar_desc;
%if %quote(&yaxis_label)=%quote(&null) %then %let yaxis_label=&yvar_desc (&yvar_func_desc);

title3 "&yvar_func_desc.(&yvar_desc) by &xvar_desc";
proc sgplot data=work.__tmpc2 &sgplot_options;
%if %quote(&by) ne %quote(&null) %then %do;
  by &by;
  *title4 "#BYLINE";
  %end;
%if %quote(&xvar_format) ne %quote(&null) %then %do;
  format &xvar &xvar_format;
  %end;
%if %quote(&yvar_format) ne %quote(&null) %then %do;
  format &yvar &yvar_format;
  %end;
%if %quote(&group_var) ne %quote(&null) and %quote(&group_var_format) ne %quote(&null) %then %do;
  format &group_var &group_var_format;
    %end;
series x=&xvar y=&yvar / %if %quote(&group_var) ne %quote(&null) %then %do;
                      group=&group_var
                      %end;
                   &plot_options
                   ;
xaxis label="&xaxis_label" &xaxis_options;
yaxis label="&yaxis_label" &yaxis_options;
run;

title3;
title4;

%if %quote(&histogram)=%quote(Y) %then %do;
  %if &nbv>0 or %quote(&group_var) ne %quote(&null) %then %do;
      %if %quote(&group_var) ne %quote(&null) %then %let last_by_var=&group_var;
      %else %let last_by_var=&&byvar&nbv;

      proc sort data=work.__tmpc2;
      by &bylist;
      run;

      data work.__tmpc2_hist1(keep=&bylist total_obs);
        set work.__tmpc2;
        by &bylist;
        retain total_obs 0;
        if first.&last_by_var then do;
          total_obs=0;
          end;
        total_obs=total_obs+nobs;
        if last.&last_by_var then output;
        else delete;
        run;

      proc sort data=work.__tmpc2_hist1;
      by &bylist;
      run;

      data work.__tmpc2_hist2;
        merge work.__tmpc2 work.__tmpc2_hist1;
        by &bylist;
        pct_obs=nobs/total_obs;
        run;
      %end;
    %else %do;
      proc sql;
        reset noprint;
        select sum(nobs) into :total_obs from work.__tmpc2
        ;quit;

      data work.__tmpc2_hist2;
        set work.__tmpc2;
        pct_obs=nobs/&total_obs;
        run;
      %end;

  %if %quote(&histogram_xaxis_label)=%quote(&null)
  %then %let histogram_xaxis_label=&xaxis_label;

  %if %quote(&histogram_yaxis_label)=%quote(&null)
  %then %let histogram_yaxis_label=Observations;

  title3 "&xvar_desc Distribution";
  %if %quote(&by) ne %quote(&null) %then %do;
     *title4 "#BYLINE";
     %end;
  proc sgplot data=work.__tmpc2_hist2 &sgplot_options;
    %if %quote(&by) ne %quote(&null) %then %do;
     by &by;
     %end;
  format nobs comma10.;
  format pct_obs percent7.1;
    %if %quote(&xvar_format) ne %quote(&null) %then %do;
      format &xvar &xvar_format;
      %end;
    %if %quote(&group_var) ne %quote(&null) and %quote(&group_var_format) ne %quote(&null) %then %do;
      format &group_var &group_var_format;
        %end;
  vbar &xvar / %if %quote(&group_var) ne %quote(&null) %then %do;
              group=&group_var
              %end;
          &histogram_plot_options
          ;
    xaxis label="&histogram_xaxis_label" &histogram_xaxis_options;
    yaxis label="&histogram_yaxis_label" &histogram_yaxis_options;
    run;
    
    title3;
    title4;
    
    %if %quote(&group_var)=%quote(&null) %then %do;
      title3 "&yvar_func_desc.(&yvar_desc) by &xvar_desc";
      proc sgplot data=work.__tmpc2_hist2 &sgplot_options;
        %if %quote(&by) ne %quote(&null) %then %do;
         by &by;
         *title4 "#BYLINE";
         %end;
      %else %do;
         title4;
         %end;
      format nobs comma10.;
      format pct_obs percent7.1;
        %if %quote(&xvar_format) ne %quote(&null) %then %do;
          format &xvar &xvar_format;
          %end;
        %if %quote(&yvar_format) ne %quote(&null) %then %do;
      format &yvar &yvar_format;
      %end;
      vbar &xvar / &histogram_plot_options.;
      vline &xvar / response=&yvar stat=sum y2axis &plot_options;
        xaxis label="&histogram_xaxis_label" &histogram_xaxis_options;        
        yaxis label="&histogram_yaxis_label" &histogram_yaxis_options;
        y2axis label="&yaxis_label" &yaxis_options;
        run;
        
        title3;
        title4;
      %end;
  %end;

%goto Exit;
%ErrorHandling:
%let errflag=0;

%if %quote(&data)=%quote(&null) %then %do;
   %let errflag=1;
   %tme_ErrorMessage(AppName=&application_name &version,
                     module=N/A,
                     code=&errflag,
                     parameter=DATA,
                     msg1=No value entered,
                     msg2=Valid entry is an existing SAS dataset,
                     msg3=
                    );
   %goto Exit;
   %end;


%let errflag=%tme_ValidDataset(type=EXISTS,name=&data,AppName=&application_name &version,module=N/A,code=2,parameter=DATA);
%if %quote(&errflag) ne %quote(0) %then %goto Exit;

%let errflag=%tme_ValidVariable(type=A,name=&xvar,data=&data,testlength=N,AppName=&application_name &version,module=N/A,code=3,parameter=XVAR);
%if %quote(&errflag) ne %quote(0) %then %goto Exit;

%if %qupcase(&xvar_rank) ne %quote(Y) and %qupcase(&xvar_rank) ne %quote(N) %then %do;
   %let errflag=4;
   %tme_ErrorMessage(AppName=&application_name &version,
                     module=N/A,
                     code=&errflag,
                     parameter=XVAR_RANK,
                     msg1=Invalid Entry,
                     msg2=Valid values are Y or N,
                     msg3=
                    );
   %goto Exit;
   %end;

%if %qupcase(&xvar_rank)=%quote(Y) %then %do;
   %if %qupcase(&xvar_rank_order) ne %quote(A) and %qupcase(&xvar_rank) ne %quote(D) %then %do;
      %let errflag=5;
      %tme_ErrorMessage(AppName=&application_name &version,
                        module=N/A,
                        code=&errflag,
                        parameter=XVAR_RANK_ORDER,
                        msg1=Invalid Entry,
                        msg2=Valid values are A or D,
                        msg3=
                       );
      %goto Exit;
      %end;

   %if %tm_IsNumber(&xvar_rank_groups)=0 %then %do;
      %let errflag=6A;
      %tme_ErrorMessage(AppName=&application_name &version,
                        module=N/A,
                        code=&errflag,
                        parameter=XVAR_RANK_GROUPS,
                        msg1=Entry is not a valid number,
                        msg2=Valid entry is an integer between 1 and 100,
                        msg3=
                       );
      %goto Exit;
      %end;

   %if %index(&xvar_rank_groups,%str(.))>0 %then %do;
      %let errflag=6B;
      %tme_ErrorMessage(AppName=&application_name &version,
                        module=N/A,
                        code=&errflag,
                        parameter=XVAR_RANK_GROUPS,
                        msg1=Entry is not an integer,
                        msg2=Valid entry is an integer between 1 and 100,
                        msg3=
                       );
      %goto Exit;
      %end;

   %if &xvar_rank_groups<0 or &xvar_rank_groups>100 %then %do;
      %let errflag=6C;
      %tme_ErrorMessage(AppName=&application_name &version,
                        module=N/A,
                        code=&errflag,
                        parameter=XVAR_RANK_GROUPS,
                        msg1=Entry is out of range,
                        msg2=Valid entry is an integer between 1 and 100,
                        msg3=
                       );
      %goto Exit;
      %end;
   %end;

%let errflag=%tme_ValidVariable(type=N,name=&yvar,data=&data,testlength=N,
                                AppName=&application_name &version,module=N/A,code=7,parameter=YVAR);
%if %quote(&errflag) ne %quote(0) %then %goto Exit;

%if %quote(&by) ne %quote(&null) %then %do;
    %tme_ValidVariableList(type=A,
                           vars=&by,
                           data=&data,
                           testlength=N,
                           maxlength=32,
                           rcMacroVar=errflag,
                           module=&application_name &version,
                           code=11,
                           parameter=BY
                           );
    %if %quote(&errflag) ne %quote(0) %then %goto Exit;
    %end;

%if %quote(&group_var) ne %quote(&null) %then %do;
  %let errflag=%tme_ValidVariable(type=A,name=&group_var,data=&data,testlength=N,
                                  AppName=&application_name &version,module=N/A,code=12,parameter=group_var);
  %if %quote(&errflag) ne %quote(0) %then %goto Exit;
  %end;


%if %qupcase(&yvar_function) ne %quote(MEAN) and
    %qupcase(&yvar_function) ne %quote(COUNT) and
    %qupcase(&yvar_function) ne %quote(CSS) and
    %qupcase(&yvar_function) ne %quote(CV) and
    %qupcase(&yvar_function) ne %quote(MAX) and
    %qupcase(&yvar_function) ne %quote(MIN) and
    %qupcase(&yvar_function) ne %quote(NMISS) and
    %qupcase(&yvar_function) ne %quote(PRT) and
    %qupcase(&yvar_function) ne %quote(RANGE) and
    %qupcase(&yvar_function) ne %quote(STD) and
    %qupcase(&yvar_function) ne %quote(STDERR) and
    %qupcase(&yvar_function) ne %quote(SUM) and
    %qupcase(&yvar_function) ne %quote(T) and
    %qupcase(&yvar_function) ne %quote(USS) and
    %qupcase(&yvar_function) ne %quote(VAR)
%then %do;
  %let errflag=13;
  %tme_ErrorMessage(AppName=&application_name &version,
                    module=N/A,
                    code=&errflag,
                    parameter=YVAR_FUNCTION,
                    msg1=Entry is invalid.,
                    msg2=Valid entries are MEAN COUNT CSS CV MAX MIN NMISS PRT RANGE STD STDERR SUM T USS VAR,
                    msg3=
                   );
  %goto Exit;
  %end;


%if %qupcase(&histogram) ne %quote(Y) and %qupcase(&histogram) ne %quote(N) %then %do;
   %let errflag=14;
   %tme_ErrorMessage(AppName=&application_name &version,
                     module=N/A,
                     code=&errflag,
                     parameter=HISTOGRAM,
                     msg1=Invalid Entry,
                     msg2=Valid values are Y or N,
                     msg3=
                    );
   %goto Exit;
   %end;

%if %qupcase(&plot_area_border) ne %quote(Y) and %qupcase(&plot_area_border) ne %quote(N) %then %do;
   %let errflag=15;
   %tme_ErrorMessage(AppName=&application_name &version,
                     module=N/A,
                     code=&errflag,
                     parameter=PLOT_AREA_BORDER,
                     msg1=Invalid Entry,
                     msg2=Valid values are Y or N,
                     msg3=
                    );
   %goto Exit;
   %end;

%if %qupcase(&graph_frame) ne %quote(Y) and %qupcase(&graph_frame) ne %quote(N) %then %do;
   %let errflag=15;
   %tme_ErrorMessage(AppName=&application_name &version,
                     module=N/A,
                     code=&errflag,
                     parameter=GRAPH_FRAME,
                     msg1=Invalid Entry,
                     msg2=Valid values are Y or N,
                     msg3=
                    );
   %goto Exit;
   %end;

%if %quote(&errflag)=%quote(0) %then %goto NoErrors;

%Exit:
%mend;




