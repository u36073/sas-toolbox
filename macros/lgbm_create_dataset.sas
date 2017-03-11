%macro lgbm_create_dataset(train_data=,
                           valid_data=,
                           label=,
                           templib=work,
                           lgbm_executable=D:\applications\LightGBM-2017-03-01\windows\x64\Release\lightgbm.exe,
                           );


options xsync noxwait;
%local null rn tempdir rwd i j k q num_vars out_csv library;
%let null=;
%let tempdir=%sysfunc(pathname(&templib.));

%macro delete_file(file);
data _null_;
    fname="___df";
    rc=filename(fname,"&file.");
    if rc = 0 and fexist(fname) then
       rc=fdelete(fname);
    rc=filename(fname);
run;
%mend;

%macro check_dir(dir) ;
%local rc fileref ;
%let rc = %sysfunc(filename(fileref,&dir)) ;
%if %sysfunc(fexist(&fileref)) %then %do;
   %put NOTE: The directory "&dir" exists;
   %end;
%else %do;
   %sysexec md "&dir.";
   %put %sysfunc(sysmsg()) The directory has been created. ;
   %end;
%let rc=%sysfunc(filename(fileref));
%mend;

%if %quote(&train_data.) ne %quote(&null.) %then %do;
   %let train_lib=%scan(&train_data.,1,%str(.%());    
   %if %quote(%scan(&train_data.,1,%str(%()))=%quote(&train_lib.) %then %do;
      %let train_lib=work;
      %let train_dsn=%scan(&train_data.,1,%str(%());
      %end;
   %else %do;
      %let train_dsn=%scan(&train_data.,2,%str(.%());
      %end;
   %let train_path=%sysfunc(pathname(&train_lib.));
   %let train_csv=&train_path.\&train_dsn..train.lgbm.&label..csv;
   %end;

%if %quote(&valid_data.) ne %quote(&null.) %then %do;
   %let valid_lib=%scan(&valid_data.,1,%str(.%());    
   %if %quote(%scan(&valid_data.,1,%str(%()))=%quote(&valid_lib.) %then %do;
      %let valid_lib=work;
      %let valid_dsn=%scan(&valid_data.,1,%str(%());
      %end;
   %else %do;
      %let valid_dsn=%scan(&valid_data.,2,%str(.%());
      %end;
   %let valid_path=%sysfunc(pathname(&valid_lib.));
   %let valid_csv=&valid_path.\&valid_dsn..valid.lgbm.&label..csv;
   %end;

/* Get Random Number to use in naming temp files and datasets */
data _null_;
   x=int(99998*ranuni(-1))+1;
   call symput('rn',strip(x));
   run;

filename ct&rn. "&train_csv." lrecl=32000;
filename cv&rn. "&valid_csv." lrecl=32000;
filename p&rn. "&tempdir.\lgbm_conf_&rn..conf" lrecl=10000;

proc contents data=&train_data. out=&templib.._contents noprint;
run;

proc sql;
reset noprint;
select count(*) into :num_vars from &templib.._contents where type=1 and strip(upcase(name)) ne strip(upcase("&label."));
quit;

%do i=1 %to &num_vars;
   %local var&i;
   %end;

data _null_;
   set &templib.._contents;
   retain k 0;
   if type=1 and strip(upcase(name)) ne strip(upcase("&label.")) then do;
      k=k+1;
      call symput(cats('var',k),strip(lowcase(name)));
      end;
   run;

data _null_;
   set &train_data;
   file ct&rn.;
   length __pv $ 32;
   if _n_=1 then do;
      put "&label.," @;
      %do i=1 %to &num_vars;
         %if &i ne &num_vars %then %do;
            put "&&var&i..," @;
            %end;
         %else %do;
            put "&&var&i..";
            %end;
         %end;
      end;
      
   if &label.=. then __pv='';
   else __pv=strip(put(&label.,best32.9));
   put __pv +(-1) @;
   
   %do i=1 %to &num_vars.;
      if &&var&i..=. then __pv='';
      else __pv=strip(put(&&var&i..,best32.9));
      put "," __pv +(-1) @;
      %end;
   put;
   run;
   

data _null_;
   set &valid_data;
   file cv&rn.;
   length __pv $ 32;   
   if _n_=1 then do;
      put "&label.," @;
      %do i=1 %to &num_vars;
         %if &i ne &num_vars %then %do;
            put "&&var&i..," @;
            %end;
         %else %do;
            put "&&var&i..";
            %end;
         %end;
      end;
      
   if &label.=. then __pv='';
   else __pv=strip(put(&label.,best32.9));
   put __pv +(-1) @;
   
   %do i=1 %to &num_vars.;
      if &&var&i..=. then __pv='';
      else __pv=strip(put(&&var&i..,best32.9));
      put "," __pv +(-1) @;
      %end;
   put;
   run;   

data _null_;
   file p&rn.;
   put "task=train";
   put "boosting_type=gbdt";
   put "objective=regression";
   put "header=true";
   put "train=&train_csv.";
   put "test=&valid_csv.";   
   put "save_binary=true";
   put "label=name:&label.";
   put "num_iterations=1";
   put "metric=mse";
   put "metric_freq=1";
   put "is_training_metric=true";
   put "early_stopping_round=5";
   put "bin_construct_sample_cnt=500000";   
   run;

filename b&rn. "&tempdir.\lgbm&rn._batch.bat" lrecl=1024;
data _null_;
   file b&rn.;
   drive=substr(strip("&tempdir."),1,2);
   put drive;
   path=substr(strip("&tempdir."),3,length(strip("&tempdir."))-2);
   put 'cd "' path +(-1) '"';
   put '"' "&lgbm_executable." '"' " config=lgbm_conf_&rn..conf";
   run;

filename b&rn. clear;
data _null_;
   length cmd $ 10000;
   cmd=strip(cats('"',"&tempdir.\lgbm&rn._batch.bat",'"'));
   call system(cmd);
   run;

filename ct&rn. clear;
filename cv&rn. clear;
filename p&rn. clear;
%delete_file(&tempdir.\lgbm&rn._batch.bat);
%delete_file(&tempdir.\lgbm_conf_&rn..conf);

proc sql; 
reset noprint;
drop table &templib.._contents;
quit;

%mend;
