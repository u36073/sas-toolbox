%macro tm_xgboost(data=,
                  out=,
                  id_vars=,
                  pred_varname=,   
                  training_eval_test_data=, /* Not implemented yet. */                      
                  model_save_dir=,             
                  depvar=,                 
                  tempdir=,
                  xgboost_exe=D:\applications\xgboost-0.47\xgboost\build\Release\xgboost.exe,                  
                  booster=gbtree,
                  silent=,
                  nthread=,                
                  objective=reg:linear,
                  base_score=,
                  eta=,
                  gamma=,
                  lambda=,
                  lambda_bias=,
                  alpha=,                         
                  max_depth=,
                  min_child_weight=,
                  max_delta_step=,
                  subsample=,
                  colsample_bytree=,
                  colsample_bylevel=,
                  tree_method=,
                  sketch_eps=,
                  scale_pos_weight=,
                  sample_type=,
                  normalize_type=,
                  rate_drop=,
                  skip_drop=,
                  eval_metric=,
                  eval_train=1,
                  seed=,
                  use_buffer=,
                  num_round=,
                  save_period=,
                  task=train,
                  model_in=,        /* Must be name of file in the directory specified by model_save_dir */
                  fmap=,
                  name_dump=,                
                  pred_margin=
                  );

%macro delete_file(file);
data _null_;
    fname="___df";
    rc=filename(fname,"&file.");
    if rc = 0 and fexist(fname) then
       rc=fdelete(fname);
    rc=filename(fname);
run;
%mend;

options xsync noxwait;
%local null rn templib rwd i j k q max_xround;
%let null=;
%let max_xround=.;

/* Get Random Number to use in naming temp files and datasets */
data _null_;
   x=int(99998*ranuni(-1))+1;
   call symput('rn',strip(x));
   run;

/* Set temp directory to SAS Work directory if not specified in the macro call */
%if %quote(&tempdir.)=%quote(&null) %then %do;
    %let tempdir="%trim(%left(%sysfunc(getoption(work))))";
    %let templib=work;
    %end;
%else %do;
    libname w&rn. "&tempdir.";
    %let templib=w&rn.;
    %end;
    
%if %quote(&model_save_dir.)=%quote(&null.) %then %do;    
   %let model_save_dir=&tempdir.;
   %end;

libname m&rn. "&model_save_dir.";

%if %quote(&model_in.) ne %quote(&null.) %then %do;
   %let nimod=%tm_parselist(imod,&model_in.);
   %do i=1 %to &nimod.;
      %let imod_nround&i.=%scan(&&imod&i..,1,.);
      %let imod_nround&i.=%eval(&&imod_nround&i.. + 0);
      %end;
   %end;
%else %do;
   %let nimod=0;
   %end;
    
data &templib..xgbm&rn._train(drop=&id_vars.) 
     &templib..xgbm&rn._idvars(keep=_id&rn. &id_vars. &depvar.);
   set &data.;
   _id&rn.=_n_;
   run;


%tm_libsvm_export(data=&templib..xgbm&rn._train,
                  depvar=&depvar.,
                  keyvars=_id&rn.,
                  out_dir=&tempdir.,
                  out_prefix=xgbm&rn._train_,
                  out_spec_data=m&rn..libsvm_spec
                  );
                 
                  
proc export data=m&rn..libsvm_spec
            outfile="&model_save_dir.\libsvm_spec.csv" 
            dbms=csv replace;
putnames=YES;
run;               
                                                      
/* Derive the XGBOOST compatible path strings for the temp and model save directories. */
data _null_;
	length x $ 2000;
	x=translate("&tempdir.","/","\");
	call symput('xgb_tempdir',strip(x));
	x=translate("&model_save_dir.","/","\");
	call symput('xgb_model_save_dir',strip(x));	
	run;

%macro set_if_not_null(x);
%local null;
%let null=;
%if %quote(&&&x) ne %quote(&null.) %then %do;
   put "&x. = &&&x.";
   %end;
%mend;

%macro set_if_not_null_f(x);
%local null;
%let null=;
%if %quote(&&&x) ne %quote(&null.) %then %do;
   put "&x." ' = "' "&&&x." '"';
   %end;
%mend;

%macro create_conf(conf_fileref=,conf_task=,conf_model_in=);
%local null;
%let null=;

data _null_;
   file &conf_fileref.;
   %set_if_not_null(booster);
   %set_if_not_null(silent);
   %set_if_not_null(nthread);   
   %set_if_not_null(use_buffer);
   %set_if_not_null(num_round);
   %set_if_not_null(max_depth);
   put "task = &conf_task.";
   %set_if_not_null(save_period); 
   %set_if_not_null_f(fmap);
   %set_if_not_null_f(name_dump);   
   put 'data= "' "&xgb_tempdir./xgbm&rn._train_libvsm.txt" '"';
   put 'test:data= "' "&xgb_tempdir./xgbm&rn._train_libvsm.txt" '"';  
   put 'model_dir = "' "&xgb_model_save_dir." '"';   
   %if %qupcase(&task.)=%quote(PRED) %then %do;
      put 'name_pred ="' "&xgb_tempdir./xgb&rn._predictions.txt" '"';
      %end;
      
   %if %qupcase(&conf_model_in.) ne %quote(&null.) %then %do;
      put 'model_in ="' "&xgb_model_save_dir./&conf_model_in." '"';     
      %end;         

   %set_if_not_null_f(objective);
   %set_if_not_null(base_score);
   %set_if_not_null(eval_train);
   %set_if_not_null_f(eval_metric);
   %set_if_not_null(seed);   
   %set_if_not_null(eta);
   %set_if_not_null(alpha);   
   %set_if_not_null(gamma);   
   %set_if_not_null(lambda);
   %set_if_not_null(lambda_bias);    
   %set_if_not_null(min_child_weight);
   %set_if_not_null(max_delta_step);
   %set_if_not_null(subsample);
   %set_if_not_null(colsample_bytree);
   %set_if_not_null(colsample_bylevel); 
   %set_if_not_null_f(tree_method);
   %set_if_not_null(sketch_eps);
   %set_if_not_null(scale_pos_weight);
   %set_if_not_null_f(sample_type);
   %set_if_not_null_f(normalize_type);
   %set_if_not_null(rate_drop);
   %set_if_not_null(skip_drop); 
   run;
%mend;

%if %qupcase(&task.)=%quote(TRAIN) %then %do;
   filename ct&rn. "&model_save_dir.\xgboost_train.conf" lrecl=10000;
   %if &nimod.>0 %then %do;
      %create_conf(conf_fileref=ct&rn.,conf_task=train,conf_model_in=&imod1.);  
      %end;
   %else %do;
      %create_conf(conf_fileref=ct&rn.,conf_task=train,conf_model_in=);    
      %end;
   
   filename bf&rn. "&tempdir.\xgb&rn._batch.bat" lrecl=1024; 
   data _null_;   
      file bf&rn.;
      drive=substr(strip("&model_save_dir."),1,2);
      put drive;
      path=substr(strip("&model_save_dir."),3,length(strip("&model_save_dir."))-2);
      put 'cd "' path +(-1) '"';
      put '"' "&xgboost_exe." '"' " xgboost_train.conf > xgboost_train.log 2>&1";
      run;
   filename bf&rn. clear;   
   data _null_;
      call system("&tempdir.\xgb&rn._batch.bat");
      run;   
      
   %delete_file(&tempdir.\xgb&rn._batch.bat);   
         
   data _null_;     
      infile "&model_save_dir.\xgboost_train.log" lrecl=10000 end=last;
      if _n_=1 then do;
         put "******************************************************";
         put "**                                                  **";
         put "** XGBOOST Run Log                                  **";
         put "**                                                  **";  
         put "******************************************************";
         put;
         end;
      input @1 x $1. @;
      put _infile_;
      if last then do;
         put;
         put "******************************************************";
         put "**                                                  **";
         put "** End of XGBOOST Run Log                           **";
         put "**                                                  **";  
         put "******************************************************";
         end; 
      run;                       
   %end;   
%else %if %qupcase(&task.)=%quote(PRED) %then %do;
   %do i=1 %to &nimod.;
      filename ct&rn. "&model_save_dir.\xgboost_score_&&imod&i...conf" lrecl=10000;
      %create_conf(conf_fileref=ct&rn.,conf_task=pred,conf_model_in=&&imod&i..);    
      
      filename bf&rn. "&tempdir.\xgb&rn._batch.bat" lrecl=1024; 
      data _null_;   
         file bf&rn.;
         drive=substr(strip("&model_save_dir."),1,2);
         put drive;
         path=substr(strip("&model_save_dir."),3,length(strip("&model_save_dir."))-2);
         put 'cd "' path +(-1) '"';
         put '"' "&xgboost_exe." '"' " xgboost_score_&&imod&i...conf > xgboost_score_&&imod&i...log 2>&1";
         run;
      filename bf&rn. clear;   
      data _null_;
         call system("&tempdir.\xgb&rn._batch.bat");
         run;   
         
      %delete_file(&tempdir.\xgb&rn._batch.bat);     
               
      data _null_;     
         infile "&model_save_dir.\xgboost_score_&&imod&i...log" lrecl=10000 end=last;
         if _n_=1 then do;
            put "******************************************************";
            put "**                                                  **";
            put "** XGBOOST Run Log                                  **";
            put "**                                                  **";  
            put "******************************************************";
            put;
            end;
         input @1 x $1. @;
         put _infile_;
         if last then do;
            put;
            put "******************************************************";
            put "**                                                  **";
            put "** End of XGBOOST Run Log                           **";
            put "**                                                  **";  
            put "******************************************************";
            end; 
         run;
            
      data &templib..xgbm&rn._pred&i.;
         length _id&rn. &pred_varname._nr&&imod_nround&i.. 8;
         infile "&tempdir./xgb&rn._predictions.txt" dsd delimiter=',' missover;
         input &pred_varname._nr&&imod_nround&i.. @;
         _id&rn=_n_;
         output;
         run;         
      %delete_file(&tempdir.\xgb&rn._predictions.txt);   
      %end;
   
   proc sql;
      create table &out(drop=_id&rn.) as
      select a.*
             %do i=1 %to &nimod.;     
               ,x&i..&pred_varname._nr&&imod_nround&i..
               %end;   
      from &templib..xgbm&rn._idvars a
      %do i=1 %to &nimod.;
         join &templib..xgbm&rn._pred&i. x&i.
           on a._id&rn.=x&i.._id&rn.
         %end;
      ;quit;   
      
   proc sql;
   %do i=1 %to &nimod.; 
      drop table &templib..xgbm&rn._pred&i.;
      %end;
   quit;
   %end;     

proc sql;
drop table &templib..xgbm&rn._idvars;
drop table &templib..xgbm&rn._train;
drop table &templib..xgbm&rn._train_libsvm_cmap;
drop table &templib..xgbm&rn._train_libsvm_kmap;
quit;

%delete_file(&tempdir.\xgbm&rn._train_libvsm.txt);

%if %qupcase(&templib.) ne %quote(WORK) %then %do;
   libname &templib. clear;
   %end;

libname m&rn. clear;     
filename ct&rn. clear;                                            
%mend;                 
                  
                  
                  
                  
                  