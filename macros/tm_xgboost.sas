%macro tm_xgboost(data=,
                  out=,
                  out_keep_vars=,
                  pred_varname=,   
                  training_eval_test_data=, /* Not implemented yet. */                      
                  model_save_dir=,             
                  depvar=,                 
                  tempdir=,
                  xgboost_exe=D:\applications\xgboost-0.47\xgboost\build\Release\xgboost.exe,                  
                  booster=gbtree,
                  silent=,
                  nthread=,                
                  objective="reg:linear",
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
    
data &templib..xgbm&rn._train;
   set &data.;
   _id&rn.=_n_;
   run;

%tm_libsvm_export(data=&templib..xgbm&rn._train,
                  depvar=&depvar.,
                  keyvars=_id&rn.,
                  out_dir=&tempdir.,
                  out_prefix=xgbm&rn._train_,
                  out_spec_data=m&rn..&model_name._libsvm_spec
                  );
                  
proc export data=m&rn..&model_name._libsvm_spec
            outfile="&model_save_dir.\&model_name._libsvm_spec.csv" 
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
   put '&x. = "' "&&&x." '"';
   %end;
%mend;

%macro create_conf(conf_fileref,conf_task);
data _null_;
   file &conf_fileref.;
   
   put "#";
   put "# General Parameters";
   put "#";
   put;
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
   put 'data= "' "&xgb_tempdir.\\xgbm&rn._train_libvsm.txt" '"';
   put 'test:data= "' "&xgb_tempdir.\\xgbm&rn._train_libvsm.txt" '"';  
   put 'model_dir = "' "&xgb_model_save_dir." '"';   
   %if %qupcase(&task.)=%quote(PRED) %then %do;
      put 'name_pred ="' "&tempdir./xgb&rn._predictions.txt" '"';
      %end;   

   %set_if_not_null_f(objective);
   %set_if_not_null(base_score);
   %set_if_not_null_f(eval_metric);
   %set_if_not_null(seed);   
   %set_if_not_null(eta);
   %set_if_not_null(aplpha);   
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
   %create_conf(ct&rn.,train);    
   
   data _null_;
      length cmd $ 5000;
      cmd=cat('"', "&xgboost_exe.", '" "', "&model_save_dir.\xgboost_train.conf", '" > "',
              "&model_save_dir.\xgboost_train.log", '" 2>&1');
      put cmd;
      call system(cmd);
      run;
   
         
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
         %end;                        
   %end;   
%else %if %qupcase(&task.)=%quote(PRED) %then %do;
   filename ct&rn. "&model_save_dir.\xgboost_score_&model_in..conf" lrecl=10000;
   %create_conf(ct&rn.,pred);    
   
   data _null_;
      length cmd $ 5000;
      cmd=cat('"', "&xgboost_exe.", '" "', "&model_save_dir.\xgboost_score_&model_in..conf", '" > "',
              "&model_save_dir.\xgboost_score_&model_in..log", '" 2>&1');
      put cmd;
      call system(cmd);
      run;
            
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
         %end; 
         
   data &templib.xgbm&rn._preditions;
      length _id&rn. &pred_varname. 8;
      infile "&tempdir./xgb&rn._predictions.txt";
      input @1 &pred_varname best32. @;
      _id&rn=_n_;
      output;
      run;
   
   %if %quote(&out_keep_vars.)=%quote(&null.) %then %do;
      proc sql;
      create table &out as
      select a.*,
             b.&pred_varname.   
      from &templib..xgbm&rn._train a
      join &templib.xgbm&rn._preditions b
         on a._id&rn.=b._id&rn.
      ;quit;
      %end;
   %else %do;
      proc sql;
      create table &out as
      select a.*,
             b.&pred_varname.   
      from &templib..xgbm&rn._train(keep=_id&rn. &key_vars) a
      join &templib.xgbm&rn._preditions b
         on a._id&rn.=b._id&rn.
      ;quit;
      %end;      
   %end;                                                      
%mend;                 
                  
                  
                  
                  
                  