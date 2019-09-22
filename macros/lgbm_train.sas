%macro lgbm_train(train_data=,
                   train_csv=,
                   valid_data=,
                   valid_csv=,
				       delete_csv_files=n,
                   tempdir=,
                   sas_code=,
                   sas_code_model_name=,
                   sas_code_model_desc=,

                   label=,
                   weight=,
                   query=,
                   ignore_column=,
                   categorical_feature=,


                   objective=,

                   boosting_type=,
                   num_iterations=,
                   learning_rate=,
                   num_leaves=,
                   num_threads=,

                   metric=,
                   metric_freq=,
                   is_training_metric=,
                   ndcg_at=,

                   max_depth=,
                   min_data_in_leaf=,
                   min_sum_hessian_in_leaf=,
                   feature_fraction=,
                   feature_fraction_seed=,
                   bagging_fraction=,
                   bagging_freq=,
                   bagging_seed=,
                   early_stopping_round=,
                   lambda_l1=,
                   lambda_l2=,
                   min_gain_to_split=,
                   drop_rate=,
                   skip_drop=,
                   max_drop=,
                   uniform_drop=,
                   xgboost_dart_mode=,
                   drop_seed=,
                   top_rate=,
                   other_rate=,

                   max_bin=,
                   data_random_seed=,
                   output_model=,
                   input_model=,
                   is_sparse=,
                   two_round=,
                   verbosity=,
                   bin_construct_sample_cnt=,

                   sigmoid=1.0,
                   huber_delta=1.0,
                   fair_c=1.0,
                   poission_max_delta_step=0.7,
                   scale_pos_weight=1.0,
                   is_unbalance=false,
                   max_position=20,
                   label_gain=,
                   num_class=1,
                   lightgbm_exec=D:\applications\LightGBM-2017-06-05\windows\x64\Release\lightgbm.exe
                   );

options xsync noxwait;
%local null rn tempdir rwd i j k q p num_vars train_csv valid_csv train_lib valid_lib;
%local train_dsn valid_dsn train_path valid_path;
%let null=;

/* Get Random Number to use in naming temp files and datasets */
data _null_;
   x=int(99998*ranuni(-1))+1;
   call symput('rn',strip(x));
   run;

%if %quote(&tempdir.)=%quote(&null) %then %let tempdir=%sysfunc(pathname(work));

libname _t&rn "&tempdir.";

%macro delete_file(file);
data _null_;
    fname="___df";
    rc=filename(fname,"&file.");
    if rc = 0 and fexist(fname) then
       rc=fdelete(fname);
    rc=filename(fname);
run;
%mend;

%macro check_dir(dir);
%local rc fileref;
%let rc = %sysfunc(filename(fileref,&dir)) ;
%if %sysfunc(fexist(&fileref)) %then
   %put NOTE: The directory "&dir" exists ;
%else %do;
   %sysexec md "&dir.";
   %put %sysfunc(sysmsg()) The directory has been created.;
   %end;
%let rc=%sysfunc(filename(fileref));
%mend;

%macro tm_ParseList(prefix,list);
    %local n word flag null i nwords;
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

data _t&rn..ref_parameter_&rn.;
length parameter data_type default $ 32  enum_values  $ 256 min_value max_value 8;
parameter='label';data_type='variable';default='';enum_values='';min_value='';max_value='';output;
parameter='weight';data_type='variable';default='';enum_values='';min_value='';max_value='';output;
parameter='query';data_type='variable';default='';enum_values='';min_value='';max_value='';output;
parameter='ignore_column';data_type='variable_list';default='';enum_values='';min_value='';max_value='';output;
parameter='categorical_feature';data_type='variable_list';default='';enum_values='';min_value='';max_value='';output;
parameter='objective';data_type='enum';default='regression';enum_values='regression regression_l1 huber fair poisson binary lambdarank multiclass';min_value='';max_value='';output;
parameter='boosting_type';data_type='enum';default='gbdt';enum_values='gbdt dart goss';min_value='';max_value='';output;
parameter='num_iterations';data_type='int';default='10';enum_values='';min_value='1';max_value='';output;
parameter='learning_rate';data_type='double';default='0.1';enum_values='';min_value='0.00000001';max_value='1';output;
parameter='num_leaves';data_type='int';default='127';enum_values='';min_value='1';max_value='';output;
parameter='num_threads';data_type='int';default='1';enum_values='';min_value='1';max_value='';output;
parameter='metric';data_type='enum_list';default='';enum_values='l1 mae mean_absolute_error l2 mse mean_squared_error huber fair poisson ndcf map auc binary_logloss binary_error multi_logloss multi_error';min_value='';max_value='';output;
parameter='metric_freq';data_type='int';default='1';enum_values='';min_value='1';max_value='';output;
parameter='is_training_metric';data_type='bool';default='false';enum_values='';min_value='';max_value='';output;
parameter='ndcg_at';data_type='int_list';default='';enum_values='';min_value='';max_value='';output;
parameter='max_depth';data_type='int';default='-1';enum_values='';min_value='';max_value='';output;
parameter='min_data_in_leaf';data_type='int';default='100';enum_values='';min_value='1';max_value='';output;
parameter='min_sum_hessian_in_leaf';data_type='double';default='10';enum_values='';min_value='';max_value='';output;
parameter='feature_fraction';data_type='double';default='1';enum_values='';min_value='0.00000001';max_value='1';output;
parameter='feature_fraction_seed';data_type='int';default='2';enum_values='';min_value='';max_value='';output;
parameter='bagging_fraction';data_type='double';default='1';enum_values='';min_value='0.00000001';max_value='1';output;
parameter='bagging_freq';data_type='int';default='0';enum_values='';min_value='0';max_value='';output;
parameter='bagging_seed';data_type='int';default='3';enum_values='';min_value='';max_value='';output;
parameter='early_stopping_round';data_type='int';default='0';enum_values='';min_value='0';max_value='';output;
parameter='lambda_l1';data_type='double';default='0';enum_values='';min_value='';max_value='';output;
parameter='lambda_l2';data_type='double';default='0';enum_values='';min_value='';max_value='';output;
parameter='min_gain_to_split';data_type='double';default='0';enum_values='';min_value='0';max_value='';output;
parameter='drop_rate';data_type='double';default='0.1';enum_values='';min_value='0.00000001';max_value='1';output;
parameter='skip_drop';data_type='double';default='0.5';enum_values='';min_value='0.00000001';max_value='1';output;
parameter='max_drop';data_type='int';default='50';enum_values='';min_value='';max_value='';output;
parameter='uniform_drop';data_type='bool';default='false';enum_values='';min_value='';max_value='';output;
parameter='xgboost_dart_mode';data_type='bool';default='false';enum_values='';min_value='';max_value='';output;
parameter='drop_seed';data_type='int';default='4';enum_values='';min_value='';max_value='';output;
parameter='top_rate';data_type='double';default='0.2';enum_values='';min_value='';max_value='';output;
parameter='other_rate';data_type='double';default='0.1';enum_values='';min_value='';max_value='';output;
parameter='max_bin';data_type='int';default='255';enum_values='';min_value='1';max_value='';output;
parameter='data_random_seed';data_type='int';default='1';enum_values='';min_value='';max_value='';output;
parameter='output_model';data_type='string';default='LightGBM_model.txt';enum_values='';min_value='';max_value='';output;
parameter='input_model';data_type='string';default='';enum_values='';min_value='';max_value='';output;
parameter='is_sparse';data_type='bool';default='true';enum_values='';min_value='';max_value='';output;
parameter='two_round';data_type='bool';default='false';enum_values='';min_value='';max_value='';output;
parameter='verbosity';data_type='int';default='1';enum_values='';min_value='';max_value='';output;
parameter='bin_construct_sample_cnt';data_type='int';default='500000';enum_values='';min_value='1';max_value='';output;
parameter='sigmoid';data_type='double';default='1';enum_values='';min_value='';max_value='';output;
parameter='huber_delta';data_type='double';default='1';enum_values='';min_value='';max_value='';output;
parameter='fair_c';data_type='double';default='1';enum_values='';min_value='';max_value='';output;
parameter='poission_max_delta_step';data_type='double';default='0.7';enum_values='';min_value='';max_value='';output;
parameter='scale_pos_weight';data_type='double';default='1';enum_values='';min_value='';max_value='';output;
parameter='is_unbalance';data_type='bool';default='false';enum_values='';min_value='';max_value='';output;
parameter='max_position';data_type='int';default='20';enum_values='';min_value='';max_value='';output;
parameter='label_gain';data_type='double_list';default='';enum_values='';min_value='';max_value='';output;
parameter='num_class';data_type='int';default='';enum_values='';min_value='1';max_value='';output;run;
run;

%let create_train_csv=N;
%let create_valid_csv=N;
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
   %let train_csv=&train_path.\&train_dsn..lgbmt.&label..csv;
   
   filename t__csv "&train_csv.";
   %if not %sysfunc(fexist(t__csv)) %then %let create_train_csv=Y;
   filename t__csv clear;
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
   %let valid_csv=&valid_path.\&valid_dsn..lgbmv.&label..csv;
   
   filename v__csv "&valid_csv.";
   %if not %sysfunc(fexist(v__csv)) %then %let create_valid_csv=Y;
   filename v__csv clear;
   %end;   
   
%if &create_train_csv=Y or &create_valid_csv=Y %then %do;
	%lgbm_create_dataset(train_data=&train_data.,
						 valid_data=&valid_data.,
						 label=&label.,                           
						 templib=work,
						 lgbm_executable=&lightgbm_exec.
						 );  
	%end;

data _null_;
   set _t&rn..ref_parameter_&rn. end=last;
   call symput(cats('parm',_n_),strip(parameter));
   call symput(cats('dtype',_n_),strip(data_type));
   call symput(cats('default',_n_),strip(default));
   call symput(cats('enums',_n_),strip(enum_values));
   call symput(cats('minval',_n_),strip(min_value));
   call symput(cats('maxval',_n_),strip(max_value));

   if last then do;
      call symput('num_parms',strip(_n_));
      end;
   run;

filename p&rn. "&tempdir.\lgbm_conf_&rn..conf" lrecl=100000;

%let delete_output_model=N;
%if %quote(&output_model)=%quote(&null) %then %do;
   %let output_model=&tempdir.\lgbm_output_model_&rn..txt;
   %let delete_output_model=Y;
   %end;

data _null_;
   file p&rn.;
   length parameter $ 32 value $ 32000;
   
   put "task=train";
   put "header=true";
   put "train=&train_csv.";
   
   %if %quote(&valid_data.) ne %quote(&null.) %then %do;
      put "test=&valid_csv.";
      %end;
  
   
   %do i=1 %to &num_parms.;
      %let p=&&parm&i..;
      %let v=&&&p..;
      
      %put &&dtype&i.. --> &p. --> &v.;
      

      parameter=strip(lowcase("&p."));
      value='';
      %if %qupcase(&&dtype&i..)=%quote(VARIABLE_LIST) and %quote(&v.) ne %quote(&null.) %then %do;
         value='name:';
         %let nitems=%tm_parselist(item,&v.);
         %do k=1 %to &nitems;
            value=cats(value,strip("&&item&k.."));
            %if &k ne &nitems %then %do;
               value=cats(value,",");
               %end;
            %end;
         put parameter +(-1) "=" value;
         %end;
      %else %if %qupcase(&&dtype&i..)=%quote(ENUM_LIST) and %quote(&v.) ne %quote(&null.) %then %do;
         value='';
         %let nitems=%tm_parselist(item,&v.);
         %do k=1 %to &nitems;
            value=cats(value,strip("&&item&k.."));
            %if &k ne &nitems %then %do;
               value=cats(value,",");
               %end;
            %end;
         put parameter +(-1) "=" value;
         %end;
      %else %if %qupcase(&&dtype&i..)=%quote(VARIABLE) and %quote(&v.) ne %quote(&null.) %then %do;
         value=cats('name:',"&v.");
         put parameter +(-1) "=" value;
         %end;
      %else %if %qupcase(&&dtype&i..)=%quote(ENUM) %then %do;
         %if %quote(&v.) ne %quote(&null.) %then %do;
            value=strip("&v.");
            %end;
         %else %if %quote(&&default&i..) ne %quote(&null.) %then %do;
            value=strip("&&default&i..");
            %end;

         if value ne '' then put parameter +(-1) "=" value;
         %end;
      %else %if %qupcase(&&dtype&i..)=%quote(STRING) %then %do;
         %if %quote(&v.) ne %quote(&null.) %then %do;
            value=strip("&v.");
            %end;
         %else %if %quote(&&default&i..) ne %quote(&null.) %then %do;
            value=strip("&&default&i..");
            %end;

         if value ne '' then put parameter +(-1) "=" value;
         %end;         
      %else %if %qupcase(&&dtype&i..)=%quote(BOOL) %then %do;
         %if %quote(&v.) ne %quote(&null.) %then %do;
            value=strip("&v.");
            %end;
         %else %if %quote(&&default&i..) ne %quote(&null.) %then %do;
            value=strip("&&default&i..");
            %end;

         if value ne '' then put parameter +(-1) "=" value;
         %end;
      %else %if %qupcase(&&dtype&i..)=%quote(INT_LIST) %then %do;
         %if %quote(&v.) ne %quote(&null.) %then %do;
            value=strip("&v.");
            %end;
         %else %if %quote(&&default&i..) ne %quote(&null.) %then %do;
            value=strip("&&default&i..");
            %end;

         if value ne '' then put parameter +(-1) "=" value;
         %end;
      %else %if %qupcase(&&dtype&i..)=%quote(DOUBLE_LIST) %then %do;
         %if %quote(&v.) ne %quote(&null.) %then %do;
            value=strip("&v.");
            %end;
         %else %if %quote(&&default&i..) ne %quote(&null.) %then %do;
            value=strip("&&default&i..");
            %end;

         if value ne '' then put parameter +(-1) "=" value;
         %end;
      %else %if %qupcase(&&dtype&i..)=%quote(INT) %then %do;
         %if %quote(&v.) ne %quote(&null.) %then %do;
            value=strip(int(input("&v.",? best32.)));
            %end;
         %else %if %quote(&&default&i..) ne %quote(&null.) %then %do;
            value=strip(int(input("&&default&i..",? best32.)));
            %end;

         if value ne '' then put parameter +(-1) "=" value;
         %end;
      %else %if %qupcase(&&dtype&i..)=%quote(DOUBLE) %then %do;
         %if %quote(&v.) ne %quote(&null.) %then %do;
            value=strip(put(input("&v.",? best32.),best32.9));
            %end;
         %else %if %quote(&&default&i..) ne %quote(&null.) %then %do;
            value=strip(put(input("&&default&i..",? best32.),best32.9));
            %end;

         if value ne '' then put parameter +(-1) "=" value;
         %end;
      %end;
   run;

filename bf&rn. "&tempdir.\lgbm_batch_&rn..bat" lrecl=1024;
data _null_;
   file bf&rn.;
   drive=substr(strip("&tempdir."),1,2);
   put drive;
   path=substr(strip("&tempdir."),3,length(strip("&tempdir."))-2);
   put 'cd "' path +(-1) '"';
   put '"' "&lightgbm_exec." '"' " config=lgbm_conf_&rn..conf > lgbm_train_&rn..log 2>&1";
   run;
filename bf&rn. clear;
data _null_;
   length cmd $ 10000;
   cmd=strip(cats('"',"&tempdir.\lgbm_batch_&rn..bat",'"'));
   call system(cmd);
   run;

data _t&rn..lgbm_log_&rn.;
   infile "&tempdir.\lgbm_train_&rn..log" lrecl=10000 end=last;
   length dataset metric $ 32 value 8;
   if _n_=1 then do;
      put "******************************************************";
      put "**                                                  **";
      put "** LightGBM Run Log                                 **";
      put "**                                                  **";
      put "******************************************************";
      put;
      end;
   input @1 _x $1. @;
   put _infile_;

   dataset='';
   if indexw(_infile_,'training',' ')>0 then dataset='Training';
   %do i=1 %to 10;
      else if indexw(_infile_,"valid._&i.",' ')>0 then dataset="Validation &i.";
      %end;

   if dataset ne '' then do;
      metric=scan(_infile_,5,' ');
      value=input(scan(_infile_,7,' '),best32.);
      output;
      end;

   if last then do;
      put;
      put "******************************************************";
      put "**                                                  **";
      put "** End of LightGBM Run Log                          **";
      put "**                                                  **";
      put "******************************************************";
      end;
   run;

%macro lightgbm2sas(infile=,outfile=,
                    outvar=p_long_range,
                    description=,
                    prefix=_,
                    tree_array_size=50000,
                    code_array_size=50000
                    );
%local null;
%let null=;

filename _lgsin "&infile" recfm=n;
filename _lgsout "&outfile" lrecl=100000;


data _input(keep=row_id line);
   infile _lgsin recfm=n eof=end_of_file;
   length current_char next_char $ 1 varname $ 64;
   retain first_character 1 current_char varname '' var_count char_count 0 eq_flag 0;

   length row_id $ 64 line $ 32000;   
   retain _header_complete 0 row_id line '';
    
   input next_char $char1. @@;
   if next_char='20'x then next_char="|";
  
   char_count=char_count+1;
          
   if first_character=1 then do;
     current_char=next_char;
     first_character=0;
     return;
     end;

   if not eq_flag and current_char not in ('0D'x,'0A'x) then do;      
      if current_char ne '=' then do;
         if row_id ne '' then row_id=cat(strip(row_id),strip(current_char));
         else row_id=strip(current_char);
         *put row_id;
         end;
      else do;
         *put row_id;
         eq_flag=1;
         line='';
         if strip(row_id)='feature_names' then do;
            var_count=0;
            varname='';
            end;
         end;
      end;
   else if eq_flag and current_char not in ('0D'x,'0A'x) then do;
      if strip(row_id)='feature_names' then do;
         if current_char ne "|" then do;
            if varname ne '' then varname=cats(varname,current_char);
            else varname=strip(current_char);
            end;
         if current_char="|" or next_char in ('0D'x,'0A'x) then do;
            var_count=var_count+1;
            call symput(cats('var',var_count),strip(varname));
            varname='';
            if next_char in ('0D'x,'0A'x) then call symput('nvars',strip(var_count));
            end;
         end;
      else if row_id in ("Tree","num_leaves","split_feature","split_gain","threshold","decision_type","left_child",
                         "right_child","leaf_parent","leaf_value","leaf_count","internal_value","internal_count",
                         "shrinkage","num_class") 
      then do;
         if line ne '' then line=cats(line,current_char);
         else line=strip(current_char);
         end;
      end;
      

   if next_char in ('0D'x,'0A'x) then do;
      if row_id = "num_class" then do;
         call symput('nclasses',strip(line));
         end;
      else if row_id in ("Tree","num_leaves","split_feature","split_gain","threshold","decision_type","left_child",
                         "right_child","leaf_parent","leaf_value","leaf_count","internal_value","internal_count",
                         "shrinkage") 
      then do;
         output;
         end;
      
      *put row_id char_count;

      row_id='';
      eq_flag=0;
      end;

   current_char = next_char;
   return;
   end_of_file: put current_char +(-1);
   run;

proc ds2;
data work._output(keep=(tree_id indent code_line) overwrite=yes);

declare char(512) code_line;
declare char(32000) line;
declare char(64) word;
declare double tree_id header_complete tree_complete num_leaves tree_id shrinkage indent;
declare char(64) feature_names[&nvars.];
declare double split_feature[&tree_array_size.];
declare double split_gain[&tree_array_size.];
declare double threshold[&tree_array_size.];
declare double decision_type[&tree_array_size.];
declare double left_child[&tree_array_size.];
declare double right_child[&tree_array_size.];
declare double leaf_parent[&tree_array_size.];
declare double leaf_value[&tree_array_size.];
declare double leaf_count[&tree_array_size.];
declare double internal_value[&tree_array_size.];
declare double internal_count[&tree_array_size.];

retain tree_id header_complete tree_complete num_leaves;

method to_array(in_out char text, double x[*]);
   declare double flag k i;
   declare char(512) word;
   flag=0;
   k=0;
   do until(flag=1);
      k=k+1;
      word=scan(text,k,'|');
      if strip(word)='' then do;
         flag=1;
         end;
      else do;
         x[k]=inputn(word,'best32.');
         end;
      end;
   if k<=dim(x) then do;
      do i=k to dim(x);
         x[i]=.;
         end;
      end;
end;

method to_array(in_out char text, char x[*]);
   declare double flag k i;
   declare char(512) word;
   flag=0;
   k=0;
   do until(flag=1);
      k=k+1;
      word=scan(text,k,' ');
      if strip(word)='' then do;
         flag=1;
         end;
      else do;
         x[k]=word;
         end;
      end;
   if k<=dim(x) then do;
      do i=k to dim(x);
         x[i]=.;
         end;
      end;
end;

method get_field_name(double node_id, double prev_node_idx, double is_child) returns char;
   declare double idx;
   idx = if is_child=1 then leaf_parent[node_id+1] else prev_node_idx;
   return(feature_names[split_feature[idx+1]+1]);
end;

method get_threshold(double node_id, double prev_node_idx, double is_child) returns double;
   declare double idx;
   idx = if is_child=1 then leaf_parent[node_id+1] else prev_node_idx;
   return(threshold[idx+1]);
end;

method gc_simple_predicate(double tab_len, double node_id, double is_left_child, double prev_node_idx, double is_leaf);
   declare char(12) op cst;
   if is_left_child=1 then do;
      op = if decision_type[prev_node_idx+1]=1 then '=' else '<=';
      cst = 'if';
      indent=tab_len;
      code_line=cat(strip(cst),' ',
                    strip(get_field_name(node_id, prev_node_idx, is_leaf)),' ',
                    strip(op),' ',
                    get_threshold(node_id, prev_node_idx, is_leaf),
                    ' then do;'
                    );
      output work._output;
      end;
   else do;
      op = if decision_type[prev_node_idx+1]=1 then 'ne' else '>';
      cst = 'else if';
      indent=tab_len;
      code_line='else do;';
      output work._output;
      end;


end;

method gc_nodes(double node_id, double tab_len, double is_left_child, double prev_node_idx);
   declare double is_leaf score;
   if node_id < 0 then do;
      node_id = -1*(node_id+1);
      score = leaf_value[node_id+1];
      is_leaf = 1;
      end;
   else do;
      score = internal_value[node_id+1];
      is_leaf = 0;
      end;

   gc_simple_predicate(tab_len, node_id, is_left_child, prev_node_idx, is_leaf);

   if is_leaf=0 then do;
      gc_nodes(left_child[node_id+1], tab_len+1, 1, node_id);
      gc_nodes(right_child[node_id+1], tab_len+1, 0, node_id);
      end;
   else do;
      indent=tab_len+1;
      code_line = cat(%tslit(&prefix.tree),strip(tree_id),'=',strip(put(score,best32.12)),';');
      output work._output;
      end;

   indent=tab_len+1;
   code_line='end;';
   output work._output;
end;

method init();
   tree_id=0;
   header_complete=0;
   tree_complete=0;
   num_leaves=0;
   indent=0;
   
   %do i=1 %to &nvars.;
      feature_names[&i.] = strip(%tslit(&&var&i..));
      %end;
   
   code_line='/****************************************************************************************'; 
   output work._output;
   code_line=%tslit(** &outvar.); output work._output;
   %if %quote(&description.) ne %quote(&null.) %then %do;
      code_line=cat('** ',%tslit(&description.)); output work._output;
      %end;
   code_line='**'; output work._output;
   code_line=cat('** Model Generated Using Microsoft LightGBM. (https://github.com/Microsoft/LightGBM)'); 
   output work._output;
   code_line=cat('** SAS Code Generated On ',put(today(),mmddyy10.),' at ',put(time(),timeampm11.)); 
   output work._output;
   code_line='*****************************************************************************************/'; 
   output work._output;
   code_line='';
end;

method run();
   set _input;
   if row_id='num_leaves' then do;
      num_leaves=inputn(line,'best32.');
      end;

   if row_id='split_feature' then do;
      to_array(line,split_feature);
      end;

   if row_id='split_gain' then do;
      to_array(line,split_gain);
      end;

   if row_id='threshold' then do;
      to_array(line,threshold);
      end;

   if row_id='decision_type' then do;
      to_array(line,decision_type);
      end;

   if row_id='left_child' then do;
      to_array(line,left_child);
      end;

   if row_id='right_child' then do;
      to_array(line,right_child);
      end;

   if row_id='leaf_parent' then do;
      to_array(line,leaf_parent);
      end;

   if row_id='leaf_value' then do;
      to_array(line,leaf_value);
      end;

   if row_id='leaf_count' then do;
      to_array(line,leaf_count);
      end;

   if row_id='internal_value' then do;
      to_array(line,internal_value);
      end;

   if row_id='internal_count' then do;
      to_array(line,internal_count);
      end;

   if row_id='shrinkage' then do;
      shrinkage=inputn(line,'best32.');
      tree_complete=1;
      end;

   if tree_complete=1 then do;
      tree_id=tree_id+1;
      indent=0;
      code_line=''; output work._output; output work._output;
      code_line='/***********************************************************'; output work._output;
      code_line=cat('**  Tree ',strip(tree_id)); output work._output;
      code_line='***********************************************************/'; output work._output;
      gc_nodes(left_child[1], 0, 1, 0);
      gc_nodes(right_child[1], 0, 0, 0);
      tree_complete=0;
      end;
end;

method term();
   indent=0;
   code_line='';
   output work._output;
   %if &nclasses.=1 %then %do;
      code_line=cat(%tslit(&outvar.),'=sum(of ',%tslit(&prefix.tree1-&prefix.tree),strip(tree_id),');');
      output work._output;
      %end;
   %else %do;
      indent=0;    
      code_line=cat('array treeval{*} ',%tslit(&prefix.tree1-&prefix.tree),strip(tree_id),';'); output work._output;
      code_line='__flag=0; __i=0;'; output work._output;
      %do i=0 %to %eval(&nclasses.-1);
         code_line=cat(%tslit(&outvar.),'_c',%tslit(&i.),'=0;'); output work._output;
         %end;          
      code_line='do until(__flag=1);'; output work._output;       
      indent=1;
      %do i=0 %to %eval(&nclasses.-1);
         code_line='__i=__i+1;'; output work._output;
         code_line=cat(%tslit(&outvar.),'_c',%tslit(&i.),
                   '=sum(',%tslit(&outvar.),'_c',%tslit(&i.),',treeval{__i});'); 
         output work._output;         
         %end;  
      code_line=cat('if __i>=',strip(tree_id),' then __flag=1;'); output work._output;
      code_line='end;'; output work._output;
      indent=0;
      code_line=''; output work._output;
      code_line='drop __i __flag;'; output work._output;
      %end;
   
   code_line=cat('drop ',%tslit(&prefix.tree1-&prefix.tree),strip(tree_id),';');
   output work._output;
end;
run;
quit;

data _null_;
   file _lgsout;
   set work._output;
   spaces=3*indent;
   put @spaces code_line;
   run;
%mend;

%lightgbm2sas(infile=&output_model.,
              outfile=&sas_code.,
              prefix=_,
              outvar=&sas_code_model_name.,
              description=&sas_code_model_desc.
              );

proc sql;
drop table _t&rn..ref_parameter_&rn.;
drop table _t&rn..lgbm_train_&rn.;
quit;

libname _t&rn clear;

%if %qupcase(&delete_output_model)=%quote(Y) %then %do;
   %delete_file(&tempdir.\lgbm_output_model_&rn..txt);
   %end;
%delete_file(&tempdir.\lgbm_conf_&rn..conf);
%delete_file(&tempdir.\lgbm_batch_&rn..bat);
%delete_file(&tempdir.\lgbm_train_&rn..log);
%delete_file(&tempdir.\LightGBM_model.txt);

%if %qupcase(&delete_csv_files)=%quote(Y) %then %do;
	%delete_file(&train_csv.);
	%delete_file(&valid_csv.);
	%delete_file(&train_csv..bin);
	%delete_file(&valid_csv..bin);
	%end;

%mend;