%macro lgbm_delete_dataset(train_data=,
                           valid_data=,
                           label=,
                           lgbm_dataset_name=,
                           lgbm_dataset_dir=
                          );
%local null rn tempdir out_csv library;
%let null=;

%macro delete_file(file);
data _null_;
    fname="___df";
    rc=filename(fname,"&file.");
    if rc = 0 and fexist(fname) then
       rc=fdelete(fname);
    rc=filename(fname);
run;
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
   %let train_csv=&train_path.\&train_dsn..lgbmt.&label..csv;
   %put &train_csv.;
   %delete_file(&train_csv.);
   %delete_file(&train_csv..bin);
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
   %put &valid_csv.;
   %delete_file(&valid_csv.);
   %delete_file(&valid_csv..bin);
   %end;
   
%mend;
