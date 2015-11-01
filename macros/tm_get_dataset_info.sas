/******************************************************************************
 Macro: tm_get_dataset_info
 
 Parameters: Named
    data=         SAS Dataset
    nvarsname=    Name of macro variable to hold the # of variables in the 
                  dataset
    NamePrefix=   Prefix to use for macro variables containing the name of
                  each variable
    TypePrefix=   Prefix to use for macro variables containing the type of
                  each variable
    FormatPrefix= Prefix to use for macro variables containing the format
                  associated with each variable
    LabelPrefix=  Prefix to use for macro variables containing the Label
                  associated with each variable
 
 This macro gets information for each variable and the number of variables then
 loads this information into macro variables.
 
 The prefixes are used as the base name of each macro variable and numerical
 suffix is appended to it for each variable in the dataset.  They can then
 be referenced by looping from 1 to the number of variables, which is captured
 in the macro variable specified in the [nvarsname] parameter.
******************************************************************************/
%macro tm_get_dataset_info(data=,                                                                                                                                                      
                        nvarsname=nvars,                                                                                                                                            
                        NamePrefix=var,                                                                                                                                             
                        TypePrefix=type,                                                                                                                                            
                        FormatPrefix=format,                                                                                                                                        
                        LabelPrefix=label);                                                                                                                                                                                               
                        
%local dsid i;                                                                                                                                                                      
%global &nvarsname;                                                                                                                                                                 
%let dsid = %sysfunc(open(&data,i));                                                                                                                                                
%let &nvarsname = %sysfunc(attrn(&dsid,NVARS));                                                                                                                                     
%do i=1 %to &&&nvarsname;                                                                                                                                                           
  %global &NamePrefix&i &TypePrefix&i &FormatPrefix&i &LabelPrefix&i;                                                                                                               
  %let &NamePrefix&i=%sysfunc(varname(&dsid,&i));                                                                                                                                   
  %let &TypePrefix&i=%sysfunc(vartype(&dsid,&i));                                                                                                                                   
  %let &FormatPrefix&i=%sysfunc(varfmt(&dsid,&i));                                                                                                                                  
  %let &LabelPrefix&i=%sysfunc(varlabel(&dsid,&i));                                                                                                                                 
  %end;                                                                                                                                                                             
%let rc=%sysfunc(close(&dsid));                                                                                                                                                     
%mend;   