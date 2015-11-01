
/****************************************************************************************/
/*   Macro: tm_VarInDataset                                                             */
/*                                                                                      */
/*   Parameters: Positional                                                             */
/*     x     name of a variable                                                         */
/*     data  SAS data set                                                               */
/*                                                                                      */
/*   This macro determines if a variable name exists in a SAS data set.  It resolves    */
/*   to a value of 0 or 1.                                                              */
/*                                                                                      */
/*     0  Variable not found in dataset                                                 */
/*     1  Variable found in dataset                                                     */
/*                                                                                      */
/*   Example:   %if %quote(%VarInDataset(segment,work.test))=%quote(1) %then ........;  */
/****************************************************************************************/

%macro tm_VarInDataset(x,data);
%local dsid i nvars name rc rv;
%let dsid = %sysfunc(open(&data,i));
%let nvars = %sysfunc(attrn(&dsid,NVARS));
%let rv=0;
%do i=1 %to &nvars;
  %let name=%sysfunc(varname(&dsid,&i));
  %if %qupcase(&name)=%qupcase(&x) %then %let rv=1;
  %end;
%let rc=%sysfunc(close(&dsid));
&rv
%mend;