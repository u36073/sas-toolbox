
/**********************************************************************************/
/*   Macro: tm_dsobs                                                              */
/*                                                                                */
/*   Parameters: Positional                                                       */
/*     data     SAS data set                                                      */
/*                                                                                */
/*   This macro gets the number of observations in a SAS dataset and resolves to  */
/*   that value.                                                                  */
/*                                                                                */
/*   Example:  %do i=1 %to %tm_dsobs(work.test);  .....  %end;                    */
/**********************************************************************************/

%macro tm_dsobs(data);
  %local dsid rc nobs;
  %let dsid = %sysfunc(open(&data,i));
  %let nobs=%sysfunc(attrn(&dsid,NOBS));
  %let rc=%sysfunc(close(&dsid));
  &nobs
%mend;