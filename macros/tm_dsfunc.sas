
/**********************************************************************/
/*  Macro:  tm_dsfunc                                                 */
/*                                                                    */
/*  Parameters: positional                                            */
/*                                                                    */
/*    fun             function name                                   */
/*    mv_root_name    root name of macro variable sequence            */
/*    start           element in sequence to start iterating through  */
/*    stop            element in sequence to stop iteration           */
/*                                                                    */
/*  Example:                                                          */
/*                                                                    */
/*    %let var1=segment_a_score;                                      */
/*    %let var2=segment_b_score;                                      */
/*    %let var3=segment_c_score;                                      */
/*                                                                    */
/*    data test;                                                      */
/*       avg_score=%tm_dsfunc(mean,var,1,3);                          */
/*       run;                                                         */
/**********************************************************************/

%macro tm_dsfunc(fun,mv_root_name,start,stop);
%local i;
&fun.(&&&mv_root_name&start %do i=%eval(&start+1) %to &stop; ,&&&mv_root_name&i %end; )
%mend;