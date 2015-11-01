/******************************************************************************
 Macro: tm_ListIntersectExclude

 Parameters: Positional

    list1       list of values separated by a space
    list2           list of values separated by a space

 The values can't contain spaces.  The macro resolves to a space
 separated list excluding values that appear in both lists.

 Ex.

 %let list1=x1 x2 score1 score2 y1 z3 mailing_seq_number;
 %let list2=x2 z3 score1 score4 account_number brmodel_score;

 %let varlist=%ListIntersectExclude(&list1,&list2);

 &varlist resolves to
                x2 score2 y1 mailing_seq_number z3 score4 account_number brmodel_score;
******************************************************************************/

%macro tm_ListIntersectExclude(list1,list2);
    %local null nitems1 nitems2 x i j rv;

    %let null=;
    %let rv=&null;

    %if &list1 ne &null and &list2 ne &null %then %do;
        %let nitems1=0;
       %let x=%scan(&list1,1,%str( ));
       %do %until(&x=&null);
        %let nitems1=%eval(&nitems1+1);
        %let x=%scan(&list1,&nitems1,%str( ));
        %end;
       %let nitems1=%eval(&nitems1-1);

       %do i=1 %to &nitems1;
        %local l1v&i;
        %let l1v&i=%trim(%left(%upcase(%scan(&list1,&i,%str( )))));
        %end;

       %let nitems2=0;
       %let x=%scan(&list2,1,%str( ));
       %do %until(&x=&null);
        %let nitems2=%eval(&nitems2+1);
        %let x=%scan(&list2,&nitems2,%str( ));
        %end;
       %let nitems2=%eval(&nitems2-1);

       %do i=1 %to &nitems2;
        %local l2v&i;
        %let l2v&i=%trim(%left(%upcase(%scan(&list2,&i,%str( )))));
        %end;

       %do i=1 %to &nitems1;
        %do j=1 %to &nitems2;
            %if &&l1v&i=&&l2v&j %then %let l1v&i=;
            %end;
        %end;

       %do i=1 %to &nitems1;
        %if &&l1v&i ne &null %then %let rv=&rv &&l1v&i;
        %end;
       %end;
    %else %if &list1 ne &null and &list2=&null %then %do;
        %let rv=&list1;
       %end;
    %else %if &list1=&null %then %do;
        %let rv=&null;
        %end;
&rv
%mend;