%macro tm_sqlserv_table_exists(db=,schema=,table=,rv=);
%local workpath rn null;
%let workpath=%sysfunc(pathname(work));
%let rn=%sysfunc(int(%sysevalf(9999999*(%sysfunc(ranuni(-1))))));
%let null=;

%global &rv;

%let sql=select b.name as schema_name,
					 a.name as table_name,
				    a.object_id as table_id,
					 b.schema_id				    				           
			from &db..sys.tables a
			join &db..sys.schemas b
			  on a.schema_id=b.schema_id
			order by 1,2,3,4
			;
%put &sql.;			

%tm_sqlserv_sqlcmd(command=%bquote(&sql.),out=__tables&rn.,print_output=n);

proc sql;
reset noprint;
select count(*) into: tcount
from __tables&rn.
where upcase(table_name)=upcase(compress("&table.","[]")) and 
      upcase(schema_name)=upcase(compress("&schema.","[]"))
;quit;

%if &tcount. > 0 %then %let &rv.=1;
%else %let &rv.=0;
%mend;



