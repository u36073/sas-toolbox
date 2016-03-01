%macro tm_sqlserv_setenv(version=14,
                         path_sqlcmd=C:\Program Files (x86)\Microsoft SQL Server\Client SDK\ODBC\110\Tools\Binn\sqlcmd,
								 path_bcp=C:\Program Files (x86)\Microsoft SQL Server\Client SDK\ODBC\110\Tools\Binn\bcp,
								 protocol=tcp,
								 server=192.168.1.111,
								 port=1433,
								 user=tmoore,
								 password=catbird
								 );
%global sqlserv_version
        sqlserv_sqlcmd
		  sqlserv_bcp
		  sqlserv_protocol
		  sqlserv_server
		  sqlserv_port
		  sqlserv_user
		  sqlserv_password
		  ;

%let null=;

%if %qupcase(&version) ne %quote(&null) %then %let sqlserv_version=&version;
%if %quote(&path_sqlcmd) ne %quote(&null) %then %let sqlserv_sqlcmd=&path_sqlcmd;
%if %quote(&protocol) ne %quote(&null) %then %let sqlserv_protocol=&protocol;
%if %quote(&server) ne %quote(&null) %then %let sqlserv_server=&server;
%if %quote(&port) ne %quote(&null) %then %let sqlserv_port=&port;
%if %quote(&user) ne %quote(&null) %then %let sqlserv_user=&user;
%if %quote(&password) ne %quote(&null) %then %let sqlserv_password=&password;
%mend;
