#include <string>
#include <iostream>
#include <stdexcept>
using namespace std;

#include "sqlite3_plus.h"

int main(void) {
	try {
		sqlite3::connection con("test.db");

		string countstr=con.executescalar("select count(*) from sqlite_master where name='t_test';");
		if(countstr=="0") con.executenonquery("create table t_test(number);");

		con.executenonquery("begin;");

		for(unsigned int i=0; i<10000; i++)
			con.executenonquery("insert into t_test values(%u);", i);

		con.executenonquery("commit;");
		con.close();
	}
	catch(exception &ex) {
		cerr << "Exception Occured: " << ex.what() << endl;
	}

	return 0;
}
