#include <iostream>
#include <stdexcept>
using namespace std;

#include "sqlite3_plus.h"

int main(void) {
	try {
		sqlite3::connection con("test.db");

		sqlite3::reader reader=con.executereader("select * from t_test;");

		while(reader.read())
			cout << reader.getcolname(0) << ": " << reader.getint32(0) << endl;

		reader.close();
		con.close();
	}
	catch(exception &ex) {
		cerr << "Exception Occured: " << ex.what() << endl;
	}

	return 0;
}
