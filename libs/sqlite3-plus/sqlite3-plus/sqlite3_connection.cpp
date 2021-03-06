/*
	Copyright (c) 2004 Cory Nelson

	Permission is hereby granted, free of charge, to any person obtaining
	a copy of this software and associated documentation files (the
	"Software"), to deal in the Software without restriction, including
	without limitation the rights to use, copy, modify, merge, publish,
	distribute, sublicense, and/or sell copies of the Software, and to
	permit persons to whom the Software is furnished to do so, subject to
	the following conditions:

	The above copyright notice and this permission notice shall be included
	in all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
	IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
	CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
	TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
	SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#include <string>
#include <stdexcept>
using namespace std;

#include <cstdarg>
#include "sqlite3_plus.h"
#include "sqlite3_internal.h"

namespace sqlite3 {
	connection::connection() {
		this->db=NULL;
	}

	connection::connection(const char *db) {
		this->db=NULL;
		this->open(db);
	}

	connection::~connection() {
        try {
            this->close();
        } catch (exception &) {
            // XXX - Log this error.
        }
	}

	void connection::open(const char *db) {
		if(_sqlite3_open(db, &this->db)!=SQLITE_OK)
			throw runtime_error("open: unable to open database");
	}

	void connection::close() {
		if(this->db) {
			if(_sqlite3_close(this->db)!=SQLITE_OK)
				throw runtime_error(string("close: ")+_sqlite3_errmsg(this->db));
			this->db=NULL;
		}
	}

	__int64 connection::insertid() const {
		if(this->db==NULL) throw runtime_error("insertid: connection is closed");
		return _sqlite3_last_insert_rowid(this->db);
	}

	reader connection::_executereader(const char *fmt, va_list args) {
		char *sql=_sqlite3_vmprintf(fmt, args);

		reader r;
		r.con=this;

		int ret=_sqlite3_prepare(this->db, sql, &r.pimpl->vm);
		_sqlite3_free(sql);

		if(ret!=SQLITE_OK) throw runtime_error(string("executereader: ")+_sqlite3_errmsg(this->db));

		r.argc=_sqlite3_column_count(r.pimpl->vm);

		return r;
	}

	int connection::executenonquery(const char *fmt, ...) {
		va_list args;
		va_start(args, fmt);
		reader r=this->_executereader(fmt, args);
		va_end(args);

		r.read();
		r.close();

		return _sqlite3_changes(this->db);
	}
	
	string connection::executescalar(const char *fmt, ...) {
		va_list args;
		va_start(args, fmt);
		reader r=this->_executereader(fmt, args);
		va_end(args);

		string ret;
		if(r.read()) ret=r[0];
		r.close();

		return ret;
	}

	reader connection::executereader(const char *fmt, ...) {
		va_list args;
		va_start(args, fmt);
		reader r=this->_executereader(fmt, args);
		va_end(args);

		return r;
	}
};
