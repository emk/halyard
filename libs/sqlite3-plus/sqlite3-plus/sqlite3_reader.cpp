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

#include "sqlite3_plus.h"
#include "sqlite3_internal.h"

namespace sqlite3 {
    reader::impl::~impl() {
        try {
            close();
        } catch(std::exception &) {
            // XXX - Log this error.
        }
    }

    void reader::impl::close() {
		if(this->vm) {
			_sqlite3_finalize(this->vm);
			this->vm=NULL;
		}
    }
    
	reader::reader()
        : pimpl(new impl)
    {
	}

	bool reader::read() {
		if(!this->pimpl->vm) throw runtime_error("read: reader is closed");

		int ret=_sqlite3_step(this->pimpl->vm);
		switch(ret) {
			case SQLITE_ROW:
				return true;
			case SQLITE_DONE:
				return false;
			default:
				throw runtime_error(_sqlite3_errmsg(this->con->db));
		}
	}
	
	void reader::close() {
        this->pimpl->close();
	}

	void reader::reset() {
		if(!this->pimpl->vm) throw runtime_error("reset: reader is closed");
		if(_sqlite3_reset(this->pimpl->vm)!=SQLITE_OK)
			throw runtime_error(_sqlite3_errmsg(this->con->db));
	}
	
	int reader::getint32(int index) const {
		if(!this->pimpl->vm) throw runtime_error("getint32: reader is closed");
		if((index)>(argc-1)) throw out_of_range("getint32: index out of range");
		return _sqlite3_column_int(this->pimpl->vm, index);
	}

	__int64 reader::getint64(int index) const {
		if(!this->pimpl->vm) throw runtime_error("getint64: reader is closed");
		if((index)>(argc-1)) throw out_of_range("getint64: index out of range");
		return _sqlite3_column_int64(this->pimpl->vm, index);
	}

	double reader::getdouble(int index) const {
		if(!this->pimpl->vm) throw runtime_error("getdouble: reader is closed");
		if((index)>(argc-1)) throw out_of_range("getdouble: index out of range");
		return _sqlite3_column_double(this->pimpl->vm, index);
	}

	string reader::getstring(int index) const {
		if(!this->pimpl->vm) throw runtime_error("getstring: reader is closed");
		if(index>(argc-1)) throw out_of_range("getstring: index out of range");
		return string(_sqlite3_column_text(this->pimpl->vm, index), _sqlite3_column_bytes(this->pimpl->vm, index));
	}

	string reader::getcolname(int index) const {
		if(!this->pimpl->vm) throw runtime_error("getcolname: reader is closed");
		if(index>(argc-1)) throw out_of_range("getcolname: index out of range");
		return _sqlite3_column_name(this->pimpl->vm, index);
	}

	string reader::operator[](int index) const {
		return this->getstring(index);
	}
};
