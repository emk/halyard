// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE_SMTPXX
//
// smtpxx - A portable C++ SMTP library for use with netxx
// Copyright 2004 Trustees of Dartmouth College
// 
// All Rights Reserved
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in
//    the documentation and/or other materials provided with the
//    distribution.
// 3. Neither the name of the Author nor the names of its contributors
//    may be used to endorse or promote products derived from this software
//    without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
// PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR
// OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
// USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
// AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
// OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
// SUCH DAMAGE.
//
// @END_LICENSE_SMTPXX

#ifdef WIN32
// Turn off _set_se_translator warning from boost test harness.
// Boost isn't doing anything wrong, despite the warning.
// See: http://www.boost.org/more/error_handling.html
#pragma warning(disable: 4535)
#endif

#include <string>
#include <vector>
#include "smtpxx.h"
#include <boost/test/minimal.hpp>

using namespace smtpxx;

void test_email() {
    email msg("Test message", "foo@bar.com");
    BOOST_CHECK(msg.subject() == "Test message");
    BOOST_CHECK(msg.from() == "foo@bar.com");
    msg.to("baz1@bar.com");
    msg.to("baz2@bar.com");
    BOOST_CHECK(msg.to().size() == 2);
    BOOST_CHECK(msg.to()[0] == "baz1@bar.com");
    BOOST_CHECK(msg.to()[1] == "baz2@bar.com");
    msg.cc("baz3@bar.com");
    BOOST_CHECK(msg.cc().size() == 1);
    BOOST_CHECK(msg.cc()[0] == "baz3@bar.com");
}

int test_main(int, char **) {
    test_email();
    return 0;
}
