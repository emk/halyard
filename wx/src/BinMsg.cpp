// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"

extern "C" {
#include "../libs/quake2/game/binmsg.h"
};

USING_NAMESPACE_FIVEL

REGISTER_TEST_CASE_FILE(BinMsg);


//=========================================================================
//  Tests
//=========================================================================

#if BUILD_TEST_CASES

#define CHECK_BINMSG(MSG,DATA) \
    do { \
        CHECK_EQ(MSG.buffer_size, sizeof(DATA) - 1); \
        CHECK_EQ(memcmp(MSG.buffer, DATA, MSG.buffer_size), 0); \
    } while (0)

#define CHKRET(CALL) \
    do { \
        if (!(CALL)) \
            THROW("failed: " #CALL); \
    } while (0)

BEGIN_TEST_CASE(TestBinMsg, TestCase) {
    binmsg_byte buffer[BINMSG_MAX_SIZE];
    binmsg_message msg;
    binmsg_array_info nested;

    CHKRET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test"));
    CHKRET(binmsg_build_done(&msg));
    CHECK_BINMSG(msg, "MStest\0A\0\0\0\0");

    CHKRET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test2"));
    CHKRET(binmsg_build_done(&msg));
    CHECK_BINMSG(msg, "MStest2\0A\0\0\0\0");

    CHKRET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test"));
    CHKRET(binmsg_add_int(&msg.args, 2));
    CHKRET(binmsg_build_done(&msg));
    CHECK_BINMSG(msg, "MStest\0A\0\0\0\1I\0\0\0\2");

    CHKRET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test"));
    CHKRET(binmsg_add_int(&msg.args, 2));
    CHKRET(binmsg_build_done(&msg));
    CHECK_BINMSG(msg, "MStest\0A\0\0\0\1I\0\0\0\2");

    CHKRET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test"));
    CHKRET(binmsg_add_string(&msg.args, "foo"));
    CHKRET(binmsg_build_done(&msg));
    CHECK_BINMSG(msg, "MStest\0A\0\0\0\1Sfoo\0");

    CHKRET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test"));
    CHKRET(binmsg_add_bool(&msg.args, 0));
    CHKRET(binmsg_add_bool(&msg.args, 1));
    CHKRET(binmsg_build_done(&msg));
    CHECK_BINMSG(msg, "MStest\0A\0\0\0\2B\0B\1");

    CHKRET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test"));
    CHKRET(binmsg_add_bool(&msg.args, 0));
    CHKRET(binmsg_add_array_begin(&msg.args, &nested));
    CHKRET(binmsg_add_int(&nested, 3));
    CHKRET(binmsg_add_array_end(&nested));
    CHKRET(binmsg_add_bool(&msg.args, 1));
    CHKRET(binmsg_build_done(&msg));
    CHECK_BINMSG(msg, "MStest\0A\0\0\0\3B\0A\0\0\0\1I\0\0\0\3B\1");

} END_TEST_CASE(TestBinMsg);

#endif // BUILD_TEST_CASES
