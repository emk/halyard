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

#define CHECK_RET(CALL) \
    do { \
        if (!(CALL)) \
            THROW("failed: " #CALL); \
    } while (0)

#define CHECK_BINMSG(MSG,DATA) \
    do { \
        CHECK_EQ(MSG.buffer_size, sizeof(DATA) - 1); \
        CHECK_EQ(memcmp(MSG.buffer, DATA, MSG.buffer_size), 0); \
    } while (0)

void CHECK_PARSE(binmsg_message *parsed, binmsg_message *built,
                 binmsg_string wanted_name, int wanted_argc)
{
    CHECK_RET(binmsg_parse(parsed, built->buffer, built->buffer_size));
    CHECK_EQ(strcmp(parsed->name, wanted_name), 0);
    CHECK_EQ(parsed->args.size, wanted_argc);
}

void CHECK_NEXT_TYPE(binmsg_array *array, binmsg_type wanted) {
    binmsg_type type;
    CHECK_RET(binmsg_get_next_type(array, &type));
    CHECK_EQ(type, wanted);
}

BEGIN_TEST_CASE(TestBinMsg, TestCase) {
    binmsg_byte buffer[BINMSG_MAX_SIZE];
    binmsg_message msg, msg2;
    binmsg_array nested;

    binmsg_int i;
    binmsg_float f;
    binmsg_string str;
    binmsg_bool b;

    CHECK_RET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test"));
    CHECK_RET(binmsg_build_done(&msg));
    CHECK_BINMSG(msg, "MStest\0A\0\0\0\0");
    CHECK_PARSE(&msg2, &msg, "test", 0);

    CHECK_RET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test2"));
    CHECK_RET(binmsg_build_done(&msg));
    CHECK_BINMSG(msg, "MStest2\0A\0\0\0\0");
    CHECK_PARSE(&msg2, &msg, "test2", 0);

    CHECK_RET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test"));
    CHECK_RET(binmsg_add_int(&msg.args, 2));
    CHECK_RET(binmsg_build_done(&msg));
    CHECK_BINMSG(msg, "MStest\0A\0\0\0\1I\0\0\0\2");
    CHECK_PARSE(&msg2, &msg, "test", 1);
    CHECK_NEXT_TYPE(&msg2.args, BINMSG_INT_TYPE);
    CHECK_RET(binmsg_get_int(&msg2.args, &i));
    CHECK_EQ(i, 2);

    CHECK_RET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test"));
    CHECK_RET(binmsg_add_int(&msg.args, 3));
    CHECK_RET(binmsg_build_done(&msg));
    CHECK_BINMSG(msg, "MStest\0A\0\0\0\1I\0\0\0\3");
    CHECK_PARSE(&msg2, &msg, "test", 1);
    CHECK_NEXT_TYPE(&msg2.args, BINMSG_INT_TYPE);
    CHECK_RET(binmsg_get_int(&msg2.args, &i));
    CHECK_EQ(i, 3);

    CHECK_RET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test"));
    CHECK_RET(binmsg_add_float(&msg.args, 10.0));
    CHECK_RET(binmsg_build_done(&msg));
    // We don't check the serialized form because it may not actually be
    // portable.  Floating point formats vary.
    CHECK_PARSE(&msg2, &msg, "test", 1);
    CHECK_NEXT_TYPE(&msg2.args, BINMSG_FLOAT_TYPE);
    CHECK_RET(binmsg_get_float(&msg2.args, &f));
    CHECK_EQ(f, 10.0);

    CHECK_RET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test"));
    CHECK_RET(binmsg_add_string(&msg.args, "foo"));
    CHECK_RET(binmsg_build_done(&msg));
    CHECK_BINMSG(msg, "MStest\0A\0\0\0\1Sfoo\0");
    CHECK_PARSE(&msg2, &msg, "test", 1);
    CHECK_NEXT_TYPE(&msg2.args, BINMSG_STRING_TYPE);
    CHECK_RET(binmsg_get_string(&msg2.args, &str));
    CHECK_EQ(strcmp(str, "foo"), 0);

    CHECK_RET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test"));
    CHECK_RET(binmsg_add_bool(&msg.args, 0));
    CHECK_RET(binmsg_add_bool(&msg.args, 1));
    CHECK_RET(binmsg_build_done(&msg));
    CHECK_BINMSG(msg, "MStest\0A\0\0\0\2B\0B\1");
    CHECK_PARSE(&msg2, &msg, "test", 2);
    CHECK_NEXT_TYPE(&msg2.args, BINMSG_BOOL_TYPE);
    CHECK_RET(binmsg_get_bool(&msg2.args, &b));
    CHECK_EQ(b, 0);
    CHECK_NEXT_TYPE(&msg2.args, BINMSG_BOOL_TYPE);
    CHECK_RET(binmsg_get_bool(&msg2.args, &b));
    CHECK_EQ(b, 1);

    CHECK_RET(binmsg_build(&msg, buffer, BINMSG_MAX_SIZE, "test"));
    CHECK_RET(binmsg_add_bool(&msg.args, 0));
    CHECK_RET(binmsg_add_array_begin(&msg.args, &nested));
    {
        CHECK_RET(binmsg_add_int(&nested, 3));
    }
    CHECK_RET(binmsg_add_array_end(&nested));
    CHECK_RET(binmsg_add_bool(&msg.args, 1));
    CHECK_RET(binmsg_build_done(&msg));
    CHECK_BINMSG(msg, "MStest\0A\0\0\0\3B\0A\0\0\0\1I\0\0\0\3B\1");

    CHECK_PARSE(&msg2, &msg, "test", 3);
    CHECK_NEXT_TYPE(&msg2.args, BINMSG_BOOL_TYPE);
    CHECK_RET(binmsg_get_bool(&msg2.args, &b));
    CHECK_EQ(b, 0);
    CHECK_NEXT_TYPE(&msg2.args, BINMSG_ARRAY_TYPE);
    CHECK_RET(binmsg_get_array(&msg2.args, &nested));
    CHECK_EQ(nested.size, 1);
    {
        CHECK_NEXT_TYPE(&nested, BINMSG_INT_TYPE);
        CHECK_RET(binmsg_get_int(&nested, &i));
        CHECK_EQ(i, 3);
    }
    CHECK_NEXT_TYPE(&msg2.args, BINMSG_BOOL_TYPE);
    CHECK_RET(binmsg_get_bool(&msg2.args, &b));
    CHECK_EQ(b, 1);

} END_TEST_CASE(TestBinMsg);

#endif // BUILD_TEST_CASES
