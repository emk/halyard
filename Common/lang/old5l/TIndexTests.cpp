// -*- Mode: C++; tab-width: 4; -*-

#include "ImlUnit.h"
#include "TCommon.h"
#include "TIndex.h"
#include "TParser.h"

USING_NAMESPACE_FIVEL

extern void test_TIndex (void);

// A helper class to test the index-management code.
class TestIndexManager : public TIndexManager {
public:
    TestIndexManager() {}

    void MakeNewIndex(TIndexFile *inFile, const char *inName,
					  int32 inStart, int32 inEnd)
    {
		Add(new TIndex(inFile, inName, inStart, inEnd));
    }
};

#define TEST_INDEX(NAME,VALUE) \
	do { \
	    TIndex *node = (TIndex*) test_manager.Find(NAME); \
	    TEST(node != NULL); \
	    node->SetScript(); \
	    TEST(std::string(node->GetScript()) == VALUE); \
	} while (0)    

void test_TIndex (void)
{
    // Install support for top-level forms of type "test".
    TestIndexManager test_manager;
    TParser::RegisterIndexManager("test", &test_manager);

    // Parse our index file.
    gIndexFileManager.NewIndex("test");

    // Check the values of our various tests.
	TEST_INDEX("one", "(test one 123)");
	TEST_INDEX("two", "(test two 456)");
	TEST_INDEX("three", "(test three (\\#1 is first!))");
}
