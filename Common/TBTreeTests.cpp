// -*- Mode: C++; tab-width: 4; -*-

#include "TBTree.h"
#include "ImlUnit.h"

USING_NAMESPACE_FIVEL

extern void test_TBTree (void);

void test_TBTree (void)
{
    // Test our node constructors.
    TBNode n1("n1");
    TEST(strcmp(n1.Key(), "n1") == 0);
    TBNode n2(TString("n2"));
    TEST(strcmp(n2.Key(), "n2") == 0);

    // Build a simple tree.
    TBTree t1;
    TBNode *t1n1 = new TBNode("t1n1");
    TBNode *t1n2 = new TBNode("t1n2");
    t1.Add(t1n1);
    t1.Add(t1n2);
    TEST(t1.Find("t1n1") == t1n1);
    TEST(t1.Find("t1n2") == t1n2);
    TEST(t1.Find("t1n3") == NULL);
    TEST(t1.Find("t0n3") == NULL);
    
    // Try removing a node.
    t1.RemoveIfExists("t1n2");
    TEST(t1.Find("t1n1") == t1n1);
    TEST(t1.Find("t1n2") == NULL);
    TEST(t1.Find("t1n3") == NULL);
    TEST(t1.Find("t0n3") == NULL);

    // Try removing the root node.
    t1.RemoveIfExists("t1n1");
    TEST(t1.Find("t1n1") == NULL);
    TEST(t1.Find("t1n2") == NULL);
    TEST(t1.Find("t1n3") == NULL);
    TEST(t1.Find("t0n3") == NULL);

    // Try removing a node which has already been removed.
    t1.RemoveIfExists("t1n1");
    TEST(t1.Find("t1n1") == NULL);
    TEST(t1.Find("t1n2") == NULL);
    TEST(t1.Find("t1n3") == NULL);
    TEST(t1.Find("t0n3") == NULL);
}
