// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef TestHarness_H
#define TestHarness_H

#include "TestCase.h"

class StageFrame;
class FancyStatusBar;

class TestHarness : public ITestProgressMeter
{
	static TestHarness *sInstance;

	StageFrame *mFrame;
	FancyStatusBar *mStatusBar;

public:
	static TestHarness *GetInstance();

	TestHarness();

	void UpdateTestProgress(int inTestIndex, int inTestCount,
							const TestCaseReport &inReport);

	void RunTests();

};

#endif // TestHarness_H
