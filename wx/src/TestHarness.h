// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef TestHarness_H
#define TestHarness_H

class TestHarness
{
	static TestHarness *sInstance;

public:
	static TestHarness *GetInstance();

	void RunTests();

};

#endif // TestHarness_H
