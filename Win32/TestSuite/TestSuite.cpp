#include "stdafx.h"
#include "ImlUnit.h"
#include <iostream.h>

extern void test_CryptStream (void);

int main (int argc, char **argv) {
	test_CryptStream();

	return tests_finished();
}

