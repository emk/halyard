#include "CStream.h"
#include "ImlUnit.h"
#include "CVariable.h"

USING_NAMESPACE_FIVEL

extern void test_CStream (void);


//=========================================================================
//  Support Code
//=========================================================================
//  These functions and variables are required to run the tests.

static CStream& sample_callback(CStream &stream)
{
	TEST(stream.curchar() == 'a');
	TEST(stream.nextchar() == '(');
	TString temp;
	stream >> temp;
	TEST(temp == "bc de");
	return stream;
}


//=========================================================================
//  test_CStream
//=========================================================================

void test_CStream (void) 
{	
	// A simple test.
	CStream s1 = "abc \t def (ghi jkl mno) pqr";
	TEST(s1.curchar() == 'a');
	TEST(s1.nextchar() == 'b');
	TEST(s1.curchar() == 'b');
	TEST(s1.prevchar() == 'a');
	TEST(s1.GetPos() == 1);
	int startpos = s1.GetPos();
	s1.scanword();
	TEST(s1.copystr(startpos, s1.GetPos() - startpos) == "bc");
	s1.skipwhite();
	TEST(s1.GetPos() == 6);
	TString temp;
	s1 >> temp;
	TEST(temp == "def");
	TEST(!s1.eof());
	TEST(s1.more());
	s1 >> temp;
	TEST(temp == "ghi jkl mno");
	s1 >> temp;
	TEST(temp == "pqr");
	TEST(s1.eof());
	TEST(!s1.more());

	// Input formats.
	// TODO - Test all operator >> here.
	
	s1 = "abc 32767 -32768 2147483647 -2147483648 0.5";
	TString temp2;
	s1 >> temp2;
	TEST(temp2 == "abc");
	int16 temp3;
	s1 >> temp3;
	TEST(temp3 == 32767);
	s1 >> temp3;
	TEST(temp3 == -32768);
	int32 temp4;
	s1 >> temp4;
	TEST(temp4 == 2147483647);
	s1 >> temp4;
	TEST(temp4 == -2147483648);
	double temp5;
	s1 >> temp5;
	TEST(temp5 == 0.500000);
	
	// Input using callback function.
	s1 = "a(bc de)f";
	s1 >> &sample_callback;
	s1 >> temp;
	TEST(temp == "f");
	
	// Input using TRect and TPoint
	s1 = "0 1 2 3 4 5";
	TRect temp6;
	s1 >> temp6;
	TEST(temp6.Left() == 0);
	TEST(temp6.Top() == 1);
	TEST(temp6.Right() == 2);
	TEST(temp6.Bottom() == 3);
	TPoint temp7;
	s1 >> temp7;
	TEST(temp7.X() == 4);
	TEST(temp7.Y() == 5);
	
	// TODO - Really tricky, nasty escaping tests.
	s1 = "abc\\(";
	s1 >> temp;
	TEST(temp == "abc\\(");
	s1 = "abc\\\\(abc \\) def ) #56\na \\#56 1\\)2\\)3\\)";
	s1 >> temp;
	TEST(temp == "abc\\\\");
	s1 >> temp;
	TEST(temp == "abc \\) def");
	s1 >> temp;
	TEST(temp == "a");
	s1 >> temp;
	TEST(temp == "\\#56");
	s1 >> temp;
	TEST(temp == "1\\)2\\)3\\)");
	
	gVariableManager.SetString("myVar", "Hello World");
	s1 = "\\$myVar$  \\\\$myVar$";
	s1 >> temp;
	TEST(temp == "\\$myVar0");
	s1 >> temp;
	TEST(temp == "\\\\Hello World");
	
	// TODO - Test open, close, discard
	// TODO - Test scanopen, scanclose, discard (different from above)
	s1 = "abc def (jkl\\)) mno \\(p (a \\( () )a";
	startpos = s1.GetPos();
	open(s1);
	TEST(s1.copystr(startpos, s1.GetPos() - startpos) == "abc def (");	
	startpos = s1.GetPos();
	close(s1);
	TEST(s1.copystr(startpos, s1.GetPos() - startpos) == "jkl\\))");	
	startpos = s1.GetPos();
	discard(s1);
	TEST(s1.copystr(startpos, s1.GetPos() - startpos) == " mno");
	startpos = s1.GetPos();
	s1.scanopen();
	TEST(s1.copystr(startpos, s1.GetPos() - startpos) == " \\(p (");
	startpos = s1.GetPos();
	s1.scanclose();
	TEST(s1.copystr(startpos, s1.GetPos() - startpos) == "a \\( () )");
	startpos = s1.GetPos();
	s1.discard();
	TEST(s1.copystr(startpos, s1.GetPos() - startpos) == "a");
	
	
	//TODO - Test inEscape briefly (thorough testing in above tricky escape cases)
	s1 = "\\\\ \\((\\\\(";
	TEST(!s1.inEscape(0));
	TEST(s1.inEscape(1));
	TEST(!s1.inEscape(2));
	TEST(!s1.inEscape(3));
	TEST(s1.inEscape(4));
	TEST(!s1.inEscape(5));
	TEST(!s1.inEscape(6));
	TEST(s1.inEscape(7));
	TEST(!s1.inEscape(8));
	s1.reset();
	TEST(!s1.inEscape());
	s1.nextchar();
	TEST(s1.inEscape());


	// TODO - Demonstrate potential whitespace issues with CStream
	// Although the above example work as we would expect, the whitespace
	// stripping and leaving is NOT consistent or seemingly rational
	// The examples below are included to demonstrate the weird behavior. 
	// In other words, these tests indicate that although functional,
	// the skipping of white space is certainly BROKEN!!!
	// ANY CHANGING OF THE CODE SHOULD PASS ALL ABOVE TEST CASES
	// However, failure to pass the test cases below is not NECESSARILY bad,
	// if the actual strings in temp follow a consistent and rational pattern.
	
	s1 = "abc (def ghi)( jkl mno)((pqr ))((stu ) )((vw) )     xyz\\( ()";
	s1 >> temp;
	TEST(temp == "abc");
	s1 >> temp;
	TEST(temp == "def ghi");	// note no addition of spaces before d (see below)
	s1 >> temp;
	TEST(temp == "  jkl mno");	// note the two spaces - addition of first is from??
	s1 >> temp;
	TEST(temp == " ( pqr)");	// note that a space is in front of both the parens and inside
								// although neither were there originally 
								// note also that the space at the end has been stripped 
	s1 >> temp;
	TEST(temp == " ( stu)  ");	// note also that the space after stu is stripped
								// but two spaces after the parens are there instead of the one
								// that was put in
	s1 >> temp;
	TEST(temp == " ( vw)  ");	// note again we have two spaces after the parens instead of one
								// clearly this is not due to any spaces before parens						
	s1 >> temp;
	TEST(temp == "xyz\\(");    	// note that space at the beginning is not kept unless in parens
								// as in jkl mno case
}
