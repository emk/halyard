// -*- Mode: C++; tab-width: 4; -*-

#include "TCommon.h"
#include "TStream.h"
#include "ImlUnit.h"
#include "TVariable.h"

USING_NAMESPACE_FIVEL

extern void test_TStream (void);


//=========================================================================
//  Support Code
//=========================================================================
//  These functions and variables are required to run the tests.

static TStream& sample_callback(TStream &stream)
{
	TEST(stream.curchar() == 'a');
	TEST(stream.nextchar() == '(');
	TString temp;
	stream >> temp;
	TEST(temp == "bc de");
	return stream;
}


//=========================================================================
//  test_TStream
//=========================================================================

void test_TStream (void) 
{	
	// A simple test.
	TStream s1 = "abc \t def (ghi jkl mno) pqr";
	TEST(s1.curchar() == 'a');
	TEST(s1.nextchar() == 'b');
	TEST(s1.curchar() == 'b');
	TEST(s1.prevchar() == 'a');
	TEST(s1.GetPos() == 1);
	int startpos = s1.GetPos();
	s1.scanword();
	TEST(s1.ExpandVariables(startpos, s1.GetPos() - startpos) == "bc");
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
	
	s1 = "abc foo 32767 -32768 2147483647 -2147483648 0.5 0xFFee0080";
	TString temp2;
	s1 >> temp2;
	TEST(temp2 == "abc");
	std::string temp_std_string;
	s1 >> temp_std_string;
	TEST(temp_std_string == "foo");
	int16 temp3;
	s1 >> temp3;
	TEST(temp3 == 32767);
	s1 >> temp3;
	TEST(temp3 == -32768);
	int32 temp4;
	s1 >> temp4;
	TEST(temp4 == 2147483647);
	s1 >> temp4;
	TEST(temp4 == (-2147483647)-1);
	double temp5;
	s1 >> temp5;
	TEST(temp5 == 0.500000);
	GraphicsTools::Color temp_color;
	s1 >> temp_color;
	TEST(temp_color == GraphicsTools::Color(0xFF, 0xEE, 0x00, 0x80));

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
	
	// Really tricky, nasty escaping tests.
	s1 = "abc\\(";
	s1 >> temp;
	TEST(temp == "abc\\(");
	s1 = "abc\\\\(abc \\) def ) #56\na \\#56 1\\)2\\)3\\)";
	s1 >> temp;
	TEST(temp == "abc\\\\");
	s1 >> temp;
	TEST(temp == "abc \\) def ");
	s1 >> temp;
	TEST(temp == "a");
	s1 >> temp;
	TEST(temp == "\\#56");
	s1 >> temp;
	TEST(temp == "1\\)2\\)3\\)");
	
	// Test variable interpolation.
	gVariableManager.SetString("myVar", "Hello World");
	s1 = "\\$myVar$  \\\\$myVar$";
	s1 >> temp;
	TEST(temp == "\\$myVar0");
	s1 >> temp;
	TEST(temp == "\\\\Hello World");
	s1 = "\\$ \\$";
	s1 >> temp;
	TEST(temp == "\\$");
	s1 >> temp;
	TEST(temp == "\\$");
	
	// Test open, close, discard
	// Test scanopen, scanclose, discard (different from above)
	s1 = "abc def (jkl\\)) mno \\(p (a \\( () )a";
	startpos = s1.GetPos();
	open(s1);
	TEST(s1.ExpandVariables(startpos, s1.GetPos() - startpos) == "abc def (");	
	startpos = s1.GetPos();
	close(s1);
	TEST(s1.ExpandVariables(startpos, s1.GetPos() - startpos) == "jkl\\))");	
	startpos = s1.GetPos();
	discard(s1);
	TEST(s1.ExpandVariables(startpos, s1.GetPos() - startpos) == " mno");
	startpos = s1.GetPos();
	s1.scanopen();
	TEST(s1.ExpandVariables(startpos, s1.GetPos() - startpos) == " \\(p (");
	startpos = s1.GetPos();
	s1.scanclose();
	TEST(s1.ExpandVariables(startpos, s1.GetPos() - startpos) == "a \\( () )");
	startpos = s1.GetPos();
	s1.discard();
	TEST(s1.ExpandVariables(startpos, s1.GetPos() - startpos) == "a");
	
	// Test input of percentages.
	// It's fairly important that these round the same way on
	// every platform (0.5 rounds away from 0), because these
	// affect leading calculations, which are supposed to be
	// the same everywhere.
	s1 = "3 4 (pcent 10) (pcent 20) (pcent 15) (pcent -15)";
	int32 result;
	s1 >> ValueOrPercent(10, &result);
	TEST(result == 3);
	s1 >> ValueOrPercent(20, &result);
	TEST(result == 4);
	s1 >> ValueOrPercent(10, &result);
	TEST(result == 1);
	s1 >> ValueOrPercent(20, &result);
	TEST(result == 4);
	s1 >> ValueOrPercent(10, &result);
	TEST(result == 2);
	s1 >> ValueOrPercent(10, &result);
	TEST(result == -2);
	
	// Test inEscape briefly (thorough testing in above tricky
	// escape cases)
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

	// Require whitespace handling to be sane and rational, unlike the
	// old code, which randomly inserted and removed spaces.
	s1 = "abc (def ghi)( jkl mno)((pqr ))((stu ) )((vw) )     xyz\\( ()";
	s1 >> temp;
	TEST(temp == "abc");
	s1 >> temp;
	TEST(temp == "def ghi");
	s1 >> temp;
	TEST(temp == " jkl mno");
	s1 >> temp;
	TEST(temp == "(pqr )");	
	s1 >> temp;
	TEST(temp == "(stu ) ");
	s1 >> temp;
	TEST(temp == "(vw) ");
	s1 >> temp;
	TEST(temp == "xyz\\(");
	s1 >> temp;
	TEST(temp == "");
}
