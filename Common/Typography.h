// -*- Mode: C++; tab-width: 4; -*-

#include <deque>

#include "ft2build.h"
#include FT_FREETYPE_H
#include FT_GLYPH_H

#include <wchar.h>

// TODO - Handle copy constructors, assignment operators

namespace Typography {

	//////////
	// A FreeType 2 error code, used in exceptions.
	typedef FT_Error ErrorCode;

	//////////
	// A FreeType 2 vector, used for kerning.
   	typedef FT_Vector Vector;

	//////////
	// A FreeType 2 character code, used to represent a 32-bit
	// Unicode character.
	typedef FT_ULong CharCode;

	//////////
	// A FreeType 2 glyph index.  A (face,character code) pair map
	// to a glyph index in FreeType 2.
	typedef FT_UInt GlyphIndex;

	//////////
	// A FreeType 2 glyph object.  This usually contains a bitmap and
	// a whole bunch of measurements.
	typedef FT_GlyphSlot Glyph;

	//////////
	// A distance, typically in pixels.  But some classes, including
	// GenericTextRenderingEngine, use it in a more abstract fashion.
	typedef signed long Distance;

	//////////
	// An 8-bit color channel.
	typedef unsigned char Channel;

	//////////
	// Names for a few special Unicode characters.
	enum Character {

		//////////
		// This space is treated like a letter for the purposes of
		// linebreaking.
		// TODO - Fix line-breaking code to support it.
		kNonBreakingSpace	  = 0x00A0,

		//////////
		// A soft hyphen is treated as an invisible hyphenation point.
		// There's a lot of controversy about exactly what this means:
		//	 * ISO Latin 1 allegedly feels that it should always be printed
		//	   as a hyphen, but only allows it to appear at the end of a line.
		//	 * Unicode allows this character anywhere, but only allows it to
		//	   be visible when it appears before an implicitly inserted
		//	   line break chosen by the rendering engine.
		// We compromise: We allow it anywhere, and we use it as a line-
		// breaking hint.  But if it is the last non-whitespace character
		// before *any* kind of line-break, we display it.
		kSoftHyphen			  = 0x00AD,

		//////////
		// Unicode replacement character (typically drawn as a box).
		kReplacementCharacter = 0xFFFD,

		//////////
		// This is slightly magic in our library--we always map it
		// to a FreeType GlyphIndex of 0 (which is FreeType's "no glyph
		// for the given CharCode" GlyphIndex), and any other CharCode
		// kerned against it returns (0, 0).  Basically, it's
		// guaranteed to be a CharCode equivalent of FreeType's
		// GlyphIndex '0'.
		kNoSuchCharacter	  = kReplacementCharacter
	};

	//////////
	// Justification values for line layout.
	enum Justification {
		kLeftJustification,
		kCenterJustification,
		kRightJustification
	};

	//////////
	// Per-character style flags.
	enum Style {
		kBold = 1,
		kItalic = 2,
		kUnderlined = 4,
		kShadowed = 8
	};

	//////////
	// A point.  See Distance for a description.
	struct Point {
		Distance x;
		Distance y;

		Point(Distance inX, Distance inY) : x(inX), y(inY) {}
	};

	//////////
	// An RGB color.
	struct Color {
		Channel red;
		Channel green;
		Channel blue;

		Color(Channel inRed, Channel inGreen, Channel inBlue)
			: red(inRed), green(inGreen), blue(inBlue) {}
	};

	//////////
	// A character with color and style information.
	struct StyledCharacter {
		CharCode c;
   		Color    color;
		Style    style;
	};

	//////////
	// A Typography-related exception.  Any of the functions in the
	// Typography module may throw exceptions (which is not the case
	// for the rest of the 5L code base, so be sure to catch them).
	class Error {
		ErrorCode mErrorCode;

	public:
		//////////
		// A generic, temporary error code representing a non-FreeType
		// error.  TODO - We need to provide more detail.
		enum { kOtherError = -1 };

		Error(ErrorCode inErrorCode) : mErrorCode(inErrorCode) {}
		
		ErrorCode GetErrorCode() { return mErrorCode; }

		//////////
		// Check the result of a FreeType function and throw an error
		// if necessary.
		static void CheckResult(ErrorCode inResultCode)
			{ if (inResultCode) throw Error(inResultCode); }
	};

	//////////
	// An instance of the FreeType 2 library's context.
	class Library {
	private:
		FT_Library mLibrary;
		
	public:
		Library();
		~Library();

		operator FT_Library() { return mLibrary; }
		operator FT_Library*() { return &mLibrary; }
	};
	
	//////////
	// An abstract typeface (with a specific size).
	//
	// Abstract typefaces know how to map character codes to glyphs,
	// kern two characters, and calculate the height of a line.
	// They don't know about GlyphIndex values or other low-level
	// abstractions.
	class AbstractFace {
		int mSize;
		
	public:
		AbstractFace(int inSize) : mSize(inSize) { }
		virtual ~AbstractFace() { }

		int GetSize() const { return mSize; }

		//////////
		// Load the glyph for a given character code.
		// You're free to use this pointer until the next call
		// to GetGlyph.  The face object retains ownership of the
		// pointer, so you shouldn't delete it.  If no glyph exists
		// for the specified character code, the face will return
		// a substitution character.
		//
		// For now, the glyph is always rendered to a pixmap.  This
		// may or may not change.
		virtual Glyph GetGlyph(CharCode inCharCode) = 0;

		//////////
		// Kern two character codes.  If either character code
		// is kNoSuchCharacter, this function will return (0,0).
		virtual Vector GetKerning(CharCode inPreviousChar,
								  CharCode inCurrentChar) = 0;

		//////////
		// Return a best guess for the appropriate distance between
		// two lines.  This relies on the font's data tables and
		// FreeType's drivers, so it might occasionally be a bit whacky.
		// (But it's all we've got.)
		virtual Distance GetLineHeight() = 0;
	};

	//////////
	// A simple typeface.
	//
	// A simple typeface is associated with a single FreeType 2 face
	// object.  It understands GlyphIndex values and other low-level
	// details of layout.
	class Face : public AbstractFace {
		FT_Face mFace;
		bool mHasKerning;

	public:
		Face(Library &inLibrary,
			 char *inFontFile, char *inMetricsFile,
			 int inSize);
		virtual ~Face();
		
		operator FT_Face() { return mFace; }
		operator FT_Face*() { return &mFace; }

		GlyphIndex GetGlyphIndex(CharCode inCharCode);
		Glyph GetGlyphFromGlyphIndex(GlyphIndex inGlyphIndex);

		Glyph GetGlyph(CharCode inCharCode);
		Vector GetKerning(CharCode inPreviousChar, CharCode inCurrentChar);

		Distance GetLineHeight();
	};

	//////////
	// A "stack" of simple typefaces.
	//
	// A FaceStack is used to provide more complete character sets than any
	// single typeface could provide alone.  We search through the faces
	// in each stack, beginning with the "primary" face, and then each
	// of the "secondary" faces.
	//
	// A typical use: Font FooSerif is a nice text face, but lacks a
	// delta character.  We can create a stack using FooSerif as a primary
	// face and Symbol as a second face, and the engine will do
	// The Right Thing<tm>.
	//
	// All faces in a a stack must be the same size.
	// TODO - Enforce sizes in a stack.
	//
	class FaceStack : public AbstractFace {
		// TODO - Figure out a good memory management scheme.
		std::deque<Face*> mFaceStack;

	public:
		FaceStack(Face *inPrimaryFace);
		virtual ~FaceStack();

		//////////
		// Add another face to the stack.  Faces are searched
		// in the order they are added.
		void AddSecondaryFace(Face *inFace);

		virtual Glyph GetGlyph(CharCode inCharCode);
		virtual Vector GetKerning(CharCode inPreviousChar,
								  CharCode inCurrentChar);

		virtual Distance GetLineHeight();

	private:
		//////////
		// Walk though the stack, looking a face with an appropriate
		// glyph for the specified character.
		//
		// [in] inCharCode - The character we want to find.
		// [out] outFace - A face with an appropriate glyph.
		// [out] outGlyphIndex - The index of the glyph in that face.
		void SearchForCharacter(CharCode inCharCode,
								Face **outFace,
								GlyphIndex *outGlyphIndex);
	};

	//////////
	// A segment of a line of characters, suitable for drawing as a group.
	//
	// The line-breaking routines all work in terms of line segments
	// (because they don't want to know about the rules for breaking up
	// a line into words).
	//
	// Some interesting invariants:
	//   * A line segment is always non-empty.
	//   * A line segment will never contain a soft hyphen.
	// If you change these invariants, be prepared to face assertion
	// failures and debugging fun.
	//
	// TODO - Turn this struct into a well-encapsulated class; it's
	// becoming too complex to be a struct.
	//
	struct LineSegment {

		//////////
		// A pointer to the first character in the line segment.
		const wchar_t *begin;

		//////////
		// A pointer one character *beyond* the last character in
		// the line segment, as per STL iterator conventions.
		const wchar_t *end;

		//////////
		// Is the current segment a newline character?  If so, the
		// segment contains no displayable data.
		bool isLineBreak;

		//////////
		// Should the segment be discarded at the end of a line?
		// This is typically true for whitespace.
		// TODO - This is used as a 'isHorizontalWhitespace' flag,
		// so we should probably rename it.
		bool discardAtEndOfLine;

		//////////
		// If this segment is the last on a line, do we need to draw a
		// hyphen?  Typically true for segments preceding a soft hyphen,
		// or segments which were automatically broken by the library.
		bool needsHyphenAtEndOfLine;

		// Used by various algorithms to temporarily store data.
		// XXX - Clean up.
		Distance userDistanceData;

		void SetLineSegment(const wchar_t *inBegin, const wchar_t *inEnd,
							bool inIsLineBreak = false,
							bool inDiscardAtEndOfLine = false,
							bool inNeedsHyphenAtEndOfLine = false)
		{
			begin				   = inBegin;
			end					   = inEnd;
			isLineBreak			   = inIsLineBreak;
			discardAtEndOfLine	   = inDiscardAtEndOfLine;
			needsHyphenAtEndOfLine = inNeedsHyphenAtEndOfLine;
			userDistanceData	   = 0;
		}

		LineSegment(const wchar_t *inBegin, const wchar_t *inEnd,
					bool inIsLineBreak = false,
					bool inDiscardAtEndOfLine = false,
					bool inNeedsHyphenAtEndOfLine = false)
		{
			SetLineSegment(inBegin, inEnd, inIsLineBreak,
						   inDiscardAtEndOfLine, inNeedsHyphenAtEndOfLine);
		}		

		LineSegment() { }
	};

	extern bool operator==(const LineSegment &left, const LineSegment &right);

	//////////
	// An iterator which will break a line into LineSegment objects
	// according to typical typographic rules.  It does not modify the
	// underlying text.
	class LineSegmentIterator {
		const wchar_t *mSegmentBegin;
		const wchar_t *mTextEnd;

	public:
		//////////
		// Create a new iterator.
		//
		// [in] inTextBegin - A pointer to the start of the text.
		// [in] inTextEnd - A pointer one past the end of the text.
		LineSegmentIterator(const wchar_t *inTextBegin,
							const wchar_t *inTextEnd);

		//////////
		// Return the next segment of the line, if any.
		//
		// [out] outSegment - The segment we found, or unchanged.
		// [out] return - True iff we found another segment.
		bool NextElement(LineSegment *outSegment);
	};

	//////////
	// Display-independent code to transform text into a multi-line
	// paragraph.
	//
	// GenericTextRenderingEngine is an abstract class; subclasses
	// must provide support for measuring LineSegment objects,
	// forcibly breaking segments longer than a line, and drawing
	// all the segments on a line.
	//
	// The 'Distance' values used by this class are abstract--they
	// might be pixels, or they might be simple character counts.
	// The GenericTextRenderingEngine doesn't care.  (You could use
	// it to line-break and justify character strings, and the
	// test suites actually do so.)
	//
	class GenericTextRenderingEngine {
	private:
		LineSegmentIterator mIterator;
		Distance mLineLength;
		Justification mJustification;

	protected:
		//////////
		// Create a new GenericTextRenderingEngine.
		// 
		// [in] inTextBegin - A pointer to the start of the text.
		// [in] inTextEnd - A pointer one past the end of the text.
		// [in] inLineLength - Maximum allowable line length.
		// [in] inJustification - Justification for the line.
		//
		GenericTextRenderingEngine(const wchar_t *inTextBegin,
								   const wchar_t *inTextEnd,
								   Distance inLineLength,
								   Justification inJustification);

		virtual ~GenericTextRenderingEngine() {}
		
		Distance GetLineLength() { return mLineLength; }
		Justification GetJustification() { return mJustification; }

		//////////
		// Subclasses must override this method to provide measurements
		// of segments.  If there is a previous segment, it will be
		// supplied so intersegment kerning may be calculated (if the
		// subclass so desires).  Measurements for a given segment are
		// allowed to differ, depending on whether or not the segment
		// appears at the end of a line.  (This is useful for handling
		// the LineSegment::needsHyphenAtEndOfLine field.)
		// 
		// [in] inPrevious -    The previous segment (for kerning), or NULL.
		// [in] inSegment -     The segment to measure.
		// [in] inAtEndOfLine - Should measurements assume this is the last
		//                      segment on the line?
		//
		virtual Distance MeasureSegment(LineSegment *inPrevious,
										LineSegment *inSegment,
										bool inAtEndOfLine) = 0;

		//////////
		// Subclasses must override this method to forcibly extract a
		// line's worth of data from the front of a segment.  Subclasses
		// are welcome to throw exceptions if this process fails.
		// 
		// [in/out] ioRemaining - On input, the segment from which to
		//                        extract a line.  On output, whatever
		//                        is left over after extraction.  (Remember,
		//                        segments must be non-empty!)
		// [out] outExtracted -   A segment that fits on one line.
		//
		virtual void ExtractOneLine(LineSegment *ioRemaining,
									LineSegment *outExtracted) = 0;

		//////////
		// Subclasses must override this method to actually display a
		// line.
		//
		// [in] inLine - A list of segments to display.
		// [in] inHorizontalOffset - The distance to indent this line.
		//
		virtual void RenderLine(std::deque<LineSegment> *inLine,
								Distance inHorizontalOffset) = 0;

	public:
		//////////
		// Actually draw the text.  This method may only be called
		// once.  (Should we enforce and/or fix this?)
		void RenderText();

	private:
		//////////
		// Given the space used by a line, calculate the appropriate
		// amount to indent the line to acheive the desired justification.
		Distance CalculateHorizontalOffset(Distance inSpaceUsed);

		//////////
		// Internal routine which calculates justification, calls
		// RenderLine, and removes all the segments from ioLine.
		void RenderAndResetLine(std::deque<LineSegment> *ioLine);
	};

	//////////
	// A real rendering engine which uses real fonts.
	//
	// We subclass GenericTextRenderingEngine, and provide support for
	// AbstractFaces, drawing positions, and output to bitmaps.  (We must
	// be further subclassed to implement the DrawBitmap method.)
	//
	// We assume that all Distance and Point values are measured in pixels.
	// 
	class TextRenderingEngine : public GenericTextRenderingEngine {
		AbstractFace *mFace;
		Point mLineStart;

	public:
		//////////
		// Create a new text rendering engine.
		//
		// [in] inTextBegin -  A pointer to the beginning of the text.
		// [in] inTextEnd -    A pointer one past the end of the text.
		// [in] inFace -       The face in which to display text.
		// [in] inPosition -   The x,y position of the lower-left corner
		//                     of the first character (actually, this
		//                     is technically the "origin" of the first
		//                     character in FreeType 2 terminology).
		// [in] inLineLength - The maximum number of pixels available for
		//                     a line.  This is (I hope) a hard limit.
		// [in] inJustification - The desired justification.
		//
		TextRenderingEngine(const wchar_t *inTextBegin,
							const wchar_t *inTextEnd,
							AbstractFace *inFace,
							Point inPosition,
							Distance inLineLength,
							Justification inJustification);

	protected:
		//////////
		// Subclasses must override this method to draw a bitmap to their
		// actual output device.
		//
		// [in] inBitmap -   The bitmap to draw (in FreeType 2 format).
		// [in] inPosition - The location at which to draw the bitmap.
		//
		virtual void DrawBitmap(FT_Bitmap *inBitmap, Point inPosition) = 0;

	protected:
		virtual Distance MeasureSegment(LineSegment *inPrevious,
										LineSegment *inSegment,
										bool inAtEndOfLine);

		virtual void ExtractOneLine(LineSegment *ioRemaining,
									LineSegment *outExtracted);

		virtual void RenderLine(std::deque<LineSegment> *inLine,
			                    Distance inHorizontalOffset);
    };
}
