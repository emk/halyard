// -*- Mode: C++; tab-width: 4; -*-

#include <deque>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#include <wchar.h>

// TODO - Handle copy constructors, assignment operators

namespace Typography {

	typedef FT_Error ErrorCode;
	typedef FT_Vector Vector;
	typedef FT_ULong CharCode;
	typedef FT_UInt GlyphIndex;
	typedef FT_GlyphSlot Glyph;
	typedef signed long Distance;

	enum Character {
		// This space is treated like a letter for the purposes of
		// linebreaking.
		// TODO - Fix line-breaking code to support it.
		kNonBreakingSpace	  = 0x00A0,

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

		// Unicode replacement character (typically drawn as a box).
		kReplacementCharacter = 0xFFFD,

		// This is slightly magic in our library--we always map it
		// to a FreeType GlyphIndex of 0 (which is FreeType's "no glyph
		// for the given CharCode" GlyphIndex), and any other CharCode
		// kerned against it returns (0, 0) kerning.  Basically, it's
		// guaranteed to be a CharCode equivalent of FreeType's
		// GlyphIndex '0'.
		kNoSuchCharacter	  = kReplacementCharacter
	};

	enum Justification {
		kLeftJustification,
		kCenterJustification,
		kRightJustification
	};

	struct Point {
		Distance x;
		Distance y;

		Point(Distance inX, Distance inY) : x(inX), y(inY) {}
	};

	class Error {
		ErrorCode mErrorCode;

	public:
		static const ErrorCode kOtherError = -1;

		Error(ErrorCode inErrorCode) : mErrorCode(inErrorCode) {}
		
		ErrorCode GetErrorCode() { return mErrorCode; }

		static void CheckResult(ErrorCode inResultCode)
			{ if (inResultCode) throw Error(inResultCode); }
	};

	class Library {
	private:
		FT_Library mLibrary;
		
	public:
		Library();
		~Library();

		operator FT_Library() { return mLibrary; }
		operator FT_Library*() { return &mLibrary; }
	};
	
	class AbstractFace {
		int mSize;
		
	public:
		AbstractFace(int inSize) : mSize(inSize) { }
		virtual ~AbstractFace() { }

		int GetSize() const { return mSize; }

		virtual Glyph GetGlyph(CharCode inCharCode) = 0;
		virtual Vector GetKerning(CharCode inPreviousChar,
								  CharCode inCurrentChar) = 0;

		virtual Distance GetLineHeight() = 0;
	};

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

	class FaceStack : public AbstractFace {
		// TODO - Figure out a good memory management scheme.
		deque<Face*> mFaceStack;

	public:
		FaceStack(Face *inPrimaryFace);
		virtual ~FaceStack();

		// Add another face to the stack.  Faces are searched
		// in the order they are added.
		void AddSecondaryFace(Face *inFace);

		virtual Glyph GetGlyph(CharCode inCharCode);
		virtual Vector GetKerning(CharCode inPreviousChar,
								  CharCode inCurrentChar);

		virtual Distance GetLineHeight();

	private:
		void SearchForCharacter(CharCode inCharCode,
								Face **outFace,
								GlyphIndex *outGlyphIndex);
	};

	struct LineSegment {
		const wchar_t *begin;
		const wchar_t *end;

		bool isLineBreak;
		bool discardAtEndOfLine;
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

	class LineSegmentIterator {
		const wchar_t *mSegmentBegin;
		const wchar_t *mTextEnd;

	public:
		LineSegmentIterator(const wchar_t *inTextBegin,
							const wchar_t *inTextEnd);
		bool NextElement(LineSegment *outSegment);
	};

	class GenericTextRenderingEngine {
	private:
		LineSegmentIterator mIterator;
		Distance mLineLength;
		Justification mJustification;

	protected:
		GenericTextRenderingEngine(const wchar_t *inTextBegin,
								   const wchar_t *inTextEnd,
								   Distance inLineLength,
								   Justification inJustification);
		virtual ~GenericTextRenderingEngine() {}
		
		Distance GetLineLength() { return mLineLength; }
		Justification GetJustification() { return mJustification; }

		virtual Distance MeasureSegment(LineSegment *inPrevious,
										LineSegment *inSegment,
										bool inAtEndOfLine) = 0;

		virtual void ExtractOneLine(LineSegment *ioRemaining,
									LineSegment *outExtracted) = 0;

		virtual void RenderLine(std::deque<LineSegment> *inLine,
								Distance inHorizontalOffset) = 0;

	public:
		void RenderText();

	private:
		Distance CalculateHorizontalOffset(Distance inSpaceUsed);
		void RenderAndResetLine(std::deque<LineSegment> *ioLine);
	};

	class TextRenderingEngine : public GenericTextRenderingEngine {
		AbstractFace *mFace;
		Point mLineStart;

	public:
		TextRenderingEngine(const wchar_t *inTextBegin,
							const wchar_t *inTextEnd,
							AbstractFace *inFace,
							Point inPosition,
							Distance inLineLength,
							Justification inJustification);

	protected:
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
