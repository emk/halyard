// -*- Mode: C++; tab-width: 4; -*-

#ifndef Typography_H
#define Typography_H

#include "TCommon.h"

#include <string>
#include <iostream>
#include <string>
#include <deque>
#include <list>
#include <map>

#include "ft2build.h"
#include FT_FREETYPE_H
#include FT_GLYPH_H

#include <wchar.h>

#include "TException.h"
#include "FileSystem.h"
#include "GraphicsTools.h"

// TODO - Handle copy constructors, assignment operators

namespace Typography {

	using GraphicsTools::Distance;
	using GraphicsTools::Point;
	using GraphicsTools::Image;
	using GraphicsTools::Color;
	using GraphicsTools::PixMap;
	using GraphicsTools::GreyMap;

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
	// A face style.  This is combined with a face, colors, and other
	// information to make a full-fledged Style.
	//
	enum /* FaceStyle */ {
		// These styles are directly supported by the FamilyDatabase.
		kRegularFaceStyle = 0,
		kBoldFaceStyle = 1,
		kItalicFaceStyle = 2,
		kBoldItalicFaceStyle = kBoldFaceStyle | kItalicFaceStyle,
		kIntrisicFaceStyles = kBoldFaceStyle | kItalicFaceStyle,

		// These styles are implemented manually.
		kUnderlineFaceStyle = 4,
		kShadowFaceStyle = 8
	};

	//////////
	// 'FaceStyle' is an integer, not an enumeration, so we can do
	// bitwise operations on FaceStyles under picky C++ compilers.
	//
	typedef int FaceStyle;

	//////////
	// A Typography-related exception.  Any of the functions in the
	// Typography module may throw exceptions (which is not the case
	// for the rest of the 5L code base, so be sure to catch them).
	//
	class Error : public FIVEL_NS TException {
	public:
		explicit Error(const char *inFile, int inLine, int inErrorCode);
		explicit Error(const char *inFile, int inLine,
					   const std::string &inErrorMessage)
			: TException(inFile, inLine, inErrorMessage) {}
		
		virtual const char *GetClassName() const
		    { return "Typography::Error"; }

		//////////
		// Check the result of a FreeType function and throw an error
		// if necessary.
		//
		static void CheckResult(const char *inFile, int inLine,
								int inResultCode)
			{ if (inResultCode) throw Error(inFile, inLine, inResultCode); }
	};

	//////////
	// An instance of the FreeType 2 library's context.
	//
	class Library {
	private:
		FT_Library mLibrary;
		static Library *sLibrary;
		
	public:
		Library();
		~Library();

		operator FT_Library() { return mLibrary; }
		operator FT_Library*() { return &mLibrary; }

		static Library *GetLibrary();
	};

	//////////
	// An individual, rendered glyph.  This is basically a copy of
	// all the information we need from a FT_GlyphSlot, wrapped
	// behind a nice interface so we can cache it.
	//
	class Glyph {
		FT_Vector mAdvance;
		FT_Glyph_Metrics mMetrics;
		Point mGreyMapOffset;
		GreyMap mGreyMap;

	public:
		Glyph(FT_GlyphSlot inGlyph);

		FT_Vector GetAdvance() const { return mAdvance; }
		const FT_Glyph_Metrics *GetMetrics() const { return &mMetrics; }
		Point GetGreyMapOffset() const { return mGreyMapOffset; }
		const GreyMap *GetGreyMap() const { return &mGreyMap; }
	};

	class AbstractFace;
	class FaceStack;
	
	//////////
	// A style.
	//
	// This class must be quick to copy and efficient to store, so
	// we use an internal reference-counted 'rep' class.
	//
	class Style {
		struct StyleRep {
			int         mRefCount;

			// If you any fields here, be sure to update operator==.
			std::string mFamily;
			std::list<std::string> mBackupFamilies;
			FaceStyle   mFaceStyle;
			int         mSize;
			Distance    mLeading;
			Distance    mShadowOffset;
			Color       mColor;
			Color       mShadowColor;
			
			FaceStack   *mFace;
		};

		//////////
		// A pointer to our representation's data.
		//
		StyleRep *mRep;

		//////////
		// Make sure we don't share this representation with anybody else.
		//
		void Grab();

		//////////
		// Invalidate the cached mFace value.
		//
		void InvalidateFace();

	public:
		Style(const std::string &inFamily, int inSize);
		Style(const Style &inStyle);
		~Style();

		Style &operator=(const Style &inStyle);

		bool operator==(const Style &inStyle) const;

		//////////
		// Get the font family.  e.g., "Times", "Nimbus Roman No9 L".
		//
		std::string GetFamily() const { return mRep->mFamily; }
		Style &SetFamily(const std::string &inFamily);

		//////////
		// Get other families to use when performing font substitution.
		//
		std::list<std::string> GetBackupFamilies() const
		    { return mRep->mBackupFamilies; }
		Style &SetBackupFamilies(const std::list<std::string> &inBFs);

		//////////
		// Get the style flags for this face.
		//
		FaceStyle GetFaceStyle() const { return mRep->mFaceStyle; }
		Style &SetFaceStyle(FaceStyle inFaceStyle);

		//////////
		// Toggle the values of the specified face flags.
		//
		Style &ToggleFaceStyle(FaceStyle inToggleFlags);

		//////////
		// Get the size of the font, in points.
		//
		int GetSize() const { return mRep->mSize; }
		Style &SetSize(int inSize);

		//////////
		// Get the size of the font, in points.
		//
		Distance GetLeading() const { return mRep->mLeading; }
		Style &SetLeading(Distance inLeading);

		//////////
		// Get the offset used to draw shadows.
		//
		Distance GetShadowOffset () const { return mRep->mShadowOffset; }
		Style &SetShadowOffset(Distance inOffset);

		//////////
		// Get the color used to draw text.
		//
		Style &SetColor(Color inColor);
		Color GetColor() const { return mRep->mColor; }

		//////////
		// Get the color used to draw drop-shadows.
		//
		Style &SetShadowColor(Color inColor);
		Color GetShadowColor() const { return mRep->mShadowColor; }

		// Drawing-related information.
		// TODO - Document.
		AbstractFace *GetFace() const;
		bool          GetIsUnderlined() const;
		bool          GetIsShadowed() const;
		Distance      GetLineHeight(bool isFirstLine = false) const;
		Distance      GetDescender() const;
	};

	//////////
	// A chunk of text with style information.
	//
	class StyledText {

		Style mBaseStyle;
		std::wstring mText;
		std::map<size_t,Style> mStyleRuns;
		bool mIsBuilt;
		size_t mEnd;

	public:
		//////////
		// Create a new StyledText object.
		//
		// [in] inBaseStyle - The style to use for the first character.
		//
		explicit StyledText(const Style &inBaseStyle);

		//////////
		// Add text to the end of the styled text object.
		// 
		void AppendText(const std::wstring &inText);

		//////////
		// Add text to the end of the styled text object.
		// 
		void AppendText(wchar_t inText);

		//////////
		// Change the Style at the current offset in the string.
		//
		// [in] inStyle -  The new style to use.
		//
		void ChangeStyle(const Style &inStyle);

		//////////
		// Stop adding style changes and text, and freeze the StyledText.
		//
		void EndConstruction();

		//////////
		// Retrieve the text associated with this object.  (You can't
		// call this until you've called EndConstruction.)
		//
		// [out] return - A pointer to the std::wstring that actually
		//                stores the data.  This pointer is owned by
		//                by the object, and you shouldn't delete it.
		//
		const std::wstring *GetText() const { return &mText; }

		//////////
		// Get the style for the specified character position.  (You can't
		// call this until you've called EndConstruction.)
		//
		const Style *GetStyleAt(size_t inOffset) const;

		//////////
		// Get the default style for this block of styled text.  This
		// is typically used to calculate lineheights for empty lines.
		//
		const Style *GetDefaultStyle() const { return &mBaseStyle; }

		//////////
		// You can treat a 'StyledText' object as though it were an
		// STL container with the following value_type.
		// 
		struct value_type {

			//////////
			//  The character associated with this element.
			//
			wchar_t value;

			//////////
			//  A pointer to the style associated with this element.
			//  This pointer belongs to whoever created the value_type
			//  element, and you shouldn't delete it.  For value_type
			//  objects created by a StyledText class, this pointer
			//  remains valid as long as the StyledText object exists.
			//
			const Style *style;

			//////////
			//  Create a new styled text object.
			//
			value_type(wchar_t inValue, const Style *inStyle)
				: value(inValue), style(inStyle) {}

			//////////
			//  Create a new styled text object with no contents.
			//
			value_type() : value(kNoSuchCharacter), style(NULL) {}
		};

		//////////
		// An STL-like iterator class for iterating over the data in
		// a StyledText object.
		//
		class const_iterator {
			friend class StyledText;

			const StyledText *mStyledText;
			size_t mCurrentPosition;
			value_type mCurrentValue;

			const_iterator(const StyledText *inStyledText, size_t inPos)
				: mStyledText(inStyledText), mCurrentPosition(inPos) {}
			
		public:
			//////////
			// Construct an empty iterator.  Don't use this for anything
			// until you assign a real iterator to it.
			//
			const_iterator()
				: mStyledText(NULL), mCurrentPosition(0) {}

			const_iterator &operator++()
			{
				ASSERT(mStyledText != NULL);
				ASSERT(mCurrentPosition < mStyledText->mEnd);
				++mCurrentPosition;
				return *this;
			}

			const_iterator &operator+=(size_t inIncrement)
			{
				ASSERT(mStyledText != NULL);
				mCurrentPosition += inIncrement;
				ASSERT(mCurrentPosition <= mStyledText->mEnd);
				return *this;
			}

			friend const_iterator operator+(const const_iterator &inLeft,
											size_t inRight)
			{
				const_iterator result = inLeft;
				result += inRight;
				return result;
			}

			const_iterator &operator--()
			{
				ASSERT(mStyledText != NULL);
				ASSERT(mCurrentPosition > 0);
				--mCurrentPosition;
				return *this;
			}

			friend ptrdiff_t operator-(const const_iterator &inLeft,
									   const const_iterator &inRight)
			{
				ASSERT(inLeft.mStyledText != NULL);
				ASSERT(inLeft.mStyledText == inRight.mStyledText);
				return inLeft.mCurrentPosition - inRight.mCurrentPosition;
			}

		    bool operator==(const const_iterator &inRight) const
			{
				ASSERT(mStyledText != NULL);
				ASSERT(mStyledText == inRight.mStyledText);
				return mCurrentPosition == inRight.mCurrentPosition;
			}

			bool operator!=(const const_iterator &inRight) const
			{
				ASSERT(mStyledText != NULL);
				ASSERT(mStyledText == inRight.mStyledText);
				return mCurrentPosition != inRight.mCurrentPosition;
			}

			const value_type &operator*() const
			{
				ASSERT(mStyledText != NULL);
				ASSERT(mCurrentPosition < mStyledText->mEnd);

				// We have to be careful here--our underlying object
				// doesn't contain any actual value_type objects, and we
				// can't return a reference that points to a local varaible
				// or temporary value.  So we cache our result value in the
				// iterator itself, and return a read-only reference.
				const_cast<value_type&>(mCurrentValue) =
					value_type(mStyledText->mText[mCurrentPosition],
							   mStyledText->GetStyleAt(mCurrentPosition));
				return mCurrentValue;
			}

			const value_type *operator->() const
			{
				return &operator*();
			}
		};
		
		friend class const_iterator;

		//////////
	    // Return an iterator pointing to the first element.
		//
		const_iterator begin() const
            { ASSERT(mIsBuilt); return const_iterator(this, 0); }

		//////////
	    // Return an iterator pointing one past the last element.
		// Do not dereference this.
		//
		const_iterator end() const
            { ASSERT(mIsBuilt); return const_iterator(this, mText.length()); }
	};

	class Face;

	//////////
	// An abstract typeface (with a specific size).
	//
	// Abstract typefaces know how to map character codes to glyphs,
	// kern two characters, and calculate the height of a line.
	// They don't know about GlyphIndex values or other low-level
	// abstractions.
	//
	class AbstractFace {
		int mSize;
		
	public:
		explicit AbstractFace(int inSize) : mSize(inSize) { }
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
		virtual Glyph *GetGlyph(CharCode inCharCode) = 0;

		//////////
		// Return a best guess for the maximum height of capital letters
		// above the baseline.  (For some fonts, such as Garamond, serifs
		// may extend above the ascender.)  This relies on the font's data
		// tables and FreeType's drivers, so it might occasionally be a bit
		// whacky.  (But it's all we've got.)
		//
		virtual Distance GetAscender() = 0;

		//////////
		// Return a best guess for the maximum descender below the baseline
		// for normal lower-case characters.  This relies on
		// the font's data tables and FreeType's drivers, so it might
		// occasionally be a bit whacky.  (But it's all we've got.)
		//
		virtual Distance GetDescender() = 0;

		//////////
		// Return a best guess for the appropriate distance between
		// two lines.  This relies on the font's data tables and
		// FreeType's drivers, so it might occasionally be a bit whacky.
		// (But it's all we've got.)
		//
		virtual Distance GetLineHeight() = 0;

		//////////
		// Find the concrete face object which would be used to draw the
		// specified character.
		//
		// [in] inCharCode - The character whose face we want to find.
		// [out] return -    The face associated with that character.
		//                   (This is returned as a pointer because
		//                   Face can't be declared until AbstractFace
		//                   has been declared.  This pointer points
		//                   to somebody else's face object; don't
		//                   destroy it.)
		//
		virtual Face *GetRealFace(CharCode inCharCode) = 0;

		//////////
		// Kern two characters.  If the value of either character is
		// kNoSuchCharacter, this function will return (0,0).
		//
		// [in] inChar1 - The first character.
		// [in] inChar2 - The second character.
		// [out] result - The amount to kern.
		//
		static Vector Kern(const StyledText::value_type &inChar1,
						   const StyledText::value_type &inChar2);
	};

	//////////
	// A simple typeface.
	//
	// A simple typeface is associated with a single FreeType 2 face
	// object.  It understands GlyphIndex values and other low-level
	// details of layout.
	//
	class Face : public AbstractFace {
		// Refcounting so we can implement copy & assignment.  The use of
		// internal "rep" objects is a common C++ technique to avoid
		// copying heavyweight data structures around.  In our case, we
		// simply *can't* copy the underlying FreeType data.
		struct FaceRep {
			FT_Face mFace;
			std::map<GlyphIndex,Glyph*> mGlyphCache;
			int mRefcount;

			FaceRep(FT_Face inFace);
			~FaceRep();
		};

		FaceRep *mFaceRep;
		bool mHasKerning;

		static size_t sGlyphCacheSize;
		static size_t sGlyphCacheSizeAtLastWarning;
		static const size_t kGlyphCacheSizeWarningIncrement;

		//////////
		// Update the estimated amount of memory used to store
		// all our cached glyphs.
		//
		// [in] inGlyph - A newly created glyph.  Only call this
		//                function once per glyph.
		//
		static void UpdateGlyphCacheSize(const Glyph *inGlyph);

	public:
		Face(const char *inFontFile, const char *inMetricsFile,
			 int inSize);
		Face(const Face &inFace);
		virtual ~Face();
		
		operator FT_Face() { return mFaceRep->mFace; }
		operator FT_Face*() { return &mFaceRep->mFace; }

		std::string GetFamilyName () const
		    { return std::string(mFaceRep->mFace->family_name); }
		std::string GetStyleName () const
		    { return std::string(mFaceRep->mFace->style_name); }

		GlyphIndex GetGlyphIndex(CharCode inCharCode);
		Glyph *GetGlyphFromGlyphIndex(GlyphIndex inGlyphIndex);

		Glyph *GetGlyph(CharCode inCharCode);

		//////////
		// Kern two character codes.  If either character code
		// is kNoSuchCharacter, this function will return (0,0).
		//
		Vector GetKerning(CharCode inPreviousChar, CharCode inCurrentChar);

		Distance GetAscender();
		Distance GetDescender();
		Distance GetLineHeight();

		Face *GetRealFace(CharCode inCharCode) { return this; }

		//////////
		// Return true if and only if two 'Face' objects have the same
		// underlying FT_Face.
		//
		friend bool operator==(const Face &inLeft, const Face &inRight)
		    { return inLeft.mFaceRep->mFace == inRight.mFaceRep->mFace; }

		//////////
		// Return true if and only if two 'Face' objects don't have the
		// same underlying FT_Face.
		//
		friend bool operator!=(const Face &inLeft, const Face &inRight)
		    { return !(inLeft == inRight); }
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
	//
	class FaceStack : public AbstractFace {
		std::deque<Face> mFaceStack;

	public:
		explicit FaceStack(const Face &inPrimaryFace);
		virtual ~FaceStack();

		//////////
		// Add another face to the stack.  Faces are searched
		// in the order they are added.
		//
		void AddSecondaryFace(const Face &inFace);

		virtual Glyph *GetGlyph(CharCode inCharCode);
		virtual Distance GetAscender();
		virtual Distance GetDescender();
		virtual Distance GetLineHeight();
		virtual Face *GetRealFace(CharCode inCharCode);

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
		// An iterator pointing to the first character in this segment.
		//
		StyledText::const_iterator begin;

		//////////
		// An iterator pointing one character beyond the last character
		// in this segment.
		//
		StyledText::const_iterator end;

		//////////
		// Is the current segment a newline character?  If so, the
		// segment contains no displayable data.
		//
		bool isLineBreak;

		//////////
		// Should the segment be discarded at the end of a line?
		// This is typically true for whitespace.
		// TODO - This is used as a 'isHorizontalWhitespace' flag,
		// so we should probably rename it.
		//
		bool discardAtEndOfLine;

		//////////
		// If this segment is the last on a line, do we need to draw a
		// hyphen?  Typically true for segments preceding a soft hyphen,
		// or segments which were automatically broken by the library.
		//
		bool needsHyphenAtEndOfLine;

		// Used by various algorithms to temporarily store data.
		// XXX - Clean up.
		Distance userDistanceData;

		void SetLineSegment(const StyledText::const_iterator &inBegin,
							const StyledText::const_iterator &inEnd,
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

		explicit LineSegment(const StyledText::const_iterator &inBegin,
							 const StyledText::const_iterator &inEnd,
							 bool inIsLineBreak = false,
							 bool inDiscardAtEndOfLine = false,
							 bool inNeedsHyphenAtEndOfLine = false);

		LineSegment() {}
	};

	extern bool operator==(const LineSegment &left, const LineSegment &right);

	//////////
	// An iterator which will break a line into LineSegment objects
	// according to typical typographic rules.  It does not modify the
	// underlying text.
	class LineSegmentIterator {
		StyledText::const_iterator mSegmentBegin;
		StyledText::const_iterator mTextEnd;

	public:
		//////////
		// Create a new iterator.
		//
		// [in] inText - The text to break into segments.
		//
		explicit LineSegmentIterator(const StyledText &inText);

		//////////
		// Return the next segment of the line, if any.
		//
		// [out] outSegment - The segment we found, or unchanged.
		// [out] return - True iff we found another segment.
		//
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
		const Style *mDefaultStyle;
		Distance mLineLength;
		Justification mJustification;

	protected:
		//////////
		// Create a new GenericTextRenderingEngine.
		// 
		// [in] inText - The text to draw.  This object must not be
		//               destroyed before calling RenderText.
		// [in] inLineLength - Maximum allowable line length.
		// [in] inJustification - Justification for the line.
		//
		GenericTextRenderingEngine(const StyledText &inText,
								   Distance inLineLength,
								   Justification inJustification);

		virtual ~GenericTextRenderingEngine() {}
		
		const Style *GetDefaultStyle() { return mDefaultStyle; }
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
	// AbstractFaces, drawing positions, and output to images.
	//
	// We assume that all Distance and Point values are measured in pixels.
	// 
	class TextRenderingEngine : public GenericTextRenderingEngine {
		Image *mImage;
		bool mIsFirstLine;
		Point mLineStart;
		bool mHaveBounds;
		Distance mTopBound;
		Distance mLeftBound;
		Distance mBottomBound;
		Distance mRightBound;

	public:
		//////////
		// Create a new text rendering engine.
		//
		// [in] inText -       The styled text to draw.
		// [in] inPosition -   The upper-left corner of the text box.
		// [in] inLineLength - The maximum number of pixels available for
		//                     a line.  This is (I hope) a hard limit,
		//                     and no pixels should ever be drawn beyond it.
		// [in] inJustification - The desired justification.
		// [in] inImage -      The image into which we should draw.
		//                     This must not be deallocated until the
		//                     TextRendering engine is destroyed.  If this
		//                     image is NULL, measure the text instead of
		//                     drawing it.
		//
		TextRenderingEngine(const StyledText &inText,
							Point inPosition,
							Distance inLineLength,
							Justification inJustification,
							Image *inImage);

		//////////
		// After a call to 'RenderText', get the width of the text.  This
		// can be used with a NULL image to measure text.
		//
		Distance GetTextWidth() const
			{ return mHaveBounds ? mRightBound - mLeftBound : 0; }

		//////////
		// After a call to 'RenderText', get the height of the text.  This
		// can be used with a NULL image to measure text.
		//
		Distance GetTextHeight() const
			{ return mHaveBounds ? mBottomBound - mTopBound : 0; }

		//////////
		// After a call to 'RenderText', get the rightmost coordinate
		// of any letter drawn.
		//
		Distance GetRightBound() const { return mRightBound; }

		//////////
		// After a call to 'RenderText', get an approximate bottommost
		// coordinate.  This is based on the descender of a hypothetical
		// 'g' on the last line drawn.  Note that there may be no characters
		// on this last line if the text ends in "\n".
		// 
		Distance GetBottomBound() const { return mBottomBound; }

	private:
		//////////
		// Draw a GreyMap to our image.
		//
		// [in] inPosition - The location at which to draw the bitmap.
		// [in] inBitmap -   The GreyMap to draw.
		// [in] inColor -    The color to draw in.
		//
		void DrawGreyMap(Point inPosition,
						 const GreyMap *inGreyMap,
						 Color inColor);
		
		//////////
		// Process a single character.
		//
		// [in/out] ioPrevious - On input, the character before the
		//                       current character.  Set to 
		//                       (kNoSuchCharacter, NULL) if there is no
		//                       previous character.  On output,
		//                       the value of 'inCurrent'.
		// [in] inCurrent -      The character to process.
		// [in/out] inPosition - On input, the position at which to position
		//                       and/or draw the character.  On output,
		//                       the position for the next character.
		//                       The 'y' position will not be changed.
		// [in] inShouldDraw -   Should we actually draw this character to
		//                       our mImage object?
		//
		void ProcessCharacter(StyledText::value_type *ioPrevious,
							  StyledText::value_type inCurrent,
							  Point *ioPosition,
							  bool inShouldDraw);

	protected:
		virtual Distance MeasureSegment(LineSegment *inPrevious,
										LineSegment *inSegment,
										bool inAtEndOfLine);

		virtual void ExtractOneLine(LineSegment *ioRemaining,
									LineSegment *outExtracted);

		virtual void RenderLine(std::deque<LineSegment> *inLine,
			                    Distance inHorizontalOffset);
    };

	//////////
	// A FamilyDatabase knows how to load fonts from disk, given
	// a family name, a FaceStyle and a size in points.
	// For example: ("Times", kBoldItalicFaceStyle, 12).
	//
	// A FamilyDatabase will create and use an on-disk font cache to avoid
	// opening zillions of font files at application startup.
	//
	// <h2>FamilyDatabase Internals</h2>
	//
	// There are four levels of organization within a family database.
	// From lowest to highest, these are:
	//
	//   * AvailableFace - Stores information about a single font file.
	//   * FaceSizeGroup - Stores information about all the font files
	//     with the same family name and FaceStyle.
	//   * Family - Stores information about all the fonts files with
	//     the same family name.
	//   * FamilyDatabase - Stores information about all available fonts.
	//
	// AvailableFaces move downward from FamilyDatabase objects to
	// FaceSizeGroup objects using a series of AddAvailableFace methods.
	// Face objects move upward from FaceSizeGroup objects to
	// FamilyDatabase objects using a series of GetFace methods.
	//
	class FamilyDatabase {
	public:
		//////////
		// We represent scalable faces by a size of kAnySize.  This is only
		// public because MSVC++ won't allow nested classes to access
		// private members; you can't do anything with it.
		//
		enum { kAnySize = 0 };
		
	private:
		//////////
		// An AvailableFace stores information about a single face on disk.
		// This is the lowest level of data storage in the FamilyDatabase.
		//
		class AvailableFace {
			std::string mFileName;
			
			int         mSize;
			std::string mFamilyName;
			std::string mStyleName;
			bool        mIsBold;
			bool        mIsItalic;
			
		public:
			//////////
			// Create a new AvailableFace, loading various information from
			// disk.  You must specify 'inFileName' relative to the 'Font'
			// directory (this is so we don't have to portably serialize
			// FileSystem::Path objects to the cache, which would by icky).
			//
			explicit AvailableFace(const std::string &inFileName);
			
			int         GetSize() const { return mSize; }
			std::string GetFamilyName() const { return mFamilyName; }
			std::string GetStyleName() const { return mStyleName; }
			bool        IsBold() const { return mIsBold; }
			bool        IsItalic() const { return mIsItalic; }
			bool        IsScalable() const { return GetSize() == kAnySize; }
			
			//////////
			// Load this face as a 'Face' object, using the specified
			// size.  If the font is not available in this size, the
			// behavior of this function is undefined.
			//
			Face   OpenFace(int inSize) const;
			
			//////////
			// Write some face cache header information to an ostream.
			//
			static void WriteSerializationHeader(std::ostream &out);

			//////////
			// Read the face cache header information from an ostream,
			// and validate it.
			//
			static void ReadSerializationHeader(std::istream &in);
		
			//////////
			// Construct an AvailableFace object using cache data from
			// a stream, and advance to the start of the next face object.
			//
			AvailableFace(std::istream &in);

			//////////
			// Serialize an AvailableFace object to a stream as cache data.
			//
			void        Serialize(std::ostream &out) const;
		};
		
		//////////
		// A FaceSizeGroup stores all the AvailableFace objects for a
		// given (family name, face style) pair.  It is the second-lowest
		// level of organization in a FamilyDatabase, after AvailableFace.
		//
		// It may contain a single scalable face (say, "Nimbus Mono",
		// kRegularFaceStyle), a set of bitmap fonts in various sizes (all
		// the italic "Times" faces), or some combination of the above.
		//
		// A FaceSizeGroup keeps a cache of all Face objects it opens
		// on behalf of the user.
		//
		class FaceSizeGroup {
			std::map<int,AvailableFace> mAvailableFaces;
			std::map<int,Face> mFaces;
			
		public:
			FaceSizeGroup() {}
			
			void AddAvailableFace(const AvailableFace &inFace);
			Face GetFace(int inSize);
			
			void Serialize(std::ostream &out) const;
		};
		
		//////////
		// A Family stores all the various sizes and styles for a given
		// font family (e.g., "Times", "Nimbus Mono").  It's the
		// third-lowest level of organization in a FamilyDatabase.
		//
		// If bold or italic faces are missing, a Family object will
		// try to find an appropriate substitute in a different style.
		// 
		class Family {
			std::string   mFamilyName;
			
			FaceSizeGroup mRegularFaces;
			FaceSizeGroup mBoldFaces;
			FaceSizeGroup mItalicFaces;
			FaceSizeGroup mBoldItalicFaces;
			
		public:
			explicit Family(const std::string &inFamilyName)
				: mFamilyName(inFamilyName) {}
			
			void AddAvailableFace(const AvailableFace &inFace);
			Face GetFace(FaceStyle inStyle, int inSize);
			
			void Serialize(std::ostream &out) const;
		};
		
	private:
		std::map<std::string,Family> mFamilyMap;

		//////////
		// Does the file pointed to by 'inPath' look like a font file?
		//
		static bool IsFontFile(const FileSystem::Path &inPath);
		
		//////////
		// Store an AvailableFace object in an appropriate place
		// in the database.
		//
		void AddAvailableFace(const AvailableFace &inFace);

		static FamilyDatabase *sFamilyDatabase;
		
	public:
		//////////
		// Get a global instance of FamilyDatabase.
		//
		static FamilyDatabase *GetFamilyDatabase();

		FamilyDatabase() {}
		
		//////////
		// Load all the fonts in the application's Font directory.
		//
		void ReadFromFontDirectory();

		//////////
		// Read in available font information from a font cache.
		//
		void ReadFromCache(std::istream &in);

		//////////
		// Write the entire database to a font cache.
		//
		void WriteToCache(std::ostream &out) const;
		
		//////////
		// Look up a face.
		//
		// [in] inFamilyName - The family (e.g., "Nimbus Mono").
		// [in] inStyle -      The style.  May be kRegularFaceStyle,
		//                     kBoldFaceStyle, kItalicFaceStyle,
		//                     or kBoldItalicFaceStyle.  Other styles
		//                     are generated by the drawing routines.
		// [in] inSize -       A font size, in points.
		// [out] return -      An appropriate Face object.
		//
		Face GetFace(const std::string &inFamilyName,
					 FaceStyle inStyle, int inSize);
	};
}

#endif // Typography_H
