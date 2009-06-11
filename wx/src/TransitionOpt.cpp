// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#include "AppHeaders.h"
#include <wx/rawbmp.h>

#include "Transition.h"

// Included so we can call ProcessIdle.
#include "HalyardApp.h"

using namespace Halyard;


//=========================================================================
//  Transition
//=========================================================================

/// The amount of time required to show a single frame of an arbitrary
/// transition, at leat until we know better.  On an 800MHz laptop,
/// crossfades require approximately 40 milliseconds.  We set this
/// value artificially low so that we'll run extra frames in the beginning
/// and get a more accurate sample.
#define DEFAULT_MILLISECONDS_PER_BLOCK (20)

/// A graphical transition effect.
class Transition
{
    double mTotalBlocks;
    int mTotalMilliseconds;

    double BlocksPerFrame(TransitionResources &inResources) const;
    double EstimateMillisecondsPerBlock() const;

protected:
    /// Returns true if this transition only needs to update the dirty
    /// rectangle.  This is most useful for transitions like crossfades,
    /// where the value of a given pixel is not affected by surrounding
    /// pixels.
    virtual bool AffectsOnlyDirtyRect() const { return false; }

    /// Show a single step of the transition.
    ///
    /// \param inStep  A number > 0.0 and < 1.0, corresponding to the step
    ///          to show.
    /// \param inResources  The resources to use.
    virtual void ShowStep(double inStep, TransitionResources &inResources) = 0;

public:
    Transition();
    virtual ~Transition() {}
    void RunTransition(int inMilliseconds, TransitionResources &inResources);
};

Transition::Transition()
    : mTotalBlocks(0), mTotalMilliseconds(0)
{
    // Do nothing.
}

/// A block is equivalent to one full screen's worth of pixels.
double Transition::BlocksPerFrame(TransitionResources &inResources) const
{
    if (!AffectsOnlyDirtyRect()) {
        return 1.0;
    } else {
        double pixels_per_block =
            (inResources.GetBeforeBmp().GetWidth() *
             inResources.GetBeforeBmp().GetHeight());
        double pixels_per_frame =
            (inResources.GetDirtyRect().GetWidth() *
             inResources.GetDirtyRect().GetHeight());
        return pixels_per_frame / pixels_per_block;
    }    
}

/// Based on our history, how many milliseconds do we think it takes to
/// draw a block?
///
/// We have to be careful about this, because our minimum stopwatch
/// resolution tends to be longer than some transitions.  This means we
/// need to (1) be careful of outrageous results and (b) average over as
/// much data as possible.
double Transition::EstimateMillisecondsPerBlock() const {
    double est_ms_per_block = DEFAULT_MILLISECONDS_PER_BLOCK;
    if (mTotalBlocks > 0)
    {
        est_ms_per_block = mTotalMilliseconds / mTotalBlocks;

        // Don't believe really small numbers until we have a large sample
        // size.  These constants are arbitrary.
        if (mTotalMilliseconds < 1000)
            est_ms_per_block = Max(DEFAULT_MILLISECONDS_PER_BLOCK,
                                   est_ms_per_block);
    }
    return est_ms_per_block;
}

void Transition::RunTransition(int inMilliseconds,
                               TransitionResources &inResources)
{
#ifdef DEBUG
    // Just because we're paranoid, doesn't mean they're not out to get
    // us...  Make sure the dirty rect is sane.
    wxRect dirty(inResources.GetDirtyRect());
    wxRect all(0, 0,
               inResources.GetBeforeBmp().GetWidth(),
               inResources.GetBeforeBmp().GetHeight());
    ASSERT(all.GetLeft() <= dirty.GetLeft());
    ASSERT(all.GetTop() <= dirty.GetTop());
    ASSERT(dirty.GetRight() <= all.GetRight());
    ASSERT(dirty.GetBottom() <= all.GetBottom());
#endif // DEBUG

    // Figure out what step size to use for the transition.
    double est_ms_per_block = EstimateMillisecondsPerBlock();
    double blocks_per_frame = BlocksPerFrame(inResources);
    double est_ms_per_frame = est_ms_per_block * blocks_per_frame;
    double frames = (inMilliseconds / est_ms_per_frame);
    double step = 1.0 / (frames + 1);
    double panic_ms = Max(2000, 4 * inMilliseconds);

    gLog.Trace("halyard.transition",
               "Transition: %d ms, est. %f ms/frame, est. %f frames",
               inMilliseconds, est_ms_per_frame, frames);

    if (frames >= 1.0 && step > 0.0)
    {
        // Run the transition.
        //
        /// \todo This timing loop is based on the assumpting that our
        /// timer resolution is ridiculously poor.  We could time things
        /// much better, with less interference Stage::IdleElements, if we
        /// had a high-resolution timer.
        wxStopWatch watch;
        for (double s = step; s < 1.0; s += step)
        {
            mTotalBlocks += blocks_per_frame;
            ShowStep(s, inResources);
            
            // This makes our fades less smooth, and our timing incorrect.
            // Unfortunately, if we don't do it, we can't do any
            // transitions during QuickTime playback, which the script
            // authors find unacceptable.
            wxGetApp().ProcessIdle();

            // Just to be safe!
            if (watch.Time() > panic_ms)
            {
                gLog.Debug("halyard.transition",
                           "Transition: way too long, aborting");
                break;
            }
        }
        mTotalMilliseconds += watch.Time();
    }
}


//=========================================================================
//  CrossFade
//=========================================================================

/// A gradual fade from one image to another.
class CrossFade : public Transition
{
    bool AffectsOnlyDirtyRect() const { return true; }
    void ShowStep(double inStep, TransitionResources &inResources);
};

void CrossFade::ShowStep(double inStep, TransitionResources &inResources)
{
    wxNativePixelData before_data(inResources.GetBeforeBmp());
    wxNativePixelData after_data(inResources.GetAfterBmp());
    wxNativePixelData scratch_data(inResources.GetScratchBmp());

    // Figure out our blending factors.  Use a 0-256 scale so
    // we can later divide by 256 using a fast '>> 8'.
    int alpha = 256 * inStep;
    int beta  = 256 - alpha;

    wxRect bounds(inResources.GetDirtyRect());
    int line_width(bounds.GetWidth());

    // Our outer loop.
    wxNativePixelData::Iterator before_row_start(before_data);
    wxNativePixelData::Iterator after_row_start(after_data);
    wxNativePixelData::Iterator scratch_row_start(scratch_data);
    before_row_start.Offset(before_data, bounds.GetLeft(), bounds.GetTop());
    after_row_start.Offset(after_data, bounds.GetLeft(), bounds.GetTop());
    scratch_row_start.Offset(scratch_data, bounds.GetLeft(), bounds.GetTop());
    for (int y = bounds.GetHeight(); y > 0; --y)
    {
        wxNativePixelData::Iterator before(before_row_start);
        wxNativePixelData::Iterator after(after_row_start);
        wxNativePixelData::Iterator scratch(scratch_row_start);

        // Our fast inner loop.
        for (int y = line_width; y > 0; --y)
        {
            scratch.Red() = (after.Red() * alpha + before.Red() * beta) >> 8;
            scratch.Green() =
                (after.Green() * alpha + before.Green() * beta) >> 8;
            scratch.Blue() =
                (after.Blue() * alpha + before.Blue() * beta) >> 8;

            ++before;
            ++after;
            ++scratch;
        }

        before_row_start.OffsetY(before_data, 1);
        after_row_start.OffsetY(after_data, 1);
        scratch_row_start.OffsetY(scratch_data, 1);
    }

    // Draw our scratch buffer to the screen.
    wxMemoryDC scratch_dc;
    scratch_dc.SelectObjectAsSource(inResources.GetScratchBmp());
    inResources.GetOutputDC().Blit(bounds.GetLeft(), bounds.GetTop(),
                                   bounds.GetWidth(), bounds.GetHeight(),
                                   &scratch_dc,
                                   bounds.GetLeft(), bounds.GetTop());
}


//=========================================================================
//  FadeBlack
//=========================================================================

enum FadeBlackType {
    FADE_TO_BLACK,
    FADE_FROM_BLACK
};

/// A fade to or from a black screen.
class FadeBlack : public Transition
{
    FadeBlackType mType;

public:
    FadeBlack(FadeBlackType inType) : mType(inType) {}

protected:
    void ShowStep(double inStep, TransitionResources &inResources);
};

void FadeBlack::ShowStep(double inStep, TransitionResources &inResources)
{
    // Decide whether to fade to or from black, and prepare appropriately.
    wxBitmap source_bitmap;
    int alpha;
    if (mType == FADE_TO_BLACK)
    {
        alpha = 256 * (1.0 - inStep);
        source_bitmap = inResources.GetBeforeBmp();
    }
    else
    {
        ASSERT(mType == FADE_FROM_BLACK);
        alpha = 256 * inStep;
        source_bitmap = inResources.GetAfterBmp();
    }

    // Prepare our iterators.
    wxNativePixelData source_data(source_bitmap);
    wxNativePixelData scratch_data(inResources.GetScratchBmp());

    // Our outer loop.
    wxNativePixelData::Iterator source_row_start(source_data);
    wxNativePixelData::Iterator scratch_row_start(scratch_data);
    for (int y = source_bitmap.GetHeight(); y > 0; --y)
    {
        wxNativePixelData::Iterator source(source_row_start);
        wxNativePixelData::Iterator scratch(scratch_row_start);

        // Our fast inner loop.
        for (int y = source_bitmap.GetWidth(); y > 0; --y)
        {
            scratch.Red()   = (source.Red()   * alpha) >> 8;
            scratch.Green() = (source.Green() * alpha) >> 8;
            scratch.Blue()  = (source.Blue()  * alpha) >> 8;

            ++source;
            ++scratch;
        }

        source_row_start.OffsetY(source_data, 1);
        scratch_row_start.OffsetY(scratch_data, 1);
    }

    // Draw our scratch buffer to the screen.
    inResources.GetOutputDC().DrawBitmap(inResources.GetScratchBmp(), 0, 0,
                                         false);
}


//=========================================================================
//  DirectionalTransition
//=========================================================================

enum Direction {
    DIRECTION_LEFT,
    DIRECTION_RIGHT,
    DIRECTION_UP,
    DIRECTION_DOWN
};

/// A transition which can move in one of four directions.
class DirectionalTransition : public Transition
{
    Direction mDirection;

protected:
    /// We pass these parameters to quite a few subroutines, so let's
    /// group them into a handy struct.
    struct Params {
        /// Overall height and width of the transition.
        int height, width;

        /// Width of the original and new image.  Only for horizontal
        /// transitions.
        int before_width, after_width;

        /// Height of the original and new image.  Only for vertical
        /// transitions.
        int before_height, after_height;

        /// The wxDCs for the images used by this transition.
        wxDC *before_dc, *after_dc, *output_dc;
    };

    void ShowStep(double inStep, TransitionResources &inResources);
    virtual void ShowStepLeft(Params &p) = 0;
    virtual void ShowStepRight(Params &p) = 0;
    virtual void ShowStepUp(Params &p) = 0;
    virtual void ShowStepDown(Params &p) = 0;

public:
    DirectionalTransition(Direction inDir) : mDirection(inDir) {}
};

/// This function does a lot of common setup for all the different
/// versions of ShowStepXXX defined by our subclasses.
///
/// \see ShowStepLeft(), ShowStepRight(), ShowStepUp(), ShowStepDown()
void DirectionalTransition::ShowStep(double inStep,
                                     TransitionResources &inResources)
{

    // Set up our basic parameters.
    Params p;
    p.width  = inResources.GetBeforeBmp().GetWidth();
    p.height = inResources.GetBeforeBmp().GetHeight();
    p.after_width   = p.width * inStep;
    p.before_width  = p.width - p.after_width;
    p.after_height  = p.height * inStep;
    p.before_height = p.height - p.after_height;

    // Set up our DCs.
    wxMemoryDC before_dc, after_dc;
    before_dc.SelectObjectAsSource(inResources.GetBeforeBmp());
    after_dc.SelectObjectAsSource(inResources.GetAfterBmp());
    p.before_dc = &before_dc;
    p.after_dc  = &after_dc;
    p.output_dc = &inResources.GetOutputDC();

    // Choose which version of the transition to run.
    switch (mDirection)
    {
        case DIRECTION_LEFT:  ShowStepLeft(p);  break;
        case DIRECTION_RIGHT: ShowStepRight(p); break;
        case DIRECTION_UP:    ShowStepUp(p);    break;
        case DIRECTION_DOWN:  ShowStepDown(p);  break;
        default: ASSERT(false);
    }
}


//=========================================================================
//  Wipe
//=========================================================================

///  A transition where the new image "wipes" over the old image.
///
///  NOTE - Transition currently assumes that steps take constant time
///  when computing speeds.  For now, we slow down wipes considerably
///  to ensure this.  A different timing algorithm could be used with
///  "incremental" wipes.
class Wipe : public DirectionalTransition
{
    void ShowStepLeft(Params &p);
    void ShowStepRight(Params &p);
    void ShowStepUp(Params &p);
    void ShowStepDown(Params &p);

public:
    Wipe(Direction inDirection) : DirectionalTransition(inDirection) {}
};

void Wipe::ShowStepLeft(Params &p)
{
    p.output_dc->Blit(0, 0, p.before_width, p.height,
                      p.before_dc, 0, 0);
    p.output_dc->Blit(p.before_width, 0, p.after_width, p.height,
                      p.after_dc, p.before_width, 0);
}

void Wipe::ShowStepRight(Params &p)
{
    p.output_dc->Blit(p.after_width, 0, p.before_width, p.height,
                      p.before_dc, p.after_width, 0);
    p.output_dc->Blit(0, 0, p.after_width, p.height,
                      p.after_dc, 0, 0);
}

void Wipe::ShowStepUp(Params &p)
{
    p.output_dc->Blit(0, 0, p.width, p.before_height,
                      p.before_dc, 0, 0);
    p.output_dc->Blit(0, p.before_height, p.width, p.after_height,
                      p.after_dc, 0, p.before_height);
}

void Wipe::ShowStepDown(Params &p)
{
    p.output_dc->Blit(0, p.after_height, p.width, p.before_height,
                      p.before_dc, 0, p.after_height);
    p.output_dc->Blit(0, 0, p.width, p.after_height,
                      p.after_dc, 0, 0);
}


//=========================================================================
//  Push
//=========================================================================

/// A transition where the new image pushes the old image off screen.
class Push : public DirectionalTransition
{
    void ShowStepLeft(Params &p);
    void ShowStepRight(Params &p);
    void ShowStepUp(Params &p);
    void ShowStepDown(Params &p);

public:
    Push(Direction inDirection) : DirectionalTransition(inDirection) {}
};

void Push::ShowStepLeft(Params &p)
{
    p.output_dc->Blit(0, 0, p.before_width, p.height,
                      p.before_dc, p.after_width, 0);
    p.output_dc->Blit(p.before_width, 0, p.after_width, p.height,
                      p.after_dc, 0, 0);
}

void Push::ShowStepRight(Params &p)
{
    p.output_dc->Blit(p.after_width, 0, p.before_width, p.height,
                      p.before_dc, 0, 0);
    p.output_dc->Blit(0, 0, p.after_width, p.height,
                      p.after_dc, p.before_width, 0);
}

void Push::ShowStepUp(Params &p)
{
    p.output_dc->Blit(0, 0, p.width, p.before_height,
                      p.before_dc, 0, p.after_height);
    p.output_dc->Blit(0, p.before_height, p.width, p.after_height,
                      p.after_dc, 0, 0);
}

void Push::ShowStepDown(Params &p)
{
    p.output_dc->Blit(0, p.after_height, p.width, p.before_height,
                      p.before_dc, 0, 0);
    p.output_dc->Blit(0, 0, p.width, p.after_height,
                      p.after_dc, 0, p.before_height);
}


//=========================================================================
//  TransitionManager
//=========================================================================

TransitionManager::TransitionManager()
{
    // Basics.
    RegisterTransition("crossfade", new CrossFade());
    RegisterTransition("toblack",   new FadeBlack(FADE_TO_BLACK));
    RegisterTransition("fromblack", new FadeBlack(FADE_FROM_BLACK));

    // Wipes.
    RegisterTransition("wipeleft",  new Wipe(DIRECTION_LEFT));
    RegisterTransition("wiperight", new Wipe(DIRECTION_RIGHT));
    RegisterTransition("wipeup",    new Wipe(DIRECTION_UP));
    RegisterTransition("wipedown",  new Wipe(DIRECTION_DOWN));

    // Pushes.
    RegisterTransition("pushleft",  new Push(DIRECTION_LEFT));
    RegisterTransition("pushright", new Push(DIRECTION_RIGHT));
    RegisterTransition("pushup",    new Push(DIRECTION_UP));
    RegisterTransition("pushdown",  new Push(DIRECTION_DOWN));
}

TransitionManager::~TransitionManager()
{
    TransitionMap::iterator i = mTransitions.begin();
    for (; i != mTransitions.end(); ++i)
        delete i->second;
}

void TransitionManager::RegisterTransition(const std::string &inName,
                                           Transition *inTransition)
{
    TransitionMap::iterator found = mTransitions.find(inName);
    if (found == mTransitions.end())
        mTransitions.insert(TransitionMap::value_type(inName, inTransition));
    else
    {
        gLog.Warn("halyard.transition", "Duplicate transition: %s",
                  inName.c_str());
        delete inTransition;
    }    
}

void TransitionManager::RunTransition(const std::string &inName,
                                      int inMilliseconds,
                                      TransitionResources &inResources)
{
    TransitionMap::iterator found = mTransitions.find(inName);
    if (found == mTransitions.end())
        gLog.Warn("halyard.transition", "Unknown transition: %s",
                  inName.c_str());
    else
        found->second->RunTransition(inMilliseconds, inResources);
}
