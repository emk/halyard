// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

#ifndef MediaElement_H
#define MediaElement_H

//////////
// An abstract interface for elements which play media streams.  This
// is used as a mixin class.
//
class IMediaElement
{
public:
    IMediaElement() {} 
	virtual ~IMediaElement() {}

    //////////
    // Returns true if the media is looping.
    //
    virtual bool IsLooping() = 0;

	//////////
	// Cease playback, and make sure all future calls to
	// HasReachedFrame() return true.
	//
	virtual void EndPlayback() = 0;

    //////////
    // Pause the media stream.
    //
    // Note: This method may not be happy if the underlying movie layer
    // doesn't like to be paused.  In particular, streaming network
    // QuickTime is chancy.
    //
    virtual void Pause() = 0;

    //////////
    // Resume playing the media stream after a pause.
    //
    virtual void Resume() = 0;

    //////////
    // Set the volume of this media element.
    //
    // [in] inChannel - The channel to change.  Typical values include
    //                  "left", "right" and "all", although future media
    //                  frameworks may define more channels.
    // [in] inVolume - The volume.  0.0 is silent, 1.0 is the media's
    //                 normal volume.
    //
	virtual void SetVolume(const std::string &inChannel, double inVolume) = 0;
};

#endif // MediaElement_H
