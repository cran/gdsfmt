// ===========================================================
//     _/_/_/   _/_/_/  _/_/_/_/    _/_/_/_/  _/_/_/   _/_/_/
//      _/    _/       _/             _/    _/    _/   _/   _/
//     _/    _/       _/_/_/_/       _/    _/    _/   _/_/_/
//    _/    _/       _/             _/    _/    _/   _/
// _/_/_/   _/_/_/  _/_/_/_/_/     _/     _/_/_/   _/_/
// ===========================================================
//
// dStream.cpp: Stream classes and functions
//
// Copyright (C) 2012	Xiuwen Zheng
//
// This file is part of CoreArray.
//
// CoreArray is free software: you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License Version 3 as
// published by the Free Software Foundation.
//
// CoreArray is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with CoreArray.
// If not, see <http://www.gnu.org/licenses/>.

#include <dStream.h>
#include <ctype.h>
#include <limits>

#ifndef COREARRAY_NO_STD_IN_OUT
#	include <iostream>
#endif


#ifdef __BORLANDC__
#pragma hdrstop
#endif


using namespace std;
using namespace CoreArray;

static const char *SFCreateErrorEx = "Can not create file '%s'. %s";
static const char *SFOpenErrorEx = "Can not open file '%s'. %s";

static const char *rsInvalidAllocLevel =
	"Unknown TAllocLevel in function 'InitAllocator'.";
static const char *rsMemAllocSwitch =
	"Unable to change '%d' to '%d' in Allocator!";
static const char *rsReadOnlyMode =
	"The current Allocator is in the ReadOnly mode!";
static const char *rsWriteOnlyMode =
	"The current Allocator is in the WriteOnly mode!";

static const char *rsBlockInvalidPos = "Invalid Position: %lld in CdBlockStream.";
static const char *rsInvalidBlockLen = "Invalid length of Block!";


#define FPosMask	(0x7FFFFFFFFFFFll)
#define FPosMaskBit (0x800000000000ll)
#define FPosNMask	(0xFFFFFFFFFFFFll)


// ErrAllocator

ErrAllocator::ErrAllocator(EdAllocType Ed)
{
	switch (Ed) {
		case eaRead:
			fMessage = rsWriteOnlyMode; break;
		case eaWrite:
            fMessage = rsReadOnlyMode; break;
	}
}

ErrAllocator::ErrAllocator(TAllocLevel OldLevel, TAllocLevel NewLevel)
{
	fMessage = Format(rsMemAllocSwitch, OldLevel, NewLevel);
}


// Invalid Operations

static COREARRAY_FASTCALL void InvalidRead(TdAllocator &obj, const TdPtr64 I,
	void *Buf, ssize_t Len)
{
	throw ErrAllocator(ErrAllocator::eaRead);
}

static COREARRAY_FASTCALL int InvalidCompare(TdAllocator &obj, const TdPtr64 I,
	const void *Buf, ssize_t Len)
{
	throw ErrAllocator(ErrAllocator::eaRead);
}

static COREARRAY_FASTCALL UInt8 InvalidRead8(TdAllocator &obj, const TdPtr64 I)
{
	throw ErrAllocator(ErrAllocator::eaRead);
}

static COREARRAY_FASTCALL UInt16 InvalidRead16(TdAllocator &obj, const TdPtr64 I)
{
	throw ErrAllocator(ErrAllocator::eaRead);
}

static COREARRAY_FASTCALL UInt32 InvalidRead32(TdAllocator &obj, const TdPtr64 I)
{
	throw ErrAllocator(ErrAllocator::eaRead);
}

static COREARRAY_FASTCALL UInt64 InvalidRead64(TdAllocator &obj, const TdPtr64 I)
{
	throw ErrAllocator(ErrAllocator::eaRead);
}

static COREARRAY_FASTCALL float InvalidRead32f(TdAllocator &obj, const TdPtr64 I)
{
	throw ErrAllocator(ErrAllocator::eaRead);
}

static COREARRAY_FASTCALL double InvalidRead64f(TdAllocator &obj, const TdPtr64 I)
{
	throw ErrAllocator(ErrAllocator::eaRead);
}

static COREARRAY_FASTCALL void InvalidCapacity(TdAllocator &obj, const TdPtr64 Size)
{
	throw ErrAllocator(ErrAllocator::eaWrite);
}

static COREARRAY_FASTCALL void InvalidWrite(TdAllocator &obj, const TdPtr64 I,
	void const* Buf, ssize_t Len)
{
	throw ErrAllocator(ErrAllocator::eaWrite);
}

static COREARRAY_FASTCALL void InvalidFill(TdAllocator &obj, const TdPtr64 I,
	const TdPtr64 Len, UInt8 Value)
{
	throw ErrAllocator(ErrAllocator::eaWrite);
}

static COREARRAY_FASTCALL void InvalidMove(TdAllocator &obj, const TdPtr64 Source,
	const TdPtr64 Dest, const TdPtr64 Len)
{
	throw ErrAllocator(ErrAllocator::eaWrite);
}

static COREARRAY_FASTCALL void InvalidSwap(TdAllocator &obj, const TdPtr64 I1,
	const TdPtr64 I2, const TdPtr64 Len)
{
	throw ErrAllocator(ErrAllocator::eaWrite);
}

static COREARRAY_FASTCALL void InvalidWrite8(TdAllocator &obj, const TdPtr64 I,
	UInt8 Value)
{
	throw ErrAllocator(ErrAllocator::eaWrite);
}

static COREARRAY_FASTCALL void InvalidWrite16(TdAllocator &obj, const TdPtr64 I,
	UInt16 Value)
{
	throw ErrAllocator(ErrAllocator::eaWrite);
}

static COREARRAY_FASTCALL void InvalidWrite32(TdAllocator &obj, const TdPtr64 I,
	UInt32 Value)
{
	throw ErrAllocator(ErrAllocator::eaWrite);
}

static COREARRAY_FASTCALL void InvalidWrite64(TdAllocator &obj, const TdPtr64 I,
	const UInt64 Value)
{
	throw ErrAllocator(ErrAllocator::eaWrite);
}

static COREARRAY_FASTCALL void InvalidWrite32f(TdAllocator &obj, const TdPtr64 I,
	const float Value)
{
	throw ErrAllocator(ErrAllocator::eaWrite);
}

static COREARRAY_FASTCALL void InvalidWrite64f(TdAllocator &obj, const TdPtr64 I,
	const double Value)
{
	throw ErrAllocator(ErrAllocator::eaWrite);
}

// blChunkMemory

static COREARRAY_FASTCALL void BaseDone(TdAllocator &obj)
{
	if (obj.Base) {
		free(obj.Base);
		obj.Capacity = 0; obj.Base = NULL;
		obj._Done = NULL;
    }
}

static COREARRAY_FASTCALL void BaseCapacity(TdAllocator &obj, const TdPtr64 Size)
{
	if (Size > 0)
	{
		bool fail = (Size >= LONG_MAX);
		if (!fail)
		{
			unsigned char *p = (unsigned char*)realloc((void*)obj.Base, Size);
			if (p) {
				obj.Base = p;
                obj.Capacity = Size;
			} else
				fail = true;
		}
		if (fail)
		{
			SwitchAllocator(obj, true, true, blTempFile);
			obj._SetCapacity(obj, Size);
		}
	} else
		BaseDone(obj);
}

static COREARRAY_FASTCALL void BaseCapacityMem(TdAllocator &obj, const TdPtr64 Size)
{
	if (Size > 0)
	{
		bool fail = (Size >= LONG_MAX);
		if (!fail)
		{
			unsigned char *p = (unsigned char*)realloc((void*)obj.Base, Size);
			if (p) {
				obj.Base = p;
				obj.Capacity = Size;
			} else
				fail = true;
		}
		if (fail) throw bad_alloc();
	} else
		BaseDone(obj);
}

static COREARRAY_FASTCALL void BaseRead(TdAllocator &obj, const TdPtr64 I,
	void *Buf, ssize_t Len)
{
	memcpy(Buf, (void*)(obj.Base + (ssize_t)I), Len);
}

static COREARRAY_FASTCALL void BaseWrite(TdAllocator &obj, const TdPtr64 I,
	void const* Buf, ssize_t Len)
{
	memcpy((void*)(obj.Base + (ssize_t)I), Buf, Len);
}

static COREARRAY_FASTCALL void BaseFill(TdAllocator &obj, const TdPtr64 I,
	const TdPtr64 Len, UInt8 Value)
{
	memset((void*)(obj.Base + (ssize_t)I), Value, Len);
}

static COREARRAY_FASTCALL void BaseMove(TdAllocator &obj, const TdPtr64 Source,
	const TdPtr64 Dest, const TdPtr64 Len)
{
	if ((ssize_t)Source != (ssize_t)Dest)
	{
		memmove((void*)(obj.Base + (ssize_t)Dest),
			(void*)(obj.Base + (ssize_t)Source), Len);
	}
}

static COREARRAY_FASTCALL void BaseSwap(TdAllocator &obj, const TdPtr64 I1,
	const TdPtr64 I2, const TdPtr64 Len)
{
	SwapData((void*)(obj.Base + (ssize_t)I1),
		(void*)(obj.Base + (ssize_t)I2), Len);
}

static COREARRAY_FASTCALL int BaseComp(TdAllocator &obj, const TdPtr64 I,
	const void *Buf, ssize_t Len)
{
	unsigned char *p1 = obj.Base + (ssize_t)I;
	unsigned char *p2 = (unsigned char*)Buf;
	for (; Len > 0; Len--, p1++, p2++)
	{
		if (*p1 < *p2)
			return -1;
		else if (*p1 > *p2)
			return 1;
	}
	return 0;
}

static COREARRAY_FASTCALL UInt8 BaseRead8(TdAllocator &obj, const TdPtr64 I)
{
	return *((UInt8*)(obj.Base + (ssize_t)I));
}

static COREARRAY_FASTCALL UInt16 BaseRead16(TdAllocator &obj, const TdPtr64 I)
{
	return *((UInt16*)(obj.Base + (ssize_t)I));
}

static COREARRAY_FASTCALL UInt32 BaseRead32(TdAllocator &obj, const TdPtr64 I)
{
	return *((UInt32*)(obj.Base + (ssize_t)I));
}

static COREARRAY_FASTCALL UInt64 BaseRead64(TdAllocator &obj, const TdPtr64 I)
{
	return *((UInt64*)(obj.Base + (ssize_t)I));
}

static COREARRAY_FASTCALL float BaseRead32f(TdAllocator &obj, const TdPtr64 I)
{
	return *((float*)(obj.Base + (ssize_t)I));
}

static COREARRAY_FASTCALL double BaseRead64f(TdAllocator &obj, const TdPtr64 I)
{
	return *((double*)(obj.Base + (ssize_t)I));
}

static COREARRAY_FASTCALL void BaseWrite8(TdAllocator &obj, const TdPtr64 I,
	UInt8 Value)
{
	*((UInt8*)(obj.Base + (ssize_t)I)) = Value;
}

static COREARRAY_FASTCALL void BaseWrite16(TdAllocator &obj, const TdPtr64 I,
	UInt16 Value)
{
	*((UInt16*)(obj.Base + (ssize_t)I)) = Value;
}

static COREARRAY_FASTCALL void BaseWrite32(TdAllocator &obj, const TdPtr64 I,
	UInt32 Value)
{
	*((UInt32*)(obj.Base + (ssize_t)I)) = Value;
}

static COREARRAY_FASTCALL void BaseWrite64(TdAllocator &obj, const TdPtr64 I,
	const UInt64 Value)
{
	*((UInt64*)(obj.Base + (ssize_t)I)) = Value;
}

static COREARRAY_FASTCALL void BaseWrite32f(TdAllocator &obj, const TdPtr64 I,
	const float Value)
{
	*((float*)(obj.Base + (ssize_t)I)) = Value;
}

static COREARRAY_FASTCALL void BaseWrite64f(TdAllocator &obj, const TdPtr64 I,
	const double Value)
{
	*((double*)(obj.Base + (ssize_t)I)) = Value;
}

// blFilter

static COREARRAY_FASTCALL void FilterDone(TdAllocator &obj)
{
	if (obj.Filter)
	{
		obj.Filter->FlushWrite();
		obj.Filter->Release(); obj.Filter = NULL;
		obj._Done = NULL;
	}
}

static COREARRAY_FASTCALL void FilterCapacity(TdAllocator &obj, const TdPtr64 Size)
{
	if (Size > 0)
	{
		obj.Filter->FlushWrite();
		obj.Filter->SetSize(Size);
		obj.Capacity = Size;
	} else {
		obj.Filter->SetSize(0);
		obj.Capacity = 0;
	}
	obj.Filter->RefreshStream();
}

static COREARRAY_FASTCALL void FilterRead(TdAllocator &obj, const TdPtr64 I, void *Buf,
	ssize_t Len)
{
	obj.Filter->SetPosition(I);
	obj.Filter->Read(Buf, Len);
}

static COREARRAY_FASTCALL void FilterWrite(TdAllocator &obj, const TdPtr64 I,
	void const* Buf, ssize_t Len)
{
	obj.Filter->SetPosition(I);
	obj.Filter->Write(Buf, Len);
}

static COREARRAY_FASTCALL void FilterFill(TdAllocator &obj, const TdPtr64 I,
	const TdPtr64 Len, UInt8 Value)
{
	if (Len <= 0) return;

	char Buf[65536];
	TdPtr64 L = Len;
	ssize_t xL;
	obj.Filter->SetPosition(I);
	if (L > (TdPtr64)sizeof(Buf))
		memset((void*)Buf, Value, sizeof(Buf));
	else
		memset((void*)Buf, Value, L);
	while (L > 0)
	{
		xL = (L >= (TdPtr64)sizeof(Buf)) ? (ssize_t)sizeof(Buf) : L;
		obj.Filter->Write(Buf, xL);
		L -= xL;
	}
}

static COREARRAY_FASTCALL void FilterMove(TdAllocator &obj, const TdPtr64 I1,
	const TdPtr64 I2, const TdPtr64 Len)
{
	if ((Len > 0) && (I1 != I2))
	{
		char Buf[65536];
		CdStream *Stream;
		TdPtr64 p1, p2, Cnt;
		ssize_t L;

		obj.Filter->FlushBuffer();
		Cnt = Len; Stream = obj.Filter->Stream();
		if ((I1>I2) || (I1+Len<=I2))
		{
			p1 = I1; p2 = I2;
			while (Cnt > 0)
			{
				L = (Cnt >= (TdPtr64)sizeof(Buf)) ? (ssize_t)sizeof(Buf) : Cnt;
				// Read
				Stream->SetPosition(p1); Stream->ReadBuffer((void*)Buf, L);
				// Write
				Stream->SetPosition(p2); Stream->WriteBuffer((void*)Buf, L);
				// Iterate
				p1 += L; p2 += L; Cnt -= L;
            }
		} else {
			p1 = I1 + Len; p2 = I2 + Len;
			while (Cnt > 0)
			{
				L = (Cnt >= (TdPtr64)sizeof(Buf)) ? (ssize_t)sizeof(Buf) : Cnt;
				// Iterate
				p1 -= L; p2 -= L; Cnt -= L;
				// Read
				Stream->SetPosition(p1); Stream->ReadBuffer((void*)Buf, L);
				// Write
				Stream->SetPosition(p2); Stream->WriteBuffer((void*)Buf, L);
            }
		}
		obj.Filter->RefreshStream();
	}
}

static COREARRAY_FASTCALL void FilterSwap(TdAllocator &obj, const TdPtr64 I1,
	const TdPtr64 I2, const TdPtr64 Len)
{
	if ((Len > 0) && (I1 != I2))
	{
		char Buf1[65536], Buf2[65536];
		CdStream * Stream;
		TdPtr64 p1, p2, Cnt;
		ssize_t L;

		obj.Filter->FlushBuffer();
		Cnt = Len; Stream = obj.Filter->Stream();
		p1 = I1; p2 = I2;
		while (Cnt > 0)
		{
			L = (Cnt >= (TdPtr64)sizeof(Buf1)) ? (ssize_t)sizeof(Buf1) : Cnt;
			// Read
			Stream->SetPosition(p1); Stream->ReadBuffer((void*)Buf1, L);
			Stream->SetPosition(p2); Stream->ReadBuffer((void*)Buf2, L);
			// Write
			Stream->SetPosition(p1); Stream->WriteBuffer((void*)Buf2, L);
			Stream->SetPosition(p2); Stream->WriteBuffer((void*)Buf1, L);
			// Iterate
			p1 += L; p2 += L; Cnt -= L;
        }
		obj.Filter->RefreshStream();
	}
}

static COREARRAY_FASTCALL int FilterComp(TdAllocator &obj, const TdPtr64 I,
	const void *Buf, ssize_t Len)
{
	unsigned char * p = (unsigned char*)Buf;
	obj.Filter->SetPosition(I);

	for (; Len > 0; Len--, p++)
	{
		unsigned char ch = obj.Filter->rUInt8();
		if (ch < *p)
			return -1;
		else if (ch > *p)
			return 1;
	}
	return 0;
}

static COREARRAY_FASTCALL UInt8 FilterRead8(TdAllocator &obj, const TdPtr64 I)
{
	obj.Filter->SetPosition(I);
	return obj.Filter->rUInt8();
}

static COREARRAY_FASTCALL UInt16 FilterRead16(TdAllocator &obj, const TdPtr64 I)
{
	obj.Filter->SetPosition(I);
	return obj.Filter->rUInt16();
}

static COREARRAY_FASTCALL UInt32 FilterRead32(TdAllocator &obj, const TdPtr64 I)
{
	obj.Filter->SetPosition(I);
	return obj.Filter->rUInt32();
}

static COREARRAY_FASTCALL UInt64 FilterRead64(TdAllocator &obj, const TdPtr64 I)
{
	obj.Filter->SetPosition(I);
	return obj.Filter->rUInt64();
}

static COREARRAY_FASTCALL float FilterRead32f(TdAllocator &obj, const TdPtr64 I)
{
	obj.Filter->SetPosition(I);
	return obj.Filter->rFloat32();
}

static COREARRAY_FASTCALL double FilterRead64f(TdAllocator &obj, const TdPtr64 I)
{
	obj.Filter->SetPosition(I);
	return obj.Filter->rFloat64();
}

static COREARRAY_FASTCALL void FilterWrite8(TdAllocator &obj, const TdPtr64 I,
	UInt8 Value)
{
	obj.Filter->SetPosition(I);
	obj.Filter->wUInt8(Value);
}

static COREARRAY_FASTCALL void FilterWrite16(TdAllocator &obj, const TdPtr64 I,
	UInt16 Value)
{
	obj.Filter->SetPosition(I);
	obj.Filter->wUInt16(Value);
}

static COREARRAY_FASTCALL void FilterWrite32(TdAllocator &obj, const TdPtr64 I,
	UInt32 Value)
{
	obj.Filter->SetPosition(I);
	obj.Filter->wUInt32(Value);
}

static COREARRAY_FASTCALL void FilterWrite64(TdAllocator &obj, const TdPtr64 I,
	const UInt64 Value)
{
	obj.Filter->SetPosition(I);
	obj.Filter->wUInt64(Value);
}

static COREARRAY_FASTCALL void FilterWrite32f(TdAllocator &obj, const TdPtr64 I,
	const float Value)
{
	obj.Filter->SetPosition(I);
	obj.Filter->wFloat32(Value);
}

static COREARRAY_FASTCALL void FilterWrite64f(TdAllocator &obj, const TdPtr64 I,
	const double Value)
{
	obj.Filter->SetPosition(I);
	obj.Filter->wFloat64(Value);
}



void CoreArray::InitAllocator(TdAllocator &Allocator, bool CanRead,
	bool CanWrite, TAllocLevel vLevel, CBufdStream* BufFilter)
{
	// Initialize
	memset((void*)&Allocator, 0, sizeof(TdAllocator));
	Allocator.Level = vLevel;

	// reading functions
	switch (vLevel)
	{
		case blChunkMemory:
			Allocator._Done = BaseDone;
			Allocator._SetCapacity = BaseCapacity;
			Allocator._Read = BaseRead;
			Allocator._Write = BaseWrite;
			Allocator._Fill = BaseFill;
			Allocator._Move = BaseMove;
			Allocator._Swap = BaseSwap;
			Allocator._Compare = BaseComp;
			Allocator._r8 = BaseRead8;
			Allocator._r16 = BaseRead16;
			Allocator._r32 = BaseRead32;
			Allocator._r64 = BaseRead64;
			Allocator._r32f = BaseRead32f;
			Allocator._r64f = BaseRead64f;
			Allocator._w8 = BaseWrite8;
			Allocator._w16 = BaseWrite16;
			Allocator._w32 = BaseWrite32;
			Allocator._w64 = BaseWrite64;
			Allocator._w32f = BaseWrite32f;
			Allocator._w64f = BaseWrite64f;
			break;
		case blTempFile: case blFilter:
			if (vLevel == blTempFile)
				BufFilter = new CBufdStream(new CdTempStream(""));
			(Allocator.Filter = BufFilter)->AddRef();
			Allocator._Done = FilterDone;
			Allocator._SetCapacity = FilterCapacity;
			Allocator._Read = FilterRead;
			Allocator._Write = FilterWrite;
			Allocator._Fill = FilterFill;
			Allocator._Move = FilterMove;
			Allocator._Swap = FilterSwap;
			Allocator._Compare = FilterComp;
			Allocator._r8 = FilterRead8;
			Allocator._r16 = FilterRead16;
			Allocator._r32 = FilterRead32;
			Allocator._r64 = FilterRead64;
			Allocator._r32f = FilterRead32f;
			Allocator._r64f = FilterRead64f;
			Allocator._w8 = FilterWrite8;
			Allocator._w16 = FilterWrite16;
			Allocator._w32 = FilterWrite32;
			Allocator._w64 = FilterWrite64;
			Allocator._w32f = FilterWrite32f;
			Allocator._w64f = FilterWrite64f;
        	break;
		default:
			throw ErrAllocator(rsInvalidAllocLevel);
	}

	if (!CanRead)
	{
		Allocator._Read = InvalidRead;
		Allocator._Compare = InvalidCompare;
		Allocator._r8 = InvalidRead8;
		Allocator._r16 = InvalidRead16;
		Allocator._r32 = InvalidRead32;
		Allocator._r64 = InvalidRead64;
		Allocator._r32f = InvalidRead32f;
		Allocator._r64f = InvalidRead64f;
	}
	if (!CanWrite)
	{
		Allocator._SetCapacity = InvalidCapacity;
		Allocator._Write = InvalidWrite;
		Allocator._Fill = InvalidFill;
		Allocator._Move = InvalidMove;
		Allocator._Swap = InvalidSwap;
		Allocator._w8 = InvalidWrite8;
		Allocator._w16 = InvalidWrite16;
		Allocator._w32 = InvalidWrite32;
		Allocator._w64 = InvalidWrite64;
		Allocator._w32f = InvalidWrite32f;
		Allocator._w64f = InvalidWrite64f;
	}
}

void CoreArray::InitAllocatorEx(TdAllocator &Allocator, bool CanRead,
	bool CanWrite, CdStream* Stream)
{
	CBufdStream *Filter;
	Filter = new CBufdStream(Stream);
	InitAllocator(Allocator, CanRead, CanWrite, blFilter, Filter);
}

void CoreArray::DoneAllocator(TdAllocator &Allocator)
{
	Allocator._Done(Allocator);
}

void CoreArray::InitMemAllocator(TdAllocator &Allocator, const TdPtr64 Size)
{
	memset((void*)&Allocator, 0, sizeof(TdAllocator));
	Allocator.Level = blChunkMemory;
	Allocator._Done = BaseDone;
	Allocator._SetCapacity = BaseCapacityMem;
	Allocator._Read = BaseRead;
	Allocator._Write = BaseWrite;
	Allocator._Fill = BaseFill;
	Allocator._Move = BaseMove;
	Allocator._Swap = BaseSwap;
	Allocator._Compare = BaseComp;
	Allocator._r8 = BaseRead8;
	Allocator._r16 = BaseRead16;
	Allocator._r32 = BaseRead32;
	Allocator._r64 = BaseRead64;
	Allocator._r32f = BaseRead32f;
	Allocator._r64f = BaseRead64f;
	Allocator._w8 = BaseWrite8;
	Allocator._w16 = BaseWrite16;
	Allocator._w32 = BaseWrite32;
	Allocator._w64 = BaseWrite64;
	Allocator._w32f = BaseWrite32f;
	Allocator._w64f = BaseWrite64f;
	Allocator._SetCapacity(Allocator, Size);
}

void CoreArray::SwitchAllocator(TdAllocator &Allocator, bool CanRead,
	bool CanWrite, const TAllocLevel NewLevel, CBufdStream* BufFilter)
{
	TdAllocator R;
    int vSwitch = 1;

	InitAllocator(R, CanRead, CanWrite, NewLevel, BufFilter);
	switch (Allocator.Level)
	{
		case blChunkMemory:
			switch (NewLevel)
			{
				case blChunkMemory:
					vSwitch = 2; break;
				case blTempFile:
					R.Filter->SetSize(Allocator.Capacity);
					SaveAllocator(Allocator, R.Filter, 0, Allocator.Capacity);
                	break;
				default: vSwitch = 0;
            }
			break;
		default:
			if (Allocator.Level != NewLevel)
               	vSwitch = 0;
	}

	switch (vSwitch)
	{
		case 1:
			Allocator = R; break;
		case 0:
			throw ErrAllocator(Allocator.Level, NewLevel);
	}
}

void CoreArray::LoadAllocator(TdAllocator &Allocator, CdStream* Source,
	TdPtr64 Start, TdPtr64 Len)
{
	char Buf[65536];
	ssize_t xL;
	while (Len > 0)
	{
		xL = (Len >= (TdPtr64)sizeof(Buf)) ? (ssize_t)sizeof(Buf) : Len;
		Source->ReadBuffer((void*)Buf, xL);
		Allocator.Write(Start, (void*)Buf, xL);
		Start += xL; Len -= xL;
	}
}

void CoreArray::SaveAllocator(TdAllocator &Allocator, CdStream* Dest,
	TdPtr64 Start, TdPtr64 Len)
{
	char Buf[65536];
	ssize_t xL;
	while (Len > 0)
	{
		xL = (Len >= (TdPtr64)sizeof(Buf)) ? (ssize_t)sizeof(Buf) : Len;
		Allocator.Read(Start, (void*)Buf, xL);
		Dest->WriteBuffer((void*)Buf, xL);
		Start += xL; Len -= xL;
	}
}

void CoreArray::LoadAllocator(TdAllocator &Allocator, CBufdStream* Source,
	TdPtr64 Start, TdPtr64 Len)
{
	char Buf[65536];
	ssize_t xL;
	while (Len > 0)
	{
		xL = (Len >= (TdPtr64)sizeof(Buf)) ? (ssize_t)sizeof(Buf) : Len;
		Source->Read((void*)Buf, xL);
		Allocator.Write(Start, (void*)Buf, xL);
		Start += xL; Len -= xL;
	}
}

void CoreArray::SaveAllocator(TdAllocator &Allocator, CBufdStream* Dest,
	TdPtr64 Start, TdPtr64 Len)
{
	char Buf[65536];
	ssize_t xL;
	while (Len > 0)
	{
		xL = (Len >= (TdPtr64)sizeof(Buf)) ? (ssize_t)sizeof(Buf) : Len;
		Allocator.Read(Start, (void*)Buf, xL);
		Dest->Write((void*)Buf, xL);
		Start += xL; Len -= xL;
	}
}

// CdHandleStream

ssize_t CdHandleStream::Read(void *Buffer, ssize_t Count)
{
	if (Count > 0)
		return SysHandleRead(fHandle, Buffer, Count);
	else
		return 0;
}

ssize_t CdHandleStream::Write(void *const Buffer, ssize_t Count)
{
	if (Count > 0)
		return SysHandleWrite(fHandle, Buffer, Count);
	else
		return 0;
}

TdPtr64 CdHandleStream::Seek(const TdPtr64 Offset, TdSysSeekOrg Origin)
{
	TdPtr64 rv = SysHandleSeek(fHandle, Offset, Origin);

	if (rv < 0)
	{
		RaiseLastOSError<ErrOSError>();
		return 0;
	} else
		return rv;
}

void CdHandleStream::SetSize(const TdPtr64 NewSize)
{
	if (!SysHandleSetSize(fHandle, NewSize))
    	RaiseLastOSError<ErrOSError>();
}

// CdFileStream

static TSysShareMode ShMode[4] =
	{ saNone, saRead, saNone, saNone };

CdFileStream::CdFileStream(const char * const AFileName, TdOpenMode Mode)
{
	if (Mode == fmCreate)
	{
		fHandle = SysCreateFile(AFileName, 0);
		if (fHandle == NullSysHandle)
			throw Err_dStream(SFCreateErrorEx, AFileName, LastSysErrMsg().c_str());
	} else{
		fHandle = SysOpenFile(AFileName, (TSysOpenMode)(Mode-fmOpenRead),
			ShMode[Mode]);
		if (fHandle == NullSysHandle)
			throw Err_dStream(SFOpenErrorEx, AFileName, LastSysErrMsg().c_str());
	}
	fFileName = AFileName;
}

CdFileStream::~CdFileStream()
{
	if (fHandle != NullSysHandle)
	{
		if (!SysCloseHandle(fHandle))
			RaiseLastOSError<ErrOSError>();
	}
}

// CdTempStream

CdTempStream::CdTempStream(const char * const Path): CdFileStream(
	TempFileName("tmp", Path).c_str(), CdFileStream::fmCreate) { }

CdTempStream::~CdTempStream()
{
	if (fHandle != NullSysHandle)
	{
		if (!SysCloseHandle(fHandle))
		{
			fHandle = NullSysHandle;
			RaiseLastOSError<ErrOSError>();
        }
		fHandle = NullSysHandle;
		remove(fFileName.c_str());
	}
}

// CdMemoryStream

CdMemoryStream::CdMemoryStream(size_t Size)
{
	InitMemAllocator(fAllocator, Size);
	fPosition = 0;
}

ssize_t CdMemoryStream::Read(void *Buffer, ssize_t Count)
{
	fAllocator.Read(fPosition, Buffer, Count);
	fPosition += Count;
	return Count;
}

ssize_t CdMemoryStream::Write(void *const Buffer, ssize_t Count)
{
	fAllocator.Write(fPosition, Buffer, Count);
	fPosition += Count;
	return Count;
}

TdPtr64 CdMemoryStream::Seek(const TdPtr64 Offset, TdSysSeekOrg Origin)
{
	switch (Origin)
	{
		case soBeginning:
			fPosition = Offset;
			break;
		case soCurrent:
        	fPosition += Offset;
			break;
		case soEnd:
        	fPosition += fAllocator.Capacity;
			break;
		default:
			return -1;
	}
	if ((fPosition < 0) || (fPosition > fAllocator.Capacity))
		throw Err_dStream("Invalid position (%d).", fPosition);
	return fPosition;
}

TdPtr64 CdMemoryStream::GetSize()
{
    return fAllocator.Capacity;
}

void CdMemoryStream::SetSize(const TdPtr64 NewSize)
{
	fAllocator.SetCapacity(NewSize);
}

void *CdMemoryStream::BufPointer()
{
    return fAllocator.Base;
}


#ifndef COREARRAY_NO_STD_IN_OUT

// CdStdInStream

CdStdInStream::CdStdInStream() {}

CdStdInStream::~CdStdInStream() {}

ssize_t CdStdInStream::Read(void *Buffer, ssize_t Count)
{
	cin.read((char*)Buffer, Count);
	return Count;
}

ssize_t CdStdInStream::Write(void *const Buffer, ssize_t Count)
{
	throw Err_dStream("Invalid CdStdInStream::Write.");
}

TdPtr64 CdStdInStream::Seek(const TdPtr64 Offset, TdSysSeekOrg Origin)
{
	return 0;
}

TdPtr64 CdStdInStream::GetSize()
{
	return 0;
}

void CdStdInStream::SetSize(const TdPtr64 NewSize)
{
	throw Err_dStream("Invalid CdStdInStream::SetSize.");
}

// CdStdOutStream

CdStdOutStream::CdStdOutStream()
{
}

CdStdOutStream::~CdStdOutStream()
{

}

ssize_t CdStdOutStream::Read(void *Buffer, ssize_t Count)
{
	throw Err_dStream("Invalid CdStdOutStream::Read.");
}

ssize_t CdStdOutStream::Write(void *const Buffer, ssize_t Count)
{
	cout.write((const char*)Buffer, Count);
	return Count;
}

TdPtr64 CdStdOutStream::Seek(const TdPtr64 Offset, TdSysSeekOrg Origin)
{
	return 0;
}

TdPtr64 CdStdOutStream::GetSize()
{
	return 0;
}

void CdStdOutStream::SetSize(const TdPtr64 NewSize)
{
	throw Err_dStream("Invalid CdStdOutStream::SetSize.");
}

#endif


// CdBaseZStream

CdBaseZStream::CdBaseZStream(CdStream* vStream)
{
	(fStream = vStream)->AddRef();
	fStreamBase = fStreamPos = vStream->Position();
	fTotalIn = fTotalOut = 0;
	memset((void*)&fZStream, 0, sizeof(z_stream));
	memset((void*)fBuffer, 0, sizeof(fBuffer));
}

CdBaseZStream::~CdBaseZStream()
{
	fStream->Release();
}

static const char *SZInvalid = "Invalid ZStream operation '%s'!";

static short ZLevels[13] = {
		Z_NO_COMPRESSION,       // zcNone
		Z_BEST_SPEED,           // zcFastest
		Z_DEFAULT_COMPRESSION,  // zcDefault
		Z_BEST_COMPRESSION,     // zcMax
		1, 2, 3, 4, 5, 6, 7, 8, 9
	};

static short ZStrategies[5] = {
		Z_DEFAULT_STRATEGY,     // zsDefault
		Z_FILTERED,             // zsFiltered
		Z_HUFFMAN_ONLY,         // zsHuffman
		Z_RLE,                  // zsRLE
		Z_FIXED                 // zsFixed
	};

COREARRAY_FORCE_INLINE static int ZCheck(int Code)
{
	if (Code < 0) throw EZLibError(Code);
	return Code;
}

// CdZIPDeflate

CdZIPDeflate::CdZIPDeflate(CdStream* Dest, TZLevel DeflateLevel):
	CdBaseZStream(Dest)
{
	PtrExtRec = NULL;
	fZStream.next_out = fBuffer;
	fZStream.avail_out = sizeof(fBuffer);
	fHaveClosed = false;

	ZCheck( deflateInit_(&fZStream, ZLevels[DeflateLevel],
		ZLIB_VERSION, sizeof(fZStream)) );
}

CdZIPDeflate::CdZIPDeflate(CdStream* Dest, TZLevel DeflateLevel,
	int windowBits, int memLevel, TZStrategy Strategy): CdBaseZStream(Dest)
{
	fZStream.next_out = fBuffer;
	fZStream.avail_out = sizeof(fBuffer);
	fHaveClosed = false;

	#define Z_DEFLATED 8
	ZCheck( deflateInit2_(&fZStream, ZLevels[DeflateLevel],
		Z_DEFLATED, windowBits, memLevel, ZStrategies[Strategy],
		ZLIB_VERSION, sizeof(fZStream)) );
	#undef Z_DEFLATED
}

CdZIPDeflate::~CdZIPDeflate()
{
	try {
		Close();
	} catch (...) {
		deflateEnd(&fZStream);
		throw;
	}
	deflateEnd(&fZStream);
}

ssize_t CdZIPDeflate::Read(void *Buffer, ssize_t Count)
{
	throw EZLibError(SZInvalid, "Read");
}

ssize_t CdZIPDeflate::Write(void *const Buffer, ssize_t Count)
{
	fZStream.next_in = (Bytef*)Buffer;
	fZStream.avail_in = Count;
	if (fStream->Position() != fStreamPos)
		fStream->SetPosition(fStreamPos);

	int L = fZStream.avail_in;
	while (fZStream.avail_in > 0)
	{
		ZCheck(deflate(&fZStream, Z_NO_FLUSH));
		fTotalIn += L - fZStream.avail_in;
		L = fZStream.avail_in;
		if (fZStream.avail_out == 0)
		{
			fStream->WriteBuffer((void*)fBuffer, sizeof(fBuffer));
			fStreamPos += sizeof(fBuffer);
			fTotalOut += sizeof(fBuffer);
			fZStream.next_out = fBuffer;
			fZStream.avail_out = sizeof(fBuffer);
		}
	}

	if (Count > 0) fHaveClosed = false;
	return Count;
}

TdPtr64 CdZIPDeflate::Seek(const TdPtr64 Offset, TdSysSeekOrg Origin)
{
	if (Offset==0 && Origin==soCurrent)
    	return fTotalIn;
	else
		throw EZLibError(SZInvalid, "Seek");
}

void CdZIPDeflate::SetSize(const TdPtr64 NewSize)
{
	if (NewSize != fTotalIn)
		throw EZLibError(SZInvalid, "SetSize");
}

void CdZIPDeflate::Close()
{
	SyncFlush(Z_FINISH);
}

ssize_t CdZIPDeflate::Pending()
{
	return sizeof(fBuffer) - fZStream.avail_out;
}

void CdZIPDeflate::SyncFlush(int Code)
{
	if (!fHaveClosed)
	{
		if (PtrExtRec)
		{
			WriteBuffer((void*)PtrExtRec->Buf, PtrExtRec->Size);
			PtrExtRec = NULL;
		}

		fZStream.next_in = NULL;
		fZStream.avail_in = 0;
		if (fStream->Position() != fStreamPos)
			fStream->SetPosition(fStreamPos);

		while (ZCheck(deflate(&fZStream, Code)) != Z_STREAM_END)
		{
			int L = sizeof(fBuffer) - fZStream.avail_out;
			fStream->WriteBuffer((void*)fBuffer, L);
			fTotalOut += L;
			fZStream.next_out = fBuffer;
			fZStream.avail_out = sizeof(fBuffer);
		}

		if (fZStream.avail_out < sizeof(fBuffer))
		{
			int L = sizeof(fBuffer)-fZStream.avail_out;
			fStream->WriteBuffer((void*)fBuffer, L);
			fTotalOut += L;
		}

		fHaveClosed = true;
	}
}

// CdZIPInflate

CdZIPInflate::CdZIPInflate(CdStream* Source): CdBaseZStream(Source)
{
	fZStream.next_in = fBuffer;
	fZStream.avail_in = 0;
	fBlockSize = 1024*1024; // 1024K
	fRandomAccess = true;
	fBlockStart = fCurPos = 0;
	ZCheck(inflateInit_(&fZStream, ZLIB_VERSION, sizeof(fZStream)));
}

CdZIPInflate::CdZIPInflate(CdStream* Source, int windowBits):
	CdBaseZStream(Source)
{
	fZStream.next_in = fBuffer;
	fZStream.avail_in = 0;
	fBlockSize = 1024*1024; // 1024K
	fRandomAccess = true;
	fBlockStart = fCurPos = 0;
	ZCheck(inflateInit2_(&fZStream, windowBits, ZLIB_VERSION, sizeof(fZStream)));
}

CdZIPInflate::~CdZIPInflate()
{
	inflateEnd(&fZStream);
	ClearPoints();
}

ssize_t CdZIPInflate::Read(void *Buffer, ssize_t Count)
{
	fZStream.next_out = (Bytef*)Buffer;
	if (fStream->Position() != fStreamPos)
		fStream->SetPosition(fStreamPos);

	int OldOut, ZResult = Z_OK;
	ssize_t rv=0, L;

	while (Count>0 && ZResult!=Z_STREAM_END)
	{
		if (fZStream.avail_in == 0)
		{
			fZStream.avail_in = fStream->Read((void*)fBuffer, sizeof(fBuffer));
			if (fZStream.avail_in == 0)
				return rv;
			fStreamPos += fZStream.avail_in;
			fZStream.next_in = fBuffer;
		}

		L = fBlockStart + fBlockSize - fCurPos;
		fZStream.avail_out = (Count>=L) ? L : Count;
		OldOut = fZStream.avail_out;
		ZResult = ZCheck(inflate(&fZStream, Z_NO_FLUSH));

		L = OldOut-fZStream.avail_out;
		fCurPos += L; Count -= L; rv += L;

		if (fCurPos >= fBlockStart + fBlockSize)
		{
			fBlockStart += fBlockSize;
			// Add an access point
			if (fRandomAccess)
			{
				int DivI = fCurPos / fBlockSize;
				if (DivI > 0)
				{
					TZIPPointRec * pRec = PointIndex(DivI-1);
					if (!pRec)
					{
						// Should be current position, i.e BlockSize time an integer
						pRec = AddPoint();
						pRec->SourcePos = fStreamPos - fZStream.avail_in;
						inflateCopy(&pRec->Rec, &fZStream);
						pRec->Rec.avail_in = 0;
					}
				}
			}
		}
	}

	if ((ZResult==Z_STREAM_END) && (fZStream.avail_in>0))
	{
		fStream->SetPosition(fStream->Position() - fZStream.avail_in);
		fStreamPos = fStream->Position();
		fZStream.avail_in = 0;
	}

	if (fStreamPos-fStreamBase > fTotalIn)
		fTotalIn = fStreamPos - fStreamBase;
	if (fCurPos > fTotalOut)
		fTotalOut = fCurPos;

	return rv;
}

ssize_t CdZIPInflate::Write(void *const Buffer, ssize_t Count)
{
	throw EZLibError(SZInvalid, "Write");
}

TdPtr64 CdZIPInflate::Seek(const TdPtr64 Offset, TdSysSeekOrg Origin)
{
	if ((Offset==0) && (Origin==soBeginning))
	{
		ZCheck(inflateReset(&fZStream));
		fZStream.next_in = fBuffer;
		fZStream.avail_in = 0;
		fStream->SetPosition(0);
		fStreamPos = fCurPos = fBlockStart = 0;
		return 0;
	} else if (Origin != soEnd)
	{
		if (Offset==0 && Origin==soCurrent)
			return fCurPos;

		TdPtr64 vOff = (Origin==soCurrent) ? (fCurPos + Offset) : Offset;

		if (vOff < fCurPos)
		{
			if (fRandomAccess && vOff>=fBlockSize)
			{
				inflateEnd(&fZStream);
				int i = vOff / fBlockSize;
				TZIPPointRec *p = PointIndexEx(i-1);
				fStream->SetPosition(fStreamPos = p->SourcePos);
				fBlockStart = fCurPos = (TdPtr64)i * fBlockSize;
				inflateCopy(&fZStream, &p->Rec);
				vOff -= fCurPos;
			} else
				Seek(0, soBeginning);
		} else {
			if (fRandomAccess && vOff>=fBlockStart+fBlockSize)
			{
				int i = vOff / fBlockSize;
				TZIPPointRec *p = PointIndex(i-1);
				if (p)
				{
					inflateEnd(&fZStream);
					fStream->SetPosition(fStreamPos = p->SourcePos);
					fBlockStart = fCurPos = (TdPtr64)i * fBlockSize;
					inflateCopy(&fZStream, &p->Rec);
                }
			}
			vOff -= fCurPos;
		}

    	char buf[4096*2];
		int DivI = vOff / sizeof(buf);
		while (DivI > 0)
		{
			ReadBuffer((void*)buf, sizeof(buf));
			DivI--;
        }
		ReadBuffer((void*)buf, vOff % sizeof(buf));
	} else
		throw EZLibError(SZInvalid, "Seek");

	return fCurPos;
}

TdPtr64 CdZIPInflate::GetSize()
{
	return -1;
}

void CdZIPInflate::SetSize(const TdPtr64 NewSize)
{
	throw EZLibError(SZInvalid, "SetSize");
}

void CdZIPInflate::ClearPoints()
{
	vector<TZIPPointRec>::iterator it;
	for (it=vPoints.begin(); it != vPoints.end(); it++)
		inflateEnd(&it->Rec);
	vPoints.clear();
}

void CdZIPInflate::SetRandomAccess(bool Value)
{
	if (Value != fRandomAccess)
	{
		fRandomAccess = Value;
		ClearPoints();
	}
}

void CdZIPInflate::SetBlockSize(ssize_t Value)
{
	if (fBlockSize!=Value && Value>=(ssize_t)sizeof(fBuffer))
	{
		fBlockSize = Value;
		ClearPoints();
	}
}

CdZIPInflate::TZIPPointRec *CdZIPInflate::AddPoint()
{
	vPoints.resize(vPoints.size()+1);
	return &vPoints.back();
}

CdZIPInflate::TZIPPointRec *CdZIPInflate::PointIndex(unsigned int i)
{
	if (i < vPoints.size())
		return &vPoints[i];
	else return NULL;
}

CdZIPInflate::TZIPPointRec *CdZIPInflate::PointIndexEx(unsigned int i)
{
	return &vPoints.at(i);
}

// EZLibError

EZLibError::EZLibError(int Code)
{
	fErrCode = Code;
	fMessage = zError(Code);
}


// CdBlockStream

static void xClearList(CdBlockStream::TBlockInfo *Head)
{
	CdBlockStream::TBlockInfo *p = Head;
	while (p != NULL)
	{
		CdBlockStream::TBlockInfo *q = p;
		p = p->Next;
		delete q;
    }
}

CdBlockStream::TBlockInfo::TBlockInfo()
{
	Next = NULL;
	BlockStart = BlockSize = StreamStart = StreamNext = 0;
	Head = false;
}

TdPtr64 CdBlockStream::TBlockInfo::AbsStart()
{
	return StreamStart - (Head ? (HeadSize+2*TdPosType::size) : (2*TdPosType::size));
}

void CdBlockStream::TBlockInfo::SetSize(CdStream &Stream, const TdPtr64 Size)
{
	BlockSize = Size;
	TdPtr64 L = Head ? (HeadSize+2*TdPosType::size) : (2*TdPosType::size);
	Stream.SetPosition(StreamStart - L);
	Stream << TdPosType((Size+L) | (Head ? FPosMaskBit : 0));
}

void CdBlockStream::TBlockInfo::SetNext(CdStream &Stream, const TdPtr64 Next)
{
	StreamNext = Next;
	Stream.SetPosition(StreamStart -
		(Head ? (HeadSize+TdPosType::size) : TdPosType::size));
	Stream << TdPosType(Next);
}

void CdBlockStream::TBlockInfo::SetSize2(CdStream &Stream,
	const TdPtr64 Size, const TdPtr64 Next)
{
	BlockSize = Size;
	TdPtr64 L = Head ? (HeadSize+2*TdPosType::size) : (2*TdPosType::size);
	Stream.SetPosition(StreamStart - L);
	Stream << TdPosType((Size+L) | (Head ? FPosMaskBit : 0))
		<< TdPosType(Next);
}


CdBlockStream::CdBlockStream(CdBlockCollection &vCollection):
	fCollection(vCollection)
{
	fID = 0;
	fList = fCurrent = NULL;
	fPosition = fBlockCapacity = 0;
	fBlockSize = 0;
	fNeedSyncSize = false;
	vCollection.fStream->AddRef();
}

CdBlockStream::~CdBlockStream()
{
	SyncSizeInfo();
	xClearList(fList);
	fCollection.fStream->Release();
}

ssize_t CdBlockStream::Read(void *Buffer, ssize_t Count)
{
	TdPtr64 LastPos = fPosition;
	if ((LastPos+Count) > fBlockSize)
		Count = fBlockSize - LastPos;

	if (fCurrent && (Count>0))
	{
		char *p = (char*)Buffer;
		CdStream &vStream = fCollection.Stream();
		TdPtr64 I, L;
		ssize_t RL;
		do {
			I = fPosition - fCurrent->BlockStart;
			L = fCurrent->BlockSize - I;
			if (Count < L)
			{
				vStream.SetPosition(fCurrent->StreamStart + I);
				RL = vStream.Read((void*)p, Count);
				fPosition += RL;
				break;
			} else {
				if (L > 0)
				{
					vStream.SetPosition(fCurrent->StreamStart + I);
					RL = vStream.Read((void*)p, L);
					Count -= RL; fPosition += RL; p += RL;
					if (RL != L) break;
                }
				fCurrent = fCurrent->Next;
				if ((fCurrent==NULL) || (Count<=0))
					break;
			}
		} while (true);
	}

	return fPosition - LastPos;
}

ssize_t CdBlockStream::Write(void *const Buffer, ssize_t Count)
{
	TdPtr64 LastPos = fPosition;

	if (Count > 0)
	{
		TdPtr64 L = fPosition + Count;
		if (L > fBlockCapacity)
			fCollection._IncStreamSize(*this, L);

		char *p = (char*)Buffer;
		CdStream &vStream = fCollection.Stream();
		ssize_t RL;
		do {
			TdPtr64 I = fPosition - fCurrent->BlockStart;
			L = fCurrent->BlockSize - I;
			if (Count < L)
			{
				vStream.SetPosition(fCurrent->StreamStart+I);
				fPosition += vStream.Write((void*)p, Count);
				break;
			} else {
				if (L > 0)
				{
					vStream.SetPosition(fCurrent->StreamStart+I);
					RL = vStream.Write((void*)p, L);
					Count -= RL; fPosition += RL; p += RL;
					if (RL != L) break;
				}
				fCurrent = fCurrent->Next;
				if ((fCurrent==NULL) || Count<=0)
					break;
			}
		} while (true);

		if (fPosition > fBlockSize)
		{
			fBlockSize = fPosition;
			fNeedSyncSize = true;
			SyncSizeInfo();
		}
	}
	return fPosition - LastPos;
}

TdPtr64 CdBlockStream::Seek(const TdPtr64 Offset, TdSysSeekOrg Origin)
{
	TdPtr64 rv = 0;
	switch (Origin)
	{
		case soBeginning:
			if (fPosition != Offset)
				rv = Offset;
			else
				return Offset;
			break;
		case soCurrent:
			if (Offset != 0)
				rv = fPosition + Offset;
			else
				return fPosition;
			break;
		case soEnd:
			if (Offset != 0)
				rv = fBlockSize + Offset;
			else
				return fBlockSize;
			break;
		default:
			return -1;
	}
	if ((rv < 0) || (rv > fBlockSize))
		throw Err_dStream(rsBlockInvalidPos, rv);
	fCurrent = _FindCur(rv);
	return (fPosition = rv);
}

TdPtr64 CdBlockStream::GetSize()
{
	return fBlockSize;
}

void CdBlockStream::SetSize(const TdPtr64 NewSize)
{
	if ((0<=NewSize) && (NewSize!=fBlockSize) && (NewSize!=fBlockSize))
	{
		if (NewSize > fBlockCapacity)
			fCollection._IncStreamSize(*this, NewSize);
		else if (NewSize < fBlockCapacity)
			fCollection._DecStreamSize(*this, NewSize);
		fBlockSize = NewSize;
		fNeedSyncSize = true;
		SyncSizeInfo();
	}
}

void CdBlockStream::SetSizeOnly(const TdPtr64 NewSize)
{
	if ((0<=NewSize) && (NewSize!=fBlockSize))
	{
		if (NewSize > fBlockCapacity)
            SetSize(NewSize);
    	fBlockSize = NewSize;
		fNeedSyncSize = true;
		SyncSizeInfo();
	}
}

bool CdBlockStream::ReadOnly() const
{
    return fCollection.fReadOnly;
}

int CdBlockStream::ListCount() const
{
	int rv = 0;
	TBlockInfo *p = fList;
	while (p != NULL) { p = p->Next; rv++; }
	return rv;
}

void CdBlockStream::SyncSizeInfo()
{
	if (fNeedSyncSize)
	{
		if (fList)
		{
			CdStream &s = fCollection.Stream();
			s.SetPosition(fList->StreamStart - TdPosType::size);
			s << TdPosType(fBlockSize);
        }
    	fNeedSyncSize = false;
	}
}

CdBlockStream::TBlockInfo *CdBlockStream::_FindCur(const TdPtr64 Pos)
{
	if (Pos < fBlockCapacity)
	{
		TBlockInfo *p = fCurrent;
		if (p == NULL) p = fList;
		if (p == NULL) return NULL;
		if (Pos < p->BlockStart) p = fList;

		TBlockInfo *n = p;
		while ((p != NULL) && (Pos >= p->BlockStart))
		{
			n = p; p = p->Next;
		}
		return n;
	} else
		return NULL;
}

// CdBlockCollection

CdBlockCollection::CdBlockCollection(const TdPtr64 vCodeStart)
{
	fStream = NULL;
	fStreamSize = 0;
	fUnuse = NULL;
	vNextID = 1; // start from 1
	fCodeStart = vCodeStart;
	fClassMgr = &dObjManager();
	fReadOnly = false;
}

CdBlockCollection::~CdBlockCollection()
{
	Clear();
}

void CdBlockCollection::_IncStreamSize(CdBlockStream &Block,
	const TdPtr64 NewCapacity)
{
	// NewCapacity > fBlockCapacity
	if (Block.fList != NULL)
	{
		CdBlockStream::TBlockInfo *p = Block.fList;
		while (p->Next) p = p->Next;

		TdPtr64 L = p->BlockSize + p->StreamStart;

		// to check if it is the last block
		if (L == fStreamSize)
		{
			// immediately increase size of the original stream
			L += NewCapacity - Block.fBlockCapacity;
			fStream->SetSize(L); fStreamSize = L;
			// set size
			p->SetSize(*fStream, NewCapacity - p->BlockStart);
			Block.fBlockCapacity = NewCapacity;
			// check Block.fCurrent
			if (Block.fCurrent == NULL)
			{
				p = Block.fList;
				while (p->Next) p = p->Next;
				Block.fCurrent = p;
			}
		} else if (L < fStreamSize)
		{
			// Need a new block
			CdBlockStream::TBlockInfo *n =
				_NeedBlock(NewCapacity - Block.fBlockCapacity, false);

			n->BlockStart = p->BlockStart + p->BlockSize;
			p->Next = n; n->Next = NULL;
			p->SetNext(*fStream, n->AbsStart());

			Block.fBlockCapacity = n->BlockStart + n->BlockSize;
			if (Block.fCurrent == NULL)
				Block.fCurrent = n;
		} else
			throw Err_dStream(rsInvalidBlockLen);

	} else {
		// Need a new block
		CdBlockStream::TBlockInfo *n =
			_NeedBlock(NewCapacity - Block.fBlockCapacity, true);

		n->BlockStart = 0; n->Next = NULL;
		Block.fBlockCapacity = n->BlockSize;
		Block.fList = Block.fCurrent = n;

		fStream->SetPosition(n->StreamStart -
			CdBlockStream::TBlockInfo::HeadSize);
		*fStream << Block.fID;
    	*fStream << TdPosType(0);
	}
}

void CdBlockCollection::_DecStreamSize(CdBlockStream &Block,
	const TdPtr64 NewSize)
{
	// NewSize < fBlockCapacity
	CdBlockStream::TBlockInfo *p, *q;

	p = Block.fList; q = NULL;
	while ((p!=NULL) && (p->BlockStart < NewSize))
		{	q = p; p = p->Next; }

	if (p != NULL)
	{
		// Delete the unused parts
		if (p != Block.fList)
		{
			// Delete the link
			q->Next = NULL;
			q->SetNext(*fStream, 0);
		} else {
			Block.fList = NULL;
			p->BlockSize += CdBlockStream::TBlockInfo::HeadSize;
			p->StreamStart -= CdBlockStream::TBlockInfo::HeadSize;
			p->Head = false;
		}

		while (p != NULL)
		{
            p->SetSize2(*fStream, p->BlockSize, 0);
			q = p; p = p->Next;
			// Insert into fUnuse
			q->Next = fUnuse;
			fUnuse = q;
		}
	}
}

CdBlockStream::TBlockInfo *CdBlockCollection::_NeedBlock(
	TdPtr64 Size, bool Head)
{
	if (Head)
		Size += CdBlockStream::TBlockInfo::HeadSize;

	// First, find a suitable block for Unuse
	CdBlockStream::TBlockInfo *p = fUnuse, *rv, *q, *qrv;
	rv = q = qrv = NULL;
	while (p != NULL)
	{
		if (p->BlockSize >= Size)
		{
			if (rv != NULL)
			{
				if (p->BlockSize < rv->BlockSize)
				{
					qrv = q; rv = p;
				}
			} else {
				qrv = q; rv = p;
			}
		}
		q = p; p = p->Next;
	}

	// Secend, no such block
	if (rv == NULL)
	{
		TdPtr64 Pos = fStreamSize;
		fStreamSize += 2*TdPosType::size + Size;
		fStream->SetSize(fStreamSize);

		// Result
		rv = new CdBlockStream::TBlockInfo;
		rv->StreamStart = Pos + 2*TdPosType::size +
			(Head ? CdBlockStream::TBlockInfo::HeadSize : 0);
		rv->Head = Head;
		rv->SetSize2(*fStream,
			Size - (Head ? CdBlockStream::TBlockInfo::HeadSize : 0), 0);

	} else {
		// Have such block
		rv->Head = Head;
		if (Head)
		{
			rv->BlockSize -= CdBlockStream::TBlockInfo::HeadSize;
			rv->StreamStart += CdBlockStream::TBlockInfo::HeadSize;
		}
		rv->SetSize2(*fStream, rv->BlockSize, 0);

		// Delete it
		if (qrv)
			qrv->Next = rv->Next;
		else
        	fUnuse = rv->Next;
	}

	return rv;
}

CdBlockStream *CdBlockCollection::NewBlockStream()
{
	#ifdef COREARRAY_DEBUG_CODE
	if (!fStream)
		throw Err_dStream("CdBlockCollection::fStream = NULL.");
	#endif

	// Need a new ID
	while (HaveID(vNextID)) ++vNextID;

	// New
	CdBlockStream *rv = new CdBlockStream(*this);
	rv->AddRef();
	rv->fID = vNextID; ++vNextID;
	fBlockList.push_back(rv);

	return rv;
}

bool CdBlockCollection::HaveID(TdBlockID id)
{
	vector<CdBlockStream*>::const_iterator it;
	for (it=fBlockList.begin(); it != fBlockList.end(); it++)
		if ((*it)->fID == id)
			return true;
	return false;
}

void CdBlockCollection::LoadStream(CdStream *vStream, bool vReadOnly)
{
	if (fStream)
		throw Err_dStream("Call CdBlockCollection::Clear() first.");

	// Assign
	(fStream=vStream)->AddRef();
    fReadOnly = vReadOnly;

	// Start to screen
	CdBlockStream::TBlockInfo *p, *q, *n;

	fStream->SetPosition(fCodeStart);
	fStreamSize = fStream->GetSize();

	p = fUnuse;
	while (fStream->Position() < fStreamSize)
	{
		TdPosType sSize, sNext;
		*fStream >> sSize >> sNext;
		TdPtr64 sPos = fStream->Position() +
			(sSize & FPosMask) - 2*TdPosType::size;

		CdBlockStream::TBlockInfo *n = new CdBlockStream::TBlockInfo;
		n->Head = sSize & FPosMaskBit;
		int L = n->Head ? CdBlockStream::TBlockInfo::HeadSize : 0;
		n->BlockSize = (sSize & FPosMask) - L - 2*TdPosType::size;
		n->StreamStart = fStream->Position() + L;
		n->StreamNext = sNext;

		if (p) p->Next = n; else fUnuse = n;
		p = n;

		fStream->SetPosition(sPos);
	}

	// Reorganize Block
	while (fUnuse != NULL)
	{
		// Find the Head
		p = fUnuse; q = NULL;
		while (p != NULL) {
			if (p->Head) break;
			q = p; p = p->Next;
        }

		if (p != NULL)
		{
        	// Delete p
			if (q) q->Next = p->Next; else fUnuse = p->Next;
			//
			CdBlockStream *bs = new CdBlockStream(*this);
			bs->AddRef();
			fBlockList.push_back(bs);
			//
			fStream->SetPosition(p->StreamStart -
				CdBlockStream::TBlockInfo::HeadSize);
			*fStream >> bs->fID >> bs->fBlockSize;
			bs->fBlockCapacity = p->BlockSize;
			bs->fList = bs->fCurrent = p;
			p->Next = NULL;

			// Find a list
			n = fUnuse; q = NULL;
			while ((n != NULL) && (p->StreamNext != 0))
			{
				if ((p->StreamNext == n->AbsStart()) && (!n->Head))
				{
					if (q) q->Next = n->Next; else fUnuse = n->Next;
					p->Next = n;
					n->BlockStart = p->BlockStart + p->BlockSize;
					bs->fBlockCapacity += n->BlockSize;
					p = n; n = n->Next;
					p->Next = NULL;
				} else {
					q = n; n = n->Next;
                }
			}

			// need checking codes
		} else
        	break;
	}
}

void CdBlockCollection::WriteStream(CdStream *vStream)
{
	if (fStream)
		throw Err_dStream("Call CdBlockCollection::Clear() first.");
	// Assign
	(fStream=vStream)->AddRef();
    fReadOnly = false;

	fStream->SetSize(fStreamSize=fCodeStart);
}

void CdBlockCollection::Clear()
{
	vector<CdBlockStream*>::iterator it;
	for (it=fBlockList.begin(); it != fBlockList.end(); it++)
	{
		CdBlockStream *p = *it;
		#ifdef COREARRAY_DEBUG_CODE
		if (p->Release() != 0)
			throw Err_dStream("CdBlockStream::Release() should return 0 here.");
		#else
		p->Release();
		#endif
	}
    fBlockList.clear();

	#ifdef COREARRAY_DEBUG_CODE
	if (fStream->Release() != 0)
		throw Err_dStream("CdBlockStream::Release() should return 0 here.");
	#else
	fStream->Release();
	#endif

	fStream = NULL;
	xClearList(fUnuse); fUnuse = NULL;
}

void CdBlockCollection::DeleteBlockStream(TdBlockID id)
{
	vector<CdBlockStream*>::iterator it;
	for (it=fBlockList.begin(); it != fBlockList.end(); it++)
	{
		if ((*it)->fID == id)
		{
			CdBlockStream::TBlockInfo *p, *q;
			p = (*it)->fList; q = NULL;
			while (p != NULL)
			{
				if (p->Head)
				{
					p->BlockSize += CdBlockStream::TBlockInfo::HeadSize;
					p->StreamStart -= CdBlockStream::TBlockInfo::HeadSize;
					p->Head = false;
				}
				p->SetSize2(*fStream, p->BlockSize, 0);
            	q = p; p = p->Next;
            }
			if (q) {
				q->Next = fUnuse;
				fUnuse = (*it)->fList;
				(*it)->fList = NULL;
            }

			(*it)->Release();
			fBlockList.erase(it);
			return;
		}
	}
	throw Err_dStream("Invalid block with id: %x", id.get());
}

CdBlockStream *CdBlockCollection::operator[] (const TdBlockID &id)
{
	vector<CdBlockStream*>::iterator it;
	for (it=fBlockList.begin(); it != fBlockList.end(); it++)
	{
		if ((*it)->fID == id)
			return *it;
	}

	CdBlockStream *rv = new CdBlockStream(*this);
	rv->AddRef();
	rv->fID = id;
	fBlockList.push_back(rv);
	if (vNextID.get() < id.get())
		vNextID = id.get() + 1;

	return rv;
}
