// ===========================================================
//     _/_/_/   _/_/_/  _/_/_/_/    _/_/_/_/  _/_/_/   _/_/_/
//      _/    _/       _/             _/    _/    _/   _/   _/
//     _/    _/       _/_/_/_/       _/    _/    _/   _/_/_/
//    _/    _/       _/             _/    _/    _/   _/
// _/_/_/   _/_/_/  _/_/_/_/_/     _/     _/_/_/   _/_/
// ===========================================================
//
// dSeq.hpp: CoreArray Containers for extended types
//
// Copyright (C) 2011	Xiuwen Zheng
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

#include <dSeq.hpp>

#ifdef __BORLANDC__
#pragma hdrstop
#endif


using namespace std;
using namespace CoreArray;
using namespace CoreArray::Internal;


// Bit Array

UInt8 CoreArray::MaskBit1Array[8] =
	{ 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80 };
UInt8 CoreArray::MaskBit1ArrayNot[8] =
	{ 0xFE, 0xFD, 0xFB, 0xF7, 0xEF, 0xDF, 0xBF, 0x7F };

UInt8 CoreArray::MaskBit2Array[4] =
	{ 0x03, 0x0C, 0x30, 0xC0 };
UInt8 CoreArray::MaskBit2ArrayNot[4] =
	{ 0xFC, 0xF3, 0xCF, 0x3F };

UInt8 CoreArray::MaskBit4Array[2] =
	{ 0x0F, 0xF0 };
UInt8 CoreArray::MaskBit4ArrayNot[2] =
	{ 0xF0, 0x0F };

// Class Names
const char *CoreArray::BitStreamNames[32] =
{
	"dBit1", "dBit2", "dBit3", "dBit4", "dBit5", "dBit6", "dBit7", "dBit8",
	"dBit9", "dBit10", "dBit11", "dBit12", "dBit13", "dBit14", "dBit15",
	"dBit16", "dBit17", "dBit18", "dBit19", "dBit20", "dBit21", "dBit22",
	"dBit23", "dBit24", "dBit25", "dBit26", "dBit27", "dBit28", "dBit29",
	"dBit30", "dBit31", "dBit32"
};

const char *CoreArray::SBitStreamNames[32] =
{
	"dSBit1", "dSBit2", "dSBit3", "dSBit4", "dSBit5", "dSBit6", "dSBit7", "dSBit8",
	"dSBit9", "dSBit10", "dSBit11", "dSBit12", "dSBit13", "dSBit14", "dSBit15",
	"dSBit16", "dSBit17", "dSBit18", "dSBit19", "dSBit20", "dSBit21", "dSBit22",
	"dSBit23", "dSBit24", "dSBit25", "dSBit26", "dSBit27", "dSBit28", "dSBit29",
	"dSBit30", "dSBit31", "dSBit32"
};


template<typename TClass> static CdObjRef * OnObjCreate()
{
	return new TClass();
}


static bool ifRegClass = false;

void CoreArray::RegisterClass()
{
	if (ifRegClass) return;

	#define REG_CLASS(T, CLASS, CType, Desp)	\
		dObjMgr.AddClass(TdTraits< T >::StreamName(), \
		OnObjCreate< CLASS >, CdObjClassMgr::CType, Desp)
	#define REG_CLASS_EX(Name, CLASS, CType, Desp)	\
		dObjMgr.AddClass(Name, OnObjCreate< CLASS >, \
		CdObjClassMgr::CType, Desp)

	// signed integer

#ifndef COREARRAY_LIMIT
	REG_CLASS(BITS<-2>, CdSBit2, ctArray, "signed integer of  2 bits");
	REG_CLASS(BITS<-3>, CdSBit3, ctArray, "signed integer of  3 bits");
	REG_CLASS(BITS<-4>, CdSBit4, ctArray, "signed integer of  4 bits");
	REG_CLASS(BITS<-5>, CdSBit5, ctArray, "signed integer of  5 bits");
	REG_CLASS(BITS<-6>, CdSBit6, ctArray, "signed integer of  6 bits");
	REG_CLASS(BITS<-7>, CdSBit7, ctArray, "signed integer of  7 bits");
#endif
	REG_CLASS(Int8, CdInt8, ctArray, "signed integer of  8 bits");
	REG_CLASS_EX("dSBit8", CdSBit8, ctArray, "signed integer of  8 bits");

#ifndef COREARRAY_LIMIT
	REG_CLASS(BITS<-9>, CdSBit9, ctArray, "signed integer of  9 bits");
	REG_CLASS(BITS<-10>, CdSBit10, ctArray, "signed integer of 10 bits");
	REG_CLASS(BITS<-11>, CdSBit11, ctArray, "signed integer of 11 bits");
	REG_CLASS(BITS<-12>, CdSBit12, ctArray, "signed integer of 12 bits");
	REG_CLASS(BITS<-13>, CdSBit13, ctArray, "signed integer of 13 bits");
	REG_CLASS(BITS<-14>, CdSBit14, ctArray, "signed integer of 14 bits");
	REG_CLASS(BITS<-15>, CdSBit15, ctArray, "signed integer of 15 bits");
#endif
	REG_CLASS(Int16, CdInt16, ctArray, "signed integer of 16 bits");
	REG_CLASS_EX("dSBit16", CdSBit16, ctArray, "signed integer of 16 bits");

#ifndef COREARRAY_LIMIT
	REG_CLASS(BITS<-17>, CdSBit17, ctArray, "signed integer of 17 bits");
	REG_CLASS(BITS<-18>, CdSBit18, ctArray, "signed integer of 18 bits");
	REG_CLASS(BITS<-19>, CdSBit19, ctArray, "signed integer of 19 bits");
	REG_CLASS(BITS<-20>, CdSBit20, ctArray, "signed integer of 20 bits");
	REG_CLASS(BITS<-21>, CdSBit21, ctArray, "signed integer of 21 bits");
	REG_CLASS(BITS<-22>, CdSBit22, ctArray, "signed integer of 22 bits");
	REG_CLASS(BITS<-23>, CdSBit23, ctArray, "signed integer of 23 bits");
#endif
	REG_CLASS(BITS<-24>, CdSBit24, ctArray, "signed integer of 24 bits");
	REG_CLASS_EX("dInt24", CdSBit24, ctArray, "signed integer of 24 bits");

#ifndef COREARRAY_LIMIT
	REG_CLASS(BITS<-25>, CdSBit25, ctArray, "signed integer of 25 bits");
	REG_CLASS(BITS<-26>, CdSBit26, ctArray, "signed integer of 26 bits");
	REG_CLASS(BITS<-27>, CdSBit27, ctArray, "signed integer of 27 bits");
	REG_CLASS(BITS<-28>, CdSBit28, ctArray, "signed integer of 28 bits");
	REG_CLASS(BITS<-29>, CdSBit29, ctArray, "signed integer of 29 bits");
	REG_CLASS(BITS<-30>, CdSBit30, ctArray, "signed integer of 30 bits");
	REG_CLASS(BITS<-31>, CdSBit31, ctArray, "signed integer of 31 bits");
#endif
	REG_CLASS(Int32, CdInt32, ctArray, "signed integer of 32 bits");
	REG_CLASS_EX("dSBit32", CdSBit32, ctArray, "signed integer of 32 bits");

	REG_CLASS(Int64, CdInt64, ctArray, "signed integer of 64 bits");

	// unsigned integer

	REG_CLASS(BITS<1>, CdBit1, ctArray, "unsigned integer of  1 bit");
	REG_CLASS(BITS<2>, CdBit2, ctArray, "unsigned integer of  2 bits");
	REG_CLASS(BITS<3>, CdBit3, ctArray, "unsigned integer of  3 bits");
	REG_CLASS(BITS<4>, CdBit4, ctArray, "unsigned integer of  4 bits");
	REG_CLASS(BITS<5>, CdBit5, ctArray, "unsigned integer of  5 bits");
	REG_CLASS(BITS<6>, CdBit6, ctArray, "unsigned integer of  6 bits");
	REG_CLASS(BITS<7>, CdBit7, ctArray, "unsigned integer of  7 bits");
	REG_CLASS(UInt8, CdUInt8, ctArray, "unsigned integer of  8 bits");
	REG_CLASS_EX("dBit8", CdBit8, ctArray, "unsigned integer of  8 bits");

#ifndef COREARRAY_LIMIT
	REG_CLASS(BITS<9>, CdBit9, ctArray, "unsigned integer of  9 bits");
	REG_CLASS(BITS<10>, CdBit10, ctArray, "unsigned integer of 10 bits");
	REG_CLASS(BITS<11>, CdBit11, ctArray, "unsigned integer of 11 bits");
	REG_CLASS(BITS<12>, CdBit12, ctArray, "unsigned integer of 12 bits");
	REG_CLASS(BITS<13>, CdBit13, ctArray, "unsigned integer of 13 bits");
	REG_CLASS(BITS<14>, CdBit14, ctArray, "unsigned integer of 14 bits");
	REG_CLASS(BITS<15>, CdBit15, ctArray, "unsigned integer of 15 bits");
#endif
	REG_CLASS(UInt16, CdUInt16, ctArray, "unsigned integer of 16 bits");
	REG_CLASS_EX("dBit16", CdBit16, ctArray, "unsigned integer of 16 bits");

#ifndef COREARRAY_LIMIT
	REG_CLASS(BITS<17>, CdBit17, ctArray, "unsigned integer of 17 bits");
	REG_CLASS(BITS<18>, CdBit18, ctArray, "unsigned integer of 18 bits");
	REG_CLASS(BITS<19>, CdBit19, ctArray, "unsigned integer of 19 bits");
	REG_CLASS(BITS<20>, CdBit20, ctArray, "unsigned integer of 20 bits");
	REG_CLASS(BITS<21>, CdBit21, ctArray, "unsigned integer of 21 bits");
	REG_CLASS(BITS<22>, CdBit22, ctArray, "unsigned integer of 22 bits");
	REG_CLASS(BITS<23>, CdBit23, ctArray, "unsigned integer of 23 bits");
#endif
	REG_CLASS(BITS<24>, CdBit24, ctArray, "unsigned integer of 24 bits");
	REG_CLASS_EX("dUInt24", CdBit24, ctArray, "unsigned integer of 24 bits");

#ifndef COREARRAY_LIMIT
	REG_CLASS(BITS<25>, CdBit25, ctArray, "unsigned integer of 25 bits");
	REG_CLASS(BITS<26>, CdBit26, ctArray, "unsigned integer of 26 bits");
	REG_CLASS(BITS<27>, CdBit27, ctArray, "unsigned integer of 27 bits");
	REG_CLASS(BITS<28>, CdBit28, ctArray, "unsigned integer of 28 bits");
	REG_CLASS(BITS<29>, CdBit29, ctArray, "unsigned integer of 29 bits");
	REG_CLASS(BITS<30>, CdBit30, ctArray, "unsigned integer of 30 bits");
	REG_CLASS(BITS<31>, CdBit31, ctArray, "unsigned integer of 31 bits");
#endif
	REG_CLASS(UInt32, CdUInt32, ctArray, "unsigned integer of 32 bits");
	REG_CLASS_EX("dBit32", CdBit32, ctArray, "unsigned integer of 32 bits");

	REG_CLASS(UInt64, CdUInt64, ctArray, "unsigned integer of 64 bits");

	// float

	REG_CLASS(Float32, CdFloat32, ctArray, "float  32");
	REG_CLASS(Float64, CdFloat64, ctArray, "float  64");
	REG_CLASS(Float128, CdFloat128, ctArray, "float 128");

	// string

	REG_CLASS(UTF8*, CdFStr8, ctArray, "UTF-08 string");
	REG_CLASS(UTF16*, CdFStr16, ctArray, "UTF-16 string");
	REG_CLASS(UTF32*, CdFStr32, ctArray, "UTF-32 string");

    // stream container

	dObjMgr.AddClass("dStream", OnObjCreate<CdGDSStreamContainer>,
		CdObjClassMgr::ctStream, "Stream Container");

	ifRegClass = true;

	#undef REG_CLASS
	#undef REG_CLASS_EX
}


// bit operator

void CoreArray::bitClear(TdAllocator &alloc, TdPtr64 p, Int64 Len)
{
	UInt8 k, kEnd, B;
	if (Len > 0)
	{
		// Head
		k = ((UInt8)p) & 0x07; p = p >> 3;
		if (k > 0)
		{
			B = alloc.r8(p);
			kEnd = (7 < k+(ssize_t)Len-1) ? 7 : (k+(ssize_t)Len-1);
			for (; k <= kEnd; k++, Len--)
				B = B & MaskBit1ArrayNot[k];
			alloc.w8(p, B); p++;
		}
		// Middle
		if (Len >= 8)
		{
			B = ((UInt8)Len) & 0x07; Len = Len >> 3;
			alloc.Fill(p, Len, 0);
			p += Len; Len = B;
		}
		// End
		if (Len > 0)
		{
			B = alloc.r8(p);
			B = B & (0xFF << ((UInt8)Len));
			alloc.w8(p, B);
		}
	}
}

void CoreArray::bitBinShr(void *Buf, size_t NByte, UInt8 NShr)
{
	UInt32 *p32, *p32a, D32;
	UInt8 *p8, *p8a, D8, xNShr, xNShr8;

	NShr &= 0x07;
	if (NShr == 0) return;
	xNShr = 32 - NShr; xNShr8 = 8 - NShr;

	p32 = (UInt32*)Buf; p32a = NULL;
	while (NByte >= 4)
	{
		D32 = *p32; *p32 = D32 >> NShr;
		if (p32a != NULL)
			*p32a |= D32 << xNShr;
		p32a = p32; ++p32; NByte-= 4;
	}

	p8 = (UInt8*)p32;
	p8a = (p32a) ? (((UInt8*)p32a)+3) : NULL;
	while (NByte > 0)
	{
		D8 = *p8; *p8 = D8 >> NShr;
		if (p8a)
			*p8a |= D8 << xNShr8;
		p8a = p8; ++p8; --NByte;
	}
}

void CoreArray::bitBinShl(void *Buf, size_t NByte, UInt8 NShl)
{
	UInt32 *p32, D32a, D32;
	UInt8 *p8, D8a, D8, xNShl, xNShl8;

	NShl &= 0x07;
	if (NShl == 0) return;
	xNShl = 32 - NShl; xNShl8 = 8 - NShl;

	p32 = (UInt32*)Buf; D32a = 0;
	while (NByte >= 4) {
		D32 = *p32; *p32 = (D32 << NShl) | D32a;
		D32a = D32 >> xNShl;
		++p32; NByte -=4;
	}

	p8 = (UInt8*)p32; D8a = D32a;
	while (NByte > 0) {
		D8 = *p8; *p8 = (D8 << NShl) | D8a;
		D8a = D8 >> xNShl8;
		++p8; --NByte;
	}
}

inline static UInt8 xb(UInt8 v)
{
	return v & 0x07;
};

inline static size_t bitCpyToBuf(TdAllocator &alloc, const TdPtr64 pS,
	void *Buf, size_t L)
{
	TdPtr64 ppS, p;

	ppS = pS >> 3; p = pS + L;
	L = (xb(p) == 0) ? ((p >> 3) - ppS) : ((p >> 3) - ppS + 1);
	alloc.Read(ppS, Buf, L);
	return L;
}

void CoreArray::bitBufToCpy(TdAllocator &alloc, TdPtr64 pD, void *Buf, size_t L)
{
	UInt8 *pB, i, B, xpD, xpDL;
	TdPtr64 p;

	pB = (UInt8*)Buf; xpD = xb(pD);
	if (xpD > 0)
	{
		p = pD >> 3;
		B = alloc.r8(p);
    	xpDL = ((xpD+L-1) < 7) ? (xpD+L-1) : 7;
		for (i = xpD; i <= xpDL; i++)
		{
			B = (B & MaskBit1ArrayNot[i]) | (*pB & MaskBit1Array[i]);
			--L; pD++;
		}
		alloc.w8(p, B); ++pB;
	}

	pD >>= 3;
	if (L >= 8)
	{
		B = L & 0x07; L >>= 3;
		alloc.Write(pD, (void*)pB, L);
		pB += L; pD += L; L = B;
	}

	if (L > 0)
	{
		B = alloc.r8(pD);
		for (i = 0; (size_t)i < L; i++)
			B = (B & MaskBit1ArrayNot[i]) | (*pB & MaskBit1Array[i]);
		alloc.w8(pD, B);
	}
}

void CoreArray::bitMoveBits(TdAllocator &alloc, TdPtr64 pS, TdPtr64 pD, TdPtr64 Len)
{
	UInt8 Buf[65536];
	size_t L, LD;

	if (pS < pD)
	{
		pS += Len; pD += Len;
		while (Len > 0)
		{
			L = (Len <= (TdPtr64)(sizeof(Buf)*8-16)) ? Len : (sizeof(Buf)*8-16);
			pS -= L; pD -= L; Len -= L;
			LD = bitCpyToBuf(alloc, pS, Buf, L);
			if (xb(pS) < xb(pD))
				bitBinShl((void*)Buf, LD+1, xb(pD) - xb(pS));
			else if (xb(pS) > xb(pD))
				bitBinShr((void*)Buf, LD, xb(pS) - xb(pD));
			bitBufToCpy(alloc, pD, Buf, L);
		}
	} else if (pS > pD)
	{
		while (Len > 0)
		{
			L = (Len <= (TdPtr64)(sizeof(Buf)*8-16)) ? Len : (sizeof(Buf)*8-16);
			LD = bitCpyToBuf(alloc, pS, Buf, L);
			if (xb(pS) < xb(pD))
				bitBinShl((void*)Buf, LD+1, xb(pD) - xb(pS));
			else if (xb(pS) > xb(pD))
				bitBinShr((void*)Buf, LD, xb(pS) - xb(pD));
			bitBufToCpy(alloc, pD, Buf, L);
			pS += L; pD += L; Len -= L;
		}
	}
}
