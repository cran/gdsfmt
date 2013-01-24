// ===========================================================
//     _/_/_/   _/_/_/  _/_/_/_/    _/_/_/_/  _/_/_/   _/_/_/
//      _/    _/       _/             _/    _/    _/   _/   _/
//     _/    _/       _/_/_/_/       _/    _/    _/   _/_/_/
//    _/    _/       _/             _/    _/    _/   _/
// _/_/_/   _/_/_/  _/_/_/_/_/     _/     _/_/_/   _/_/
// ===========================================================
//
// dType.h: Basic template classes for elementary types
//
// Copyright (C) 2013	Xiuwen Zheng
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

/**
 *	\file     dType.h
 *	\author   Xiuwen Zheng
 *	\version  1.0
 *	\date     2007 - 2013
 *	\brief    Basic template classes for elementary types
 *	\details
**/


#ifndef _dType_H_
#define _dType_H_

#include <CoreDEF.h>

#ifndef __STDC_LIMIT_MACROS
#  define __STDC_LIMIT_MACROS
#endif

#ifdef COREARRAY_UNIX
#  include <unistd.h>
#endif

#ifdef COREARRAY_MSC
#  include <msvc/stdint.h>
#else
#  include <stdint.h>
#endif

#include <cfloat>
#include <limits>
#include <string>


namespace CoreArray
{
	/// Memory data type id
	enum TSVType {
		svCustom = 0,   ///< Unknown or customized type
		svCustomInt,    ///< Customized signed integer
		svCustomUInt,   ///< Customized unsigned integer
		svCustomFloat,  ///< Customized float number
		svCustomStr,    ///< Customized string type
		svInt8,         ///< Signed integer of 8 bits
		svUInt8,        ///< Unsigned integer of 8 bits
		svInt16,        ///< Signed integer of 16 bits
		svUInt16,       ///< Unsigned integer of 16 bits
		svInt32,        ///< Signed integer of 32 bits
		svUInt32,       ///< Unsigned integer of 32 bits
		svInt64,        ///< Signed integer of 64 bits
		svUInt64,       ///< Unsigned integer of 64 bits
		svFloat32,      ///< Float number of single precision (32 bits)
		svFloat64,      ///< Float number of double precision (64 bits)
		svStrUTF8,      ///< UTF-8 string
		svStrUTF16      ///< UTF-16 string
	};


	/// Whether x (TSVType) is an integer or not
	#define COREARRAY_SV_INTEGER(x) \
		((svInt8<=(x) && (x)<=svUInt64) || (x)==svCustomInt || (x)==svCustomUInt)

	/// Whether x (TSVType) is a signed integer or not
	#define COREARRAY_SV_SINT(x) \
		((x)==svInt8 || (x)==svInt16 || (x)==svInt32 || (x)==svInt64 || (x)==svCustomInt)

	/// Whether x (TSVType) is an unsigned integer or not
	#define COREARRAY_SV_UINT(x) \
		((x)==svUInt8 || (x)==svUInt16 || (x)==svUInt32 || (x)==svUInt64 || (x)==svCustomUInt)

	/// Whether x (TSVType) is a float number or not
	#define COREARRAY_SV_FLOAT(x) \
		((x)==svFloat32 || (x)==svFloat64 || (x)==svCustomFloat)

	/// Whether x (TSVType) is a string or not
	#define COREARRAY_SV_STRING(x) \
		((x)==svStrUTF8 || (x)==svStrUTF16 || (x)==svCustomStr)


	#define COREARRAY_TR_UNKNOWN    -1
	#define COREARRAY_TR_CUSTOM     0
	#define COREARRAY_TR_INTEGER    1
	#define COREARRAY_TR_FLOAT      2
	#define COREARRAY_TR_STRING     3
	#define COREARRAY_TR_FIXEDSTR   4


	// Integers

	/// Signed integer of 8 bits
	typedef int8_t      Int8;
	/// Unsigned integer of 8 bits
	typedef uint8_t     UInt8;
	/// Signed integer of 16 bits
	typedef int16_t     Int16;
	/// Unsigned integer of 16 bits
	typedef uint16_t    UInt16;
	/// Signed integer of 32 bits
	typedef int32_t     Int32;
	/// Unsigned integer of 32 bits
	typedef uint32_t    UInt32;
	/// Signed integer of 64 bits
	typedef int64_t     Int64;
	/// Unsigned integer of 64 bits
	typedef uint64_t    UInt64;


	/// CoreArray Boolean
	typedef int8_t      CBOOL;


	#if defined(COREARRAY_MSC) && !defined(ssize_t)
	typedef ptrdiff_t	ssize_t;
	#endif

	// Integer Traits

	template<typename T> struct TdTraits
	{
    	typedef T TType;
		static const int trVal = COREARRAY_TR_UNKNOWN;
		static const unsigned BitOf = sizeof(T)*8u;
		static const bool isClass = false;
		static const TSVType SVType = svCustom;
	};

	template<> struct TdTraits<Int8>
	{
		typedef Int8 TType;
		static const int trVal = COREARRAY_TR_INTEGER;
		static const unsigned BitOf = 8u;
		static const bool isClass = false;
		static const TSVType SVType = svInt8;

		static const char * TraitName() { return "Int8"; }
		static const char * StreamName() { return "dInt8"; }

		COREARRAY_INLINE static short Min() { return INT8_MIN; }
		COREARRAY_INLINE static short Max() { return INT8_MAX; }
	};

	template<> struct TdTraits<UInt8>
	{
		typedef UInt8 TType;
		static const int trVal = COREARRAY_TR_INTEGER;
		static const unsigned BitOf = 8u;
		static const bool isClass = false;
		static const TSVType SVType = svUInt8;
		enum {
			isNumeric = true
		};
		static const char * TraitName() { return "UInt8"; }
		static const char * StreamName() { return "dUInt8"; }

		COREARRAY_INLINE static unsigned short Min() { return 0; }
		COREARRAY_INLINE static unsigned short Max() { return UINT8_MAX; }
	};

	template<> struct TdTraits<Int16>
	{
		typedef Int16 TType;
		static const int trVal = COREARRAY_TR_INTEGER;
		static const unsigned BitOf = 16u;
		static const bool isClass = false;
		static const TSVType SVType = svInt16;

		static const char * TraitName() { return "Int16"; }
		static const char * StreamName() { return "dInt16"; }

		COREARRAY_INLINE static Int16 Min() { return INT16_MIN; }
		COREARRAY_INLINE static Int16 Max() { return INT16_MAX; }
	};

	template<> struct TdTraits<UInt16>
	{
		typedef UInt16 TType;
		static const int trVal = COREARRAY_TR_INTEGER;
		static const unsigned BitOf = 16u;
		static const bool isClass = false;
		static const TSVType SVType = svUInt16;

		static const char * TraitName() { return "UInt16"; }
		static const char * StreamName() { return "dUInt16"; }

		COREARRAY_INLINE static UInt16 Min() { return 0; }
		COREARRAY_INLINE static UInt16 Max() { return UINT16_MAX; }
	};

	template<> struct TdTraits<Int32>
	{
		typedef Int32 TType;
		static const int trVal = COREARRAY_TR_INTEGER;
		static const unsigned BitOf = 32u;
		static const bool isClass = false;
		static const TSVType SVType = svInt32;

		static const char * TraitName() { return "Int32"; }
		static const char * StreamName() { return "dInt32"; }

		COREARRAY_INLINE static Int32 Min() { return INT32_MIN; }
		COREARRAY_INLINE static Int32 Max() { return INT32_MAX; }
	};

	template<> struct TdTraits<UInt32>
	{
		typedef UInt32 TType;
		static const int trVal = COREARRAY_TR_INTEGER;
		static const unsigned BitOf = 32u;
		static const bool isClass = false;
		static const TSVType SVType = svUInt32;

		static const char * TraitName() { return "UInt32"; }
		static const char * StreamName() { return "dUInt32"; }

		COREARRAY_INLINE static UInt32 Min() { return 0; }
		COREARRAY_INLINE static UInt32 Max() { return UINT32_MAX; }
	};

	template<> struct TdTraits<Int64>
	{
		typedef Int64 TType;
		static const int trVal = COREARRAY_TR_INTEGER;
		static const unsigned BitOf = 64u;
		static const bool isClass = false;
		static const TSVType SVType = svInt64;

		static const char * TraitName() { return "Int64"; }
		static const char * StreamName() { return "dInt64"; }

		COREARRAY_INLINE static Int64 Min() { return std::numeric_limits<Int64>::min(); }
		COREARRAY_INLINE static Int64 Max() { return std::numeric_limits<Int64>::max(); }
	};

	template<> struct TdTraits<UInt64>
	{
		typedef UInt64 TType;
		static const int trVal = COREARRAY_TR_INTEGER;
		static const unsigned BitOf = 64u;
		static const bool isClass = false;
		static const TSVType SVType = svUInt64;

		static const char * TraitName() { return "UInt64"; }
		static const char * StreamName() { return "dUInt64"; }

		COREARRAY_INLINE static UInt64 Min() { return 0; }
		COREARRAY_INLINE static UInt64 Max() { return std::numeric_limits<UInt64>::max(); }
	};



	// Bit Type

	namespace Internal
	{
		template<unsigned bits> struct _BitIndex
		{
			static const int Val =
				((bits >= 1) ? 1 : 0) + ((bits >= 9) ? 1 : 0) +
				((bits >= 17) ? 1 : 0) + ((bits >= 33) ? 1 : 0) +
				((bits >= 65) ? 1 : 0) + ((bits >= 129) ? 1 : 0);
		};

		template<int Index, bool Sign> struct _Bit2Int {};

		template<> struct _Bit2Int<1, false>
		{
			typedef UInt8 Type;
			typedef UInt16 TypeEx;
			static const Type Val = UINT8_MAX;
			static const TypeEx ValEx = UINT16_MAX;
		};
		template<> struct _Bit2Int<1, true>
		{
			typedef Int8 Type;
			typedef Int16 TypeEx;
			static const Type Val = -1;
			static const TypeEx ValEx = -1;
		};

		template<> struct _Bit2Int<2, false>
		{
			typedef UInt16 Type;
			typedef UInt32 TypeEx;
			static const Type Val = UINT16_MAX;
			static const TypeEx ValEx = UINT32_MAX;
		};
		template<> struct _Bit2Int<2, true>
		{
			typedef Int16 Type;
			typedef Int32 TypeEx;
			static const Type Val = -1;
			static const TypeEx ValEx = -1;
		};

		template<> struct _Bit2Int<3, false>
		{
			typedef UInt32 Type;
			typedef UInt64 TypeEx;
			static const Type Val = UINT32_MAX;
			static const TypeEx ValEx = UInt64(0) - UInt64(1); // UINT64_MAX;
		};
		template<> struct _Bit2Int<3, true>
		{
			typedef Int32 Type;
			typedef Int64 TypeEx;
			static const Type Val = -1;
			static const TypeEx ValEx = -1;
		};
	}


	/// the fundamental type of bits
	template<int bits> struct BITS
	{
	public:
		static const unsigned NumBit = (bits > 0) ? bits : (-bits);
		static const bool Sign = (bits > 0);
		static const unsigned Flag = 1u << (NumBit-1);
	private:
		static const int _index = Internal::_BitIndex<NumBit>::Val;
		typedef typename Internal::_Bit2Int<_index, true>::Type _IntType;
		typedef typename Internal::_Bit2Int<_index, true>::TypeEx _IntTypeEx;
		static const _IntType _IntVal = Internal::_Bit2Int<_index, true>::Val;
		static const _IntTypeEx _IntValEx = Internal::_Bit2Int<_index, true>::ValEx;
	public:
		typedef typename Internal::_Bit2Int< _index, bits<0 >::Type IntType;
		typedef typename Internal::_Bit2Int< _index, bits<0 >::TypeEx IntTypeEx;

		static const IntType Mask = ~(_IntVal << NumBit);
		static const _IntType NOTMask = (_IntVal << NumBit);
		static const IntTypeEx Mask2 = ~(_IntValEx << NumBit);
		static const _IntTypeEx NOTMask2 = (_IntValEx << NumBit);

		BITS() {}
		BITS(IntType val) { fVal = val & Mask; }

		BITS<bits> & operator+=(IntType val)
			{ fVal = (fVal + val) & Mask; return (*this); }
		BITS<bits> & operator-=(IntType val)
			{ fVal = (fVal - val) & Mask; return (*this); }
		BITS<bits> & operator=(IntType val)
			{ fVal = val & Mask; return (*this); }
	#ifdef COREARRAY_BORLANDC
		// Due to silly Borland C++ compiler
		operator Int64() const
		{
			IntType I = (bits >= 0) ? fVal : ((fVal & Flag) ? (fVal | NOTMask) : fVal);
			return I;
		}
	#else
		operator IntType() const
		{
			return (bits >= 0) ? fVal : ((fVal & Flag) ? (fVal | NOTMask) : fVal);
		}
	#endif

	protected:
		IntType fVal;
	};


	template<> struct BITS<0>	{};
	template<> struct BITS<-1>	{};

	/// Signed integer of 24 bits.
	typedef BITS<-24>	Int24;
	/// Unsigned integer of 24 bits.
	typedef BITS<24>	UInt24;


	template<typename IntType, int bits>
	COREARRAY_INLINE IntType BITS_ifsign(IntType val)
	{
		return (val & BITS<bits>::Flag) ? (val | BITS<bits>::NOTMask2) : val;
	}


	// Bit Type Traits

	// "dBit1" ... "dBit32"
	extern const char *BitStreamNames[];

	// "dSBit1" ... "dSBit32"
	extern const char *SBitStreamNames[];

	template<int bits> struct TdTraits< BITS<bits> >
	{
		typedef typename BITS<bits>::IntType TType;
		static const int trVal = COREARRAY_TR_INTEGER;
		static const unsigned BitOf = (bits > 0) ? bits : (-bits);
		static const bool isClass = false;
		static const TSVType SVType = svCustomInt;

		static const char * StreamName()
		{
			if (bits > 0)
				return BitStreamNames[bits-1];
			else
				return SBitStreamNames[(-bits)-1];
		}
		static const char * TraitName() { return StreamName()+1; }

		COREARRAY_INLINE static Int64 Min()
			{ return (bits > 0) ? 0 : (Int64(-1) << (BITS<bits>::NumBit-1)); }
		COREARRAY_INLINE static Int64 Max()
			{ return (bits > 0) ? BITS<bits>::Mask : (BITS<bits>::Mask ^ BITS<bits>::Flag); }
	};




	// Float

	/// Float number of single precision (32 bits)
	typedef float Float32;
	/// Float number of double precision (64 bits)
	typedef double Float64;
	/// Float number of long precision
	typedef long double LongFloat;


	// Float Traits

	template<> struct TdTraits<Float32>
	{
		typedef Float32 TType;
		static const int trVal = COREARRAY_TR_FLOAT;
		static const unsigned BitOf = 32u;
		static const bool isClass = false;
		static const TSVType SVType = svFloat32;

		static const char * TraitName() { return "Float32"; }
		static const char * StreamName() { return "dFloat32"; }

		COREARRAY_INLINE static Float32 Min() { return FLT_MIN; }
		COREARRAY_INLINE static Float32 Max() { return FLT_MAX; }
		COREARRAY_INLINE static Float32 Epsilon() { return FLT_EPSILON; }
		COREARRAY_INLINE static int Digits() { return FLT_MANT_DIG; }
	};

	template<> struct TdTraits<Float64>
	{
		typedef Float64 TType;
		static const int trVal = COREARRAY_TR_FLOAT;
		static const unsigned BitOf = 64u;
		static const bool isClass = false;
		static const TSVType SVType = svFloat64;

		static const char * TraitName() { return "Float64"; }
		static const char * StreamName() { return "dFloat64"; }

		COREARRAY_INLINE static Float64 Min() { return DBL_MIN; }
		COREARRAY_INLINE static Float64 Max() { return DBL_MAX; }
		COREARRAY_INLINE static Float64 Epsilon() { return DBL_EPSILON; }
		COREARRAY_INLINE static int Digits() { return DBL_MANT_DIG; }
	};

	template<> struct TdTraits<LongFloat>
	{
		typedef LongFloat TType;
		static const int trVal = COREARRAY_TR_FLOAT;
		static const unsigned BitOf = sizeof(LongFloat)*8u;
		static const bool isClass = false;
		static const TSVType SVType = svCustomFloat;

		static const char * StreamName()
		{
		#if defined(COREARRAY_HAVE_FLOAT128)
			return "dFloat128";
		#elif defined(COREARRAY_LONGFLOAT_IS_DOUBLE)
			return "dFloat64";
		#else
        	return "dFloat80";
		#endif
		}
		static const char * TraitName() { return StreamName()+1; }

		COREARRAY_INLINE static LongFloat Min() { return LDBL_MIN; }
		COREARRAY_INLINE static LongFloat Max() { return LDBL_MAX; }
		COREARRAY_INLINE static LongFloat Epsilon() { return LDBL_EPSILON; }
		COREARRAY_INLINE static int Digits() { return LDBL_MANT_DIG; }
	};



	// String

	/// UTF-8 character
	typedef char UTF8;

#if (WCHAR_MAX == UINT16_MAX) || (WCHAR_MAX == INT16_MAX)
#  define COREARRAY_SIZEOF_WCHAR 2
	/// UTF-16 character
	typedef wchar_t UTF16;
	/// UTF-32 character
	typedef Int32 UTF32;

#elif (WCHAR_MAX == UINT32_MAX) || (WCHAR_MAX == INT32_MAX)
#  define COREARRAY_SIZEOF_WCHAR 4
	/// UTF-16 character
	typedef Int16 UTF16;
	/// UTF-32 character
	typedef wchar_t UTF32;

#else
#  error "Unable to determine sizeof(wchar_t)."
#endif

	/// UTF-8 string
	typedef std::string					UTF8String;
	/// UTF-16 string
	typedef std::basic_string<UTF16>	UTF16String;
	/// UTF-32 string
	typedef std::basic_string<UTF32>	UTF32String;


	// String Traits

	template<> struct TdTraits<UTF8String>
	{
		typedef UTF8String TType;
		typedef UTF8 ElmType;
		static const int trVal = COREARRAY_TR_STRING;
		static const unsigned BitOf = 8u;
		static const bool isClass = true;
		static const TSVType SVType = svStrUTF8;

		static const char * TraitName() { return "UTF8"; }
	};

	template<> struct TdTraits<UTF8*>
	{
		typedef UTF8String TType;
		typedef UTF8 ElmType;
		static const int trVal = COREARRAY_TR_FIXEDSTR;
		static const unsigned BitOf = 8u;
		static const bool isClass = false;
		static const TSVType SVType = svStrUTF8;

		static const char * TraitName() { return "FStr8"; }
		static const char * StreamName() { return "dFStr8"; }
	};

	template<> struct TdTraits<UTF16String>
	{
		typedef UTF16String TType;
		typedef UTF16 ElmType;
		static const int trVal = COREARRAY_TR_STRING;
		static const unsigned BitOf = 16u;
		static const bool isClass = true;
		static const TSVType SVType = svStrUTF16;

		static const char * TraitName() { return "UTF16"; }
	};

	template<> struct TdTraits<UTF16*>
	{
		typedef UTF16String TType;
		typedef UTF16 ElmType;
		static const int trVal = COREARRAY_TR_FIXEDSTR;
		static const unsigned BitOf = 16u;
		static const bool isClass = false;
		static const TSVType SVType = svStrUTF16;

		static const char * TraitName() { return "FStr16"; }
		static const char * StreamName() { return "dFStr16"; }
	};

	template<> struct TdTraits<UTF32String>
	{
		typedef UTF32String TType;
		typedef UTF32 ElmType;
		static const int trVal = COREARRAY_TR_STRING;
		static const unsigned BitOf = 32u;
		static const bool isClass = true;
		static const TSVType SVType = svCustomStr;

		static const char * TraitName() { return "UTF32"; }
	};

	template<> struct TdTraits<UTF32*>
	{
		typedef UTF32String TType;
		typedef UTF32 ElmType;
		static const int trVal = COREARRAY_TR_FIXEDSTR;
		static const unsigned BitOf = 32u;
		static const bool isClass = false;
		static const TSVType SVType = svCustomStr;

		static const char * TraitName() { return "FStr32"; }
		static const char * StreamName() { return "dFStr32"; }
	};


	/// Customized type
	/** \tparam TYPE  any data type, e.g integer or float number
	 *  \tparam SIZE  to specify the structure size, can be != sizeof(TYPE)
	**/
	template<typename TYPE, ssize_t SIZE> struct TdNumber
	{
	public:
		/// The size of this type
		static const ssize_t size = SIZE;

		TdNumber() {}
		TdNumber(TYPE val) { fVal = val; }

		TdNumber<TYPE, SIZE> & operator+=(TYPE val) { fVal += val; return *this; }
		TdNumber<TYPE, SIZE> & operator-=(TYPE val) { fVal -= val; return *this; }
		TdNumber<TYPE, SIZE> & operator++ () { fVal++; return *this; }
		TdNumber<TYPE, SIZE> & operator-- () { fVal--; return *this; }
		/// Assignment
		TdNumber<TYPE, SIZE> & operator= (TYPE val) { fVal = val; return *this; }

		bool operator== (const TdNumber<TYPE, SIZE> &val) const
			{ return fVal == val.fVal; }
		bool operator!= (const TdNumber<TYPE, SIZE> &val) const
			{ return fVal != val.fVal; }

		operator TYPE() const { return fVal; }
		TYPE &get() { return fVal; }
		const TYPE &get() const { return fVal; }

		/// Return minimum value of the type
		static const TYPE min() { return TdTraits<TYPE>::Min(); }
		/// Return maximum value of the type
		static const TYPE max() { return TdTraits<TYPE>::Max(); }

	private:
		TYPE fVal;
    };
}

#endif /* _dType_H_ */
