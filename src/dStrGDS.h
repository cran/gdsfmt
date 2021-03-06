// ===========================================================
//     _/_/_/   _/_/_/  _/_/_/_/    _/_/_/_/  _/_/_/   _/_/_/
//      _/    _/       _/             _/    _/    _/   _/   _/
//     _/    _/       _/_/_/_/       _/    _/    _/   _/_/_/
//    _/    _/       _/             _/    _/    _/   _/
// _/_/_/   _/_/_/  _/_/_/_/_/     _/     _/_/_/   _/_/
// ===========================================================
//
// dStrGDS.h: GDS format with character types and functions
//
// Copyright (C) 2007 - 2014	Xiuwen Zheng
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
 *	\file     dStrGDS.h
 *	\author   Xiuwen Zheng [zhengx@u.washington.edu]
 *	\version  1.0
 *	\date     2007 - 2014
 *	\brief    GDS format with character types and functions
 *	\details
**/

#ifndef _HEADER_COREARRAY_STRING_GDS_
#define _HEADER_COREARRAY_STRING_GDS_

#include <dStruct.h>


namespace CoreArray
{
	// =======================================================================
	// Fixed-length string
	// =======================================================================

	/// Fixed-length array
	/** \tparam TYPE  data type, e.g C_UTF8, C_UTF16 and C_UTF32
	**/
	template<typename TYPE> struct COREARRAY_DLL_DEFAULT FIXED_LENGTH
	{
		typedef TYPE TType;
	};


	template<> struct COREARRAY_DLL_DEFAULT TdTraits< FIXED_LENGTH<C_UTF8> >
	{
		typedef UTF8String TType;
		typedef C_UTF8 ElmType;
		typedef char RawType;
		static const int trVal = COREARRAY_TR_FIXED_LENGTH_STRING;
		static const unsigned BitOf = 8u;
		static const bool isClass = false;
		static const C_SVType SVType = svStrUTF8;

		static const char *TraitName() { return StreamName()+1; }
		static const char *StreamName() { return "dFStr8"; }
	};

	template<> struct COREARRAY_DLL_DEFAULT TdTraits< FIXED_LENGTH<C_UTF16> >
	{
		typedef UTF16String TType;
		typedef C_UTF16 ElmType;
		typedef C_UTF16 RawType;
		static const int trVal = COREARRAY_TR_FIXED_LENGTH_STRING;
		static const unsigned BitOf = 16u;
		static const bool isClass = false;
		static const C_SVType SVType = svStrUTF16;

		static const char *TraitName() { return StreamName()+1; }
		static const char *StreamName() { return "dFStr16"; }
	};

	template<> struct COREARRAY_DLL_DEFAULT TdTraits< FIXED_LENGTH<C_UTF32> >
	{
		typedef UTF32String TType;
		typedef C_UTF32 ElmType;
		typedef C_UTF32 RawType;
		static const int trVal = COREARRAY_TR_FIXED_LENGTH_STRING;
		static const unsigned BitOf = 32u;
		static const bool isClass = false;
		static const C_SVType SVType = svCustomStr;

		static const char *TraitName() { return StreamName()+1; }
		static const char *StreamName() { return "dFStr32"; }
	};



	/// Fixed-length string container
	/** \tparam TYPE  should be C_UTF8, C_UTF16 or C_UTF32
	 *  \sa  CdFStr8, CdFStr16, CdFStr32
	**/
	template<typename TYPE> class COREARRAY_DLL_DEFAULT CdFixedStr:
		public CdArray< FIXED_LENGTH<TYPE> >
	{
	public:
		typedef typename TdTraits< FIXED_LENGTH<TYPE> >::TType TType;
		typedef TYPE ElmType;

		CdFixedStr(): CdArray< FIXED_LENGTH<TYPE> >()
		{
			this->vElmSize_Ptr = 0;
		}

		virtual CdGDSObj *NewOne(void *Param=NULL)
		{
			CdFixedStr<TYPE> *rv = new CdFixedStr<TYPE>;
			rv->SetMaxLength(this->MaxLength());
			this->_AssignToDim(*rv);
			if (this->fPipeInfo)
				rv->fPipeInfo = this->fPipeInfo->NewOne();
			return NULL;
		}

		COREARRAY_INLINE ssize_t MaxLength() const
		{
			return this->fElmSize / (TdTraits<TYPE>::BitOf/8);
		}

		/// set the maximum length of fixed-length string
		void SetMaxLength(ssize_t NewLen)
		{
			if (NewLen > 0)
				this->SetElmSize(NewLen * (TdTraits<TYPE>::BitOf/8));
			else
				throw ErrArray("CdFixedStr::SetMaxLength, invalid parameter.");
		}

	protected:

		virtual void UpdateInfoProc(CdBufStream *Sender)
		{
			if (this->vElmSize_Ptr != 0)
			{
				this->fGDSStream->SetPosition(this->vElmSize_Ptr);
				BYTE_LE<CdStream>(this->fGDSStream) << C_UInt32(this->fElmSize);
			}
		}

		/// loading function for serialization
		virtual void Loading(CdReader &Reader, TdVersion Version)
		{
			static const char *VAR_ESIZE = "ESIZE";
			C_UInt32 esize = 0;
			Reader[VAR_ESIZE] >> esize;
			this->vElmSize_Ptr = Reader.PropPosition(VAR_ESIZE);
			this->fElmSize = esize;
			CdAllocArray::Loading(Reader, Version);
		}

		/// saving function for serialization
		virtual void Saving(CdWriter &Writer)
		{
			static const char *VAR_ESIZE = "ESIZE";
			Writer[VAR_ESIZE] << C_UInt32(this->fElmSize);
			this->vElmSize_Ptr = Writer.PropPosition(VAR_ESIZE);
			CdAllocArray::Saving(Writer);
		}

	private:
		SIZE64 vElmSize_Ptr;
	};



	/// Template functions for allocator
	
	template<typename TYPE, typename MEM_TYPE>
		struct COREARRAY_DLL_DEFAULT ALLOC_FUNC< FIXED_LENGTH<TYPE>, MEM_TYPE>
	{
		/// string type
		typedef typename TdTraits< FIXED_LENGTH<TYPE> >::TType StrType;

		/// read an array from CdAllocator
		static MEM_TYPE *Read(CdIterator &I, MEM_TYPE *Buffer, ssize_t n)
		{
			const typename TdTraits< FIXED_LENGTH<TYPE> >::RawType
				ZERO_CHAR = 0;
			const ssize_t ElmSize =
				static_cast<CdAllocArray*>(I.Handler)->ElmSize();
			const ssize_t N = ElmSize / sizeof(TYPE);
			StrType s(N, ZERO_CHAR), ss;

			// TODO: check string object
			I.Allocator->SetPosition(I.Ptr);
			I.Ptr += n * ElmSize;
			for (; n > 0; n--)
			{
				s.resize(N);
				I.Allocator->ReadData((void*)s.c_str(), ElmSize);
				size_t pos = s.find(ZERO_CHAR);
				if (pos != string::npos) s.resize(pos);
				COREARRAY_ENDIAN_LE_TO_NT_ARRAY((TYPE*)s.c_str(), s.size());
				ss.assign(s.begin(), s.end()); // compatible with platforms
				ValCvtArray(Buffer, &ss, 1);
				Buffer ++;
			}
			return Buffer;
		}

		/// read an array from CdAllocator
		static MEM_TYPE *ReadEx(CdIterator &I, MEM_TYPE *Buffer, ssize_t n, const C_BOOL sel[])
		{
			const typename TdTraits< FIXED_LENGTH<TYPE> >::RawType
				ZERO_CHAR = 0;
			const ssize_t ElmSize =
				static_cast<CdAllocArray*>(I.Handler)->ElmSize();
			const ssize_t N = ElmSize / sizeof(TYPE);
			StrType s(N, ZERO_CHAR), ss;

			// TODO: check string object
			I.Allocator->SetPosition(I.Ptr);
			I.Ptr += n * ElmSize;
			for (; n > 0; n--)
			{
				if (*sel++)
				{
					s.resize(N);
					I.Allocator->ReadData((void*)s.c_str(), ElmSize);
					size_t pos = s.find(ZERO_CHAR);
					if (pos != string::npos) s.resize(pos);
					COREARRAY_ENDIAN_LE_TO_NT_ARRAY((TYPE*)s.c_str(), s.size());
					ss.assign(s.begin(), s.end()); // compatible with platforms
					ValCvtArray(Buffer, &ss, 1);
					Buffer ++;
				} else {
					I.Allocator->SetPosition(
						I.Allocator->Position() + ElmSize);
				}
			}
			return Buffer;
		}

		/// write an array to CdAllocator
		static const MEM_TYPE *Write(CdIterator &I, const MEM_TYPE *Buffer,
			ssize_t n)
		{
			CdFixedStr<TYPE> *Ary =
				static_cast< CdFixedStr<TYPE>* >(I.Handler);
			ssize_t ElmSize = Ary->ElmSize();
			StrType s;

			// determine whether need to extend ElmSize
			ssize_t MaxSize = 0;
			const MEM_TYPE *p = Buffer;
			for (ssize_t m=n; m > 0; m--, p++)
			{
				VAL_CONV<StrType, MEM_TYPE>::Cvt(&s, p, 1);
				ssize_t L = s.size()*sizeof(TYPE);
				if (L > MaxSize) MaxSize = L;
			}

			if (MaxSize > ElmSize)
			{
				Ary->SetMaxLength(MaxSize / sizeof(TYPE));
				I.Ptr = (I.Ptr / ElmSize) * MaxSize;
				ElmSize = MaxSize;
			}

			I.Allocator->SetPosition(I.Ptr);
			I.Ptr += n * ElmSize;
			const size_t N = ElmSize / sizeof(TYPE);

			for (; n > 0; n--)
			{
				VAL_CONV<StrType, MEM_TYPE>::Cvt(&s, Buffer, 1);
				COREARRAY_ENDIAN_NT_TO_LE_ARRAY((TYPE*)s.c_str(), s.size());
				s.resize(N, 0);
				I.Allocator->WriteData(s.c_str(), ElmSize);
				Buffer ++;
			}

			return Buffer;
		}
	};


	/// Fixed-length UTF-8 string
	typedef CdFixedStr<C_UTF8>     CdFStr8;
	/// Fixed-length UTF-16 string
	typedef CdFixedStr<C_UTF16>    CdFStr16;
	/// Fixed-length UTF-32 string
	typedef CdFixedStr<C_UTF32>    CdFStr32;



	// =======================================================================
	// Variable-length string
	// =======================================================================

	/// Variable-length array
	/** \tparam TYPE  data type, e.g C_UTF8, C_UTF16 and C_UTF32
	**/
	template<typename TYPE> struct COREARRAY_DLL_DEFAULT
		VARIABLE_LENGTH
	{
		typedef TYPE TType;
	};

	template<> struct COREARRAY_DLL_DEFAULT TdTraits< VARIABLE_LENGTH<C_UTF8> >
	{
		typedef UTF8String TType;
		typedef C_UTF8 ElmType;
		typedef char RawType;
		static const int trVal = COREARRAY_TR_VARIABLE_LENGTH_STRING;
		static const unsigned BitOf = 8u;
		static const bool isClass = true;
		static const C_SVType SVType = svStrUTF8;

		static const char *StreamName() { return "dVStr8"; }
		static const char *TraitName() { return StreamName()+1; }
	};

	template<> struct COREARRAY_DLL_DEFAULT TdTraits< VARIABLE_LENGTH<C_UTF16> >
	{
		typedef UTF16String TType;
		typedef C_UTF16 ElmType;
		typedef C_UTF16 RawType;
		static const int trVal = COREARRAY_TR_VARIABLE_LENGTH_STRING;
		static const unsigned BitOf = 16u;
		static const bool isClass = true;
		static const C_SVType SVType = svStrUTF16;

		static const char *StreamName() { return "dVStr16"; }
		static const char *TraitName() { return StreamName()+1; }
	};

	template<> struct COREARRAY_DLL_DEFAULT TdTraits< VARIABLE_LENGTH<C_UTF32> >
	{
		typedef UTF32String TType;
		typedef C_UTF32 ElmType;
		typedef C_UTF32 RawType;
		static const int trVal = COREARRAY_TR_VARIABLE_LENGTH_STRING;
		static const unsigned BitOf = 32u;
		static const bool isClass = true;
		static const C_SVType SVType = svCustomStr;

		static const char *StreamName() { return "dVStr32"; }
		static const char *TraitName() { return StreamName()+1; }
	};


	/// Variable-length string container
	/** \tparam T  should be VARIABLE_LENGTH<C_UTF8>,
	 *             VARIABLE_LENGTH<C_UTF16> or VARIABLE_LENGTH<C_UTF32>
	 *  \sa  CdVStr8, CdVStr16, CdVStr32
	**/
	template<typename TYPE> class COREARRAY_DLL_DEFAULT CdVarStr:
		public CdArray< VARIABLE_LENGTH<TYPE> >
	{
	public:
		typedef typename TdTraits< FIXED_LENGTH<TYPE> >::TType TType;
		typedef TYPE ElmType;

		CdVarStr(): CdArray< VARIABLE_LENGTH<TYPE> >()
		{
			this->_ActualPosition = 0;
			this->_CurrentIndex = 0;
			this->_TotalSize = 0;
		}

        virtual CdGDSObj *NewOne(void *Param = NULL)
		{
			CdVarStr<TYPE> *rv = new CdVarStr<TYPE>;
			this->_AssignToDim(*rv);
			if (this->fPipeInfo)
				rv->fPipeInfo = this->fPipeInfo->NewOne();
			return rv;
		}

/*		virtual void Clear()
		{
			CdArray<TYPE>::Clear();
			this->_RewindIndex();
		}

		void AppendString(const ElmTypeEx val)
		{
			this->Append(&val, 1, TdTraits<T>::SVType);
		}
*/


		SIZE64 _ActualPosition;
		C_Int64 _CurrentIndex;
		SIZE64 _TotalSize;

		void _RewindIndex()
		{
			this->_ActualPosition = 0;
			this->_CurrentIndex = 0;
		}

		COREARRAY_INLINE TType _ReadString()
		{
			TYPE Ch;
			TType Val;
			BYTE_LE<CdAllocator> SS(this->fAllocator);
			SS.SetPosition(this->_ActualPosition);
			do {
				SS >> Ch;
				this->_ActualPosition += sizeof(Ch);
				if (Ch != 0) Val.push_back(Ch);
			} while (Ch != 0);
			this->_CurrentIndex ++;
			COREARRAY_ENDIAN_LE_TO_NT_ARRAY((TYPE*)Val.c_str(), Val.size());
			return Val;
		}

		COREARRAY_INLINE void _SkipString()
		{
			TYPE Ch;
			BYTE_LE<CdAllocator> SS(this->fAllocator);
			SS.SetPosition(this->_ActualPosition);
			do {
				SS >> Ch;
				this->_ActualPosition += sizeof(Ch);
			} while (Ch != 0);
			this->_CurrentIndex ++;
		}

		COREARRAY_INLINE void _WriteString(TType val)
		{
			TYPE Ch = 0;
			size_t pos = val.find(Ch);
			if (pos != string::npos) val.resize(pos);
			ssize_t str_size = (ssize_t)(val.size() * sizeof(TYPE));

			ssize_t old_len = 0;
			this->fAllocator.SetPosition(this->_ActualPosition);
			do {
				this->fAllocator.ReadData(&Ch, sizeof(Ch));
				if (Ch != 0) old_len ++;
			} while (Ch != 0);

			if (old_len > (ssize_t)val.size())
			{
				this->fAllocator.Move(this->_ActualPosition + old_len,
					this->_ActualPosition + (int)val.size(),
					this->_TotalSize - this->_ActualPosition - old_len);
				this->_TotalSize -= (old_len - (int)val.size());
			} else if (old_len < (ssize_t)val.size())
			{
				this->fAllocator.Move(this->_ActualPosition + old_len,
					this->_ActualPosition + (int)val.size(),
					this->_TotalSize - this->_ActualPosition - old_len);
				this->_TotalSize += ((int)val.size() - old_len);
			}

			BYTE_LE<CdAllocator> SS(this->fAllocator);
			SS.SetPosition(this->_ActualPosition);
			SS.W((TYPE*)val.c_str(), str_size);
			Ch = 0; SS << Ch;

			this->_ActualPosition += str_size + sizeof(Ch);
			this->_CurrentIndex ++;
		}

		COREARRAY_INLINE void _AppendString(TType val)
		{
			const typename TdTraits< FIXED_LENGTH<TYPE> >::RawType Ch = 0;
			size_t pos = val.find(Ch);
			if (pos != string::npos) val.resize(pos);
			ssize_t str_size = (ssize_t)(val.size() * sizeof(TYPE));

			SIZE64 OldSize = this->_TotalSize;
			BYTE_LE<CdAllocator> SS(this->fAllocator);
			SS.SetPosition(OldSize);
			SS.W((TYPE*)val.c_str(), str_size);
			SS << Ch;

			this->_TotalSize += str_size + (ssize_t)sizeof(Ch);
		}

		COREARRAY_INLINE void _Find_Position(SIZE64 Index)
		{
			if (Index != this->_CurrentIndex)
			{
				if (Index < this->_CurrentIndex)
					this->_RewindIndex();

				BYTE_LE<CdAllocator> SS(this->fAllocator);
				SS.SetPosition(this->_ActualPosition);
				while (this->_CurrentIndex < Index)
				{
					TYPE Ch;
					do {
						SS >> Ch;
						this->_ActualPosition += sizeof(Ch);
					} while (Ch != 0);
					this->_CurrentIndex ++;
				}
			}
		}

		COREARRAY_INLINE C_Int64 _TotalCount() const
		{
			return this->fTotalCount;
		}


	protected:

		/// offset the iterator
		virtual void IterOffset(CdIterator &I, SIZE64 val)
		{
			I.Ptr += val;
		}

		virtual SIZE64 AllocSize(C_Int64 Num)
		{
			return (Num == this->fTotalCount) ? this->_TotalSize : 0;
		}
	};


	/// Template functions for allocator
	
	template<typename TYPE, typename MEM_TYPE> struct COREARRAY_DLL_DEFAULT
		ALLOC_FUNC< VARIABLE_LENGTH<TYPE>, MEM_TYPE>
	{
		/// read an array from CdAllocator
		static MEM_TYPE *Read(CdIterator &I, MEM_TYPE *Buffer, ssize_t n)
		{
			CdVarStr<TYPE> *IT = static_cast< CdVarStr<TYPE>* >(I.Handler);
			IT->_Find_Position(I.Ptr / sizeof(TYPE));
			I.Ptr += n * sizeof(TYPE);
			for (; n > 0; n--)
			{
				typename TdTraits< VARIABLE_LENGTH<TYPE> >::TType s =
					IT->_ReadString();
				ValCvtArray(Buffer, &s, 1);
				Buffer ++;
			}
			return Buffer;
		}

		/// read an array from CdAllocator
		static MEM_TYPE *ReadEx(CdIterator &I, MEM_TYPE *Buffer, ssize_t n,
			const C_BOOL sel[])
		{
			CdVarStr<TYPE> *IT = static_cast< CdVarStr<TYPE>* >(I.Handler);
			IT->_Find_Position(I.Ptr / sizeof(TYPE));
			I.Ptr += n * sizeof(TYPE);
			for (; n > 0; n--)
			{
				if (*sel++)
				{
					typename TdTraits< VARIABLE_LENGTH<TYPE> >::TType s =
						IT->_ReadString();
					ValCvtArray(Buffer, &s, 1);
					Buffer ++;
				} else {
					IT->_SkipString();
				}
			}
			return Buffer;
		}

		/// write an array to CdAllocator
		static const MEM_TYPE *Write(CdIterator &I, const MEM_TYPE *Buffer,
			ssize_t n)
		{
			CdVarStr<TYPE> *IT = static_cast< CdVarStr<TYPE>* >(I.Handler);
			typename TdTraits< VARIABLE_LENGTH<TYPE> >::TType s;

			SIZE64 Idx = I.Ptr / sizeof(TYPE);
			IT->_Find_Position(Idx);

			for (; n > 0; n--)
			{
				VAL_CONV< typename TdTraits< VARIABLE_LENGTH<TYPE> >::TType,
					MEM_TYPE >::Cvt(&s, Buffer, 1);
				Buffer ++;
				if (Idx < IT->_TotalCount())
					IT->_WriteString(s);
				else
					IT->_AppendString(s);
			}

			return Buffer;
		}
	};


	/// Variable-length of UTF-8 string
	typedef CdVarStr<C_UTF8>     CdVStr8;
	/// Variable-length of UTF-16 string
	typedef CdVarStr<C_UTF16>    CdVStr16;
	/// Variable-length of UTF-32 string
	typedef CdVarStr<C_UTF32>    CdVStr32;
}

#endif /* _HEADER_COREARRAY_STRING_GDS_ */
