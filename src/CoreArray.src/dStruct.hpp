// ===========================================================
//     _/_/_/   _/_/_/  _/_/_/_/    _/_/_/_/  _/_/_/   _/_/_/
//      _/    _/       _/             _/    _/    _/   _/   _/
//     _/    _/       _/_/_/_/       _/    _/    _/   _/_/_/
//    _/    _/       _/             _/    _/    _/   _/
// _/_/_/   _/_/_/  _/_/_/_/_/     _/     _/_/_/   _/_/
// ===========================================================
//
// dStruct.hpp: Data container - array, matrix, etc
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

/**
 *	\file     dStruct.hpp
 *	\author   Xiuwen Zheng
 *	\version  1.0
 *	\date     2007 - 2011
 *	\brief    Data container - array, matrix, etc
 *	\details
*/

#ifndef _dStruct_H_
#define _dStruct_H_

#include <dBase.hpp>
#include <dStream.hpp>
#include <dFile.hpp>
#include <bitset>
#include <ctime>

namespace CoreArray
{
	/// Message code
	enum {
		/// Reset, no parameter
		mcReset				= 0x00,		// [ ]
		/// Refresh, no parameter
		mcRefresh			= 0x01,		// [ ]
		/// Number of dimensions changes, no parameter
		mcDimChanged		= 0x02,		// [ ]
		/// The index of dimension changes, a Int32 to specify which dimension
		mcDimLength			= 0x03,		// [ *32* ] { DimIndex }
		/// The size of element changes, no parameter
		mcElmSize			= 0x04,		// [ ]
		/// The cell content changes, an array of Int32 to specify the position
		mcCell				= 0x05,		// [ *32*, ... ]
		/// The attribute changes, no parameter
		mcAttrChanged		= 0x10,		// [ ]

    	/// Begin to load, Int64 to specify the stream size
		mcBeginLoad			= -0x10,	// [ *64* ] { Size }
    	/// Loading, Int64 to specify the stream position
		mcLoading			= -0x11,	// [ *64* ] { Stream Position }
    	/// Loading finishes, Int64 to specify the stream size
		mcLoaded			= -0x12,	// [ *64* ] { Size }
    	/// Begin to save, Int64 to specify the stream size
		mcBeginSave			= -0x13,	// [ *64* ] { Count }
    	/// Saving, Int64 to specify the stream position
		mcSaving			= -0x14,	// [ *64*, *32* ] { Index, Working Stream }
    	/// Saving finishes, Int64 to specify the stream size
		mcSaved				= -0x15		// [ *64* ] { Size }
	};


	/// Interval of notification, when saving or loading an object
	const std::clock_t COREARRAY_NOTIFY_TICK	= CLOCKS_PER_SEC / 4;

	/// Data storage mode
	enum TdStoreMode {
		lmKeepInStream = 0,  ///< data is kept in a CoreArray GDS file
		lmKeepInMem          ///< data is loaded in memory
	};

	class CdContainer;
	class CdSequenceX;

	#pragma pack(push,1)
	/// Iterator for CoreArray array-oriented container
	/** sizeof(TdIterator) = 32 */
	struct TdIterator
	{
        /// the handler of this iterator
		CdContainer* Handler;
        /// a pointer
		union
		{
			TdPtr64 Ptr;
			unsigned char VByte[32-sizeof(CdContainer*)];
		};

		/// next position
		TdIterator & operator++();
		/// previous position
		TdIterator & operator--();
		/// move to next offset position
		TdIterator & operator+= (const ssize_t offset);
		/// move back to previous offset position
		TdIterator & operator-= (const ssize_t offset);

		/// get an integer
		Int64 toInt();
		/// get a float number
		long double toFloat();
		/// get a string
		UTF16String toStr();
		/// set an integer
		void IntTo(const Int64 v);
		/// set a float number
		void FloatTo(const long double v);
		/// set a string
		void StrTo(const UTF16String &v);

		/// read an array of data from this iterator
		/** this iterator will move to next Cnt position
		 *  \param OutBuf  output buffer
		 *  \param Cnt     how many to read to a buffer
		 *  \param OutSV   specify type of output data
		 *  \sa  ::wData, CdContainer::_IterRData
		**/
		size_t rData(void *OutBuf, size_t Cnt, TSVType OutSV);
		/// write an array of data to the position of this iterator
		/** this iterator will move to next Cnt position
		 *  \param InBuf  input buffer
		 *  \param Cnt    how many to write from a buffer
		 *  \param InSV   specify type of input data
		 *  \sa  ::rData, CdContainer::_IterWData
		**/
		size_t wData(const void *InBuf, size_t Cnt, TSVType InSV);
	};
	#pragma pack(pop)


	class CdBaseOp
	{
	public:
		CdBaseOp();
		CdBaseOp(CBufdStream* vFilter);
		virtual ~CdBaseOp();

		virtual void InitParam() = 0;
		virtual void BeginOp(CdContainer* dC) { Init(); }
		virtual void EndOp(CdContainer* dC) { };

		inline CBufdStream *Filter() const { return fFilter; };
		void SetFilter(CBufdStream *Value);
		inline Int32 Row() const { return fRow; };
		inline Int32 Col() const { return fCol; };
		inline Int32 RowCnt() const { return fRowCnt; };
		inline Int32 ColCnt() const { return fColCnt; };
	protected:
		Int32 fRow, fCol, fRowCnt, fColCnt;
		CBufdStream* fFilter;
		virtual void Init();
	};

	struct TdAutoBaseOp
	{
	public:
		TdAutoBaseOp(CdBaseOp *vOp, CdContainer *vdC)
			{ Op = vOp; dC = vdC; Op->BeginOp(dC); }
		~TdAutoBaseOp()
        	{ Op->EndOp(dC); }
	private:
		CdBaseOp *Op;
		CdContainer *dC;
	};


	class CdBaseOpRead: public CdBaseOp
	{
	public:
		enum TdOpStatus { dsValid, dsTab, dsEndL, dsEOF };

		CdBaseOpRead();
		CdBaseOpRead(CBufdStream* vFilter);
		virtual ~CdBaseOpRead();

		virtual CdBaseOpRead &Read(TdIterator &it) = 0;
		inline TdOpStatus Status() const { return fStatus; };
	protected:
		TdOpStatus fStatus;
		virtual void Init();
	};

	class CdBaseOpWrite: public CdBaseOp
	{
	public:
		CdBaseOpWrite(): CdBaseOp() { }
		CdBaseOpWrite(CBufdStream* vFilter): CdBaseOp(vFilter) { }
		virtual ~CdBaseOpWrite() { }

		virtual CdBaseOpWrite &Write(TdIterator &it) = 0;
		virtual void WriteLn() = 0;
		virtual void WriteTab() = 0;
	};

	class CdOpReadText: public CdBaseOpRead
	{
	public:
		CdOpReadText();
		CdOpReadText(CBufdStream* vFilter);
		CdOpReadText(char const *const FileName);
		CdOpReadText(const UTF16String &FileName);
		virtual ~CdOpReadText();

		virtual void InitParam();
		virtual void BeginOp(CdContainer* dC);
		virtual CdBaseOpRead &Read(TdIterator &it);
		UTF8String rCell();
		long double rFloat();
		Int64 rInt();
		CdOpReadText &SkipCell(int Num);
		CdOpReadText &NextLine();

		UTF8String Delimit();
		void SetDelimit(const UTF8String &str);

		bool RepDelimit;
	protected:
		std::bitset<256> fDelimit;
		bool ReadItem(TdPtr64 &vPos, int &vCnt);
	};

	class CdOpWriteText: public CdBaseOpWrite
	{
	public:
		CdOpWriteText();
		CdOpWriteText(CBufdStream* vFilter);
		CdOpWriteText(char const *const FileName);
		virtual ~CdOpWriteText() { }

		virtual void InitParam();
		virtual CdBaseOpWrite &Write(TdIterator &it);

		virtual void WriteLn() { wLn(); };
		virtual void WriteTab() { wTab(); };

		CdOpWriteText &wLn();
		CdOpWriteText &wTab();
		CdOpWriteText &wCell(char const* const Text);
		CdOpWriteText &wFormat(char const* const fmt, ...);
		CdOpWriteText &wInt(int Value);
		CdOpWriteText &wInt(int const* Values, ssize_t num);
		CdOpWriteText &wFloat(const float Value);
		CdOpWriteText &wFloat(const double Value);
		CdOpWriteText &wFloat(float const* Values, ssize_t num);
		CdOpWriteText &wFloat(double const* Values, ssize_t num);

		UTF8String Delimit, Ln;
	protected:
		void WriteItem();
	};


	struct TdDefParamText
	{
		std::clock_t TimeInterval;

		TdDefParamText()
		{
        	TimeInterval = COREARRAY_NOTIFY_TICK;
        }
	};


	/// The root class for CoreArray container
	/** This class provide basic functions for a container. */
	class CdContainer: public CdGDSObj
	{
	public:
		friend struct TdIterator;
		friend class CdBaseOpRead;
		friend class CdOpReadText;
		friend class CdBaseOpWrite;
		friend class CdOpWriteText;

		CdContainer();
		virtual ~CdContainer();

		/// Throw a ErrSequence error
        virtual CdGDSObj *NewOne(void *Param = NULL);
		/// Throw a ErrSequence error
		virtual void AssignOne(CdGDSObj &Source, void *Param = NULL);

    	/// Return TSVType of data type
		virtual TSVType SVType() { return svCustom; };
		/// Return number of bits for the element type
		virtual unsigned BitOf() { return 0; };

		/// The start iterator
		virtual TdIterator atStart() = 0;
		/// The end iterator
		virtual TdIterator atEnd() = 0;

		/// Clear the container
		virtual void Clear() = 0;
		/// Return true, if the container is empty
		virtual bool Empty();
		/// Return number of elements in the container
		virtual Int64 Count();

		/// Load from a stream of text format
		/** \param Reader  a stream buffer object
		 *  \param Param   specify the details of text format, e.g.
		 *     tab-delimited or comma-delimted; to use the default settings if NULL
		**/
		virtual void LoadStreamText(CBufdStream &Reader,
			TdDefParamText *Param = NULL) = 0;
		/// Load from a file of text format
		/** \param FileName  the file name of input
		 *  \param Param     specify the details of text format, e.g.
		 *                   tab-delimited or comma-delimted
		**/
		void LoadFromText(const char *FileName, TdDefParamText *Param = NULL);

		/// Save to a stream of text format
		/** \param Writer  a stream buffer object
		 *  \param Param   specify the details of text format, e.g.
		 *     tab-delimited or comma-delimted; to use the default settings if NULL
		**/
		virtual void SaveStreamText(CBufdStream &Writer,
			TdDefParamText *Param = NULL) = 0;
		/// Save to a file of text format
		/** \param FileName  the file name of output
		 *  \param Param     specify the details of text format, e.g.
		 *     tab-delimited or comma-delimted; to use the default settings if NULL
		**/
		void SaveToText(const char *FileName, TdDefParamText *Param = NULL);

		/// Return the mode of data storage
		inline TdStoreMode StoreMode() const { return fStoreMode; };
		/// Set the mode of data storage
		virtual void SetLoadMode(TdStoreMode Mode);

		/// Set the mode of data compression
		virtual void SetPackedMode(const char *Mode);

		/// Close the writing mode if it is in compression, and sync the file
		virtual void CloseWriter();

	protected:
		/// Storage mode
		TdStoreMode fStoreMode;

		/// get an integer from *it
		virtual Int64 _toInt(TdIterator &it) = 0;
		/// get a float number from *it
		virtual long double _toFloat(TdIterator &it) = 0;
		/// get a string from *it
		virtual UTF16String _toStr(TdIterator &it) = 0;
		/// set an integer to *it
		virtual void _IntTo(TdIterator &it, const Int64 v) = 0;
		/// set a float number to *it
		virtual void _FloatTo(TdIterator &it, const LongFloat v) = 0;
		/// set a string to *it
		virtual void _StrTo(TdIterator &it, const UTF16String &v) = 0;
		/// set *source to *it
		virtual void _Assign(TdIterator &it, TdIterator &source) = 0;
		/// return 1, if *it1 > *it2; 0, if *it1 = *it2; -1, if *it1 < *it2
		virtual int _Compare(TdIterator &it1, TdIterator &it2) = 0;
        /// move to the next position
		virtual void _Advance(TdIterator &it) = 0;
		/// move back to the previous position
		virtual void _Previous(TdIterator &it) = 0;
		/// offset the position
		virtual void _Offset(TdIterator &it, ssize_t I);
        /// return true, if the iterator is at the start position
		virtual bool _isStart(const TdIterator &it) = 0;
        /// return true, if the iterator is at the ending position
		virtual bool _isEnd(const TdIterator &it) = 0;
		/// return true, if it1 == it2
		virtual bool _isEqual(TdIterator &it1, TdIterator &it2) = 0;
    	/// load *it from a filter
		virtual void _LoadIter(TdIterator &it, CdFilter &Reader) = 0;
		/// save *it to a filter
		virtual void _SaveIter(TdIterator &it, CdFilter &Writer) = 0;
    	/// initialize *it
		virtual void _InitIter(TdIterator &it, ssize_t Len) = 0;
    	/// free *it
		virtual void _DoneIter(TdIterator &it, ssize_t Len) = 0;
    	/// swap *it1 and *it2
		virtual void _Swap(TdIterator &it1, TdIterator &it2) = 0;
		/// read an array of data from this iterator
		virtual size_t _IterRData(TdIterator &it, void *OutBuf, size_t Cnt, TSVType OutSV);
		/// write an array of data to the position of this iterator
		virtual size_t _IterWData(TdIterator &it, const void *InBuf, size_t Cnt, TSVType InSV);
		/// load *it from a stream buffer of text format
		virtual void _LoadUTF8(TdIterator &it, CBufdStream &Buf, size_t Len);

    	/// Prepare an allocator for data stored in the CoreArray GDS file
		virtual void KeepInStream(CdFilter &Reader, void * Data) = 0;

		virtual CdBaseOpRead* DefOpRead(CBufdStream *Stream);
		virtual CdBaseOpWrite* DefOpWrite(CBufdStream *Stream);
	};


	struct TIterDataExt
	{
		void *pBuf;
		Int32 LastDim, *Index;
		CdSequenceX *Seq;
	};


	/// Container of array-oriented data
	class CdSequenceX: public CdContainer
	{
	public:
        /// The maximum number of dimensions
		static const size_t MaxSeqDim = 256;
        /// A type of dimension
		typedef Int32 TSeqDimBuf[MaxSeqDim];

		CdSequenceX();
		virtual ~CdSequenceX();

		virtual void AssignOne(CdGDSObj &Source, void *Param = NULL);

		virtual int DimCnt() const = 0;
		virtual void AddDim(Int32 NewDimLen = -1) = 0;
		virtual void DeleteDim() = 0;
		virtual void GetDimLen(Int32 *Dims) const = 0;
		virtual void SetDimLen(const Int32 *Lens, size_t LenCnt) = 0;
		virtual Int32 GetDLen(int DimIndex) const = 0;
		virtual void SetDLen(int DimIndex, Int32 Value) = 0;

		virtual TdIterator Iterator(Int32 const* DimIndex) = 0;

		virtual void rData(Int32 const* Start, Int32 const* Length,
			void *OutBuffer, TSVType OutSV);
		virtual void rDataEx(Int32 const* Start, Int32 const* Length,
			CBOOL *Selection[], void *OutBuffer, TSVType OutSV);
		virtual void wData(Int32 const* Start, Int32 const* Length,
			void const* InBuffer, TSVType InSV);
		virtual void Append(void const* Buffer, ssize_t Cnt, TSVType InSV) = 0;
		virtual void AppendIter(TdIterator &iter, ssize_t Cnt) = 0;

		virtual void LoadStreamText(CBufdStream &Reader,
			TdDefParamText *Param = NULL);
		virtual void SaveStreamText(CBufdStream &Writer,
			TdDefParamText *Param = NULL);
	protected:
		virtual void LoadDirect(CdFilter &Reader);
		virtual void SaveDirect(CdFilter &Writer);
		void xCheckRect(const Int32 *Start, const Int32 *Length) const;
		void xAssignToDim(CdSequenceX &Dest) const;
	};



	const size_t ARRAY_BUF_LEN = 0x10000;


    class CdVectorX;

	struct TIterVDataExt
	{
		CdVectorX *Seq;
		char *pBuf;
		Int32 LastDim;
		TdPtr64 p64;
    	bool AppendMode; // true -- call from "Append"
	};

	/// Array-oriented container, adding a TdAllocator
	class CdVectorX: public CdSequenceX
	{
	public:

	#ifdef COREARRAY_MSC
		template<typename TOutside, typename TInside,
			bool TraitEqual = (TdTraits<TOutside>::BitOf==TdTraits<TInside>::BitOf
				&& TdTraits<TOutside>::trVal==TdTraits<TInside>::trVal
				&& TdTraits<TOutside>::isClass==TdTraits<TInside>::isClass),
			int OutTrait = TdTraits<TOutside>::trVal,
			int InTrait = TdTraits<TInside>::trVal >
		friend struct TdVectorData;
	#else
		template<typename, typename, bool, int, int>
		friend struct TdVectorData;
	#endif

		CdVectorX(ssize_t vElmSize, size_t vDimCnt=0, bool vDirectMem=false);
		virtual ~CdVectorX();

		virtual bool Empty();
		virtual void Clear();
		virtual TdIterator atStart();
		virtual TdIterator atEnd();
		virtual void SaveStruct(CdFilter &Writer, bool IncludeName);
		virtual void Synchronize();
        virtual void CloseWriter();

		virtual void AddDim(Int32 NewDimLen = -1);
		virtual void DeleteDim();
		virtual void GetDimLen(Int32 *Dims) const;
		virtual void SetDimLen(const Int32 *Lens, size_t LenCnt);

		virtual TdIterator Iterator(Int32 const* DimIndex);
		virtual TdPtr64 IndexPtr(Int32 const* DimIndex);
		virtual int DimCnt() const { return (int)fDims.size(); };
		virtual Int32 GetDLen(int DimIndex) const;
		virtual void SetDLen(int DimIndex, Int32 Value);

		virtual void SetLoadMode(TdStoreMode Mode);
		virtual void SetPackedMode(const char *Mode);

		virtual void rData(Int32 const* Start, Int32 const* Length, void *OutBuffer, TSVType OutSV);
		virtual void Append(void const* Buffer, ssize_t Cnt, TSVType InSV);

		void CheckRange(Int32 const* DimI);

		virtual Int64 Count();
		virtual void SetCount(const Int64 Value);
		virtual Int64 CurrectCnt() const { return fCurrentCnt; }
		inline TdAllocator &Allocator() { return fAllocator; }
		/// Return memory size in bytes used for storage
		inline TdPtr64 MemSize() { return AllocNeed(true); }
        ///
		inline TdPtr64 CapacityMem() { return fCapacityMem; }
		void SetCapacityMem(const TdPtr64 NewMem);
		inline ssize_t ElmSize() const { return fElmSize; }

	protected:
		struct TdDimItem
		{
			Int32 DimLen;
			Int64 DimElmSize, DimElmCnt;
			TdDimItem() { DimLen = DimElmSize = DimElmCnt = 0; };
		};
		std::vector<TdDimItem> fDims;
		TdAllocator fAllocator;

		TdPtr64 fEndPtr, fCapacityMem;
		ssize_t fElmSize;
		Int64 fCurrentCnt, fCount;
		bool fNeedUpdate;

		virtual bool _isStart(const TdIterator &it);
		virtual bool _isEnd(const TdIterator &it);
		virtual bool _isEqual(TdIterator &it1, TdIterator &it2);
		virtual void _Advance(TdIterator &it);
		virtual void _Previous(TdIterator &it);
		virtual void _Offset(TdIterator &it, ssize_t I);
		virtual void _LoadIter(TdIterator &it, CdFilter &Reader);
		virtual void _SaveIter(TdIterator &it, CdFilter &Writer);
		virtual void _InitIter(TdIterator &it, ssize_t Len);
		virtual void _DoneIter(TdIterator &it, ssize_t Len);
		virtual void _Swap(TdIterator &it1, TdIterator &it2);
		virtual void KeepInStream(CdFilter &Reader, void * Data);
		virtual TdPtr64 AllocNeed(bool Full);

		virtual void LoadBefore(CdFilter &Reader, TdVersion Version);
		virtual void LoadAfter(CdFilter &Reader, TdVersion Version);
		virtual void SaveBefore(CdFilter &Writer);
		virtual void SaveAfter(CdFilter &Writer);
        virtual void GetPipeInfo();

		virtual void LoadDirect(CdFilter &Reader);
		virtual void SaveDirect(CdFilter &Writer);
		virtual void NeedMemory(const TdPtr64 NewMem);
		virtual void UpdateInfoProc(CBufdStream *Sender) {}; // call from UpdateInfo

		void UpdateInfo(CBufdStream *Sender);
		void SetElmSize(ssize_t NewSize);

		void xDimAuto(int DimIndex);
		void xInitIter(TdIterator &it, Int64 Len);
		void xDoneIter(TdIterator &it, Int64 Len);
		void xCheckRect(Int32 const* Start, Int32 const* Length);
		void xSetSmallBuf();
		void xSetLargeBuf();

		virtual bool DirectMem() { return false; };
		virtual bool DirectStream() { return true; };

	private:
		TdBlockID vAllocID;
		CdBlockStream *vAlloc_Stream;
		TdPtr64 vAlloc_Ptr, vCnt_Ptr;
		void xSetCapacity(const TdPtr64 NewMem);
	};



	/// Exception for CoreArray stream
	class ErrBaseOp: public Err_dObj
	{
	public:
		ErrBaseOp(): Err_dObj() { };
		ErrBaseOp(const UTF8String &msg) { fMessage = msg; }
		ErrBaseOp(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
	};

	/// Exception for container
	class ErrContainer: public Err_dObj
	{
	public:
		ErrContainer(): Err_dObj() { };
		ErrContainer(const UTF8String &msg) { fMessage = msg; }
		ErrContainer(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
	};

	/// Exception for array-oriented container
	class ErrSequence: public ErrContainer
	{
	public:
		ErrSequence(): ErrContainer() { };
		ErrSequence(const UTF8String &msg) { fMessage = msg; }
		ErrSequence(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
	};

	/// Exception for container algorithm
	class ErrAlgorithm: public Err_dObj
	{
	public:
		ErrAlgorithm(): Err_dObj() { };
		ErrAlgorithm(const UTF8String &msg) { fMessage = msg; }
		ErrAlgorithm(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
	};


	namespace Internal
	{
		// public template

		template<typename T, int TraitVal = TdTraits<T>::trVal> struct TdIterToVal;

		template<typename T> struct TdIterToVal<T, COREARRAY_TR_INTEGER>
		{
			inline static T Get(TdIterator &it) { return (T)(it.toInt()); }
			inline static void Set(TdIterator &it, const T v) { return it.IntTo(v); }
		};
		template<typename T> struct TdIterToVal<T, COREARRAY_TR_FLOAT>
		{
			inline static T Get(TdIterator &it) { return it.toFloat(); }
			inline static void Set(TdIterator &it, const T v) { return it.FloatTo(v); }
		};
		template<typename T> struct TdIterToVal<T, COREARRAY_TR_STRING>
		{
			inline static T Get(TdIterator &it)
				{ return TValCvt<T, UTF16String>::Cvt(it.toStr()); }
			inline static void Set(TdIterator &it, const T &v)
				{ return it.StrTo(TValCvt<UTF16String, T>::Cvt(v)); }
		};


		template<typename Function>
		void SeqIterRect(Int32 const* Start, Int32 const* Length, int DimCnt,
			TIterDataExt &Rec, Function Proc)
		{
			if (Start && Length)
			{
				CdSequenceX::TSeqDimBuf DFor, DForLen;
				Int32 *ForP = &DFor[0], *ForLenP = &DForLen[0];
				Int32 ForI = 0, ForEnd = DimCnt-1;

				memset((void*)DFor, 0, sizeof(DFor));
				DFor[0] = Start[0]; DForLen[0] = Length[0];
				Rec.Index = &DFor[0];
				while (ForI >= 0)
				{
					if (*ForLenP > 0)
					{
						if (ForI < ForEnd)
						{
							++ForI; *(++ForP) = *(++Start);
							*(++ForLenP) = *(++Length);
							continue;
						} else
							Proc(Rec);
					}
					--ForP; --ForLenP; --Start; --Length; --ForI;
					if (ForI >= 0)
						{ ++(*ForP); --(*ForLenP); }
				}
			} else
				Proc(Rec);
		};

		template<typename Function>
		void SeqIterRectEx(Int32 const* Start, Int32 const* Length, CBOOL *Sel[],
			int DimCnt, TIterDataExt &Rec, Function Proc)
		{
			if (Start && Length)
			{
				CdSequenceX::TSeqDimBuf DFor, DForLen;
				Int32 *ForP = &DFor[0], *ForLenP = &DForLen[0];
				Int32 ForI = 0, ForEnd = DimCnt-1;
                CBOOL *Selection = Sel[ForEnd];

				Rec.Index = &DFor[0];
				DFor[0] = Start[0]; DForLen[0] = Length[0];
				while (ForI >= 0)
				{
					if ((ForI >= ForEnd) || Sel[ForI][*ForP])
					{
						if (*ForLenP > 0)
						{
							if (ForI < ForEnd)
							{
								++ForI; *(++ForP) = *(++Start);
								*(++ForLenP) = *(++Length);
								continue;
							} else
								Proc(Rec, Selection);
						}
					} else {
						++(*ForP); --(*ForLenP);
						if (*ForLenP > 0) continue;
                    }
					--ForP; --ForLenP; --Start; --Length; --ForI;
					if (ForI >= 0)
						{ ++(*ForP); --(*ForLenP); }
				}
			} else
				Proc(Rec, Sel[0]);
		};

		template<typename Function>
		void VecIterRect(Int32 const* Start, Int32 const* Length, int DimCnt,
			TIterVDataExt &Rec, Function Functor)
		{
			if (Start && Length)
			{
				CdSequenceX::TSeqDimBuf DFor, DForLen;
				Int32 *ForP = &DFor[0], *ForLenP = &DForLen[0];
				Int32 ForI = 0, ForEnd = DimCnt-1;

				DFor[0] = Start[0]; DForLen[0] = Length[0];
				while (ForI >= 0)
				{
					if (*ForLenP > 0)
					{
						if (ForI < ForEnd)
						{
							++ForI; *(++ForP) = *(++Start);
							*(++ForLenP) = *(++Length);
							continue;
						} else {
							Rec.p64 = Rec.Seq->IndexPtr(DFor);
							Functor(Rec);
						}
					}
					--ForP; --ForLenP; --Start; --Length; --ForI;
					if (ForI >= 0)
						{ ++(*ForP); --(*ForLenP); }
				}
			} else
				Functor(Rec);
		};

		template<typename Function>
		void VecIterRectEx(Int32 const* Start, Int32 const* Length, CBOOL *Sel[],
			int DimCnt, TIterVDataExt &Rec, Function Functor)
		{
			if (Start && Length)
			{
				CdSequenceX::TSeqDimBuf DFor, DForLen;
				Int32 *ForP = &DFor[0], *ForLenP = &DForLen[0];
				Int32 ForI = 0, ForEnd = DimCnt-1;
                CBOOL *Selection = Sel[ForEnd];

				DFor[0] = Start[0]; DForLen[0] = Length[0];
				while (ForI >= 0)
				{
					if ((ForI >= ForEnd) || Sel[ForI][*ForP - *Start])
					{
						if (*ForLenP > 0)
						{
							if (ForI < ForEnd)
							{
								++ForI; *(++ForP) = *(++Start);
								*(++ForLenP) = *(++Length);
								continue;
							} else {
								Rec.p64 = Rec.Seq->IndexPtr(DFor);
								Functor(Rec, Selection);
							}
						}
					} else {
						++(*ForP); --(*ForLenP);
						if (*ForLenP > 0) continue;
					}
					--ForP; --ForLenP; --Start; --Length; --ForI;
					if (ForI >= 0)
						{ ++(*ForP); --(*ForLenP); }
				}
			} else
				Functor(Rec, Sel[0]);
		};


		template<unsigned SizeOf> struct TdIterMove
		{
			inline static void Read(void *rv, TdAllocator &I, const TdPtr64 p64)
				{ I.Read(p64, (void*)rv, SizeOf); }
			inline static void Write(void const* rv, TdAllocator &I, const TdPtr64 p64)
				{ I.Write(p64, (void*)rv, SizeOf); }
		};

		template<> struct TdIterMove<1u>
		{
			inline static void Read(void *rv, TdAllocator &I, const TdPtr64 p64)
				{ *((UInt8*)rv) = I.r8(p64); }
			inline static void Write(void const* rv, TdAllocator &I, const TdPtr64 p64)
				{ I.w8(p64, *((UInt8*)rv)); }
		};

		template<> struct TdIterMove<2u>
		{
			inline static void Read(void *rv, TdAllocator &I, const TdPtr64 p64)
				{ *((UInt16*)rv) = I.r16(p64); }
			inline static void Write(void const* rv, TdAllocator &I, const TdPtr64 p64)
				{ I.w16(p64, *((UInt16*)rv)); }
		};

		template<> struct TdIterMove<4u>
		{
			inline static void Read(void *rv, TdAllocator &I, const TdPtr64 p64)
				{ *((UInt32*)rv) = I.r32(p64); }
			inline static void Write(void const* rv, TdAllocator &I, const TdPtr64 p64)
				{ I.w32(p64, *((UInt32*)rv)); }
		};

		template<> struct TdIterMove<8u>
		{
			inline static void Read(void *rv, TdAllocator &I, const TdPtr64 p64)
				{ *((UInt64*)rv) = I.r64(p64); }
			inline static void Write(void const* rv, TdAllocator &I, const TdPtr64 p64)
				{ I.w64(p64, *((UInt64*)rv)); }
		};
	};



	#ifdef COREARRAY_MSC
	template<typename, typename, bool, int, int> struct TdVectorData {};
	#else
	template<typename TOutside, typename TInside,
		bool TraitEqual = (TdTraits<TOutside>::BitOf==TdTraits<TInside>::BitOf
			&& TdTraits<TOutside>::trVal==TdTraits<TInside>::trVal
			&& TdTraits<TOutside>::isClass==TdTraits<TInside>::isClass),
		int OutTrait = TdTraits<TOutside>::trVal,
		int InTrait = TdTraits<TInside>::trVal >
	struct TdVectorData {};
	#endif

	template<typename TOutside, typename TInside, int O, int I>
		struct TdVectorData<TOutside, TInside, true, O, I>
	{
		// Read
		inline static void rArray(TIterVDataExt &Rec)
		{
			ssize_t Lx = Rec.LastDim * sizeof(TInside);
			Rec.Seq->Allocator().Read(Rec.p64, (void*)Rec.pBuf, Lx);
			Rec.pBuf += Lx;
		}
		inline static void rArrayEx(TIterVDataExt &Rec, CBOOL *Sel)
		{
			for (size_t L = Rec.LastDim; L > 0; L--)
			{
				if (*Sel++)
				{
					Internal::TdIterMove<sizeof(TInside)>::Read(Rec.pBuf,
						Rec.Seq->fAllocator, Rec.p64);
					Rec.pBuf += sizeof(TInside);
				}
				Rec.p64 += sizeof(TInside);
			}
		}
		inline static void rItem(void *OutBuffer, const TdPtr64 p64, CdVectorX &Seq)
		{
			Internal::TdIterMove<sizeof(TInside)>::Read(OutBuffer,
				Seq.fAllocator, p64);
		}

		// Write
		inline static void wArray(TIterVDataExt &Rec)
		{
			ssize_t Lx = Rec.LastDim * sizeof(TInside);
			Rec.Seq->Allocator().Write(Rec.p64, (void*)Rec.pBuf, Lx);
			Rec.pBuf += Lx;
		}
		inline static void wItem(void const* InBuffer, const TdPtr64 p64, CdVectorX &Seq)
		{
			Internal::TdIterMove<sizeof(TInside)>::Write(InBuffer, Seq.fAllocator, p64);
		}
	};

	template<typename TOutside, typename TInside, int O, int I>
		struct TdVectorData<TOutside, TInside, false, O, I>
	{
		// Read
		static void rArray(TIterVDataExt &Rec)
		{
            char buf[ARRAY_BUF_LEN];
			Int32 Len = Rec.LastDim;
			TOutside *p = (TOutside*)Rec.pBuf;

			while (Len > 0)
			{
				ssize_t L = ((size_t)Len >= (sizeof(buf)/sizeof(TInside))) ?
					sizeof(buf)/sizeof(TInside) : Len;
				ssize_t Lx = L * sizeof(TInside);
				Len -= L;
				Rec.Seq->Allocator().Read(Rec.p64, (void*)buf, Lx);
				Rec.p64 += Lx;
				ValCvtArray<TOutside, TInside>(p, (TInside*)buf, L);
				p += L;
			}
			Rec.pBuf = (char*)p;
		}
		static void rArrayEx(TIterVDataExt &Rec, CBOOL *Sel)
		{
            char buf[ARRAY_BUF_LEN];
			Int32 Len = Rec.LastDim;
			TOutside *p = (TOutside*)Rec.pBuf;

			while (Len > 0)
			{
				ssize_t L = ((size_t)Len >= (sizeof(buf)/sizeof(TInside))) ?
					sizeof(buf)/sizeof(TInside) : Len;
				ssize_t Lx = L * sizeof(TInside);
				Len -= L;
				Rec.Seq->Allocator().Read(Rec.p64, (void*)buf, Lx);
				Rec.p64 += Lx;

				TInside *pbuf = (TInside*)buf;
				for (; L > 0; L--)
					if (*Sel++)
						*p++ = ValCvt<TOutside, TInside>(*pbuf++);
			}
			Rec.pBuf = (char*)p;
		}
		inline static void rItem(void *OutBuffer, const TdPtr64 p, CdVectorX &Seq)
		{
			char buf[sizeof(TInside)];
			Seq.fAllocator.Read(p, (void*)buf, sizeof(TInside));
			*((TOutside*)OutBuffer) = ValCvt<TOutside, TInside>(*((TInside*)buf));
		}
		// Write
		static void wArray(TIterVDataExt &Rec)
		{
			char buf[ARRAY_BUF_LEN];
			Int32 Len = Rec.LastDim;
			TOutside *s = (TOutside*)Rec.pBuf;
			while (Len > 0)
			{
				ssize_t L = ((size_t)Len >= (sizeof(buf)/sizeof(TInside))) ?
					sizeof(buf)/sizeof(TInside) : Len;
				ssize_t Lx = L * sizeof(TInside);
				Len -= L;
				ValCvtArray<TInside, TOutside>((TInside*)buf, s, L);
				s += L;
				Rec.Seq->Allocator().Write(Rec.p64, (void*)buf, Lx);
				Rec.p64 += Lx;
			}
			Rec.pBuf = (char*)s;
		}
		inline static void wItem(void const* InBuffer, const TdPtr64 p, CdVectorX &Seq)
		{
			char buf[sizeof(TInside)];
			*((TInside*)buf) = ValCvt<TInside, TOutside>(*((TOutside*)InBuffer));
			Seq.fAllocator.Write(p, (void*)buf, sizeof(TInside));
		}
	};


	/// Array-oriented container, template class
	/** \tparam T  data type, e.g. Int8, Int32 */
	template<typename T> class CdVector: public CdVectorX
	{
	public:
    	/// define ElmType = T
		typedef T ElmType;
		/// the corresponding elementary data type to ElmType
		typedef typename TdTraits<T>::TType ElmTypeEx;

		/// Constructor
		/** \param vDimCnt  number of dimensions, should be <= MaxSeqDim */
		CdVector(size_t vDimCnt = 0): CdVectorX(
			(TdTraits<T>::BitOf/8) + ((TdTraits<T>::BitOf%8)?1:0),
			vDimCnt, !TdTraits<T>::isClass) { };

    	/// new a CdVector<T> object
		virtual CdGDSObj *NewOne(void *Param = NULL)
		{
			CdVector<T> *rv = new CdVector<T>;
			xAssignToDim(*rv);
			if (fPipeInfo)
				rv->fPipeInfo = fPipeInfo->NewOne();
			return rv;
		}

		/// Assignment
		virtual void AssignOne(CdGDSObj &Source, void *Param = NULL)
		{
/*			if (dynamic_cast< CdVector<T>* >(&Source))
			{
				CdVectorX &Seq = *static_cast<CdVectorX*>(&Source);
				Seq.xAssignToDim(*this);
			} else
*/				CdSequenceX::AssignOne(Source, Param);
		}

		virtual char const* dName()
			{ return TdTraits<T>::StreamName(); };
		virtual char const* dTraitName()
			{ return TdTraits<T>::TraitName(); };

		virtual TSVType SVType() { return TdTraits<T>::SVType; };
		virtual unsigned BitOf() { return TdTraits<T>::BitOf; };

		ElmTypeEx Item(Int32 const* Index)
		{
			#ifdef COREARRAY_DEBUG_CODE
			CheckRange(Index);
			#endif
			xSetSmallBuf();
			ElmTypeEx rv;
			TdVectorData<ElmTypeEx, T>::rItem((void*)&rv, IndexPtr(Index), *this);
			return rv;
		}

		void SetItem(Int32 const* Index, const ElmTypeEx v)
		{
			#ifdef COREARRAY_DEBUG_CODE
			CheckRange(Index);
			#endif
			TdVectorData<ElmTypeEx, T>::wItem((void*)&v, IndexPtr(Index), *this);
		}

		virtual void rData(Int32 const* Start, Int32 const* Length,
			void *OutBuffer, TSVType OutSV)
		{
			#ifdef COREARRAY_DEBUG_CODE
			xCheckRect(Start, Length);
			#endif

			TIterVDataExt Rec;
			Rec.pBuf = (char*)OutBuffer;
			Rec.Seq = this; Rec.AppendMode = false;
			int vDim = DimCnt();

			if (vDim > 0)
				Rec.LastDim = *(Length + vDim - 1);
			else {
				Rec.LastDim = 1; Rec.p64 = 0; Start = Length = NULL;
			}
			if (Rec.LastDim > 0)
			{
				switch (OutSV)
				{
					case svInt8:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<Int8, T>::rArray); break;
					case svUInt8:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<UInt8, T>::rArray); break;
					case svInt16:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<Int16, T>::rArray); break;
					case svUInt16:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<UInt16, T>::rArray); break;
					case svInt32:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<Int32, T>::rArray); break;
					case svUInt32:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<UInt32, T>::rArray); break;
					case svInt64:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<Int64, T>::rArray); break;
					case svUInt64:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<UInt64, T>::rArray); break;
					case svFloat32:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<Float32, T>::rArray); break;
					case svFloat64:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<Float64, T>::rArray); break;
					case svStrUTF8:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<UTF8String, T>::rArray); break;
					case svStrUTF16:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<UTF16String, T>::rArray); break;
					default:
						CdVectorX::rData(Start, Length, OutBuffer, OutSV);
				}
			}
		};

		virtual void rDataEx(Int32 const* Start, Int32 const* Length,
			CBOOL *Selection[], void *OutBuffer, TSVType OutSV)
		{
			#ifdef COREARRAY_DEBUG_CODE
			xCheckRect(Start, Length);
			#endif

			TIterVDataExt Rec;
			Rec.pBuf = (char*)OutBuffer;
			Rec.Seq = this; Rec.AppendMode = false;
			int vDim = DimCnt();

			if (vDim > 0)
				Rec.LastDim = *(Length + vDim - 1);
			else {
				Rec.LastDim = 1; Rec.p64 = 0; Start = Length = NULL;
			}
			if (Rec.LastDim > 0)
			{
				switch (OutSV)
				{
					case svInt8:
						Internal::VecIterRectEx(Start, Length, Selection, vDim, Rec, TdVectorData<Int8, T>::rArrayEx); break;
					case svUInt8:
						Internal::VecIterRectEx(Start, Length, Selection, vDim, Rec, TdVectorData<UInt8, T>::rArrayEx); break;
					case svInt16:
						Internal::VecIterRectEx(Start, Length, Selection, vDim, Rec, TdVectorData<Int16, T>::rArrayEx); break;
					case svUInt16:
						Internal::VecIterRectEx(Start, Length, Selection, vDim, Rec, TdVectorData<UInt16, T>::rArrayEx); break;
					case svInt32:
						Internal::VecIterRectEx(Start, Length, Selection, vDim, Rec, TdVectorData<Int32, T>::rArrayEx); break;
					case svUInt32:
						Internal::VecIterRectEx(Start, Length, Selection, vDim, Rec, TdVectorData<UInt32, T>::rArrayEx); break;
					case svInt64:
						Internal::VecIterRectEx(Start, Length, Selection, vDim, Rec, TdVectorData<Int64, T>::rArrayEx); break;
					case svUInt64:
						Internal::VecIterRectEx(Start, Length, Selection, vDim, Rec, TdVectorData<UInt64, T>::rArrayEx); break;
					case svFloat32:
						Internal::VecIterRectEx(Start, Length, Selection, vDim, Rec, TdVectorData<Float32, T>::rArrayEx); break;
					case svFloat64:
						Internal::VecIterRectEx(Start, Length, Selection, vDim, Rec, TdVectorData<Float64, T>::rArrayEx); break;
					case svStrUTF8:
						Internal::VecIterRectEx(Start, Length, Selection, vDim, Rec, TdVectorData<UTF8String, T>::rArrayEx); break;
					case svStrUTF16:
						Internal::VecIterRectEx(Start, Length, Selection, vDim, Rec, TdVectorData<UTF16String, T>::rArrayEx); break;
					default:
						CdVectorX::rDataEx(Start, Length, Selection, OutBuffer, OutSV);
				}
			}
		};

		virtual void wData(Int32 const* Start, Int32 const* Length,
			void const* InBuffer, TSVType InSV)
		{
			#ifdef COREARRAY_DEBUG_CODE
			xCheckRect(Start, Length);
			#endif

			TIterVDataExt Rec;
			Rec.pBuf = (char*)InBuffer;
			Rec.Seq = this; Rec.AppendMode = false;
			int vDim = DimCnt();

			if (vDim > 0)
				Rec.LastDim = *(Length + vDim - 1);
			else {
				Rec.LastDim = 1; Rec.p64 = 0; Start = Length = NULL;
			}
			if (Rec.LastDim > 0)
			{
				switch (InSV)
				{
					case svInt8:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<Int8, T>::wArray); break;
					case svUInt8:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<UInt8, T>::wArray); break;
					case svInt16:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<Int16, T>::wArray); break;
					case svUInt16:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<UInt16, T>::wArray); break;
					case svInt32:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<Int32, T>::wArray); break;
					case svUInt32:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<UInt32, T>::wArray); break;
					case svInt64:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<Int64, T>::wArray); break;
					case svUInt64:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<UInt64, T>::wArray); break;
					case svFloat32:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<Float32, T>::wArray); break;
					case svFloat64:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<Float64, T>::wArray); break;
					case svStrUTF8:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<UTF8String, T>::wArray); break;
					case svStrUTF16:
						Internal::VecIterRect(Start, Length, vDim, Rec, TdVectorData<UTF16String, T>::wArray); break;
					default:
						CdVectorX::wData(Start, Length, InBuffer, InSV);
				}
			}
		};

		virtual void Append(void const* Buffer, ssize_t Cnt, TSVType InSV)
		{
			if (Cnt <= 0) return;

			TIterVDataExt Rec;
			Rec.pBuf = (char*)Buffer; Rec.LastDim = Cnt;
			Rec.Seq = this; Rec.AppendMode = true;

			if (!fDims.empty())
			{
				if (fDims.front().DimElmCnt <= 0)
					throw ErrSequence("The sub dimension should not be ZERO!");
				fChanged = true;
				Rec.p64 = fCurrentCnt * fElmSize;
				if (fAllocator.MemLevel())
				{
					fCurrentCnt += Cnt;
					NeedMemory(fCurrentCnt * fElmSize);
				} else if (fGDSStream)
					{ xSetLargeBuf(); xSetUpdate(); }
			} else {
				Rec.p64 = 0; Rec.LastDim = 1;
			}

			switch (InSV)
			{
				case svInt8:
					TdVectorData<Int8, T>::wArray(Rec); break;
				case svUInt8:
					TdVectorData<UInt8, T>::wArray(Rec); break;
				case svInt16:
					TdVectorData<Int16, T>::wArray(Rec); break;
				case svUInt16:
					TdVectorData<UInt16, T>::wArray(Rec); break;
				case svInt32:
					TdVectorData<Int32, T>::wArray(Rec); break;
				case svUInt32:
					TdVectorData<UInt32, T>::wArray(Rec); break;
				case svInt64:
					TdVectorData<Int64, T>::wArray(Rec); break;
				case svUInt64:
					TdVectorData<UInt64, T>::wArray(Rec); break;
				case svFloat32:
					TdVectorData<Float32, T>::wArray(Rec); break;
				case svFloat64:
					TdVectorData<Float64, T>::wArray(Rec); break;
				case svStrUTF8:
					TdVectorData<UTF8String, T>::wArray(Rec); break;
				case svStrUTF16:
					TdVectorData<UTF16String, T>::wArray(Rec); break;
				default:
					CdVectorX::Append(Buffer, Cnt, InSV);
			}

			if (!fDims.empty())
			{
				TdDimItem &R = fDims.front();
				if (!fAllocator.MemLevel()) fCurrentCnt += Cnt;
				if (fCurrentCnt >= fCount + R.DimElmCnt)
				{
					Cnt = (fCurrentCnt - fCount) / R.DimElmCnt;
					R.DimLen += Cnt;
					fCount += Cnt * R.DimElmCnt;
					fEndPtr = fCount * fElmSize;
					Notify32(mcDimLength, (Int32)0);
				}
				fChanged = true;
			} else
				Notify(mcRefresh);
		};

		virtual void AppendIter(TdIterator &iter, ssize_t Cnt)
		{
			if (Cnt > 0)
			{
				std::auto_ptr<ElmTypeEx> buf(new ElmTypeEx[Cnt]);
				ElmTypeEx *p = buf.get();
				for (ssize_t L=Cnt; L > 0; L--)
				{
					*p++ = Internal::TdIterToVal<ElmTypeEx>::Get(iter);
					++iter;
				}
				Append(buf.get(), Cnt, TdTraits<ElmTypeEx>::SVType);
			}
		}

	protected:
		virtual Int64 _toInt(TdIterator &it)
		{
			Int64 r;
			TdVectorData<Int64, T>::rItem((void*)&r, it.Ptr, *this);
			return r;
		}
		virtual long double _toFloat(TdIterator &it)
		{
			long double r;
			TdVectorData<long double, T>::rItem((void*)&r, it.Ptr, *this);
			return r;
		}
		virtual UTF16String _toStr(TdIterator &it)
		{
			UTF16String r;
			TdVectorData<UTF16String, T>::rItem((void*)&r, it.Ptr, *this);
			return r;
		}
		virtual void _IntTo(TdIterator &it, const Int64 v)
		{
			TdVectorData<Int64, T>::rItem((void*)&v, it.Ptr, *this);
		}
		virtual void _FloatTo(TdIterator &it, const LongFloat v)
		{
			TdVectorData<long double, T>::wItem((void*)&v, it.Ptr, *this);
		}
		virtual void _StrTo(TdIterator &it, const UTF16String &v)
		{
			TdVectorData<UTF16String, T>::wItem((void*)&v, it.Ptr, *this);
		}
		virtual void _Assign(TdIterator &it, TdIterator &source)
		{

		}
		virtual int _Compare(TdIterator &it1, TdIterator &it2)
		{
			return 0;
		}

		virtual size_t _IterRData(TdIterator &it, void *OutBuf, size_t Cnt,
			TSVType OutSV)
		{
			if ((it.Ptr + ((ssize_t)Cnt)*fElmSize) > fEndPtr)
            	Cnt = (fEndPtr - it.Ptr) / fElmSize;
			if (Cnt <= 0) return 0;

			TIterVDataExt Rec;
			Rec.pBuf = (char*)OutBuf;
			Rec.LastDim = Cnt; Rec.p64 = it.Ptr;
			Rec.Seq = this; Rec.AppendMode = false;

			switch (OutSV)
			{
				case svInt8:
					TdVectorData<Int8, T>::rArray(Rec); break;
				case svUInt8:
					TdVectorData<UInt8, T>::rArray(Rec); break;
				case svInt16:
					TdVectorData<Int16, T>::rArray(Rec); break;
				case svUInt16:
					TdVectorData<UInt16, T>::rArray(Rec); break;
				case svInt32:
					TdVectorData<Int32, T>::rArray(Rec); break;
				case svUInt32:
					TdVectorData<UInt32, T>::rArray(Rec); break;
				case svInt64:
					TdVectorData<Int64, T>::rArray(Rec); break;
				case svUInt64:
					TdVectorData<UInt64, T>::rArray(Rec); break;
				case svFloat32:
					TdVectorData<Float32, T>::rArray(Rec); break;
				case svFloat64:
					TdVectorData<Float64, T>::rArray(Rec); break;
				case svStrUTF8:
					TdVectorData<UTF8String, T>::rArray(Rec); break;
				case svStrUTF16:
					TdVectorData<UTF16String, T>::rArray(Rec); break;
				default:
					throw ErrSequence("Invalid SVType.");
			}
			it.Ptr += Cnt * fElmSize;
			return Cnt;
		}

		virtual size_t _IterWData(TdIterator &it, const void *InBuf, size_t Cnt,
			TSVType InSV)
		{
			if ((it.Ptr + ((ssize_t)Cnt)*fElmSize) > fEndPtr)
            	Cnt = (fEndPtr - it.Ptr) / fElmSize;
			if (Cnt <= 0) return 0;

			TIterVDataExt Rec;
			Rec.pBuf = (char*)InBuf;
			Rec.LastDim = Cnt; Rec.p64 = it.Ptr;
			Rec.Seq = this; Rec.AppendMode = false;

			switch (InSV)
			{
				case svInt8:
					TdVectorData<Int8, T>::wArray(Rec); break;
				case svUInt8:
					TdVectorData<UInt8, T>::wArray(Rec); break;
				case svInt16:
					TdVectorData<Int16, T>::wArray(Rec); break;
				case svUInt16:
					TdVectorData<UInt16, T>::wArray(Rec); break;
				case svInt32:
					TdVectorData<Int32, T>::wArray(Rec); break;
				case svUInt32:
					TdVectorData<UInt32, T>::wArray(Rec); break;
				case svInt64:
					TdVectorData<Int64, T>::wArray(Rec); break;
				case svUInt64:
					TdVectorData<UInt64, T>::wArray(Rec); break;
				case svFloat32:
					TdVectorData<Float32, T>::wArray(Rec); break;
				case svFloat64:
					TdVectorData<Float64, T>::wArray(Rec); break;
				case svStrUTF8:
					TdVectorData<UTF8String, T>::wArray(Rec); break;
				case svStrUTF16:
					TdVectorData<UTF16String, T>::wArray(Rec); break;
				default:
					throw ErrSequence("Invalid SVType.");
			}
			it.Ptr += Cnt * fElmSize;
			return Cnt;
		}

		virtual bool DirectStream()
			{ return !TdTraits<T>::isClass; };

		inline void xSetUpdate()
		{
			#ifndef COREARRAY_BORLANDC
			fAllocator.Filter->OnFlush.Set<CdVector>(
				this, &CdVector::UpdateInfo);
			#endif
		};
	};





	// ***********************************************************
	//
	// Classes
	//
	// ***********************************************************

	/// Array of signed integer with 8 bits
	typedef CdVector<Int8>		CdInt8;
	/// Array of unsigned integer with 8 bits
	typedef CdVector<UInt8>		CdUInt8;
	/// Array of signed integer with 16 bits
	typedef CdVector<Int16>		CdInt16;
	/// Array of unsigned integer with 16 bits
	typedef CdVector<UInt16>	CdUInt16;
	/// Array of signed integer with 32 bits
	typedef CdVector<Int32>		CdInt32;
	/// Array of unsigned integer with 32 bits
	typedef CdVector<UInt32>	CdUInt32;
	/// Array of signed integer with 64 bits
	typedef CdVector<Int64>		CdInt64;
	/// Array of unsigned integer with 64 bits
	typedef CdVector<UInt64>	CdUInt64;

	/// Array of float number with single precision
	typedef CdVector<Float32>		CdFloat32;
	/// Array of float number with double precision
	typedef CdVector<Float64>		CdFloat64;
	/// Array of float number with quadruple precision
	typedef CdVector<Float128>		CdFloat128;
}

#endif /* _dStruct_H_ */

