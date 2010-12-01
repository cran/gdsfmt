// ===========================================================
//     _/_/_/   _/_/_/  _/_/_/_/    _/_/_/_/  _/_/_/   _/_/_/
//      _/    _/       _/             _/    _/    _/   _/   _/
//     _/    _/       _/_/_/_/       _/    _/    _/   _/_/_/
//    _/    _/       _/             _/    _/    _/   _/
// _/_/_/   _/_/_/  _/_/_/_/_/     _/     _/_/_/   _/_/
// ===========================================================
//
// dStream.hpp: Stream classes and functions
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
 *	\file     dStream.hpp
 *	\author   Xiuwen Zheng
 *	\version  1.0
 *  \date     2007 - 2011
 *	\brief    Stream classes and functions
 *	\details
*/

#ifndef _dStream_H_
#define _dStream_H_

#include <dBase.hpp>
#include <zlib.h>


namespace CoreArray
{
	enum {
		MinBlock    = 65536,		// 64K
		SmallBlock  = 1024*1024*8,	// 8MB
		MediumBlock = 1024*1024*32,	// 32MB
		LargeBlock  = 1024*1024*96,	// 64MB
		HugeBlock   = 1024*1024*512	// 512MB
	};

	enum TAllocLevel { blChunkMemory, blTempFile, blFilter, blUnknown };

	struct TdAllocator
	{
	public:
		typedef COREARRAY_FASTCALL void (*TacDone)(TdAllocator &obj);
		typedef COREARRAY_FASTCALL void (*TacCapacity)(TdAllocator &obj, const TdPtr64 Size);
		typedef COREARRAY_FASTCALL void (*TacRead)(TdAllocator &obj, const TdPtr64 I, void *Buf, ssize_t Len);
		typedef COREARRAY_FASTCALL void (*TacWrite)(TdAllocator &obj, const TdPtr64 I, void const* Buf, ssize_t Len);
		typedef COREARRAY_FASTCALL void (*TacFill8)(TdAllocator &obj, const TdPtr64 I, const TdPtr64 Len, UInt8 Value);
		typedef COREARRAY_FASTCALL void (*TacMove)(TdAllocator &obj, const TdPtr64 Source, const TdPtr64 Dest, const TdPtr64 Len);
		typedef COREARRAY_FASTCALL void (*TacSwap)(TdAllocator &obj, const TdPtr64 I1, const TdPtr64 I2, const TdPtr64 Len);
		typedef COREARRAY_FASTCALL int (*TacCompare)(TdAllocator &obj, const TdPtr64 I, const void *Buf, ssize_t Len);
		typedef COREARRAY_FASTCALL UInt8 (*TacRead8)(TdAllocator &obj, const TdPtr64 I);
		typedef COREARRAY_FASTCALL UInt16 (*TacRead16)(TdAllocator &obj, const TdPtr64 I);
		typedef COREARRAY_FASTCALL UInt32 (*TacRead32)(TdAllocator &obj, const TdPtr64 I);
		typedef COREARRAY_FASTCALL UInt64 (*TacRead64)(TdAllocator &obj, const TdPtr64 I);
		typedef COREARRAY_FASTCALL float (*TacRead32f)(TdAllocator &obj, const TdPtr64 I);
		typedef COREARRAY_FASTCALL double (*TacRead64f)(TdAllocator &obj, const TdPtr64 I);
		typedef COREARRAY_FASTCALL void (*TacWrite8)(TdAllocator &obj, const TdPtr64 I, UInt8 Value);
		typedef COREARRAY_FASTCALL void (*TacWrite16)(TdAllocator &obj, const TdPtr64 I, UInt16 Value);
		typedef COREARRAY_FASTCALL void (*TacWrite32)(TdAllocator &obj, const TdPtr64 I, UInt32 Value);
		typedef COREARRAY_FASTCALL void (*TacWrite64)(TdAllocator &obj, const TdPtr64 I, const UInt64 Value);
		typedef COREARRAY_FASTCALL void (*TacWrite32f)(TdAllocator &obj, const TdPtr64 I, const float Value);
		typedef COREARRAY_FASTCALL void (*TacWrite64f)(TdAllocator &obj, const TdPtr64 I, const double Value);

		TAllocLevel Level;
		TdPtr64 Capacity;

		TacDone _Done;
		TacCapacity _SetCapacity;
		TacRead _Read;
		TacWrite _Write;
		TacFill8 _Fill;
		TacMove _Move;
		TacSwap _Swap;
		TacCompare _Compare;
		TacRead8 _r8;
		TacRead16 _r16;
		TacRead32 _r32;
		TacRead64 _r64;
		TacRead32f _r32f;
		TacRead64f _r64f;
		TacWrite8 _w8;
		TacWrite16 _w16;
		TacWrite32 _w32;
		TacWrite64 _w64;
		TacWrite32f _w32f;
		TacWrite64f _w64f;

		union
		{
			CBufdStream* Filter;
			unsigned char *Base;
		};

	public:
		TdAllocator() { std::memset((void*)this, 0, sizeof(TdAllocator)); };
		~TdAllocator() { if (_Done) { _Done(*this); _Done = NULL; } };

		inline bool MemLevel() const { return (Level <= blChunkMemory); };

		inline void SetCapacity(const TdPtr64 Size) { _SetCapacity(*this, Size); };

		inline void Fill(const TdPtr64 I, const TdPtr64 Len, UInt8 val)
			{ _Fill(*this, I, Len, val); };
		inline void Move(const TdPtr64 Source, const TdPtr64 Dest, const TdPtr64 Len)
			{ _Move(*this, Source, Dest, Len); };
		inline void Swap(const TdPtr64 I1, const TdPtr64 I2, const TdPtr64 Len)
        	{ _Swap(*this, I1, I2, Len); };

		inline void Read(const TdPtr64 I, void *Buf, ssize_t Len)
			{ _Read(*this, I, Buf, Len); };
		inline UInt8 r8(const TdPtr64 I) { return _r8(*this, I); };
		inline UInt16 r16(const TdPtr64 I) { return _r16(*this, I); };
		inline UInt32 r32(const TdPtr64 I) { return _r32(*this, I); };
		inline UInt64 r64(const TdPtr64 I) { return _r64(*this, I); };

		inline void Write(const TdPtr64 I, void const* Buf, ssize_t Len)
			{ _Write(*this, I, Buf, Len); };
		inline void w8(const TdPtr64 I, UInt8 val) { _w8(*this, I, val); };
		inline void w16(const TdPtr64 I, UInt16 val) { _w16(*this, I, val); };
		inline void w32(const TdPtr64 I, UInt32 val) { _w32(*this, I, val); };
		inline void w64(const TdPtr64 I, UInt64 val) { _w64(*this, I, val); };
	};

	/// Exception for TdAllocator
	class ErrAllocator: public Err_dObj
	{
	public:
		enum EdAllocType { eaRead, eaWrite };

		ErrAllocator(EdAllocType Ed);
		ErrAllocator(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
		ErrAllocator(TAllocLevel OldLevel, TAllocLevel NewLevel);
	};


	void InitAllocator(TdAllocator &Allocator, bool CanRead, bool CanWrite,
		TAllocLevel vLevel=blChunkMemory, CBufdStream* BufFilter=NULL);
	void InitAllocatorEx(TdAllocator &Allocator, bool CanRead, bool CanWrite,
		CdStream* Stream);
	void DoneAllocator(TdAllocator &Allocator);
	void InitMemAllocator(TdAllocator &Allocator, const TdPtr64 Size = 0);
	void SwitchAllocator(TdAllocator &Allocator, bool CanRead, bool CanWrite,
		const TAllocLevel NewLevel, CBufdStream* BufFilter=NULL);
	void LoadAllocator(TdAllocator &Allocator, CdStream* Source,
		TdPtr64 Start, TdPtr64 Len);
	void SaveAllocator(TdAllocator &Allocator, CdStream* Dest,
		TdPtr64 Start, TdPtr64 Len);
	void LoadAllocator(TdAllocator &Allocator, CBufdStream* Source,
		TdPtr64 Start, TdPtr64 Len);
	void SaveAllocator(TdAllocator &Allocator, CBufdStream* Dest,
		TdPtr64 Start, TdPtr64 Len);


	/// Define the size of buffer, when saving, loading, or copying
	const size_t COREARRAY_STREAM_BUFFER	= 0x10000;


	/// Stream with a handle
	class CdHandleStream: public CdStream
	{
	public:
		CdHandleStream() { fHandle = NullSysHandle; };
		CdHandleStream(TSysHandle AHandle) { fHandle = AHandle; };

		virtual ssize_t Read(void *Buffer, ssize_t Count);
		virtual ssize_t Write(void *const Buffer, ssize_t Count);
		virtual TdPtr64 Seek(const TdPtr64 Offset, TdSysSeekOrg Origin);
		virtual void SetSize(const TdPtr64 NewSize);

		inline TSysHandle Handle() const { return fHandle; };
	protected:
		TSysHandle fHandle;
	};


	/// File stream
	class CdFileStream: public CdHandleStream
	{
	public:
		enum TdOpenMode { fmCreate=0, fmOpenRead, fmOpenWrite, fmOpenReadWrite };

		CdFileStream(const char * const AFileName, TdOpenMode Mode);
		virtual ~CdFileStream();

		inline const std::string& FileName() const { return fFileName; };
	protected:
		 std::string fFileName;
		 CdFileStream(): CdHandleStream() {};
	};


	/// Temporary stream, in which a temporary file is created
	class CdTempStream: public CdFileStream
	{
	public:
		CdTempStream(const char * const Path);
		virtual ~CdTempStream();
	};


	/// Memory stream
	class CdMemoryStream: public CdStream
	{
	public:
		CdMemoryStream(size_t Size = 0);

		virtual ssize_t Read(void *Buffer, ssize_t Count);
		virtual ssize_t Write(void *const Buffer, ssize_t Count);
		virtual TdPtr64 Seek(const TdPtr64 Offset, TdSysSeekOrg Origin);

		virtual TdPtr64 GetSize();
		virtual void SetSize(const TdPtr64 NewSize);

        void *BufPointer();
	protected:
		TdAllocator fAllocator;
		ssize_t fPosition;
	};


	/// Stream for standard input
	class CdStdInStream: public CdStream
	{
	public:
		CdStdInStream();
		virtual ~CdStdInStream();

		virtual ssize_t Read(void *Buffer, ssize_t Count);
		virtual ssize_t Write(void *const Buffer, ssize_t Count);
		virtual TdPtr64 Seek(const TdPtr64 Offset, TdSysSeekOrg Origin);

		virtual TdPtr64 GetSize();
		virtual void SetSize(const TdPtr64 NewSize);
	};


	/// Stream for standard output
	class CdStdOutStream: public CdStream
	{
	public:
		CdStdOutStream();
		virtual ~CdStdOutStream();

		virtual ssize_t Read(void *Buffer, ssize_t Count);
		virtual ssize_t Write(void *const Buffer, ssize_t Count);
		virtual TdPtr64 Seek(const TdPtr64 Offset, TdSysSeekOrg Origin);

		virtual TdPtr64 GetSize();
		virtual void SetSize(const TdPtr64 NewSize);
	};


	// TdCompressRemainder

	struct TdCompressRemainder
	{
		size_t Size;
		union {
			unsigned char Buf[8];
			UInt64 Buf64;
		};

		TdCompressRemainder()
			{ std::memset((void*)this, 0, sizeof(TdCompressRemainder)); }
	};


	// Wrapper of zlib

	/// The root class of ZIP stream
	class CdBaseZStream: public CdStream
	{
	public:
		CdBaseZStream(CdStream* vStream);
		virtual ~CdBaseZStream();

		inline CdStream *Stream() const { return fStream; };
		inline TdPtr64 TotalIn() const { return fTotalIn; };
		inline TdPtr64 TotalOut() const { return fTotalOut; };
	protected:
		CdStream* fStream;
		TdPtr64 fStreamPos, fStreamBase;
		Int64 fTotalIn, fTotalOut;
		z_stream fZStream;
		unsigned char fBuffer[65536];
	};


	/// Input stream for zlib
	class CdZIPDeflate: public CdBaseZStream
	{
	public:
		enum TZLevel {
			zcNone = 0, zcFastest = 1, zcDefault = 2, zcMax = 3,
			zcLevel1, zcLevel2, zcLevel3, zcLevel4, zcLevel5,
			zcLevel6, zcLevel7, zcLevel8, zcLevel9 };
		enum TZStrategy { zsDefault, zsFiltered, zsHuffman, zsRLE, zsFixed };

		CdZIPDeflate(CdStream* Dest, TZLevel DeflateLevel);
		CdZIPDeflate(CdStream* Dest, TZLevel DeflateLevel,
			int windowBits, int memLevel, TZStrategy Strategy);
		virtual ~CdZIPDeflate();

		virtual ssize_t Read(void *Buffer, ssize_t Count);
		virtual ssize_t Write(void *const Buffer, ssize_t Count);
		virtual TdPtr64 Seek(const TdPtr64 Offset, TdSysSeekOrg Origin);
		virtual void SetSize(const TdPtr64 NewSize);
		void Close();

		ssize_t Pending();
    	inline bool HaveClosed() const { return fHaveClosed; }
		TdCompressRemainder *PtrExtRec;
	protected:
		bool fHaveClosed;
		void SyncFlush(int Code);
	};


	/// Output stream for zlib
	class CdZIPInflate: public CdBaseZStream
	{
	public:
		CdZIPInflate(CdStream* Source);
		CdZIPInflate(CdStream* Source, int windowBits);
		virtual ~CdZIPInflate();

		virtual ssize_t Read(void *Buffer, ssize_t Count);
		virtual ssize_t Write(void *const Buffer, ssize_t Count);
		virtual TdPtr64 Seek(const TdPtr64 Offset, TdSysSeekOrg Origin);
		virtual TdPtr64 GetSize();
		virtual void SetSize(const TdPtr64 NewSize);

		void ClearPoints();

		inline bool RanAccess() const { return fRanAccess; };
		void SetRanAccess(bool Value);
		inline ssize_t BlockSize() const { return fBlockSize; };
		void SetBlockSize(ssize_t Value);
	protected:
		ssize_t fBlockSize;
		bool fRanAccess;
		TdPtr64 fBlockStart, fCurPos;
	private:
		struct TZIPPointRec { TdPtr64 SourcePos; z_stream Rec; };

		std::vector<TZIPPointRec> vPoints;

		TZIPPointRec *AddPoint();
		TZIPPointRec *PointIndex(unsigned int i);
		TZIPPointRec *PointIndexEx(unsigned int i);
	};


	/// Exception for ZIP stream
	class EZLibError: public Err_dStream
	{
	public:
		EZLibError(int Code);
		EZLibError(const char *fmt, ...) { fErrCode = -1; _COREARRAY_ERRMACRO_(fmt); }
		int ErrCode() const { return fErrCode; };
	private:
		int fErrCode;
	};


	// TdBlockCollection, TdBlockStream

	/// Type of block ID, used in TdBlockCollection and TdBlockStream
	typedef TdNumber<UInt32, sizeof(UInt32)> TdBlockID;

	/// an operator, to read TdBlockID from a stream
	inline CdStream& operator>> (CdStream &s, TdBlockID& out)
		{ out = s.rUInt32(); return s; }
	/// an operator, to write TdBlockID to a stream
	inline CdStream& operator<< (CdStream &s, const TdBlockID &in)
		{ s.wUInt32(in); return s; }

	/// an operator, to read a TdBlockID from a buffer
	inline bool operator>> (CdFilter::TdVar &s, TdBlockID& out)
		{ return (s >> out.get()); }
	/// an operator, to write a TdBlockID to a buffer
	inline void operator<< (CdFilter::TdVar &s, const TdBlockID& in)
		{ s << in.get(); }



	class CdBlockCollection;

	///
	class CdBlockStream: public CdStream
	{
	public:
		friend class CdBlockCollection;

		struct TBlockInfo
		{
			static const TdPtr64 HeadSize = TdBlockID::size + TdPosType::size;

			TBlockInfo *Next;
			TdPtr64 BlockStart, BlockSize;	// Position in Block
			TdPtr64 StreamStart, StreamNext;	// Stream Position
			bool Head;

			TBlockInfo();
			TdPtr64 AbsStart();
			void SetSize(CdStream &Stream, const TdPtr64 Size);
			void SetNext(CdStream &Stream, const TdPtr64 Next);
			void SetSize2(CdStream &Stream, const TdPtr64 Size, const TdPtr64 Next);
		};

		CdBlockStream(CdBlockCollection &vCollection);
		virtual ~CdBlockStream();

		virtual ssize_t Read(void *Buffer, ssize_t Count);
		virtual ssize_t Write(void *const Buffer, ssize_t Count);
		virtual TdPtr64 Seek(const TdPtr64 Offset, TdSysSeekOrg Origin);
		virtual TdPtr64 GetSize();
		virtual void SetSize(const TdPtr64 NewSize);
        void SetSizeOnly(const TdPtr64 NewSize);

		void SyncSizeInfo();

		bool ReadOnly() const;
		int ListCount() const;

		inline TdBlockID ID() const { return fID; }
		inline TdPtr64 Capacity() const { return fBlockCapacity; }
		inline TdPtr64 Size() const { return fBlockSize; }
		inline CdBlockCollection &Collection() const { return fCollection; }
		inline const TBlockInfo *List() const { return fList; }
	protected:
		CdBlockCollection &fCollection;
		TdBlockID fID;
		TBlockInfo *fList, *fCurrent;
		TdPtr64 fPosition, fBlockCapacity;
		TdPosType fBlockSize;

	private:
    	bool fNeedSyncSize;
		inline TBlockInfo *_FindCur(const TdPtr64 Pos);
	};


	class CdBlockCollection
	{
	public:
		friend class CdBlockStream;

		CdBlockCollection(const TdPtr64 vCodeStart=0);
		virtual ~CdBlockCollection();

		void LoadStream(CdStream *vStream, bool vReadOnly);
		void WriteStream(CdStream *vStream);
		void Clear();

    	CdBlockStream *NewBlockStream();
    	void DeleteBlockStream(TdBlockID id);
		CdBlockStream *operator[] (const TdBlockID &id); // always return an object
		bool HaveID(TdBlockID id);

		inline CdStream &Stream() const { return *fStream; };
		inline CdObjClassMgr *ClassMgr() const { return fClassMgr; };
		inline bool ReadOnly() const { return fReadOnly; };
		inline const std::vector<CdBlockStream*> &BlockList() const
			{ return fBlockList; }
		inline const CdBlockStream::TBlockInfo *UnusedBlock() const
        	{ return fUnuse; }
	protected:
		CdStream *fStream;
		TdPtr64 fStreamSize;
		CdBlockStream::TBlockInfo *fUnuse;
		std::vector<CdBlockStream*> fBlockList;
		TdPtr64 fCodeStart;
		CdObjClassMgr *fClassMgr;
		bool fReadOnly;

		void _IncStreamSize(CdBlockStream &Block, const TdPtr64 NewSize);
		void _DecStreamSize(CdBlockStream &Block, const TdPtr64 NewSize);
		CdBlockStream::TBlockInfo *_NeedBlock(TdPtr64 Size, bool Head);
	private:
		TdBlockID vNextID;
	};
}

#endif /* _dStream_H_ */

