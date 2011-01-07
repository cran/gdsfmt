// ===========================================================
//     _/_/_/   _/_/_/  _/_/_/_/    _/_/_/_/  _/_/_/   _/_/_/
//      _/    _/       _/             _/    _/    _/   _/   _/
//     _/    _/       _/_/_/_/       _/    _/    _/   _/_/_/
//    _/    _/       _/             _/    _/    _/   _/
// _/_/_/   _/_/_/  _/_/_/_/_/     _/     _/_/_/   _/_/
// ===========================================================
//
// dFile.hpp: Functions and classes for CoreArray Generic Data Structure (GDS)
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
 *	\file     dFile.hpp
 *	\author   Xiuwen Zheng
 *	\version  1.0
 *	\date     2007 - 2011
 *	\brief    Functions and classes for CoreArray Generic Data Structure (GDS)
 *	\details
*/

#ifndef _dFile_H_
#define _dFile_H_

#include <dBase.hpp>
#include <dStream.hpp>


namespace CoreArray
{
	class CdGDSObj;

	/// Data pipe
	class CdPipeMgrItem
	{
	public:
		friend class CdStreamPipeMgr;
		friend class CdGDSObj;

		/// Constructor
		CdPipeMgrItem();
		/// Destructor
		virtual ~CdPipeMgrItem();

		/// Create a new CdPipeMgrItem object
        virtual CdPipeMgrItem *NewOne() = 0;

		/// Return the name of coder
		virtual const char *Coder() const = 0;
		/// Return the description of coder
		virtual const char *Description() const = 0;
		/// Return whether or not Mode is self
		virtual bool Equal(const char *Mode) const = 0;

		virtual void PushReadPipe(CBufdStream &buf) = 0;
		virtual void PushWritePipe(CBufdStream &buf) = 0;
		virtual void PopPipe(CBufdStream &buf) = 0;
		virtual bool WriteMode(CBufdStream &buf) const = 0;
		virtual void ClosePipe(CBufdStream &buf) = 0;

    	void UpdateStreamSize();
		inline TdPtr64 StreamTotalIn() const { return fStreamTotalIn; }
		inline TdPtr64 StreamTotalOut() const { return fStreamTotalOut; }

		inline CdGDSObj *Owner() { return fOwner; }
		inline TdCompressRemainder &Remainder() { return fRemainder; }
	protected:
    	CdGDSObj *fOwner;
		TdPtr64 fStreamTotalIn, fStreamTotalOut;
		TdCompressRemainder fRemainder;

		virtual CdPipeMgrItem *Match(const char *Mode) = 0;
		virtual bool GetStreamInfo(CBufdStream *Filter) = 0;
		virtual void UpdateStreamInfo(CdStream &Stream) = 0;
		virtual void LoadStream(CdFilter &Reader, TdVersion Version);
		virtual void SaveStream(CdFilter &Writer);

		static int Which(const char *txt, const char **Strs, int nStrs);
		static bool EqualStrNoCase(const char *s1, const char *s2);
	};


	/// The manager of stream pipes
	class CdStreamPipeMgr
	{
	public:
		CdStreamPipeMgr();
		~CdStreamPipeMgr();

		void Register(CdPipeMgrItem *vNewPipe);
		CdPipeMgrItem *Match(CdGDSObj &Obj, const char *Mode);

		inline const std::vector<CdPipeMgrItem*> &RegList() const
			{ return fRegList; };
	protected:
		std::vector<CdPipeMgrItem*> fRegList;
	};

	extern CdStreamPipeMgr dStreamPipeMgr;



	class CdGDSFolder;
	class CdGDSFile;

	/// Attribute class for CdGDSObj
	class CdObjAttr: public CdObject
	{
	public:
		friend class CdGDSObj;
		friend class CdGDSFolder;

		CdObjAttr(CdGDSObj &vOwner);
		virtual ~CdObjAttr();

    	void Assign(CdObjAttr &Source);
		TdsData &Add(const UTF16String &Name);
		inline TdsData &Add(const char *Name) { return Add(PChartoUTF16(Name)); }

		int IndexName(const UTF16String &Name);
		inline int IndexName(const char *Name) { return IndexName(PChartoUTF16(Name)); }

		inline bool HasName(const UTF16String &Name) { return IndexName(Name)>=0; }
		inline bool HasName(const char *Name) { return IndexName(Name)>=0; }

		void Delete(const UTF16String &Name);
		void Delete(int Index);
		void Clear();

		inline size_t Count() const { return fList.size(); }
        void Changed();

		inline CdGDSObj &Owner() const { return fOwner; }

		TdsData & operator[](const UTF16String &Name);
		inline TdsData & operator[](const char *Name)
			{ return (*this)[UTF8toUTF16(Name)]; }
		TdsData & operator[](int Index);

		inline UTF16String &Names(int Index) { return fList[Index]->name; }
		void SetName(const UTF16String &OldName, const UTF16String &NewName);
		void SetName(int Index, const UTF16String &NewName);
		void SetName(int Index, const char *NewName)
        	{ SetName(Index, PChartoUTF16(NewName)); };
	protected:
		struct TdPair {
			UTF16String name;
			TdsData val;
		};

		CdGDSObj &fOwner;
		std::vector<TdPair*> fList;
		virtual void LoadAfter(CdFilter &Reader, const TdVersion Version);
		virtual void SaveAfter(CdFilter &Writer);
	private:
		std::vector<TdPair*>::iterator Find(const UTF16String &Name);
        void xValidateName(const UTF16String &name);
	};

	
	/// CoreArray object
	class CdGDSObj: public CdObjMsg
	{
	public:
    	friend class CdPipeMgrItem;
		friend class CdObjAttr;
		friend class CdGDSFolder;
		friend class CdGDSFile;

		CdGDSObj(CdGDSFolder *vFolder = NULL);
		virtual ~CdGDSObj();

		virtual void SaveStruct(CdFilter &Writer, bool IncludeName);

		virtual CdGDSObj *NewOne(void *Param = NULL);
		virtual void AssignOne(CdGDSObj &Source, void *Param = NULL);

		virtual UTF16String Name() const;
		UTF16String FullName(const UTF16String &Delimiter) const;
		UTF16String FullName(const char *Delimiter = "/") const
			{ return FullName(PChartoUTF16(Delimiter)); };

		void SetName(const UTF16String &NewName);
		void SetName(const char *NewName)
			{ SetName(PChartoUTF16(NewName)); };

		void MoveTo(CdGDSFolder &folder);

		virtual void Synchronize();

		CdGDSFile *GDSFile();
		inline CdObjAttr &Attribute() { return fAttr; };
		inline CdBlockStream *GDSStream() const { return fGDSStream; };
		inline CdGDSFolder *Folder() const { return fFolder; };

		inline CdPipeMgrItem *PipeInfo() { return fPipeInfo; };
	protected:
		CdObjAttr fAttr;
		CdGDSFolder *fFolder;
		CdBlockStream *fGDSStream;
		CdPipeMgrItem *fPipeInfo;
		bool fChanged;

		virtual void LoadBefore(CdFilter &Reader, TdVersion Version);
		virtual void LoadAfter(CdFilter &Reader, TdVersion Version);
		virtual void SaveBefore(CdFilter &Writer);
		virtual void SaveAfter(CdFilter &Writer);
		virtual void SaveToBlockStream();
		virtual void GetPipeInfo();

		inline bool _GetStreamPipeInfo(CBufdStream *buf, bool Close)
		{
			if (Close && buf)
				fPipeInfo->ClosePipe(*buf);
			return fPipeInfo->GetStreamInfo(buf);
		}
		inline void _UpdateStreamPipeInfo(CdStream &Stream)
		{
			fPipeInfo->UpdateStreamInfo(Stream);
		}

		void _CheckGDSStream();
		static void _RaiseInvalidAssign(const std::string &msg);
	private:
		static void _GDSObjInitProc(CdObjClassMgr &Sender, CdObject *dObj, void *Data);
	};


    /// Folder class for CoreArray GDS format
	class CdGDSFolder: public CdGDSObj
	{
	public:
		friend class CdGDSObj;
		friend class CdGDSFile;

		CdGDSFolder(CdGDSFolder *vFolder = NULL);
		virtual ~CdGDSFolder();

        virtual CdGDSObj *NewOne(void *Param = NULL);
		void AssignOneEx(CdGDSFolder &Source);

		CdGDSFolder &AddFolder(const UTF16String &Name);
		CdGDSFolder &AddFolder(const char *Name)
			{ return AddFolder(PChartoUTF16(Name)); };

		CdGDSObj *AddObj(const UTF16String &Name, CdGDSObj *val=NULL);
		CdGDSObj *AddObj(const char *Name, CdGDSObj *val=NULL)
        	{ return AddObj(PChartoUTF16(Name), val); };

		void DeleteObj(int Index);
		void DeleteObj(CdGDSObj *val);

		CdGDSFolder &DirItem(int Index);
		CdGDSFolder &DirItem(const UTF16String &Name);
		CdGDSFolder &DirItem(const char *Name)
			{ return DirItem(PChartoUTF16(Name)); };

		inline CdGDSFolder &operator[] (int Index)
			{ return DirItem(Index); };
		inline CdGDSFolder &operator[] (const UTF16String &Name)
			{ return DirItem(Name); };
		inline CdGDSFolder &operator[] (const char *Name)
			{ return DirItem(Name); };

		CdGDSObj *ObjItem(int Index);
		CdGDSObj *ObjItem(const UTF16String &Name);
		CdGDSObj *ObjItem(const char *Name)
			{ return ObjItem(PChartoUTF16(Name)); };

		CdGDSObj *ObjItemEx(int Index);
		CdGDSObj *ObjItemEx(const UTF16String &Name);
		CdGDSObj *ObjItemEx(const char *Name)
			{ return ObjItemEx(PChartoUTF16(Name)); };

		CdGDSObj *Path(const UTF16String &FullName);
		CdGDSObj *Path(const char *FullName)
        	{ return Path(PChartoUTF16(FullName)); };
		CdGDSObj *PathEx(const UTF16String &FullName);
		CdGDSObj *PathEx(const char *FullName)
        	{ return PathEx(PChartoUTF16(FullName)); };

		static void SplitPath(const UTF16String &FullName, UTF16String &Path,
        	UTF16String &Name);

		bool HasChild(CdGDSObj *Obj, bool SubFolder = true);

		inline size_t Count() const { return fList.size(); };
	protected:
		struct TItem
		{
		public:
			CdGDSObj *Obj;
			TdBlockID StreamID;
			UInt32 Flag;
			UTF16String Name;
			TdPtr64 _pos;

			TItem() { Obj = NULL; StreamID = 0; Flag = 0; _pos = 0; };
			inline bool IsEmpty() const { return Flag & 0x01; };
			inline void SetEmpty() { Flag |= 0x01; };
			inline bool IsFolder() const { return Flag & 0x02; };
			inline void SetFolder() { Flag |= 0x02; };
		};
		std::vector<TItem> fList;

		virtual void LoadAfter(CdFilter &Reader, TdVersion Version);
		virtual void SaveAfter(CdFilter &Writer);
		virtual void SaveToBlockStream();

		void _Clear();
	private:
		bool _HasName(const UTF16String &Name);
		TItem &_NameItem(const UTF16String &Name);
		void _LoadItem(TItem &I);
		void _UpdateAll();
		std::vector<TItem>::iterator _FindObj(CdGDSObj *Obj);
	};


	/// Null node for CoreArray GDS format
	class CdGDSNull: public CdGDSObj
	{
	public:
		CdGDSNull(CdGDSFolder *vFolder = NULL);
        virtual CdGDSObj *NewOne(void *Param = NULL);
	protected:
		virtual void SaveToBlockStream();
	};


	/// Stream container for CoreArray GDS format
	class CdGDSStreamContainer: public CdGDSObj
	{
	public:
		CdGDSStreamContainer(CdGDSFolder *vFolder = NULL);
		virtual ~CdGDSStreamContainer();

		virtual char const* dName();
		virtual char const* dTraitName();

		void CopyFrom(CBufdStream &Source, TdPtr64 Count=-1);
		void CopyFrom(CdStream &Source, TdPtr64 Count=-1);

		void CopyTo(CBufdStream &Dest, TdPtr64 Count=-1);
		void CopyTo(CdStream &Dest, TdPtr64 Count=-1);

		TdPtr64 GetSize();
		inline CBufdStream *BufStream() { return fBufStream; };

		virtual void SetPackedMode(const char *Mode);
		virtual void CloseWriter();
	protected:
		CBufdStream *fBufStream;
		CdBlockStream *vAlloc_Stream;
		bool fNeedUpdate;
		TdBlockID vAllocID;
		TdPtr64 vAlloc_Ptr;

		virtual void LoadAfter(CdFilter &Reader, TdVersion Version);
		virtual void SaveStruct(CdFilter &Writer, bool IncludeName);
		virtual void SaveAfter(CdFilter &Writer);
	private:
	};


    /// CoreArray GDS format
	class CdGDSFile: protected CdBlockCollection
	{
	public:
		enum TdOpenMode { dmCreate, dmOpenRead, dmOpenReadWrite };

		CdGDSFile();
		CdGDSFile(const UTF16String &fn, TdOpenMode Mode);
		CdGDSFile(const char *fn, TdOpenMode Mode);
		virtual ~CdGDSFile();

		virtual void LoadFile(const UTF16String &fn, bool ReadOnly = true);
		virtual void LoadFile(const char *fn, bool ReadOnly = true);
		virtual void SaveAsFile(const UTF16String &fn);
		virtual void SaveAsFile(const char *fn);
		virtual void DuplicateFile(const UTF16String &fn);
		virtual void DuplicateFile(const char *fn);

		void SyncFile();
		void CloseFile();

		/// Clean up all unused resources
		void TidyUp();

		bool Modified();

		/// Return file size of the CdGDSFile object
		TdPtr64 GetFileSize();

		/// Return the file name of the CdGDSFile object
		inline UTF16String &FileName() { return fFileName; };

		inline CdLogRecord &Log() { return *fLog; };
		inline const char *Prefix() const { return fPrefix; };
		inline TdVersion Version() const { return fVersion; };

		inline CdGDSFolder &Root() { return fRoot; };
		inline bool ReadOnly() const { return fReadOnly; };
	protected:
		class CdGDSRoot: public CdGDSFolder
		{
		public:
			virtual UTF16String Name() const { return UTF16String(); };
		};

		CdGDSRoot fRoot;
		const char *fPrefix;
		TdVersion fVersion;
		bool fReadOnly;
		UTF16String fFileName;

		void LoadStream(CdStream* Stream, bool ReadOnly = true);
		void SaveStream(CdStream* Stream);
	private:
        CdLogRecord *fLog;

		void _Init();
		bool _HaveModify(CdGDSFolder *folder);
	};


    // Exceptions for CdGDSObj
	class ErrGDSObj: public Err_dObj
	{
	public:
		ErrGDSObj() {};
		ErrGDSObj(const std::string &msg) { fMessage = msg; }
		ErrGDSObj(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
	};

    // Exceptions for stream container
	class ErrGDSStreamContainer: public Err_dObj
	{
	public:
		ErrGDSStreamContainer() {};
		ErrGDSStreamContainer(const std::string &msg) { fMessage = msg; }
		ErrGDSStreamContainer(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
	};

	// Exceptions for GDS file
	class ErrGDSFile: public Err_dObj
	{
	public:
		ErrGDSFile() {};
		ErrGDSFile(const std::string &msg) { fMessage = msg; }
		ErrGDSFile(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
	};

};

#endif /* _dFile_H_ */

