// ===========================================================
//     _/_/_/   _/_/_/  _/_/_/_/    _/_/_/_/  _/_/_/   _/_/_/
//      _/    _/       _/             _/    _/    _/   _/   _/
//     _/    _/       _/_/_/_/       _/    _/    _/   _/_/_/
//    _/    _/       _/             _/    _/    _/   _/
// _/_/_/   _/_/_/  _/_/_/_/_/     _/     _/_/_/   _/_/
// ===========================================================
//
// dPlatform.hpp: Functions for independent platforms
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
 *	\file     dPlatform.hpp
 *	\author   Xiuwen Zheng
 *	\version  1.0
 *	\date     2007 - 2011
 *	\brief    Functions for independent platforms
 *	\details
*/

#ifndef _dPlatform_H_
#define _dPlatform_H_

#include <dType.hpp>
#include <cstdio>
#include <cstdlib>
#include <climits>
#include <limits>
#include <cstring>
#include <cstdarg>
#include <vector>
#include <queue>

#if defined(COREARRAY_WINDOWS)
	#include <windows.h>
#elif defined(COREARRAY_UNIX)
	#include <iconv.h>
#endif

#ifdef COREARRAY_POSIX_THREAD
	#include <pthread.h>
#endif


namespace CoreArray
{
	#ifndef COREARRAY_HAVE_INT128

	struct int128_t;
	struct uint128_t;

	/// Signed integer of 128 bits
	struct int128_t
	{
		#if defined(COREARRAY_LITTLE_ENDIAN)
		UInt64 Low; Int64 High;
		#elif defined(COREARRAY_BIG_ENDIAN)
		Int64 High; UInt64 Low;
		#else
		#  error "Unsupported Endianness!"
		#endif
		int128_t() {}
		int128_t(const Int64 val) { *this = val; }
		int128_t(const uint128_t &val) { *this = val; }

		int128_t & operator= (Int64 val);
		int128_t & operator= (const uint128_t &val);
		operator Int64() const;

		void toStr(char Out[]) const {}
	};

	/// Unsigned integer of 128 bits
	struct uint128_t
	{
		#if defined(COREARRAY_LITTLE_ENDIAN)
		UInt64 Low; UInt64 High;
		#elif defined(COREARRAY_BIG_ENDIAN)
		UInt64 High; UInt64 Low;
		#else
		#  error "Unsupported Endianness!"
		#endif
		uint128_t() {}
		uint128_t(const UInt64 val) { *this = val; }
		uint128_t(const int128_t &val) { *this = val; }

		uint128_t & operator= (UInt64 val);
		uint128_t & operator= (const int128_t &val);
		operator UInt64() const;

		void toStr(char Out[]) const {}
	};

	#endif


	/// Signed integer of 128 bits
	typedef int128_t    Int128;
	/// Unsigned integer of 128 bits
	typedef uint128_t   UInt128;


	/// Float number of quadruple precision
	#ifndef COREARRAY_HAVE_FLOAT128
	struct Float128
	{
	public:
		/// \todo To support Float128
		Float128() {}
		#ifndef COREARRAY_HAVE_INT128
		Float128(const Int128 &val);
		Float128(const UInt128 &val);
		#endif
		Float128(long double val);

		Float128 & operator= (long double val);
		operator long double() const;

		static Float128 min();
		static Float128 max();
	private:
		char buf[16];
	};
	#else
	typedef long float Float128;
	#endif


	template<> struct TdTraits<Int128>
	{
		typedef Int128 TType;
		static const int trVal = COREARRAY_TR_INTEGER;
		static const unsigned BitOf = 128u;
		static const bool isClass = false;
		static const TSVType SVType = svCustomInt;

		inline static const char * TraitName() { return "Int128"; };
		inline static const char * StreamName() { return "dInt128"; };

	#ifndef COREARRAY_HAVE_INT128
		static Int128 Min()
			{ Int128 rv; rv.Low = 0; rv.High = INT64_MIN; return rv; };
		static Int128 Max()
			{ Int128 rv; rv.Low = UINT64_MAX; rv.High = INT64_MAX; return rv; };
	#else
	#  error "Int128"
	#endif
	};

	template<> struct TdTraits<UInt128>
	{
		typedef UInt128 TType;
		static const int trVal = COREARRAY_TR_INTEGER;
		static const unsigned BitOf = 128u;
		static const bool isClass = false;
		static const TSVType SVType = svCustomInt;

		inline static const char * TraitName() { return "UInt128"; };
		inline static const char * StreamName() { return "dUInt128"; };

	#ifndef COREARRAY_HAVE_INT128
		static UInt128 Min()
			{ return UInt128(0); };
		static UInt128 Max()
			{ UInt128 rv; rv.Low = rv.High = -1; return rv; };
	#else
	#  error "UInt128"
	#endif
	};

	#ifndef COREARRAY_HAVE_FLOAT128
	template<> struct TdTraits<Float128>
	{
		typedef UInt128 TType;
		static const int trVal = COREARRAY_TR_FLOAT;
		static const unsigned BitOf = 128u;
		static const bool isClass = false;
		static const TSVType SVType = svCustomFloat;

		inline static const char * TraitName() { return "Float128"; };
		inline static const char * StreamName() { return "Float128"; };

		static Float128 Min()
			{ return Float128::min(); };
		static Float128 Max()
			{ return Float128::max(); };
	};
	#endif


	// Floating point number

	extern const double NaN;
	extern const double Infinity;
	extern const double NegInfinity;

	enum TFPClass { fpFinite=0, fpPosInf, fpNegInf, fpNaN };

	TFPClass FloatClassify(const float val);
	TFPClass FloatClassify(const double val);
	TFPClass FloatClassify(const long double val);

	// whether it is finite
	bool IsFinite(const float V);
	bool IsFinite(const double V);
	bool IsFinite(const long double V);

	// whether it is NaN
	bool IsNaN(const float V);
	bool IsNaN(const double V);
	bool IsNaN(const long double V);

	// whether it is +infinity
	bool IsInf(const float V);
	bool IsInf(const double V);
	bool IsInf(const long double V);

	// whether it is -infinity
	bool IsNegInf(const float V);
	bool IsNegInf(const double V);
	bool IsNegInf(const long double V);

	// whether v1 == v2
	bool EqaulFloat(const float v1, const float v2); // consider NaN
	bool EqaulFloat(const double v1, const double v2); // consider NaN
	bool EqaulFloat(const long double v1, const long double v2); // consider NaN

	/// Make FPU exception enable
	void EnableFPUException();
	/// Make FPU exception disable
	void DisableFPUException();
	/// Reset default FPU exception
	void DefaultFPUControl();

	// get a string from floating point number
	std::string FloatToStr(const float val);
	std::string FloatToStr(const double val);
	std::string FloatToStr(const long double val);
	std::string FloatToStr(const Float128 val);


	// get a string from an integer
	std::string IntToStr(const Int8 val);
	std::string IntToStr(const UInt8 val);
	std::string IntToStr(const Int16 val);
	std::string IntToStr(const UInt16 val);
	std::string IntToStr(const Int32 val);
	std::string IntToStr(const UInt32 val);
	std::string IntToStr(const Int64 val);
	std::string IntToStr(const UInt64 val);
	std::string IntToStr(const Int128 val);
	std::string IntToStr(const UInt128 val);


	// UTF functions

	UTF16String UTF7toUTF16(const char *str);
	UTF16String UTF7toUTF16(const UTF8String &s);
	UTF32String UTF7toUTF32(const char *str);
	UTF32String UTF7toUTF32(const UTF8String &s);

	UTF16String PChartoUTF16(const char* str);
	UTF16String UTF8toUTF16(const UTF8String &s);
	UTF32String UTF8toUTF32(const UTF8String &s);

	UTF8String UTF16toUTF8(const UTF16String &ws);
	UTF32String UTF16toUTF32(const UTF16String &ws);

	UTF8String UTF32toUTF8(const UTF32String &ws);
	UTF16String UTF32toUTF16(const UTF32String &ws);

	extern const char *const sLineBreak;
	extern const char *const sFileSep;

#ifdef COREARRAY_UNIX
	class TdICONV
	{
	public:
		TdICONV(const char *to, const char *from);
		~TdICONV();
		void Reset();
		size_t Cvt(const char * &inbuf, size_t &inbytesleft,
			char* &outbuf, size_t &outbytesleft);

		inline iconv_t Handle() const { return fHandle; };
//		static std::vector<std::string> List();
	protected:
		iconv_t fHandle;
	};
#endif


	// Exception

	/// Error Macro
	#define _COREARRAY_ERRMACRO_(x) { \
		va_list args; va_start(args, x); \
		Init(x, args); \
		va_end(args); \
	}

	/// The root class of exception for CoreArray library
	class ErrCoreArray: public std::exception
	{
	public:
		ErrCoreArray() {};
		ErrCoreArray(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
		ErrCoreArray(const std::string &msg) { fMessage = msg; }
		virtual const char *what() const throw() { return fMessage.c_str(); }
		virtual ~ErrCoreArray() throw() {};
	protected:
		std::string fMessage;
		void Init(const char *fmt, va_list arglist);
	};





	// Thread-safe functions



	// System Error information

	/// Exception for system error
	class ErrOSError: public ErrCoreArray
	{
	public:
		ErrOSError() {};
		ErrOSError(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
		ErrOSError(const std::string &msg) { fMessage = msg; }
	};

	// Return the last code from the OS
	int GetLastOSError();
	// Format a system error message
	UTF8String SysErrMessage(int err);
	// Format the last system error message
	UTF8String LastSysErrMsg();
	// Raise an exception with the last Operating System error code
	template<class X> inline void RaiseLastOSError()
		{ throw X(LastSysErrMsg()); }


	std::string NowDateToStr();


	// File Functions

	#if defined(COREARRAY_WINDOWS)
		typedef HANDLE TSysHandle;
	#elif defined(COREARRAY_UNIX)
		typedef int TSysHandle;
	#else
		"TSysHandle"
	#endif
	extern const TSysHandle NullSysHandle;

	enum TSysOpenMode { fmRead, fmWrite, fmReadWrite };
	enum TSysShareMode { saNone, saRead, saWrite, saReadWrite };
	enum TdSysSeekOrg { soBeginning=0, soCurrent, soEnd };

	TSysHandle SysCreateFile(char const* const AFileName, UInt32 Mode);
	TSysHandle SysOpenFile(char const* const AFileName, enum TSysOpenMode mode,
		enum TSysShareMode smode);

	bool SysCloseHandle(TSysHandle Handle);
	size_t SysHandleRead(TSysHandle Handle, void *Buffer, size_t Count);
	size_t SysHandleWrite(TSysHandle Handle, const void* Buffer, size_t Count);
	Int64 SysHandleSeek(TSysHandle Handle, Int64 Offset, enum TdSysSeekOrg sk);
	bool SysHandleSetSize(TSysHandle Handle, Int64 NewSize);


	std::string TempFileName(const char *prefix, const char *tempdir);
	bool FileExists(const std::string &FileName);


	namespace Mach
	{
		/// Return the number of available CPU cores in the system
		/** return -1, if unable to determine. **/
		int GetNumberOfCPU();

		/// Return the size in byte of level-1 cache memory
    	/** return -1, if unable to determine. **/
        int GetL1CacheMemory();

		/// Return the size in byte of level-2 cache memory
    	/** return -1, if unable to determine. **/
        int GetL2CacheMemory();
	}


	// Thread structure, classes, functions

	struct CdThreadMutex
	{
	public:
	#ifdef COREARRAY_WINDOWS
		#ifdef COREARRAY_MINGW32
			typedef _CRITICAL_SECTION TdMutex;
		#else
			typedef RTL_CRITICAL_SECTION TdMutex;
		#endif
	#elif defined(COREARRAY_POSIX_THREAD)
		typedef pthread_mutex_t TdMutex;
	#else
		"..."
	#endif
		CdThreadMutex();
		~CdThreadMutex();
		void Lock();
		void Unlock();
		bool TryLock();
		inline TdMutex &Mutex() { return mutex; };
	private:
		TdMutex mutex;
	};

	struct TdAutoMutex
	{
		CdThreadMutex * mutex;
		TdAutoMutex(CdThreadMutex *m) { mutex = m; if (m) m->Lock(); };
		~TdAutoMutex() { if (mutex) mutex->Unlock(); };
		inline void Reset(CdThreadMutex *m)
		{
			if (m != mutex) {
				if (mutex) mutex->Unlock();
				mutex = m;
				if (m) m->Lock();
			}
		}
	};



	class CdThread;

	typedef int (*TdThreadProc)(CdThread *Thread, void *Data);

	namespace Internal
	{
		class CdThBasic {
        public:
			virtual ~CdThBasic() {};
		};

		template<typename Tx> class CdThBasicEx: public CdThBasic {
		public:
			Tx Data;
		};

		struct TdThreadData
		{
			CdThread *thread;
			TdThreadProc proc;
			void *Data;
		};

		template<typename Tx> struct TdThreadDataEx
		{
			typedef int (*TdProc)(CdThread *Thread, Tx Data);
			TdProc proc;
			Tx Data;
		};

		template<typename Tx>
		int _pTdThreadEx(CdThread *Thread, void *Data) {
			CdThBasicEx< Internal::TdThreadDataEx<Tx> > *p =
				(CdThBasicEx< Internal::TdThreadDataEx<Tx> >*)Data;
			return (*p->Data.proc)(Thread, p->Data.Data);
		}
	};

    /// Thread class
	class CdThread
	{
	public:
		friend class CdThreadsSuspending;

	#if defined(COREARRAY_WINDOWS)
		typedef struct {
			HANDLE Handle;
			DWORD ThreadID;
		} TStruct;
	#elif defined(COREARRAY_POSIX_THREAD)
		typedef pthread_t TStruct;
	#else
		"..."
	#endif

		CdThread();
		CdThread(TdThreadProc proc, void *Data);
		virtual ~CdThread();

		void BeginThread();
		template<typename Tx>
			void BeginThread(int (*proc)(CdThread *, Tx), Tx val)
		{
        	Internal::CdThBasicEx< Internal::TdThreadDataEx<Tx> > *p =
				new Internal::CdThBasicEx< Internal::TdThreadDataEx<Tx> >;
			p->Data.proc = proc; p->Data.Data = val;
			vData.thread = this; vData.proc = Internal::_pTdThreadEx<Tx>;
			vPrivate = p; vData.Data = (void*)p;
			_BeginThread();
		}

		int RunThreadSafe();
		virtual int RunThread();
		int EndThread();
		void Terminate();
		inline bool Terminated() const { return terminated; };
		inline TStruct &Thread() { return thread; };
		inline int &ExitCode() { return fExitCode; };
        inline std::string &ErrorInfo() { return fErrorInfo; };
	protected:
		TStruct thread;
		int fExitCode;
		std::string fErrorInfo;
		bool terminated;
		Internal::TdThreadData vData;
		void _BeginThread(); // need vData
	private:
		Internal::CdThBasic *vPrivate;
		void Done();
	};


	// closure or delegate for C++
	template<class Tx> struct TdThreadObjProc
	{
		void (Tx::*proc)(CdThread *);
		Tx * obj;
		inline void Proc(CdThread *thread)
			{ (obj->*proc)(thread); };
	};


	/// Class for suspending and resuming thread
	class CdThreadsSuspending
	{
    public:
		CdThreadsSuspending();
        ~CdThreadsSuspending();

		void Suspend();
        void WakeUp();
	protected:
	#if defined(COREARRAY_WINDOWS)
    	HANDLE hEvent;
	#elif defined(COREARRAY_POSIX_THREAD)
		pthread_mutex_t mutex;
		pthread_cond_t threshold;
	#endif
	};




	/// Thread pool
/*	class CdThreadPool
	{
	public:
    	// There is no limit of thread numbers, if MaxThreads = 0
		CdThreadPool(size_t MaxThreads=0, size_t InitThreads=0);
		virtual ~CdThreadPool();

		template<typename Tx>
			void BeginThread(int (*proc)(CdThread *, Tx), Tx val)
		{

		}

		inline size_t MaxThreads() const { return fMaxThreads; }
		void SetMaxThreads(size_t NewMaxThreads);
	protected:
        CdThreadMutex fPoolMutex;
    	size_t fMaxThreads;
		std::queue<CdThread*> fWorkingList, fWaitingList;
	};
*/

	/// Exceptions for threads
	class ErrThread: public ErrOSError
	{
	public:
		ErrThread() {};
		ErrThread(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
		ErrThread(const std::string &msg) { fMessage = msg; }
	};


	/// Exceptions for conversion
	class ErrConvert: public ErrCoreArray
	{
	public:
		ErrConvert() {};
		ErrConvert(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
		ErrConvert(const std::string &msg) { fMessage = msg; }
	};


	void SwapData(void *d1, void *d2, size_t len);

	UTF8String Format(const char *fmt, ...);
	UTF8String _FmtNum(const char *fmt, ...); // with small buffer

	long StrToInt(char const* str);
	bool StrToInt(char const* str, long *rv);
	long StrToIntDef(char const* str, const long Default);

	double StrToFloat(char const* str);
	bool StrToFloat(char const* str, double *rv);
	double StrToFloatDef(char const* str, const double Default);




	namespace Internal
	{
		// Type Convert

		template<typename DestT, typename SourceT,
			int DTrait = TdTraits<DestT>::trVal,
			int STrait = TdTraits<SourceT>::trVal >
		struct TValCvt
		{
			inline static DestT Cvt(const SourceT &val) { return val; };
			inline static void Array(DestT *p, SourceT *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = *s++; }
		};

		template<typename DestT, typename SourceT>
			struct TValCvt<DestT, SourceT,
			COREARRAY_TR_INTEGER, COREARRAY_TR_FLOAT>
		{
		#ifdef COREARRAY_GNUG
			inline static DestT Cvt(const SourceT &val)
				{ return DestT(typename TdTraits<DestT>::TType(val)); };
			inline static void Array(DestT *p, SourceT *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = DestT(typename TdTraits<DestT>::TType(*s++)); }
		#else
			inline static DestT Cvt(const SourceT &val)
				{ return DestT(TdTraits<DestT>::TType(val)); };
			inline static void Array(DestT *p, SourceT *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = DestT(TdTraits<DestT>::TType(*s++)); }
		#endif
		};

		// Type Convert: Source UTF8String

		template<typename DestT> struct TValCvt<DestT, UTF8String,
			COREARRAY_TR_INTEGER, COREARRAY_TR_STRING>
		{
			inline static DestT Cvt(const UTF8String &val)
				{ return StrToInt(val.c_str()); }
			inline static void Array(DestT *p, UTF8String *s, ssize_t L)
				{ for (; L > 0; p++, s++, L--) *p = StrToInt(s->c_str()); }
		};

		template<typename DestT> struct TValCvt<DestT, UTF8*,
			COREARRAY_TR_INTEGER, COREARRAY_TR_STRING>
		{
			inline static DestT Cvt(const UTF8 * val)
				{ return StrToInt(val); }
			inline static void Array(DestT *p, UTF8 **s, ssize_t L)
				{ for (; L > 0; L--) *p++ = StrToInt(*s++); }
		};

		template<typename SourceT> struct TValCvt<UTF8String, SourceT,
			COREARRAY_TR_STRING, COREARRAY_TR_INTEGER>
		{
			inline static UTF8String Cvt(const SourceT val)
				{ return IntToStr(val); }
			inline static void Array(UTF8String *p, SourceT *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = IntToStr(*s++); }
		};

		template<typename DestT> struct TValCvt<DestT, UTF8String,
			COREARRAY_TR_FLOAT, COREARRAY_TR_STRING>
		{
			inline static DestT Cvt(const UTF8String &val)
				{ return StrToFloat(val.c_str()); }
			inline static void Array(DestT *p, UTF8String *s, ssize_t L)
				{ for (; L > 0; p++, s++, L--) *p = StrToFloat(s->c_str()); }
		};

		template<typename DestT> struct TValCvt<DestT, UTF8*,
			COREARRAY_TR_FLOAT, COREARRAY_TR_STRING>
		{
			inline static DestT Cvt(const UTF8 *val)
				{ return StrToFloat(val); }
			inline static void Array(DestT *p, UTF8 **s, ssize_t L)
				{ for (; L > 0; L--) *p++ = StrToFloat(*s++); }
		};

		template<typename SourceT> struct TValCvt<UTF8String, SourceT,
			COREARRAY_TR_STRING, COREARRAY_TR_FLOAT>
		{
			inline static UTF8String Cvt(const SourceT &val)
				{ return FloatToStr(val); }
			inline static void Array(UTF8String *p, const SourceT *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = FloatToStr(*s++); }
		};

		template<> struct TValCvt<UTF16String, UTF8String,
			COREARRAY_TR_STRING, COREARRAY_TR_STRING>
		{
			inline static UTF16String Cvt(const UTF8String &val)
				{ return CoreArray::UTF8toUTF16(val); }
			inline static void Array(UTF16String *p, UTF8String *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = CoreArray::UTF8toUTF16(*s++); }
		};

		template<> struct TValCvt<UTF32String, UTF8String,
			COREARRAY_TR_STRING, COREARRAY_TR_STRING>
		{
			inline static UTF32String Cvt(const UTF8String &val)
				{ return CoreArray::UTF8toUTF32(val); }
			inline static void Array(UTF32String *p, UTF8String *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = CoreArray::UTF8toUTF32(*s++); }
		};

		// Type Convert: Source UTF16String

		template<typename DestT> struct TValCvt<DestT, UTF16String,
			COREARRAY_TR_INTEGER, COREARRAY_TR_STRING>
		{
			inline static DestT Cvt(const UTF16String &val)
				{ return StrToInt(UTF16toUTF8(val).c_str()); }
			inline static void Array(DestT *p, UTF16String *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = StrToInt(UTF16toUTF8(*s++).c_str()); }
		};

		template<typename DestT> struct TValCvt<DestT, UTF16*,
			COREARRAY_TR_INTEGER, COREARRAY_TR_STRING>
		{
			inline static DestT Cvt(const UTF16 *val)
				{ return StrToInt(UTF16toUTF8(val).c_str()); }
			inline static void Array(DestT *p, UTF16 **s, ssize_t L)
				{ for (; L > 0; L--) *p++ = StrToInt(UTF16toUTF8(*s++).c_str()); }
		};

		template<typename SourceT> struct TValCvt<UTF16String, SourceT,
			COREARRAY_TR_STRING, COREARRAY_TR_INTEGER>
		{
			inline static UTF16String Cvt(const SourceT val)
				{ return UTF7toUTF16(IntToStr(val)); }
			inline static void Array(UTF16String *p, SourceT *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = UTF7toUTF16(IntToStr(*s++)); }
		};

		template<typename DestT> struct TValCvt<DestT, UTF16String,
			COREARRAY_TR_FLOAT, COREARRAY_TR_STRING>
		{
			inline static DestT Cvt(const UTF16String &val)
				{ return StrToFloat(UTF16toUTF8(val).c_str()); }
			inline static void Array(DestT *p, UTF16String *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = StrToFloat(UTF16toUTF8(*s++).c_str()); }
		};

		template<typename DestT> struct TValCvt<DestT, UTF16*,
			COREARRAY_TR_FLOAT, COREARRAY_TR_STRING>
		{
			inline static DestT Cvt(const UTF16 *val)
				{ return StrToFloat(UTF16toUTF8(val).c_str()); }
			inline static void Array(DestT *p, UTF16 **s, ssize_t L)
				{ for (; L > 0; L--) *p++ = StrToFloat(UTF16toUTF8(*s++).c_str()); }
		};

		template<typename SourceT> struct TValCvt<UTF16String, SourceT,
			COREARRAY_TR_STRING, COREARRAY_TR_FLOAT>
		{
			inline static UTF16String Cvt(const SourceT &val)
				{ return UTF7toUTF16(FloatToStr(val)); }
			inline static void Array(UTF16String *p, const SourceT *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = UTF7toUTF16(FloatToStr(*s++)); }
		};

		template<> struct TValCvt<UTF8String, UTF16String,
			COREARRAY_TR_STRING, COREARRAY_TR_STRING>
		{
			inline static UTF8String Cvt(const UTF16String &val)
				{ return CoreArray::UTF16toUTF8(val); }
			inline static void Array(UTF16String *p, UTF8String *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = CoreArray::UTF8toUTF16(*s++); }
		};

		template<> struct TValCvt<UTF32String, UTF16String,
			COREARRAY_TR_STRING, COREARRAY_TR_STRING>
		{
			inline static UTF32String Cvt(const UTF16String &val)
				{ return CoreArray::UTF16toUTF32(val); }
			inline static void Array(UTF16String *p, UTF32String *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = CoreArray::UTF32toUTF16(*s++); }
		};

		// Type Convert: Source UTF32String

		template<typename DestT> struct TValCvt<DestT, UTF32String,
			COREARRAY_TR_INTEGER, COREARRAY_TR_STRING>
		{
			inline static DestT Cvt(const UTF32String &val)
				{ return StrToInt(UTF32toUTF8(val).c_str()); }
			inline static void Array(DestT *p, UTF32String *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = StrToInt(UTF32toUTF8(*s++).c_str()); }
		};

		template<typename DestT> struct TValCvt<DestT, UTF32*,
			COREARRAY_TR_INTEGER, COREARRAY_TR_STRING>
		{
			inline static DestT Cvt(const UTF32 *val)
				{ return StrToInt(UTF32toUTF8(val).c_str()); }
			inline static void Array(DestT *p, UTF32 **s, ssize_t L)
				{ for (; L > 0; L--) *p++ = StrToInt(UTF32toUTF8(*s++).c_str()); }
		};

		template<typename SourceT> struct TValCvt<UTF32String, SourceT,
			COREARRAY_TR_STRING, COREARRAY_TR_INTEGER>
		{
			inline static UTF32String Cvt(const SourceT val)
				{ return UTF7toUTF32(IntToStr(val)); }
			inline static void Array(UTF32String *p, SourceT *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = UTF7toUTF32(IntToStr(*s++)); }
		};

		template<typename DestT> struct TValCvt<DestT, UTF32String,
			COREARRAY_TR_FLOAT, COREARRAY_TR_STRING>
		{
			inline static DestT Cvt(const UTF32String &val)
				{ return StrToFloat(UTF32toUTF8(val).c_str()); }
			inline static void Array(DestT *p, UTF32String *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = StrToFloat(UTF32toUTF8(*s++).c_str()); }
		};

		template<typename DestT> struct TValCvt<DestT, UTF32*,
			COREARRAY_TR_FLOAT, COREARRAY_TR_STRING>
		{
			inline static DestT Cvt(const UTF32 *val)
				{ return StrToFloat(UTF32toUTF8(val).c_str()); }
			inline static void Array(DestT *p, UTF32 **s, ssize_t L)
				{ for (; L > 0; L--) *p++ = StrToFloat(UTF32toUTF8(*s++).c_str()); }
		};

		template<typename SourceT> struct TValCvt<UTF32String, SourceT,
			COREARRAY_TR_STRING, COREARRAY_TR_FLOAT>
		{
			inline static UTF32String Cvt(const SourceT &val)
				{ return UTF7toUTF32(FloatToStr(val)); }
			inline static void Array(UTF32String *p, const SourceT *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = UTF7toUTF32(FloatToStr(*s++)); }
		};

		template<> struct TValCvt<UTF8String, UTF32String,
			COREARRAY_TR_STRING, COREARRAY_TR_STRING>
		{
			inline static UTF8String Cvt(const UTF32String &val)
				{ return CoreArray::UTF32toUTF8(val); }
			inline static void Array(UTF8String *p, UTF32String *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = CoreArray::UTF32toUTF8(*s++); }
		};

		template<> struct TValCvt<UTF16String, UTF32String,
			COREARRAY_TR_STRING, COREARRAY_TR_STRING>
		{
			inline static UTF16String Cvt(const UTF32String &val)
				{ return CoreArray::UTF32toUTF16(val); }
			inline static void Array(UTF16String *p, UTF32String *s, ssize_t L)
				{ for (; L > 0; L--) *p++ = CoreArray::UTF32toUTF16(*s++); }
		};
	};

	/// Conversion from SourceT to DestT
	/** \tparam  DestT    type of destination
	 *  \tparam  SourceT  type of source
	*/
	template<typename DestT, typename SourceT>
	inline DestT ValCvt(const SourceT &val)
		{ return Internal::TValCvt<DestT, SourceT>::Cvt(val); };

	/// Conversion from SourceT to DestT
	/** \tparam  DestT    type of destination
	 *  \tparam  SourceT  type of source
	*/
	template<typename DestT, typename SourceT>
	inline void ValCvtArray(DestT *p, SourceT *s, ssize_t L)
		{ Internal::TValCvt<DestT, SourceT>::Array(p, s, L); };


	// Endian

	#if defined(COREARRAY_LITTLE_ENDIAN)

		#define COREARRAY_ENDIAN_CVT(x, size)
		#define COREARRAY_ENDIAN_CVT16(x)      x
		#define COREARRAY_ENDIAN_CVT32(x)      x
		#define COREARRAY_ENDIAN_CVT64(x)      x
		#define COREARRAY_ENDIAN_CVT128(x)     x

	#elif defined(COREARRAY_BIG_ENDIAN)

		void COREARRAY_ENDIAN_CVT(void *x, size_t size);
		UInt16 COREARRAY_ENDIAN_CVT16(UInt16 x);
		UInt32 COREARRAY_ENDIAN_CVT32(UInt32 x);
		UInt64 COREARRAY_ENDIAN_CVT64(UInt64 x);
		UInt128 COREARRAY_ENDIAN_CVT128(UInt128 x);

	#else
	#  error "Unknown endianness"
    #endif
}

#endif /* _dPlatform_H_ */
