// ===========================================================
//     _/_/_/   _/_/_/  _/_/_/_/    _/_/_/_/  _/_/_/   _/_/_/
//      _/    _/       _/             _/    _/    _/   _/   _/
//     _/    _/       _/_/_/_/       _/    _/    _/   _/_/_/
//    _/    _/       _/             _/    _/    _/   _/
// _/_/_/   _/_/_/  _/_/_/_/_/     _/     _/_/_/   _/_/
// ===========================================================
//
// gdsfmt.cpp: the R interface of CoreArray library
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

#include <CoreDEF.h>
#include <dType.h>
#include <dSeq.h>

#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <map>
#include <string>
#include <memory>


using namespace std;
using namespace CoreArray;


#define LongBool int

#ifdef COREARRAY_GNUG
#  ifdef COREARRAY_WINDOWS
#    define DLLEXPORT __attribute__((dllexport))
#  else
#    define DLLEXPORT
#  endif
#else
#  define DLLEXPORT __declspec(dllexport)
#endif


#define CORETRY			try {
#define CORECATCH(cmd)	} \
	catch (exception &E) { \
		Init.LastError = E.what(); \
		cmd; \
	} \
	catch (const char *E) { \
		Init.LastError = E; \
		cmd; \
	}



/// the object for initialization
class TInit
{
public:
	/// the maximum number of supported GDS files
	const static int MaxFiles = 256;
	/// the buffer of GDS files
	CdGDSFile *Files[MaxFiles];
	/// the last error message
	string LastError;

	struct strCmp {
		bool operator()( const char* s1, const char* s2 ) const
			{ return strcmp( s1, s2 ) < 0; }
	};
	map<const char*, const char*, strCmp> ClassMap;

	/// constructor
	TInit()
	{
		// initialize the local variables
		InitClassFlag = false;
		for (int i=0; i < MaxFiles; i++) Files[i] = NULL;

		// used in gdsAddNode
		ClassMap["NULL"] = "";
		ClassMap["folder"] = "$FOLDER$";

		// Integer

		ClassMap["int8"] = TdTraits<CoreArray::Int8>::StreamName();
		ClassMap["uint8"] = TdTraits<CoreArray::UInt8>::StreamName();
		ClassMap["int16"] = TdTraits<CoreArray::Int16>::StreamName();
		ClassMap["uint16"] = TdTraits<CoreArray::UInt16>::StreamName();
		ClassMap["int24"] = TdTraits<CoreArray::Int24>::StreamName();
		ClassMap["uint24"] = TdTraits<CoreArray::UInt24>::StreamName();
		ClassMap["int32"] = TdTraits<CoreArray::Int32>::StreamName();
		ClassMap["uint32"] = TdTraits<CoreArray::UInt32>::StreamName();
		ClassMap["int64"] = TdTraits<CoreArray::Int64>::StreamName();
		ClassMap["uint64"] = TdTraits<CoreArray::UInt64>::StreamName();

		ClassMap["bit1"] = TdTraits< CoreArray::BITS<1> >::StreamName();
		ClassMap["bit2"] = TdTraits< CoreArray::BITS<2> >::StreamName();
		ClassMap["bit3"] = TdTraits< CoreArray::BITS<3> >::StreamName();
		ClassMap["bit4"] = TdTraits< CoreArray::BITS<4> >::StreamName();
		ClassMap["bit5"] = TdTraits< CoreArray::BITS<5> >::StreamName();
		ClassMap["bit6"] = TdTraits< CoreArray::BITS<6> >::StreamName();
		ClassMap["bit7"] = TdTraits< CoreArray::BITS<7> >::StreamName();
		ClassMap["bit8"] = TdTraits< CoreArray::UInt8 >::StreamName();

		ClassMap["bit9"] = TdTraits< CoreArray::BITS<9> >::StreamName();
		ClassMap["bit10"] = TdTraits< CoreArray::BITS<10> >::StreamName();
		ClassMap["bit11"] = TdTraits< CoreArray::BITS<11> >::StreamName();
		ClassMap["bit12"] = TdTraits< CoreArray::BITS<12> >::StreamName();
		ClassMap["bit13"] = TdTraits< CoreArray::BITS<13> >::StreamName();
		ClassMap["bit14"] = TdTraits< CoreArray::BITS<14> >::StreamName();
		ClassMap["bit15"] = TdTraits< CoreArray::BITS<15> >::StreamName();
		ClassMap["bit16"] = TdTraits< CoreArray::UInt16 >::StreamName();

		ClassMap["bit17"] = TdTraits< CoreArray::BITS<17> >::StreamName();
		ClassMap["bit18"] = TdTraits< CoreArray::BITS<18> >::StreamName();
		ClassMap["bit19"] = TdTraits< CoreArray::BITS<19> >::StreamName();
		ClassMap["bit20"] = TdTraits< CoreArray::BITS<20> >::StreamName();
		ClassMap["bit21"] = TdTraits< CoreArray::BITS<21> >::StreamName();
		ClassMap["bit22"] = TdTraits< CoreArray::BITS<22> >::StreamName();
		ClassMap["bit23"] = TdTraits< CoreArray::BITS<23> >::StreamName();
		ClassMap["bit24"] = TdTraits< CoreArray::UInt24 >::StreamName();

		ClassMap["bit25"] = TdTraits< CoreArray::BITS<25> >::StreamName();
		ClassMap["bit26"] = TdTraits< CoreArray::BITS<26> >::StreamName();
		ClassMap["bit27"] = TdTraits< CoreArray::BITS<27> >::StreamName();
		ClassMap["bit28"] = TdTraits< CoreArray::BITS<28> >::StreamName();
		ClassMap["bit29"] = TdTraits< CoreArray::BITS<29> >::StreamName();
		ClassMap["bit30"] = TdTraits< CoreArray::BITS<30> >::StreamName();
		ClassMap["bit31"] = TdTraits< CoreArray::BITS<31> >::StreamName();
		ClassMap["bit32"] = TdTraits< CoreArray::UInt32 >::StreamName();

		ClassMap["sbit2"] = TdTraits< CoreArray::BITS<-2> >::StreamName();
		ClassMap["sbit3"] = TdTraits< CoreArray::BITS<-3> >::StreamName();
		ClassMap["sbit4"] = TdTraits< CoreArray::BITS<-4> >::StreamName();
		ClassMap["sbit5"] = TdTraits< CoreArray::BITS<-5> >::StreamName();
		ClassMap["sbit6"] = TdTraits< CoreArray::BITS<-6> >::StreamName();
		ClassMap["sbit7"] = TdTraits< CoreArray::BITS<-7> >::StreamName();
		ClassMap["sbit8"] = TdTraits< CoreArray::Int8 >::StreamName();

		ClassMap["sbit9"] = TdTraits< CoreArray::BITS<-9> >::StreamName();
		ClassMap["sbit10"] = TdTraits< CoreArray::BITS<-10> >::StreamName();
		ClassMap["sbit11"] = TdTraits< CoreArray::BITS<-11> >::StreamName();
		ClassMap["sbit12"] = TdTraits< CoreArray::BITS<-12> >::StreamName();
		ClassMap["sbit13"] = TdTraits< CoreArray::BITS<-13> >::StreamName();
		ClassMap["sbit14"] = TdTraits< CoreArray::BITS<-14> >::StreamName();
		ClassMap["sbit15"] = TdTraits< CoreArray::BITS<-15> >::StreamName();
		ClassMap["sbit16"] = TdTraits< CoreArray::Int16 >::StreamName();

		ClassMap["sbit17"] = TdTraits< CoreArray::BITS<-17> >::StreamName();
		ClassMap["sbit18"] = TdTraits< CoreArray::BITS<-18> >::StreamName();
		ClassMap["sbit19"] = TdTraits< CoreArray::BITS<-19> >::StreamName();
		ClassMap["sbit20"] = TdTraits< CoreArray::BITS<-20> >::StreamName();
		ClassMap["sbit21"] = TdTraits< CoreArray::BITS<-21> >::StreamName();
		ClassMap["sbit22"] = TdTraits< CoreArray::BITS<-22> >::StreamName();
		ClassMap["sbit23"] = TdTraits< CoreArray::BITS<-23> >::StreamName();
		ClassMap["sbit24"] = TdTraits< CoreArray::Int24 >::StreamName();

		ClassMap["sbit25"] = TdTraits< CoreArray::BITS<-25> >::StreamName();
		ClassMap["sbit26"] = TdTraits< CoreArray::BITS<-26> >::StreamName();
		ClassMap["sbit27"] = TdTraits< CoreArray::BITS<-27> >::StreamName();
		ClassMap["sbit28"] = TdTraits< CoreArray::BITS<-28> >::StreamName();
		ClassMap["sbit29"] = TdTraits< CoreArray::BITS<-29> >::StreamName();
		ClassMap["sbit30"] = TdTraits< CoreArray::BITS<-30> >::StreamName();
		ClassMap["sbit31"] = TdTraits< CoreArray::BITS<-31> >::StreamName();
		ClassMap["sbit32"] = TdTraits< CoreArray::Int32 >::StreamName();

		// Float

		ClassMap["float32"] = TdTraits<CoreArray::Float32>::StreamName();
		ClassMap["float64"] = TdTraits<CoreArray::Float64>::StreamName();

		// String

		ClassMap["string"] = TdTraits<CoreArray::UTF8*>::StreamName();
		ClassMap["string16"] = TdTraits<CoreArray::UTF16*>::StreamName();
		ClassMap["string32"] = TdTraits<CoreArray::UTF32*>::StreamName();

		// R storage mode
		ClassMap["integer"] = TdTraits<CoreArray::Int32>::StreamName();
		ClassMap["numeric"] = TdTraits<CoreArray::Float64>::StreamName();
		ClassMap["float"] = TdTraits<CoreArray::Float32>::StreamName();
		ClassMap["double"] = TdTraits<CoreArray::Float64>::StreamName();
		ClassMap["character"] = TdTraits<CoreArray::UTF8*>::StreamName();
		ClassMap["logical"] = TdTraits<CoreArray::Int32>::StreamName();
		ClassMap["list"] = "$FOLDER$";
		ClassMap["factor"] = TdTraits<CoreArray::UTF8*>::StreamName();;
	}

	/// destructor
	~TInit()
	{
		for (int i=0; i < MaxFiles; i++)
		{
			if (Files[i] != NULL)
			{
				try {
					delete Files[i];
				}
				catch (...) { }
			}
		}
	}

	/// check init
	void CheckInit()
	{
		// register classess
		if (!InitClassFlag)
		{
			RegisterClass();
			InitClassFlag = true;
		}
	}

	/// get an empty GDS file and its index
	CdGDSFile *GetEmptyFile(int *Index)
	{
		CheckInit();
		for (int i=0; i < MaxFiles; i++)
		{
			if (Files[i] == NULL)
			{
				CdGDSFile *rv = new CdGDSFile;
				*Index = i; Files[i] = rv;
				return rv;
			}
		}
		*Index = -1;
		throw ErrSequence("You have opened 256 gds files, not allow one more!");
	}

	/// get a specified GDS file
	CdGDSFile *GetFile(int Index)
	{
		if ((Index<0) || (Index>=MaxFiles))
			throw ErrSequence("Invalid gds file!");
		CdGDSFile *rv = Files[Index];
		if (rv == NULL)
			throw ErrSequence("The gds file has been closed.");
		return rv;
	}

private:
	bool InitClassFlag;
};


static TInit Init;

/// error exception
class ErrGDSFmt: public ErrCoreArray
{
public:
	ErrGDSFmt() {};
	ErrGDSFmt(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
	ErrGDSFmt(const std::string &msg) { fMessage = msg; }
};



// for RType, don't change the values
const static int rtNULL			= 0;	//< NULL type
const static int rtInt			= 1;	//< integer
const static int rtFloat		= 2;	//< floating point number
const static int rtString		= 3;	//< character
const static int rtLogical		= 4;	//< logical variable


// error information
static const char *erNotFolder = "It is not a folder!";
static const char *erNotFile = "It is not a stream container!";


/// assign a R string to rstr
inline static void RStrAgn(const char *Text, char **rstr)
{
	*rstr = R_alloc(strlen(Text)+1, 1);
	if (*rstr == NULL)
		throw Err_dObj("R_alloc return NULL!");
	strcpy(*rstr, Text);
}

/// return a string
inline static const char * RStr(const char *Name)
{
	return (Name) ? Name : "";
}

/// copy to Buf with reduced one
static void CopyDec(int *Value, int Cnt, int *Buf)
{
	if (Value != NULL)
	{
		for (int i=1; i <= Cnt; i++)
		{
			*Buf = *Value - 1;
			if (*Buf < 0)
				throw ErrGDSFmt("Invalid 'start'!");
			Buf++; Value++;
		}
	} else {
		for (int i=1; i <= Cnt; i++)
			*Buf++ = 1;
	}
}

/// copy to Count and checking
static void CopyCnt(int *Count, int *DLen, int *Start, int Cnt)
{
	for (int k=1; k <= Cnt; k++)
	{
		if (*Count >= 0)
		{
			if ((*Count + *Start - 1) > *DLen)
				throw ErrGDSFmt("Invalid 'start' and 'count': out of bound.");
		} else {
			int i = *DLen - (*Start - 1);
			if ((i < 0) || (i > *DLen))
				throw ErrGDSFmt("Invalid 'count': out of bound.");
			*Count = i;
		}
		Count++; DLen++; Start++;
	}
}

/// get the total count
inline static Int64 TotalCount(int *Count, int Cnt)
{
	Int64 rv = 1;
	for (int i=1; i <= Cnt; i++)
		rv *= *Count++;
	return rv;
}



extern "C"
{

// *****************************************************************************
// File Operations
// *****************************************************************************

/// create a GDS file
/** \param FileName    [in] the file name
 *  \param gds         [out] the internal file index
 *  \param Root        [out] the root of GDS file
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsCreateGDS(char **FileName, int *gds, CdGDSFolder **Root,
	LongBool *err)
{
	CdGDSFile *f = NULL;
	*gds = -1;

	CORETRY
		f = Init.GetEmptyFile(gds);
		f->SaveAsFile(*FileName);
		*Root = &f->Root();
		*err = false;
	CORECATCH(
		if ((f!=NULL) && !f->Log().List().empty())
		{
			Init.LastError.append(sLineBreak);
			Init.LastError.append("Log:");
			for (size_t i=0; i < f->Log().List().size(); i++)
			{
				Init.LastError.append(sLineBreak);
				Init.LastError.append(f->Log().List()[i].Msg);
			}
		}
		if (f) delete f;
		if (*gds >= 0) Init.Files[*gds] = NULL;
		*err = true; *gds = -1; *Root = NULL
	)
}

/// open a existing GDS file
/** \param FileName    [in] the file name
 *  \param gds         [out] the internal file index
 *  \param Root        [out] the root of GDS file
 *  \param ReadOnly    [in] whether open the file in read-only mode
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsOpenGDS(char **FileName, int *gds, CdGDSFolder **Root,
	LongBool *ReadOnly, LongBool *err)
{
	CdGDSFile *f = NULL;
	*gds = -1;

	CORETRY
		f = Init.GetEmptyFile(gds);
		f->LoadFile(*FileName, *ReadOnly);
		*Root = &f->Root();
		*err = false;
	CORECATCH(
		if ((f!=NULL) && !f->Log().List().empty())
		{
			Init.LastError.append(sLineBreak);
			Init.LastError.append("Log:");
			for (size_t i=0; i < f->Log().List().size(); i++)
			{
				Init.LastError.append(sLineBreak);
				Init.LastError.append(f->Log().List()[i].Msg);
			}
		}
		if (f) delete f;
		if (*gds >= 0) Init.Files[*gds] = NULL;
		*err = true; *gds = -1; *Root = NULL
	)
}

/// close the existing GDS file
/** \param gds         [in] the internal file index
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsCloseGDS(int *gds, LongBool *err)
{
	CORETRY
		CdGDSFile *f = Init.GetFile(*gds);
		Init.Files[*gds] = NULL;
		delete f;
		*err = false;
	CORECATCH(*err = true)
}

/// synchronize a GDS file
/** \param gds         [in] the internal file index
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsSyncGDS(int *gds, LongBool *err)
{
	CORETRY
		Init.GetFile(*gds)->SyncFile();
		*err = false;
	CORECATCH(*err = true)
}

/// detect whether a file has been opened
/** \param gds         [in] the internal file index
 *  \param out_valid   [out] return 1 if the file is valid, otherwise 0
**/
DLLEXPORT void gdsFileValid(int *gds, int *out_valid)
{
	if ((0 <= *gds) && (*gds < Init.MaxFiles))
	{
		*out_valid = (Init.Files[*gds] ? 1 : 0);
	} else {
		*out_valid = 0;
	}
}

/// clean up fragments of a GDS file
/** \param FileName    [in] the file name
 *  \param verbose     [in] if TRUE, show information
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsTidyUp(char **FileName, LongBool *verbose, LongBool *err)
{
	CORETRY
		CdGDSFile file(*FileName, CdGDSFile::dmOpenReadWrite);
		if (*verbose)
		{
			Rprintf("Clean up the fragments of GDS file:\n");
			Rprintf("\topen the file '%s' (size: %s).\n", *FileName,
				IntToStr(file.GetFileSize()).c_str());
			Rprintf("\tsave it to '%s.tmp'.\n", *FileName);
		}
		file.TidyUp();
		if (*verbose)
		{
			Rprintf("\trename '%s.tmp' (size: %s).\n", *FileName,
				IntToStr(file.GetFileSize()).c_str());
		}
		*err = false;
	CORECATCH(*err = true)
}




// *****************************************************************************
// File Structure Operations
// *****************************************************************************

/// detect whether a node is valid
/** \param Node        [in] a specified GDS node
 *  \param out_valid   [out] return 1 if the file is valid, otherwise 0
**/
DLLEXPORT void gdsNodeValid(CdGDSObj **Node, int *out_valid)
{
	CORETRY
		*out_valid = 0;
		if (*Node != NULL)
		{
			CdGDSFile *file = (*Node)->GDSFile();
			if (file != NULL)
			{
				for (int i=0; i < Init.MaxFiles; i++)
				{
					if (Init.Files[i] == file)
					{
						*out_valid = 1;
						break;
					}
				}
			}
		}
	CORECATCH(*out_valid = 0)
}

/// detect whether a node is valid (internal use)
/** \param Node        [in] a specified GDS node **/
static void _NodeValid(CdGDSObj *Node)
{
	try {
		if (Node != NULL)
		{
			CdGDSFile *file = Node->GDSFile();
			if (file != NULL)
			{
				for (int i=0; i < Init.MaxFiles; i++)
				{
					if (Init.Files[i] == file)
						return;
				}
			}
			throw ErrGDSFmt("The GDS file has been closed.");
		}
	}
	catch (exception &E)
	{
		throw ErrGDSFmt("The GDS file has been closed.");
	}
	catch (...)
	{
		throw ErrGDSFmt("The GDS file has been closed.");
	}
}

/// get the number of child nodes
/** \param Node        [in] a specified GDS node
 *  \param Count       [out] return the number of child nodes
**/
DLLEXPORT void gdsNodeChildCnt(CdGDSObj **Node, int *Count)
{
	CORETRY
		// check
		_NodeValid(*Node);

		if (dynamic_cast<CdGDSFolder*>(*Node))
			*Count = static_cast<CdGDSFolder*>(*Node)->Count();
		else
			*Count = 0;
	CORECATCH(*Count = -1)
}

/// get the number of a specified node
/** \param Node        [in] a specified GDS node
 *  \param Name        [out] output the name
 *  \param Full        [in] if TRUE, return the name with full path
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsNodeName(CdGDSObj **Node, char **Name, LongBool *Full,
	LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		if (*Full)
			RStrAgn(UTF16toUTF8((*Node)->FullName()).c_str(), Name);
		else
			RStrAgn(UTF16toUTF8((*Node)->Name()).c_str(), Name);

		*err = false;
	CORECATCH(*err = true)
}

/// enumerate the names of its child nodes
/** \param Node        [in] a specified GDS node
 *  \param Names       [out] output the names
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsNodeEnumName(CdGDSObj **Node, char **Names, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		if (dynamic_cast<CdGDSFolder*>(*Node))
		{
			CdGDSFolder &Dir = *static_cast<CdGDSFolder*>(*Node);
			for (int i=0; i < (int)Dir.Count(); i++)
			{
				RStrAgn(UTF16toUTF8(Dir.ObjItem(i)->Name()).c_str(), Names);
				Names++;
			}
			*err = false;
		} else
			throw ErrGDSFmt(erNotFolder);
	CORECATCH(*err = true)
}

/// enumerate all of child nodes
/** \param Node        [in] a specified GDS node
 *  \param Ptr         [out] output child nodes
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsNodeEnumPtr(CdGDSObj **Node, void **Ptr, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		if (dynamic_cast<CdGDSFolder*>(*Node))
		{
			CdGDSFolder &Dir = *static_cast<CdGDSFolder*>(*Node);
			for (int i=0; i < (int)Dir.Count(); i++)
			{
				*Ptr = Dir.ObjItem(i);
				Ptr++;
				size_t s = sizeof(void*);
				if (s < 8) Ptr++;
			}
			*err = false;
		} else
			throw ErrGDSFmt(erNotFolder);
	CORECATCH(*err = true)
}

/// get the node with index or indices
/** \param Node        [in] a specified GDS node
 *  \param Index       [in] the index or indices of a specified node
 *  \param Cnt         [in] the length of Index
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsNodeIndex(CdGDSObj **Node, int *Index, int *Cnt,
	LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		for (int i=0; i < *Cnt; i++)
		{
			if (!dynamic_cast<CdGDSFolder*>(*Node))
				throw ErrGDSFmt(erNotFolder);
			CdGDSFolder &Dir = *static_cast<CdGDSFolder*>(*Node);
			if ((*Index < 1) || (*Index > (int)Dir.Count()))
			{
				throw ErrGDSFile("Child Index[%d], out of range 1..%d!",
					*Index, Dir.Count());
			}
			*Node = Dir.ObjItem(*Index-1);
			Index++;
		}

		*err = false;
	CORECATCH(*Node = NULL; *err = true)
}

/// get the node with specified path and name
/** \param Node        [in] a specified GDS node
 *  \param Name        [in] the path and name of a specified node
 *  \param Cnt         [in] the length of Index
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsNodeIndexEx(CdGDSObj **Node, char **Name, int *Cnt,
	LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		for (int i=0; i < *Cnt; i++)
		{
			if (!dynamic_cast<CdGDSFolder*>(*Node))
				throw ErrGDSFmt(erNotFolder);
			CdGDSFolder &Dir = *static_cast<CdGDSFolder*>(*Node);
			*Node = Dir.ObjItem(RStr(*Name));
			Name++;
		}
		*err = false;
	CORECATCH(*Node = NULL; *err = true)
}

/// get the description of a specified node
/** \param Node        [in] a specified GDS node
 *  \param Desp        [out] the description
 *  \param Name        [out] the name of a specified node
 *  \param SVType      [out] data type [rtNULL, rtInt, rtFloat, rtString, rtLogical]
 *  \param isarray     [out] indicates whether it is array-type
 *  \param DimCnt      [out] the number of dimension
 *  \param DimEach     [out] the size(s) of dimension
 *  \param MaxLen      [out] the max length of characters
 *  \param PackMode    [out] compression mode
 *  \param PackRatio   [out] the ratio of compression
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsNodeObjDesp(CdGDSObj **Node, char **Desp, char **Name, int *SVType,
	LongBool *isarray, int *DimCnt, int *DimEach, int *MaxLen,
	char **PackMode, double *PackRatio, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		// the name of node
		RStrAgn(UTF16toUTF8((*Node)->Name()).c_str(), Name);
		// the description
		RStrAgn((*Node)->dTraitName(), Desp);

		*isarray = FALSE;
		if (dynamic_cast<CdSequenceX*>(*Node))
		{
			CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);
			*SVType = Obj->SVType();
			*DimCnt = Obj->DimCnt(); Obj->GetDimLen(DimEach);
			*isarray = TRUE;

			if (Obj->PipeInfo())
			{
				RStrAgn(Obj->PipeInfo()->Coder(), PackMode);
				if (Obj->PipeInfo()->StreamTotalIn() > 0)
				{
					*PackRatio = (double)Obj->PipeInfo()->StreamTotalOut() /
						Obj->PipeInfo()->StreamTotalIn();
				} else {
					*PackRatio = NaN;
				}
			} else {
				*PackRatio = NaN;
			}

			if (dynamic_cast<CdFStr8*>(Obj))
			{
				*MaxLen = static_cast<CdFStr8*>(Obj)->MaxLength();
			} else if (dynamic_cast<CdFStr16*>(Obj))
			{
				*MaxLen = static_cast<CdFStr16*>(Obj)->MaxLength();
			} else if (dynamic_cast<CdFStr32*>(Obj))
			{
				*MaxLen = static_cast<CdFStr32*>(Obj)->MaxLength();
			} else {
				*MaxLen = -1;
			}

		} else if (dynamic_cast<CdGDSStreamContainer*>(*Node))
		{
			CdGDSStreamContainer *Obj = static_cast<CdGDSStreamContainer*>(*Node);
			*SVType = svCustom;
			*DimCnt = 1;

			if (Obj->PipeInfo())
			{
				DimEach[0] = Obj->PipeInfo()->StreamTotalIn();
				RStrAgn(Obj->PipeInfo()->Coder(), PackMode);
				if (Obj->PipeInfo()->StreamTotalIn() > 0)
				{
					*PackRatio = (double)Obj->PipeInfo()->StreamTotalOut() /
						Obj->PipeInfo()->StreamTotalIn();
				} else {
					*PackRatio = NaN;
				}
			} else {
				DimEach[0] = Obj->GetSize();;
				*PackRatio = NaN;
			}
		} else {
			*DimCnt = 0; *PackRatio = NaN;
			*SVType = svCustom;
		}

		*err = false;
	CORECATCH(*err = true)
}

/// add a new node
/** \param Node        [in] a specified GDS node
 *  \param NodeName    [in] the name of a new node
 *  \param Storage     [in] the mode of storage
 *  \param Compress    [in] the method of compression
 *  \param DimCnt      [in] the number of dimension
 *  \param Dim         [in] the size(s) of dimension
 *  \param MaxLen      [in] if the data is character format, specify the maximum nchar
 *  \param CompleteData  [in] if compressing data and TRUE, get into read mode after adding
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsAddNode(CdGDSObj **Node, char **NodeName, char **Storage,
	char **Compress, int *DimCnt, int *Dim, int *MaxLen, LongBool *CompleteData,
	LongBool *err)
{
	CdSequenceX *vObj = NULL;

	CORETRY
		// check
		_NodeValid(*Node);

		if (!dynamic_cast<CdGDSFolder*>(*Node))
			throw ErrGDSFmt(erNotFolder);
		CdGDSFolder &Dir = *static_cast<CdGDSFolder*>(*Node);

		// Class Name Mapping
		const char *nName;
		map<const char*, const char*, TInit::strCmp>::iterator it;
		it = Init.ClassMap.find(*Storage);
		if (it != Init.ClassMap.end())
			nName = it->second;
		else
			throw ErrGDSFmt(string("Not support: ") + *Storage);

		if (strcmp(nName, "$FOLDER$") == 0)
		{
			*Node = &Dir.AddFolder(RStr(*NodeName));
		} else {

			CdObjClassMgr::TdOnObjCreate OnCreate = dObjManager().NameToClass(nName);
			if (OnCreate)
			{
				CdObject *obj = OnCreate();
				if (dynamic_cast<CdSequenceX*>(obj))
					vObj = static_cast<CdSequenceX*>(obj);
				else
					delete obj;
			}

			if (vObj != NULL)
			{
				if (dynamic_cast<CdFStr8*>(vObj))
					static_cast<CdFStr8*>(vObj)->SetMaxLength(*MaxLen);

				vObj->SetPackedMode(*Compress);
				if (*DimCnt >= 1)
				{
					Dim += *DimCnt;
					for (int i = *DimCnt; i >= 1; i--)
					{
						Dim--;
						if (*Dim < 0)
							throw ErrGDSFmt("Invalid dimension size %d", *Dim);
						if (i == 1)
							vObj->AddDim(0);
						else
							vObj->AddDim(*Dim);
					}
				}
			}

			*Node = Dir.AddObj(RStr(*NodeName), vObj);
		}

		if (vObj != NULL)
		{
			if ((vObj->DimCnt()>0) && (!vObj->PipeInfo()) && *CompleteData)
				vObj->SetDLen(0, *Dim);
        }

		*err = false;
	CORECATCH(*err = true)
}

/// add a new node with a specified file
/** \param Node        [in] a specified GDS node
 *  \param NodeName    [in] the name of a new node
 *  \param FileName    [in] the name of input file
 *  \param Compress    [in] the method of compression
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsAddFile(CdGDSObj **Node, char **NodeName, char **FileName,
	char **Compress, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		// the pointer to the directory
		if (!dynamic_cast<CdGDSFolder*>(*Node))
			throw ErrGDSFmt(erNotFolder);
		CdGDSFolder &Dir = *static_cast<CdGDSFolder*>(*Node);

		TdAutoRef<CBufdStream> file(new CBufdStream(
			new CdFileStream(*FileName, CdFileStream::fmOpenRead)));
		CdGDSStreamContainer *vObj = new CdGDSStreamContainer();
		vObj->SetPackedMode(*Compress);
		*Node = Dir.AddObj(RStr(*NodeName), vObj);
		vObj->CopyFrom(*file.get());
		vObj->CloseWriter();

		*err = false;
	CORECATCH(*err = true)
}

/// get the file from a file node
/** \param Node        [in] a specified GDS node
 *  \param OutFileName [in] the name for output file
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsGetFile(CdGDSObj **Node, char **OutFileName, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);
		// the pointer to the file
		if (!dynamic_cast<CdGDSStreamContainer*>(*Node))
			throw ErrGDSFmt(erNotFile);

		CdGDSStreamContainer *Obj = static_cast<CdGDSStreamContainer*>(*Node);
		TdAutoRef<CBufdStream> file(new CBufdStream(
			new CdFileStream(*OutFileName, CdFileStream::fmCreate)));
		Obj->CopyTo(*file.get());

		*err = false;
	CORECATCH(*err = true)
}

/// delete a node
/** \param Node        [in] a specified GDS node
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsDeleteNode(CdGDSObj **Node, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		if ((*Node)->Folder())
		{
			(*Node)->Folder()->DeleteObj(*Node);
			*err = false;
		} else {
			Init.LastError = "Can not delete the root.";
			*err = true;
		}
	CORECATCH(*err = true)
}

/// rename a node
/** \param Node        [in] a specified GDS node
 *  \param NewName     [in] the new name
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsRenameNode(CdGDSObj **Node, char **NewName, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);
		(*Node)->SetName(*NewName);
		*err = false;
	CORECATCH(*err = true)
}





// *****************************************************************************
// Attribute Operations
// *****************************************************************************

/// get the number of attributes
/** \param Node        [in] a specified GDS node
 *  \param Cnt         [out] output the number of attributes
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsAttrCnt(CdGDSObj **Node, int *Cnt, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);
		*Cnt = (*Node)->Attribute().Count();
		*err = false;
	CORECATCH(*err = true)
}

/// get the types of attributes
/** \param Node        [in] a specified GDS node
 *  \param PType       [out] output the types of attributes
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsAttrType(CdGDSObj **Node, int *PType, LongBool *err)
{
	CORETRY
		for (int i = 0; i < (int)(*Node)->Attribute().Count(); i++)
		{
			TdsData &p = (*Node)->Attribute()[i];
			if (p.isInt())
				*PType = rtInt;
			else if (p.isFloat())
				*PType = rtFloat;
			else if (p.isStr())
				*PType = rtString;
			else if (p.isBool())
				*PType = rtLogical;
			else
				*PType = rtNULL;
			PType++;
		}
		*err = false;
	CORECATCH(*err = true)
}

/// get the value of attribute
/** \param Node        [in] a specified GDS node
 *  \param Index       [in] the index of attribute (from ONE)
 *  \param RType       [out] output the type of attribute
 *  \param Ptr         [in] the pointer to data field
 *  \param Name        [out] output the name of attribute
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsGetAttr(CdGDSObj **Node, int *Index, int *RType, void *Ptr,
	char **Name, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		RStrAgn(UTF16toUTF8((*Node)->Attribute().Names(*Index-1)).c_str(), Name);
		TdsData &p = (*Node)->Attribute()[*Index-1];
		switch (*RType)
		{
			case rtInt:
				*static_cast<int*>(Ptr) = p.getInt32(); break;
			case rtFloat:
				*static_cast<double*>(Ptr) = p.getFloat64(); break;
			case rtString:
				RStrAgn(p.getStr8().c_str(), static_cast<char**>(Ptr)); break;
			case rtLogical:
				*static_cast<LongBool*>(Ptr) = p.getBool(); break;
		}

		*err = false;
	CORECATCH(*err = true)
}

/// set an attribute
/** \param Node        [in] a specified GDS node
 *  \param Name        [in] the name of attribute
 *  \param RType       [in] the type of attribute
 *  \param Ptr         [in] the pointer to data field
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsPutAttr(CdGDSObj **Node, char **Name, int *RType,
	void *Ptr, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		TdsData *p;
		if ((*Node)->Attribute().HasName(*Name))
			p = &((*Node)->Attribute()[*Name]);
		else
			p = &((*Node)->Attribute().Add(*Name));
		switch (*RType)
		{
			case rtInt:
            	p->setInt32(*static_cast<int*>(Ptr)); break;
			case rtFloat:
            	p->setFloat64(*static_cast<double*>(Ptr)); break;
			case rtString:
				p->setStr8(RStr(*static_cast<char**>(Ptr))); break;
			case rtLogical:
				p->setBool(*static_cast<int*>(Ptr)); break;
		}

		*err = false;
	CORECATCH(*err = true)
}

/// delete an attribute
/** \param Node        [in] a specified GDS node
 *  \param Name        [in] the name of attribute
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsDeleteAttr(CdGDSObj **Node, char **Name, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);
		(*Node)->Attribute().Delete(UTF8toUTF16(*Name));
		*err = false;
	CORECATCH(*err = true)
}





// *****************************************************************************
// Data Operations
// *****************************************************************************

/// set error
inline static void ErrNode(CdGDSObj *Obj)
{
	Init.LastError = Obj ? "Not supported data type!" : "No dataset!";
}

/// convert RType to SVType
inline static TSVType RtoSV(int RType)
{
	switch (RType) {
		case 1: return svInt32;
		case 2: return svFloat64;
		case 3: return svStrUTF8;
		case 4: return svInt32;
		default: throw ErrGDSFmt("Invalid RType %d", RType);
	}
}

/// append data to a node
/** \param Node        [in] a specified GDS node
 *  \param RType       [in] the data type
 *  \param Ptr         [in] the pointer to data field
 *  \param TotalCnt    [in] the total number of appended data
 *  \param CntWarn     [out] if TRUE, 
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsObjAppend(CdGDSObj **Node, int *RType, void *Ptr,
	int *TotalCnt, LongBool *CntWarn, LongBool *err)
{
	if (*TotalCnt <= 0)
		{ *err = false; return; }

	CORETRY
		// check
		_NodeValid(*Node);

		if (dynamic_cast<CdSequenceX*>(*Node))
		{
			CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);
			TSVType sv = RtoSV(*RType);

			if (sv == svStrUTF8)
			{
				string *Buf = new string[*TotalCnt];
				try {
					string *d = Buf;
					char **s = (char**)Ptr;
					for (int i=0; i < *TotalCnt; i++)
						*d++ = *s++;
					Obj->Append(Buf, *TotalCnt, svStrUTF8);
				} catch (...) {
					delete [] Buf;
					throw;
				}
				delete [] Buf;
			} else
				Obj->Append(Ptr, *TotalCnt, sv);

			if (Obj->PipeInfo())
				Obj->PipeInfo()->UpdateStreamSize();
			if (dynamic_cast<CdVectorX*>(Obj))
			{
				CdVectorX *vObj = static_cast<CdVectorX*>(Obj);
				*CntWarn = vObj->CurrectCnt() != vObj->Count();
			} else
				*CntWarn = false;

			*err = false;
		} else {
			*err = true;
			ErrNode(*Node);
		}

	CORECATCH(*err = true)
}

/// get the description
/** \param Node        [in] a specified GDS node
 *  \param DimCnt      [in] the number of dimension
 *  \param Start       [in] the starting positions
 *  \param Count       [in] the count
 *  \param TotalCnt    [out] output the total number of data
 *  \param RType       [out] the data type
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsxObjDesp(CdGDSObj **Node, int *DimCnt, int *Start,
	int *Count, int *TotalCnt, int *RType, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		if (dynamic_cast<CdSequenceX*>(*Node))
		{
			CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);
			if (*DimCnt >= 0)
			{
				if (Obj->DimCnt() != *DimCnt)
					throw ErrGDSFmt("Invalid dimension!");
				if (*DimCnt > 0)
				{
					CdSequenceX::TSeqDimBuf DLen;
					Obj->GetDimLen(DLen);
					CopyCnt(Count, DLen, Start, *DimCnt);
				} else
					*Start = *Count = 1;
			} else {
				*DimCnt = Obj->DimCnt();
				Obj->GetDimLen(Count);
			}

			Int64 Total = TotalCount(Count, *DimCnt);
			*TotalCnt = Total;
			if (COREARRAY_SVINT(Obj->SVType()))
			{
				*RType = ((*Node)->Attribute().HasName("R.logical")) ?
					rtLogical : rtInt;
			} else if (COREARRAY_SVFLOAT(Obj->SVType()))
				*RType = rtFloat;
			else if (COREARRAY_SVSTR(Obj->SVType()))
				*RType = rtString;
			else
				*RType = rtNULL;

			*err = false;

		} else {
			*err = true;
			ErrNode(*Node);
		}

	CORECATCH(*err = true)
}

/// get the type of data
/** \param Node        [in] a specified GDS node
 *  \param RType       [out] the data type
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsxObjType(CdGDSObj **Node, int *RType, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		if (dynamic_cast<CdSequenceX*>(*Node))
		{
			CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);
			if (COREARRAY_SVINT(Obj->SVType()))
			{
				*RType = ((*Node)->Attribute().HasName("R.logical")) ?
					rtLogical : rtInt;
			} else if (COREARRAY_SVFLOAT(Obj->SVType()))
				*RType = rtFloat;
			else if (COREARRAY_SVSTR(Obj->SVType()))
				*RType = rtString;
			else
				*RType = rtNULL;

			*err = false;

		} else {
			*err = true;
			ErrNode(*Node);
		}

	CORECATCH(*err = true)
}


/// read string
static void rIterStr(TIterDataExt &Rec)
{
	char **p = (char**)Rec.pBuf;
	TdIterator it = Rec.Seq->Iterator(Rec.Index);
	for (int k=1; k <= Rec.LastDim; k++)
	{
		RStrAgn(UTF16toUTF8(it.toStr()).c_str(), p);
		p++; ++it;
	}
	Rec.pBuf = (char*)p;
}

/// read data from a node
/** \param Node        [in] a specified GDS node
 *  \param DimCnt      [in] the number of dimension
 *  \param Start       [in] the starting positions
 *  \param Count       [in] the count
 *  \param RType       [in] the data type
 *  \param Ptr         [out] the pointer to data field
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsObjReadData(CdGDSObj **Node, int *DimCnt, int *Start,
	int *Count, int *RType, void *Ptr, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		if (dynamic_cast<CdSequenceX*>(*Node))
		{
			CdSequenceX::TSeqDimBuf DStart;
			CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);

			if (Obj->DimCnt() != *DimCnt)
				throw ErrGDSFmt("Invalid dimension!");
			CopyDec(Start, *DimCnt, DStart);

			TSVType SV = RtoSV(*RType);
			if (!COREARRAY_SVSTR(SV))
			{
				Obj->rData(DStart, Count, Ptr, SV);
			} else {
				// Checking
				int *pStart=DStart, *pCount=Count;
				for (int i=0; i < *DimCnt; i++)
				{
					if ((*pStart<0) || (*pCount<0) || (*pStart+*pCount > Obj->GetDLen(i)))
						throw ErrGDSFmt("Invalid selection!");
					pStart++; pCount++;
				}

				int i = *DimCnt;
				if (i > 0)
				{
					TIterDataExt Rec;
					Rec.pBuf = (char*)Ptr;
					Rec.LastDim = Count[--i];
					Rec.Seq = Obj;
					if (Rec.LastDim > 0)
						Internal::SeqIterRect(DStart, Count, *DimCnt, Rec, rIterStr);
				} else {
					RStrAgn(UTF16toUTF8(Obj->Iterator(NULL).toStr()).c_str(),
						static_cast<char**>(Ptr));
				}
			}
			*err = false;
		} else {
			*err = true;
			ErrNode(*Node);
		}

	CORECATCH(*err = true)
}


/// read string from the selection
static void rIterStrEx(TIterDataExt &Rec, CBOOL *Selection)
{
	char **p = (char**)Rec.pBuf;
	TdIterator it = Rec.Seq->Iterator(Rec.Index);
	for (int k=1; k <= Rec.LastDim; k++)
	{
		if (*Selection++)
		{
			RStrAgn(UTF16toUTF8(it.toStr()).c_str(), p);
			p++;
		}
		++it;
	}
	Rec.pBuf = (char*)p;
}

/// read data from a node with a selection
/** \param Node        [in] a specified GDS node
 *  \param Selection   [in] the logical variable of selection
 *  \param RType       [in] the data type
 *  \param Ptr         [out] the pointer to data field
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsObjReadExData(CdGDSObj **Node, LongBool *Selection,
	int *RType, void *Ptr, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		if (dynamic_cast<CdSequenceX*>(*Node))
		{
			CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);
			CdSequenceX::TSeqDimBuf DStart, DLen;

			memset(&DStart, 0, sizeof(DStart));
			Obj->GetDimLen(DLen);
			int DCnt = Obj->DimCnt();
			
			vector< vector<CBOOL> > Select;
			vector< CBOOL* > SelPtr;
			Select.resize(DCnt);
			for (int i=DCnt-1; i >= 0; i--)
			{
				Select[i].resize(DLen[i]);
				for (int k=0; k < DLen[i]; k++, Selection++)
					Select[i][k] = (*Selection == TRUE);
			}
			SelPtr.resize(DCnt);
			for (int i=0; i < DCnt; i++) SelPtr[i] = &(Select[i][0]);

			TSVType SV = RtoSV(*RType);
			if (!COREARRAY_SVSTR(SV))
			{
				Obj->rDataEx(DStart, DLen, &(SelPtr[0]), Ptr, SV);
			} else {
				if (DCnt > 0)
				{
					TIterDataExt Rec;
					Rec.pBuf = (char*)Ptr;
					Rec.LastDim = DLen[DCnt-1];
					Rec.Seq = Obj;
					if (Rec.LastDim > 0)
						Internal::SeqIterRectEx(DStart, DLen, &(SelPtr[0]), DCnt, Rec, rIterStrEx);
				} else {
					RStrAgn(UTF16toUTF8(Obj->Iterator(NULL).toStr()).c_str(),
						static_cast<char**>(Ptr));
				}
			}
			*err = false;
		} else {
			*err = true;
			ErrNode(*Node);
		}

	CORECATCH(*err = true)
}


// *************************************************************************
// apply.gdsn
//

/// convert LongBool to CBOOL
/** \param from        [in] input logical vector
 *  \param to          [out] output CBOOL vector
 *  \param cnt         [in] the length of vector
**/
DLLEXPORT void gdsLongBool2CBOOL(LongBool *from, CBOOL *to, int *cnt)
{
	for (int i=0; i < *cnt; i++, from++, to++)
	{
		*to = (*from == TRUE);
	}
}

/// read data from a node for "apply.gdsn"
/** \param Node        [in] a specified GDS node
 *  \param Col         [in] the column index (starting from ONE), used for ColIdx
 *  \param ColIdx      [in] the matching indices of columns
 *  \param rCnt        [in] the count of columns
 *  \param RowSel      [in] the selection of rows
 *  \param RType       [in] the data type
 *  \param Ptr         [out] the pointer to data field
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsApplyCol(CdGDSObj **Node, int *Col, int *ColIdx, int *rCnt,
	CBOOL *RowSel, int *RType, void *Ptr, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);
		if (!dynamic_cast<CdSequenceX*>(*Node))
			ErrNode(*Node);

		CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);
		CdSequenceX::TSeqDimBuf DStart, Count;
		Obj->GetDimLen(Count);

		// the following is OK for Obj->DimCnt() == 1 or 2

		const int nCol = ColIdx[*Col + *rCnt - 2] - ColIdx[*Col - 1] + 1;
		vector<CBOOL> ColFlag(nCol, false);
		for (int i=0; i < *rCnt; i++)
			ColFlag[ColIdx[*Col + i - 1] - ColIdx[*Col - 1]] = true;
		CBOOL *SelPtr[2] = { &(ColFlag[0]), RowSel };

		DStart[0] = ColIdx[*Col - 1] - 1; DStart[1] = 0;
		Count[0] = nCol;

		if (Obj->DimCnt() == 1) Count[1] = nCol;

		TSVType SV = RtoSV(*RType);
		if (!COREARRAY_SVSTR(SV))
		{
			Obj->rDataEx(DStart, Count, SelPtr, Ptr, SV);
		} else {
			TIterDataExt Rec;
			Rec.pBuf = (char*)Ptr;
			Rec.LastDim = Count[1];
			Rec.Seq = Obj;
			if (Rec.LastDim > 0)
				Internal::SeqIterRectEx(DStart, Count, SelPtr, Obj->DimCnt(), Rec, rIterStrEx);
		}
		*err = false;

	CORECATCH(*err = true)
}


/// read data from a node for "apply.gdsn"
/** \param Node        [in] a specified GDS node
 *  \param Row         [in] the row index (starting from ONE), used for RowIdx
 *  \param RowIdx      [in] the matching indices of rows
 *  \param rCnt        [in] the count of rows
 *  \param ColSel      [in] the selection of columns
 *  \param RType       [in] the data type
 *  \param Ptr         [out] the pointer to data field
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsApplyRow(CdGDSObj **Node, int *Row, int *RowIdx, int *rCnt,
	CBOOL *ColSel, int *RType, void *Ptr, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);
		if (!dynamic_cast<CdSequenceX*>(*Node))
			ErrNode(*Node);

		CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);
		CdSequenceX::TSeqDimBuf DStart, Count;
		Obj->GetDimLen(Count);

		const int nRow = RowIdx[*Row + *rCnt - 2] - RowIdx[*Row - 1] + 1;
		vector<CBOOL> RowFlag(nRow, false);
		for (int i=0; i < *rCnt; i++)
			RowFlag[RowIdx[*Row + i - 1] - RowIdx[*Row - 1]] = true;
		CBOOL *SelPtr[2] = { ColSel, &(RowFlag[0]) };

		DStart[0] = 0; DStart[1] = RowIdx[*Row - 1] - 1;
		Count[1] = nRow;

		if (Obj->DimCnt() == 1)
		{
			DStart[0] = DStart[1]; Count[0] = Count[1];
			SelPtr[0] = SelPtr[1];
		}

		TSVType SV = RtoSV(*RType);
		if (!COREARRAY_SVSTR(SV))
		{
			Obj->rDataEx(DStart, Count, SelPtr, Ptr, SV);
		} else {
			TIterDataExt Rec;
			Rec.pBuf = (char*)Ptr;
			Rec.LastDim = Count[1];
			Rec.Seq = Obj;
			if (Rec.LastDim > 0)
				Internal::SeqIterRectEx(DStart, Count, SelPtr, Obj->DimCnt(), Rec, rIterStrEx);
		}
		*err = false;

	CORECATCH(*err = true)
}



/// write string
static void wIterStr(TIterDataExt &Rec)
{
	char **p = (char**)Rec.pBuf;
	TdIterator it = Rec.Seq->Iterator(Rec.Index);
	for (int k=1; k <= Rec.LastDim; k++)
	{
		it.StrTo(PChartoUTF16(*p));
		p++; ++it;
	}
	Rec.pBuf = (char*)p;
}

/// write data to a node
/** \param Node        [in] a specified GDS node
 *  \param DimCnt      [in] the number of dimension
 *  \param Dim         [in] the sizes of dimension
 *  \param RType       [in] the data type
 *  \param Ptr         [in] the pointer to data field
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsObjWriteAll(CdGDSObj **Node, int *DimCnt, int *Dim,
	int *RType, void *Ptr, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		if (dynamic_cast<CdSequenceX*>(*Node))
		{
			if (*DimCnt > 0)
			{
				int *p = Dim;
				for (int i=1; i <= *DimCnt; i++, p++)
					if (*p < 0)
						throw ErrGDSFmt("Invalid dimension size %d", *p);
			} else
				*DimCnt = 0;

			CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);
			if (*DimCnt < Obj->DimCnt())
			{
				throw ErrGDSFmt("New dimension should not be less than the currect.");
			} else {
				Obj->Clear();
				for (int i=Obj->DimCnt()+1; i <= *DimCnt; i++)
					Obj->AddDim(0);
				int *p = Dim + *DimCnt;
				for (int i = *DimCnt-1; i >= 0; i--)
				{
					p--; Obj->SetDLen(i, *p);
				}
			}

			CdSequenceX::TSeqDimBuf DStart, DLen;
			TSVType SV = RtoSV(*RType);
			if (!COREARRAY_SVSTR(SV))
			{
				memset((void*)DStart, 0, sizeof(DStart));
				Obj->wData(DStart, Dim, Ptr, SV);
			} else {
				int i = *DimCnt;
				if (i > 0)
				{
					TIterDataExt Rec;
					Rec.pBuf = (char*)Ptr;
					Rec.LastDim = Dim[--i];
					Rec.Seq = Obj;
					memset((void*)DStart, 0, sizeof(DStart));
					Obj->GetDimLen(DLen);
					if (Rec.LastDim > 0)
						Internal::SeqIterRect(DStart, DLen, *DimCnt, Rec, wIterStr);
				} else {
					char **s = (char**)Ptr;
					Obj->Iterator(NULL).StrTo(PChartoUTF16(*s));
                }
			}
		}
	CORECATCH(*err = true)
}

/// write data to a node
/** \param Node        [in] a specified GDS node
 *  \param DimCnt      [in] the number of dimension
 *  \param Start       [in] the starting positions
 *  \param Count       [in] the count
 *  \param RType       [in] the data type
 *  \param Ptr         [in] the pointer to data field
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsObjWriteData(CdGDSObj **Node, int *DimCnt, int *Start,
	int *Count, int *RType, void *Ptr, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		if (dynamic_cast<CdSequenceX*>(*Node))
		{
			CdSequenceX::TSeqDimBuf DStart;
			CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);
			if (Obj->DimCnt() != *DimCnt)
				throw ErrGDSFmt("Invalid dimension!");
			CopyDec(Start, *DimCnt, DStart);

			TSVType SV = RtoSV(*RType);
			if (!COREARRAY_SVSTR(SV))
			{
				Obj->wData(DStart, Count, Ptr, SV);
			} else {
				// Checking
				int *pStart=DStart, *pCount=Count;
				for (int i=0; i < *DimCnt; i++)
				{
					if ((*pStart<0) || (*pCount<0) || (*pStart+*pCount > Obj->GetDLen(i)))
						throw ErrGDSFmt("Invalid dimension!");
					pStart++; pCount++;
				}

				int i = *DimCnt;
				if (i > 0)
				{
					TIterDataExt Rec;
					Rec.pBuf = (char*)Ptr;
					Rec.LastDim = Count[--i];
					Rec.Seq = Obj;
					if (Rec.LastDim > 0)
						Internal::SeqIterRect(DStart, Count, *DimCnt, Rec, wIterStr);
				} else {
					char **s = (char**)Ptr;
					Obj->Iterator(NULL).StrTo(PChartoUTF16(*s));
				}
			}

			*err = false;
		} else {
			*err = true;
			ErrNode(*Node);
        }
	CORECATCH(*err = true)
}

/// set the dimension of data to a node
/** \param Node        [in] a specified GDS node
 *  \param DimCnt      [in] the number of dimension
 *  \param DLen        [in] the new sizes of dimension
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsObjSetDim(CdGDSObj **Node, int *DimCnt, int *DLen,
	LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		if (dynamic_cast<CdSequenceX*>(*Node))
		{
			CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);
			if (*DimCnt < Obj->DimCnt())
			{
				throw ErrGDSFmt("New dimension should not be less than the currect.");
			} else {
				for (int i=Obj->DimCnt()+1; i <= *DimCnt; i++)
					Obj->AddDim(-1);
				int *p = DLen + *DimCnt;
				for (int i=*DimCnt-1; i >= 0; i--)
				{
					p--; Obj->SetDLen(i, *p);
                }
			}
			*err = false;
		} else {
			*err = true;
			ErrNode(*Node);
		}
	CORECATCH(*err = true)
}

/// set a new compression mode
/** \param Node        [in] a specified GDS node
 *  \param Compress    [in] the compression mode
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsObjCompress(CdGDSObj **Node, char **Compress, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		if (dynamic_cast<CdContainer*>(*Node))
		{
			static_cast<CdContainer*>(*Node)->SetPackedMode(*Compress);
			*err = false;
		} else {
			*err = true;
			ErrNode(*Node);
		}
	CORECATCH(*err = true)
}

/// get into read mode of compression
/** \param Node        [in] a specified GDS node
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gdsObjPackClose(CdGDSObj **Node, LongBool *err)
{
	CORETRY
		// check
		_NodeValid(*Node);

		if (dynamic_cast<CdContainer*>(*Node))
		{
			CdContainer *vObj = static_cast<CdContainer*>(*Node);
			vObj->CloseWriter();
			*err = false;
		} else {
			ErrNode(*Node);
			*err = true;
		}
	CORECATCH(*err = true)
}

/// get the last error message
/** \param Msg        [out] output the last error message
**/
DLLEXPORT void gdsLastErrGDS(char **Msg)
{
	RStrAgn(Init.LastError.c_str(), Msg);
}


/// get number of bytes and bits
/** \param ClassName   [in] the name of class
 *  \param out_nbit    [out] the number of bits
 *  \param err         [out] return TRUE if error occurs, otherwise FALSE
**/
DLLEXPORT void gds_Internal_Class(char **ClassName, int *out_nbit, int *err)
{
	CORETRY
		Init.CheckInit();

		*out_nbit = -1;

		// Class Name Mapping
		const char *nName;
		map<const char*, const char*, TInit::strCmp>::iterator it;
		it = Init.ClassMap.find(*ClassName);
		if (it != Init.ClassMap.end())
			nName = it->second;
		else
			throw ErrGDSFmt(string("Not support: ") + *ClassName);

		// mapping
		CdObjClassMgr::TdOnObjCreate OnCreate = dObjManager().NameToClass(nName);
		if (OnCreate)
		{
			CdObject *obj = OnCreate();
			if (dynamic_cast<CdContainer*>(obj))
			{
				*out_nbit = static_cast<CdContainer*>(obj)->BitOf();
			}
			delete obj;
		} else
			throw ErrGDSFmt(string("Not support: ") + *ClassName);

		*err = false;
	CORECATCH(*err = true)
}


} // extern "C"
