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

#include <dType.hpp>
#include <dSeq.hpp>

#include <R.h>
#include <cstring>
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



class TInit
{
public:
	const static int MaxFiles		= 256;
	CdGDSFile *Files[MaxFiles];
	string LastError;

	struct strCmp {
		bool operator()( const char* s1, const char* s2 ) const
			{ return strcmp( s1, s2 ) < 0; }
	};
	map<const char *, const char *, strCmp> ClassMap;

	TInit()
	{
		RegisterClass();
		memset((void*)Files, 0, sizeof(Files));

		// used in gdsAddNode
		ClassMap["NULL"] = "";
		ClassMap["folder"] = "$FOLDER$";

		// Integer

		ClassMap["int8"] = TdTraits<CoreArray::Int8>::StreamName();
		ClassMap["uint8"] = TdTraits<CoreArray::UInt8>::StreamName();
		ClassMap["int16"] = TdTraits<CoreArray::Int16>::StreamName();
		ClassMap["uint16"] = TdTraits<CoreArray::UInt16>::StreamName();
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
		ClassMap["wstring"] = TdTraits<CoreArray::UTF16*>::StreamName();
		ClassMap["dwstring"] = TdTraits<CoreArray::UTF32*>::StreamName();

		// R storage mode
		ClassMap["integer"] = TdTraits<CoreArray::Int32>::StreamName();
		ClassMap["double"] = TdTraits<CoreArray::Float64>::StreamName();
		ClassMap["character"] = TdTraits<CoreArray::UTF8*>::StreamName();
		ClassMap["logical"] = TdTraits<CoreArray::Int32>::StreamName();
		ClassMap["list"] = "$FOLDER$";
		ClassMap["factor"] = TdTraits<CoreArray::UTF8*>::StreamName();;
	}

	~TInit()
	{
		for (int i=0; i < MaxFiles; i++) {
			if (Files[i] == NULL) {
				try { delete Files[i]; } catch (...) { }
			}
		}
	}

	CdGDSFile *GetEmptyFile(int *Index) {
		for (int i=0; i < MaxFiles; i++)
			if (Files[i] == NULL) {
				CdGDSFile *rv = new CdGDSFile;
				*Index = i; Files[i] = rv;
				return rv;
			}
		*Index = -1;
		throw ErrSequence("You have opened 256 gds files, not allow one more!");
	}

	CdGDSFile *GetFile(int Index) {
		if ((Index<0) || (Index>=MaxFiles))
			throw ErrSequence("Invalid gds file!");
		CdGDSFile *rv = Files[Index];
		if (rv == NULL)
			throw ErrSequence("The gds file has been closed.");
		return rv;
	}

private:
};

static TInit Init;

class ErrGDSFmt: public ErrCoreArray
{
public:
	ErrGDSFmt() {};
	ErrGDSFmt(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
	ErrGDSFmt(const std::string &msg) { fMessage = msg; }
};


// for RType, don't change the values
const static int rtNULL			= 0;
const static int rtInt			= 1;
const static int rtFloat		= 2;
const static int rtString		= 3;
const static int rtLogical		= 4;

static const char *erNotFolder = "It is not a folder!";

inline static void RStrAgn(const char *Text, char **rstr)
{
	*rstr = R_alloc(strlen(Text)+1, 1);
	if (*rstr == NULL)
		throw Err_dObj("R_alloc return NULL!");
	strcpy(*rstr, Text);
}

inline static const char * RStr(const char *Name)
{
	return (Name) ? Name : "";
}

static void CopyDec(int *Value, int Cnt, int *Buf)
{
	if (Value != NULL) {
		for (int i=1; i <= Cnt; i++) {
			*Buf = *Value - 1;
			if (*Buf < 0)
				throw ErrGDSFmt("Invalid 'start'!");
			Buf++; Value++;
		}
	} else
		for (int i=1; i <= Cnt; i++)
			*Buf++ = 1;
}

static void CopyCnt(int *Count, int *DLen, int *Start, int Cnt)
{
	for (int k=1; k <= Cnt; k++) {
		if (*Count >= 0) {
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

inline static Int64 TotalCount(int *Count, int Cnt)
{
	Int64 rv = 1;
	for (int i=1; i <= Cnt; i++)
		rv *= *Count++;
	return rv;
}



extern "C"
{
// File Operations

DLLEXPORT void gdsCreateGDS(char **FileName, int *gds, CdGDSFolder **Root,
	LongBool *err)
{
	CdGDSFile *f = NULL;
	try {
		f = Init.GetEmptyFile(gds);
		f->SaveAsFile(*FileName);
		*Root = &f->Root();
		*err = false;
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
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
		*err = true; *gds = -1; *Root = NULL;
	}
}

DLLEXPORT void gdsOpenGDS(char **FileName, int *gds, CdGDSFolder **Root,
	LongBool *ReadOnly, LongBool *err)
{
	CdGDSFile *f = NULL;
	try {
		f = Init.GetEmptyFile(gds);
		f->LoadFile(*FileName, *ReadOnly);
		*Root = &f->Root();
		*err = false;
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		if ((f!=NULL) && !f->Log().List().empty()) {
			Init.LastError.append(sLineBreak);
			Init.LastError.append("Log:");
			for (size_t i=0; i < f->Log().List().size(); i++) {
				Init.LastError.append(sLineBreak);
				Init.LastError.append(f->Log().List()[i].Msg);
			}
		}
		if (f) delete f;
		*err = true; *gds = -1; *Root = NULL;
	}
}

DLLEXPORT void gdsCloseGDS(int *gds, LongBool *err)
{
	try {
		CdGDSFile *f = Init.GetFile(*gds);
		Init.Files[*gds] = NULL;
		delete f;
		*err = false;
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsSyncGDS(int *gds, LongBool *err)
{
	try {
		Init.GetFile(*gds)->SyncFile();
		*err = false;
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}


// File Structure Operations

DLLEXPORT void gdsNodeChildCnt(CdGDSObj **Node, int *Count)
{
	try {
		if (dynamic_cast<CdGDSFolder*>(*Node))
			*Count = static_cast<CdGDSFolder*>(*Node)->Count();
		else
			*Count = 0;
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*Count = -1;
	}
}

DLLEXPORT void gdsNodeName(CdGDSObj **Node, char **Name, LongBool *Full,
	LongBool *err)
{
	try {
		if (*Full)
			RStrAgn(UTF16toUTF8((*Node)->FullName()).c_str(), Name);
		else
			RStrAgn(UTF16toUTF8((*Node)->Name()).c_str(), Name);
		*err = false;
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsNodeEnumName(CdGDSObj **Node, char **Names, LongBool *err)
{
	try {
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
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsNodeEnumPtr(CdGDSObj **Node, void **Ptr, LongBool *err)
{
	try {
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
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsNodeIndex(CdGDSObj **Node, int *Index, int *Cnt,
	LongBool *err)
{
	try {
		for (int i=0; i < *Cnt; i++)
		{
			if (!dynamic_cast<CdGDSFolder*>(*Node))
				throw ErrGDSFmt(erNotFolder);
			CdGDSFolder &Dir = *static_cast<CdGDSFolder*>(*Node);
			if ((*Index < 1) || (*Index > (int)Dir.Count()))
				throw ErrGDSFile("Child Index[%d], out of range 1..%d!",
					*Index, Dir.Count());
			*Node = Dir.ObjItem(*Index-1);
			Index++;
		}
		*err = false;
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*Node = NULL; *err = true;
	}
}

DLLEXPORT void gdsNodeIndexEx(CdGDSObj **Node, char **Name, int *Cnt,
	LongBool *err)
{
	try {
		for (int i=0; i < *Cnt; i++)
		{
			if (!dynamic_cast<CdGDSFolder*>(*Node))
				throw ErrGDSFmt(erNotFolder);
			CdGDSFolder &Dir = *static_cast<CdGDSFolder*>(*Node);
			*Node = Dir.ObjItem(RStr(*Name));
			Name++;
		}
		*err = false;
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*Node = NULL; *err = true;
	}
}

DLLEXPORT void gdsNodeObjDesp(CdGDSObj **Node, char **Desp, char **Name,
	int *SVType, int *DimCnt, int *DimEach,
	char **PackMode, double *PackRatio, char **Storage, LongBool *err)
{
	try {
		RStrAgn(UTF16toUTF8((*Node)->Name()).c_str(), Name);
		RStrAgn((*Node)->dName(), Desp);

		if (dynamic_cast<CdSequenceX*>(*Node))
		{
			CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);
			*SVType = Obj->SVType();
			*DimCnt = Obj->DimCnt(); Obj->GetDimLen(DimEach);

			if (Obj->PipeInfo())
			{
				RStrAgn(Obj->PipeInfo()->Coder(), PackMode);
				if (Obj->PipeInfo()->StreamTotalIn() > 0)
					*PackRatio = (double)Obj->PipeInfo()->StreamTotalOut() /
						Obj->PipeInfo()->StreamTotalIn();
				else
					*PackRatio = NaN;
			} else
				*PackRatio = NaN;

			if (Obj->StoreMode() == lmKeepInMem)
				RStrAgn("InMemory", Storage);
			else
				RStrAgn("InStream", Storage);

			{
				string s;
				if (dynamic_cast<CdVectorX*>(Obj))
				{
					CdVectorX *vObj = static_cast<CdVectorX*>(Obj);
					if (vObj->Allocator().Level == blTempFile)
					{
						CdTempStream *temp = dynamic_cast<CdTempStream*>(
							vObj->Allocator().Filter->Stream());
						if (temp)
							s = temp->FileName();
						else
							s = "";
					} else {
						switch (vObj->Allocator().Level) {
							case blChunkMemory:	s = "Chunk Memory"; break;
							case blTempFile:	s = "Linked Memory"; break;
							case blFilter:		s = "GDS File"; break;
							case blUnknown:		s = "Unknown"; break;
							default: s = "";
						}
					}
				}
				Storage++; RStrAgn(s.c_str(), Storage);
			}
		} else {
			*DimCnt = 0; *PackRatio = NaN;
			*SVType = svCustom;
		}
		*err = false;
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsAddNode(CdGDSObj **Node, char **NodeName, char **Storage,
	char **Compress, int *DimCnt, int *Dim, int *MaxLen, LongBool *err)
{
	CdSequenceX *vObj = NULL;
	try {
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

			CdObjClassMgr::TdOnObjCreate OnCreate = dObjMgr.NameToClass(nName);
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
			if ((vObj->DimCnt()>0) && (!vObj->PipeInfo()))
				vObj->SetDLen(0, *Dim);
        }

		*err = false;
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsDeleteNode(CdGDSObj **Node, LongBool *err)
{
	try {
		if ((*Node)->Folder())
		{
			(*Node)->Folder()->DeleteObj(*Node);
			*err = false;
		} else {
			Init.LastError = "Can not delete the root.";
			*err = true;
		}
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsRenameNode(CdGDSObj **Node, char **NewName, LongBool *err)
{
	try {
		(*Node)->SetName(*NewName);
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

// Attribute Operations

DLLEXPORT void gdsAttrCnt(CdGDSObj **Node, int *Cnt)
{
	try {
		*Cnt = (*Node)->Attribute().Count();
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
	}
}

DLLEXPORT void gdsAttrType(CdGDSObj **Node, int *PType)
{
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
}

DLLEXPORT void gdsGetAttr(CdGDSObj **Node, int *Index, int *RType, void *Ptr,
	char **Name, LongBool *err)
{
	try {
		RStrAgn(UTF16toUTF8((*Node)->Attribute().Names(*Index-1)).c_str(), Name);
		TdsData &p = (*Node)->Attribute()[*Index-1];
		switch (*RType) {
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
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsPutAttr(CdGDSObj **Node, char **Name, int *RType,
	void *Ptr, LongBool *err)
{
	try {
		TdsData *p;
		if ((*Node)->Attribute().HasName(*Name))
			p = &((*Node)->Attribute()[*Name]);
		else
			p = &((*Node)->Attribute().Add(*Name));
		switch (*RType) {
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
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsDeleteAttr(CdGDSObj **Node, char **Name, LongBool *err)
{
	try {
		(*Node)->Attribute().Delete(UTF8toUTF16(*Name));
		*err = false;
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

// Data Operations

inline static void ErrNode(CdGDSObj *Obj)
{
	Init.LastError = Obj ? "Not supported data type!" : "No dataset!";
}

inline static TSVType RtoSV(int RType)
{
	switch (RType) {
		case 1: return svInt32;
		case 2: return svFloat64;
		case 3: return svStrUTF8;
		case 4: return svInt32;
		default:
			throw ErrGDSFmt("Invalid RType %d", RType);
	}
}

DLLEXPORT void gdsObjAppend(CdGDSObj **Node, int *RType, void *Ptr,
	int *TotalCnt, LongBool *CntWarn, LongBool *err)
{
	if (*TotalCnt <= 0)
		{ *err = false; return; }

	try {
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
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsxObjDesp(CdGDSObj **Node, int *DimCnt, int *Start,
	int *Count, int *TotalCnt, int *RType, LongBool *err)
{
	try {
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
				*RType = -1;

			*err = false;

		} else {
			*err = true;
			ErrNode(*Node);
		}
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

static void rIterStr(TIterDataExt &Rec)
{
	char **p = (char**)Rec.pBuf;
	TdIterator it = Rec.Seq->Iterator(Rec.Index);
	for (int k=1; k <= Rec.LastDim; k++) {
		RStrAgn(UTF16toUTF8(it.toStr()).c_str(), p);
		p++; ++it;
	}
	Rec.pBuf = (char*)p;
}

DLLEXPORT void gdsObjReadData(CdGDSObj **Node, int *DimCnt, int *Start,
	int *Count, int *RType, void *Ptr, LongBool *err)
{
	try {
		if (dynamic_cast<CdSequenceX*>(*Node))
		{
			CdSequenceX::TSeqDimBuf DStart;
			CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);

			if (Obj->DimCnt() != *DimCnt)
				throw ErrGDSFmt("Invalid dimension!");
			CopyDec(Start, *DimCnt, DStart);

			TSVType SV = RtoSV(*RType);
			if (!COREARRAY_SVSTR(SV))
				Obj->rData(DStart, Count, Ptr, SV);
			else {
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
				} else
					RStrAgn(UTF16toUTF8(Obj->Iterator(NULL).toStr()).c_str(),
						static_cast<char**>(Ptr));
			}
			*err = false;
		} else {
			*err = true;
			ErrNode(*Node);
		}
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

static void wIterStr(TIterDataExt &Rec)
{
	char **p = (char**)Rec.pBuf;
	TdIterator it = Rec.Seq->Iterator(Rec.Index);
	for (int k=1; k <= Rec.LastDim; k++) {
		it.StrTo(PChartoUTF16(*p));
		p++; ++it;
	}
	Rec.pBuf = (char*)p;
}

DLLEXPORT void gdsObjWriteAll(CdGDSObj **Node, int *DimCnt, int *Dim,
	int *RType, void *Ptr, LongBool *err)
{
	try {
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
				throw ErrGDSFmt("New dimension should not be less than the currect.");
			else {
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
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsObjWriteData(CdGDSObj **Node, int *DimCnt, int *Start,
	int *Count, int *RType, void *Ptr, LongBool *err)
{
	try {
		if (dynamic_cast<CdSequenceX*>(*Node))
		{
			CdSequenceX::TSeqDimBuf DStart;
			CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);
			if (Obj->DimCnt() != *DimCnt)
				throw ErrGDSFmt("Invalid dimension!");
			CopyDec(Start, *DimCnt, DStart);

			TSVType SV = RtoSV(*RType);
			if (!COREARRAY_SVSTR(SV)) {
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
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsObjSetDim(CdGDSObj **Node, int *DimCnt, int *DLen,
	LongBool *err)
{
	try {
		if (dynamic_cast<CdSequenceX*>(*Node))
		{
			CdSequenceX *Obj = static_cast<CdSequenceX*>(*Node);
			if (*DimCnt < Obj->DimCnt())
				throw ErrGDSFmt("New dimension should not be less than the currect.");
			else {
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
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsObjCompress(CdGDSObj **Node, char **Compress, LongBool *err)
{
	try {
		if (dynamic_cast<CdContainer*>(*Node))
		{
			static_cast<CdContainer*>(*Node)->SetPackedMode(*Compress);
			*err = false;
		} else {
			*err = true;
			ErrNode(*Node);
		}
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsObjPackClose(CdGDSObj **Node, LongBool *err)
{
	try {
		if (dynamic_cast<CdContainer*>(*Node))
		{
			CdContainer *vObj = static_cast<CdContainer*>(*Node);
			vObj->CloseWriter();
			*err = false;
		} else {
			ErrNode(*Node);
			*err = true;
		}
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

DLLEXPORT void gdsLoadMode(CdGDSObj **Node, LongBool *InMem, LongBool *err)
{
	try {
		if (dynamic_cast<CdContainer*>(*Node))
		{
			CdContainer *vObj = static_cast<CdContainer*>(*Node);
			vObj->SetLoadMode(InMem ? lmKeepInMem : lmKeepInStream);
			*err = false;
		} else
			ErrNode(*Node);
	}
	catch (exception &E)
	{
		Init.LastError = E.what();
		*err = true;
	}
}

// Error function
DLLEXPORT void gdsLastErrGDS(char **Msg)
{
	RStrAgn(Init.LastError.c_str(), Msg);
}

} // extern "C"

