// ===========================================================
//     _/_/_/   _/_/_/  _/_/_/_/    _/_/_/_/  _/_/_/   _/_/_/
//      _/    _/       _/             _/    _/    _/   _/   _/
//     _/    _/       _/_/_/_/       _/    _/    _/   _/_/_/
//    _/    _/       _/             _/    _/    _/   _/
// _/_/_/   _/_/_/  _/_/_/_/_/     _/     _/_/_/   _/_/
// ===========================================================
//
// dApp.cpp: CoreArray console application
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

#include <dBase.hpp>
#include <dApp.hpp>
#include <algorithm>
#include <typeinfo>
#include <fstream>

#ifdef __BORLANDC__
#pragma hdrstop
#endif


using namespace std;
using namespace CoreArray;

static const char *rsDuplicateSF = "Duplicate option (short)'%s'!";
static const char *rsDuplicateLF = "Duplicate option (long)'%s'!";
static const char *rsOptionValue = "Do not support option values for ('%c%s', or '%c%c%s').";

static const char *HelpOptShort = "h";
static const char *HelpOptLong = "help";
static const char *ParOptShort = "p";
static const char *ParOptLong = "par";
static const char *ThreadOptShort = "th";
static const char *ThreadOptLong = "thread";


// CdParams

CdParams::CdParams(int argc, char* argv[], bool vHasParOpt)
{
	fOptionChar = '-';
	fHasParOption = vHasParOpt;
	Argc = argc; Argv = argv;
	if (vHasParOpt)
		AddParams(ParOptShort, ParOptLong, slNext);
}

CdParams::~CdParams() {}

CdParams::TOption *CdParams::HasOptionEx(const char *ShortFmt, const char *LongFmt)
{
	int opt = -1;
	if (ShortFmt && *ShortFmt) opt = sOption(ShortFmt);
	if (LongFmt && *LongFmt)
	{
		int i = lOption(LongFmt);
		if (opt < 0) opt = i;
		if (opt != i)
			throw EdConsoleError("%c%s does not match %c%c%s",
				fOptionChar, ShortFmt, fOptionChar, fOptionChar, LongFmt);
	}
	if (opt < 0)
		throw EdConsoleError("No such option: %c%s ", fOptionChar, ShortFmt);

	vector<TOption>::iterator it;
	for (it=fOption.begin(); it != fOption.end(); it++)
	{
		if (it->OptIndex == opt)
        	return &(*it);
	}
	return NULL;
}

bool CdParams::HasOption(const char *ShortFmt, const char *LongFmt, string &Content)
{
	int opt = -1;
	if (ShortFmt && *ShortFmt) opt = sOption(ShortFmt);
	if (LongFmt && *LongFmt)
	{
		int i = lOption(LongFmt);
		if (opt < 0) opt = i;
		if (opt != i)
			throw EdConsoleError("%c%s does not match %c%c%s",
				fOptionChar, ShortFmt, fOptionChar, fOptionChar, LongFmt);
	}
	if (opt < 0)
		throw EdConsoleError("No such option: %c%s ", fOptionChar, ShortFmt);

	vector<TOption>::iterator it;
	for (it=fOption.begin(); it != fOption.end(); it++)
	{
		if (it->OptIndex == opt)
		{
			Content = it->Txt1;
			return true;
		}
	}
	return false;
}

bool CdParams::HasOption(const char *ShortFmt, const char *LongFmt)
{
	string Text;
	bool rv = HasOption(ShortFmt, LongFmt, Text);
	if (rv && !Text.empty())
	{
		throw EdConsoleError(rsOptionValue, fOptionChar, ShortFmt,
			fOptionChar, fOptionChar, LongFmt);
	}
	return rv;
}

CdParams &CdParams::AddParams(const char *ShortFmt, const char *LongFmt, TSLType sltype)
{
	if (!ShortFmt) ShortFmt = "";
	if (!LongFmt) LongFmt = "";

	vector<TSLParams>::iterator it;
	for (it=fParams.begin(); it != fParams.end(); it++)
	{
		if (*ShortFmt && strcmp(ShortFmt, it->ShortFmt)==0)
			throw EdConsoleError(rsDuplicateSF, ShortFmt);
		if (*LongFmt && strcmp(LongFmt, it->LongFmt)==0)
			throw EdConsoleError(rsDuplicateLF, LongFmt);
    }
	fParams.push_back(TSLParams(ShortFmt, LongFmt, sltype));
	return *this;
}

inline static string OptionSplit(string &Text)
{
	if (Text.empty())
		throw EdConsoleError("Invalid header of option!");

	size_t i = Text.find('=');
	if (i != string::npos)
	{
		if (i == 0)
			throw EdConsoleError("Invalid header of option!");
		string rv = Text.substr(0, i);
		Text = Text.substr(i+1, string::npos);
		return rv;
	} else {
		string rv(Text);
		Text.clear();
		return rv;
	}
}

void CdParams::ParamInput()
{
	char** argv = Argv+1;
	vector<string> vs;

	fOption.clear(); fContent.clear();
	for (int i=1; i < Argc; i++, argv++)
		vs.push_back(string(*argv));
	xParamInput(vs);

	string fn, s;
	if (fHasParOption && HasOption(ParOptShort, ParOptLong, fn))
	{
		ifstream infile;
		infile.open(fn.c_str(), ifstream::in);
		vs.clear();
		while (infile >> s)
		{
			if (!s.empty() && s[0]!='#')
			{
				size_t i = 0;
				while (i != string::npos)
				{
					size_t j = s.find('\t', i);
					if (j != string::npos)
					{
						if (j > i)
							vs.push_back(s.substr(i, j-i));
						i = j + 1;
						if (i >= s.size()) break;;
					} else {
						j = s.size() - i;
						if (j > 0) vs.push_back(s.substr(i, j));
						break;
					}
				}
			}
		}
		infile.close();
		xParamInput(vs);
	}
}

void CdParams::xParamInput(vector<string> &vs)
{
	int OptIndex;
	vector<string>::iterator it;

	for (it = vs.begin(); it != vs.end(); it++)
	{
		string s(*it), opt;
		if (!s.empty())
		{
			if (s[0] == fOptionChar)
			{
				s.erase(0, 1);
				if (s[0] == fOptionChar)
				{ // long option
					s.erase(0, 1);
					opt = OptionSplit(s);
					OptIndex = lOption(opt.c_str());
				} else {
					opt = OptionSplit(s);
					OptIndex = sOption(opt.c_str());
				}
				if (OptIndex >= 0)
				{
					fOption.push_back(TOption(OptIndex));
					switch (fParams[OptIndex].SLType)
					{
					case slNone:
						if (!s.empty())
							throw EdConsoleError("Invalid usage '%s'", opt.c_str());
						break;
					case slNest:
                        fOption.back().Txt1 = s;
						break;
					case slNext:
						it++;
						if (!s.empty() || it==vs.end())
							throw EdConsoleError("Invalid usage '%s'", opt.c_str());
                        fOption.back().Txt1 = *it;
						break;
					case slNext2:
						it++;
						if (!s.empty() || it==vs.end())
							throw EdConsoleError("Invalid usage '%s'", opt.c_str());
                        fOption.back().Txt1 = *it;
						if ((++it)==vs.end())
							throw EdConsoleError("Invalid usage '%s'", opt.c_str());
                        fOption.back().Txt2 = *it;
						break;
					case slNext3:
						it++;
						if (!s.empty() || it==vs.end())
							throw EdConsoleError("Invalid usage '%s'", opt.c_str());
                        fOption.back().Txt1 = *it;
						if ((++it)==vs.end())
							throw EdConsoleError("Invalid usage '%s'", opt.c_str());
                        fOption.back().Txt2 = *it;
						if ((++it)==vs.end())
							throw EdConsoleError("Invalid usage '%s'", opt.c_str());
                        fOption.back().Txt3 = *it;
						break;
					}
				} else
					throw EdConsoleError("Invalid option '%s'", opt.c_str());
			} else
				fContent.push_back(s);
		}
	}
}

int CdParams::sOption(const char *opt)
{
	for (int i=0; i < (int)fParams.size(); i++)
		if (strcmp(opt, fParams[i].ShortFmt)==0)
        	return i;
	return -1;
}

int CdParams::lOption(const char *opt)
{
	for (int i=0; i < (int)fParams.size(); i++)
		if (strcmp(opt, fParams[i].LongFmt)==0)
        	return i;
	return -1;
}


// CConsoleApplication

CConsoleApplication::CConsoleApplication(int argc, char* argv[],
	bool vHasParOpt): CdParams(argc, argv, vHasParOpt)
{
	AddParams(HelpOptShort, HelpOptLong, slNone);
}

CConsoleApplication::~CConsoleApplication() {}

void CConsoleApplication::Run()
{
	try {
		ParamInput();
	} catch (exception &E) {
		printf("\n%s\n", E.what()); return;
	}
	try {
		DoRun();
	}
	catch (exception &E)
	{
		printf("\n%s\n", E.what());
	}
	catch (const char *E)
	{
		printf("\n%s\n", E);
	}
}

string CConsoleApplication::ExeName()
{
	return Argv[0];
}

string CConsoleApplication::Location()
{
	return "";
}

bool CConsoleApplication::DoRun()
{
	string Text;
	if (HasOption(HelpOptShort, HelpOptLong, Text))
	{
		ShowHelp(Text.c_str());
		return false;
	} else
		return true;
}

void CConsoleApplication::ShowHelp(const char *Text) {}


// CConsoleThreadApp

CConsoleThreadApp::CConsoleThreadApp(int argc, char* argv[], bool vHasParOpt):
	CConsoleApplication(argc, argv, vHasParOpt)
{
	fNumThread = 1;
	AddParams(ThreadOptShort, ThreadOptLong, slNest);
}

CConsoleThreadApp::~CConsoleThreadApp() {}

bool CConsoleThreadApp::DoRun()
{
	if (CConsoleApplication::DoRun())
	{
		string n;
		if (HasOption(ThreadOptShort, ThreadOptLong, n))
		{
			int nThread = StrToInt(n.c_str());
			if (nThread <= 0)
				throw EdConsoleError("Invalid # of threads: %d", nThread);
			fNumThread = nThread;
		} else
			fNumThread = 1;

		DoMainRun();
		return true;
	} else
    	return false;
}


