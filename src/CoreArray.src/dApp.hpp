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

/**
 *	\file     dApp.hpp
 *	\author   Xiuwen Zheng
 *	\version  1.0
 *	\date     2007 - 2011
 *	\brief    CoreArray console application
 *	\details
*/


#ifndef _dApp_H_
#define _dApp_H_

#include <dPlatform.hpp>
#include <dParallel.hpp>
#include <iostream>
#include <string>
#include <vector>

namespace CoreArray
{
	enum TSLType { slNone, slNest, slNext, slNext2, slNext3 };

	class CdParams
	{
	public:
		struct TOption
		{
			int OptIndex;
			std::string Txt1, Txt2, Txt3;
			TOption(int opt=0) { OptIndex = opt; };
		};
		struct TSLParams
		{
			const char *ShortFmt, *LongFmt;
			TSLType SLType;

			TSLParams(const char *sf, const char *lf, TSLType sl)
				{ ShortFmt=sf; LongFmt=lf; SLType=sl; };
		};

		CdParams(int argc, char* argv[], bool vHasParOpt=false);
		virtual ~CdParams();

		CdParams &AddParams(const char *ShortFmt, const char *LongFmt, TSLType sltype);

		bool HasOption(const char *ShortFmt, const char *LongFmt, std::string &Content);
		bool HasOption(const char *ShortFmt, const char *LongFmt);
		TOption *HasOptionEx(const char *ShortFmt, const char *LongFmt);

		inline std::vector<std::string> &Content() { return fContent; };
		inline std::vector<TSLParams> &Params() { return fParams; };
	protected:
		int Argc;
		char** Argv;
		std::vector<TSLParams> fParams;
		std::vector<TOption> fOption;
		std::vector<std::string> fContent;
		char fOptionChar; // by default '-'

		virtual void ParamInput();
		int sOption(const char *opt);
		int lOption(const char *opt);
		void xParamInput(std::vector<std::string> &vs);
		inline bool HasParOption() const { return fHasParOption; };
	private:
		bool fHasParOption; // by default, false
	};

	class CConsoleApplication: public CdParams
	{
	public:
		CConsoleApplication(int argc, char* argv[], bool vHasParOpt=false);
		virtual ~CConsoleApplication();

		void Run();
		std::string ExeName();
		std::string Location();

	protected:
		virtual bool DoRun();
		virtual void ShowHelp(const char *Text);
	};


	class CConsoleThreadApp: public CConsoleApplication
	{
	public:
		CConsoleThreadApp(int argc, char* argv[], bool vHasParOpt=false);
		virtual ~CConsoleThreadApp();

        inline int NumThread() const { return fNumThread; }
	protected:
		int fNumThread;

		virtual bool DoRun();
		virtual void DoMainRun() = 0;
	};



	class EdConsoleError: public ErrCoreArray
	{
	public:
		EdConsoleError() {};
		EdConsoleError(const char *fmt, ...) { _COREARRAY_ERRMACRO_(fmt); }
		EdConsoleError(const std::string &msg) { fMessage = msg; }
	};

}

#endif  /* _dApp_H_ */

