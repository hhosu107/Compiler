//------------------------------------------------------------------------------
/// @brief SnuPL/0 scanner
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/09/10 Bernhard Egger assignment 1: scans SnuPL/-1
/// 2016/03/13 Bernhard Egger assignment 1: adapted to modified SnuPL/-1 syntax
///
/// @section license_section License
/// Copyright (c) 2012-2016, Bernhard Egger
/// All rights reserved.
///
/// Redistribution and use in source and binary forms,  with or without modifi-
/// cation, are permitted provided that the following conditions are met:
///
/// - Redistributions of source code must retain the above copyright notice,
///   this list of conditions and the following disclaimer.
/// - Redistributions in binary form must reproduce the above copyright notice,
///   this list of conditions and the following disclaimer in the documentation
///   and/or other materials provided with the distribution.
///
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
/// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
/// IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
/// ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER  OR CONTRIBUTORS BE
/// LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSE-
/// QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF  SUBSTITUTE
/// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
/// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN  CONTRACT, STRICT
/// LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY
/// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
/// DAMAGE.
//------------------------------------------------------------------------------

#include <iostream>
#include <sstream>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <cstdio>

#include "scanner.h"
using namespace std;

//------------------------------------------------------------------------------
// token names
//
#define TOKEN_STRLEN 16

//------------------------------------------------------------------------------
// SnuPL/1 token types
//
char ETokenName[][TOKEN_STRLEN] = {
  "tNumber",                        ///< an integer
  "tIdent",                         ///< an identifier
  "tCharacter",                     ///< a character. ASCIIchar, "\n", "\t"
  "tString",                        ///< a string such as "hello".
  "tBool",                          ///< true value or false value

  "tPlusMinus",                     ///< '+' or '-'
  "tMulDiv",                        ///< '*' or '/'
  "tRelOp",                         ///< "=", "#", "<", "<=", ">", or ">="
  "tAndOr",                         ///< "&&"
  "tNot",                           ///< '!' before boolean value
  "tAssign",                        ///< ":="
  "tSemicolon",                     ///< a semicolon
  "tColon",                         ///< a colon
  "tComma",                         ///< a comma
  "tDot",                           ///< a dot which represents EOF
  "tLParen",                         ///< a left bracket
  "tRParen",                         ///< a right bracket
  "tLBracket",                        ///< a left large bracket '['
  "tRBracket",                        ///< a right large bracket ']'

  "tModule",                        ///< "module"
  "tBegin",                         ///< "begin"
  "tEnd",                           ///< "end"
  "tIf",                            ///< "if"
  "tThen",                          ///< "then"
  "tElse",                          ///< "else"
  "tWhile",                         ///< "while"
  "tDo",                            ///< "do"
  "tReturn",                        ///< "return"
  "tVar",                           ///< "var"
  "tProcedure",                     ///< "procedure"
  "tFunction",                      ///< "function"
  "tBoolean",                       ///< "boolean"
  "tChar",                          ///< "char"
  "tInteger",                       ///< "integer"
  "tComment",                       ///< multi single lines starting from "//"

  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tUndefined",                     ///< undefined
};


//------------------------------------------------------------------------------
// format strings used for printing tokens
//

char ETokenStr[][TOKEN_STRLEN] = {
  "tNumber (%s)",                   ///< an integer
  "tIdent (%s)",                    ///< an identifier
  "tCharacter (%s)",                ///< a character. ASCIIchar, "\n", "\t"
  "tString (%s)",                   ///< a string such as "hello".
  "tBool (%s)",                     ///< true value or false value

  "tPlusMinus (%s)",                ///< '+' or '-'
  "tMulDiv (%s)",                   ///< '*' or '/'
  "tRelOp (%s)",                    ///< relational operator
  "tAndOr (%s)",                    ///< "&&", "||"
  "tNot",                           ///< '!' before boolean value
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tColon",                         ///< a colon
  "tComma",                         ///< a comma
  "tDot",                           ///< a dot which represents EOF
  "tLParen",                         ///< a left bracket
  "tRParen",                         ///< a right bracket
  "tLBracket",                        ///< a left large bracket '['
  "tRBracket",                        ///< a right large bracket ']'

  "tModule",                        ///< "module"
  "tBegin",                         ///< "begin"
  "tEnd",                           ///< "end"
  "tIf",                            ///< "if"
  "tThen",                          ///< "then"
  "tElse",                          ///< "else"
  "tWhile",                         ///< "while"
  "tDo",                            ///< "do"
  "tReturn",                        ///< "return"
  "tVar",                           ///< "var"
  "tProcedure",                     ///< "procedure"
  "tFunction",                      ///< "function"
  "tBoolean",                       ///< "boolean"
  "tChar",                          ///< "char"
  "tInteger",                       ///< "integer"
  "tComment (%s)",                  ///< multi single lines starting from "//"

  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tUndefined (%s)",                ///< undefined
};


//------------------------------------------------------------------------------
// reserved keywords
//
pair<const char*, EToken> Keywords[] =
{
  {"module", tModule}, {"begin", tBegin}, {"end", tEnd}, {"if", tIf},
  {"then", tThen}, {"else", tElse}, {"while", tWhile}, {"do", tDo},
  {"return", tReturn}, {"var", tVar}, {"procedure", tProcedure},
  {"function", tFunction}, {"begin", tBegin}, {"boolean", tBoolean},
  {"char", tChar}, {"integer", tInteger}, {"true", tBool}, {"false", tBool}
};



//------------------------------------------------------------------------------
// CToken
//
CToken::CToken()
{
  _type = tUndefined;
  _value = "";
  _line = _char = 0;
}

CToken::CToken(int line, int charpos, EToken type, const string value)
{
  _type = type;
  _value = escape(value);
  _line = line;
  _char = charpos;
}

CToken::CToken(const CToken &token)
{
  _type = token.GetType();
  _value = token.GetValue();
  _line = token.GetLineNumber();
  _char = token.GetCharPosition();
}

CToken::CToken(const CToken *token)
{
  _type = token->GetType();
  _value = token->GetValue();
  _line = token->GetLineNumber();
  _char = token->GetCharPosition();
}

const string CToken::Name(EToken type)
{
  return string(ETokenName[type]);
}

const string CToken::GetName(void) const
{
  return string(ETokenName[GetType()]);
}

ostream& CToken::print(ostream &out) const
{
  int str_len = _value.length();
  str_len = TOKEN_STRLEN + (str_len < 64 ? str_len : 64);
  char *str = (char*)malloc(str_len);
  snprintf(str, str_len, ETokenStr[GetType()], _value.c_str());
  out << dec << _line << ":" << _char << ": " << str;
  free(str);
  return out;
}

string CToken::escape(const string text)
{
  string s;

  for(char c : text){ /// used c++0x scheme.
    switch(c){
      case '\n': s += "\\n"; break;
      case '\t': s += "\\t"; break;
      case '\0': s += "\\0"; break;
      case '\'': s += "\\'"; break;
      case '\"': s += "\\\""; break;
      case '\\': s += "\\\\"; break;
      default: s += c;
    }
  }

  return s;
}

ostream& operator<<(ostream &out, const CToken &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CToken *t)
{
  return t->print(out);
}


//------------------------------------------------------------------------------
// CScanner
//
map<string, EToken> CScanner::keywords;

CScanner::CScanner(istream *in)
{
  InitKeywords();
  _in = in;
  _delete_in = false;
  _line = _char = 1;
  _token = NULL;
  _good = in->good();
  NextToken();
}

CScanner::CScanner(string in)
{
  InitKeywords();
  _in = new istringstream(in);
  _delete_in = true;
  _line = _char = 1;
  _token = NULL;
  _good = true;
  NextToken();
}

CScanner::~CScanner()
{
  if (_token != NULL) delete _token;
  if (_delete_in) delete _in;
}

void CScanner::InitKeywords(void)
{
  if (keywords.size() == 0) {
    int size = sizeof(Keywords) / sizeof(Keywords[0]);
    for (int i=0; i<size; i++) {
      keywords[Keywords[i].first] = Keywords[i].second;
    }
  }
}

CToken CScanner::Get()
{
  CToken result(_token);

  EToken type = _token->GetType();
  _good = !(type == tIOError);

  NextToken();
  return result;
}

CToken CScanner::Peek() const
{
  return CToken(_token);
}

void CScanner::NextToken()
{
  if (_token != NULL) delete _token;

  _token = Scan();
}

void CScanner::RecordStreamPosition()
{
  _saved_line = _line;
  _saved_char = _char;
}

void CScanner::GetRecordedStreamPosition(int *lineno, int *charpos)
{
  *lineno = _saved_line;
  *charpos = _saved_char;
}

CToken* CScanner::NewToken(EToken type, const string token)
{
  return new CToken(_saved_line, _saved_char, type, token);
}

CToken* CScanner::Scan()
{
  EToken token;
  string tokval;
  char c;

  while (_in->good() && IsWhite(_in->peek())) GetChar();

  RecordStreamPosition();

  if (_in->eof()) return NewToken(tEOF);
  if (!_in->good()) return NewToken(tIOError);

  c = GetChar();
  tokval = c;
  token = tUndefined;

  switch (c) {

    case '+':
    case '-':
      token = tPlusMinus;
      break;

    case '*':
      token = tMulDiv;
      break;

    case '/':
      token = tMulDiv;

      if(_in->peek() == '/'){ /// comment!
        token = tComment;

        for(;;){
          char t = _in->peek();
          if(t == '\n') break;
          tokval += GetChar();
        }
      }
      break;

    case '&':
    case '|':
      if(c == _in->peek()){
        token = tAndOr;
        tokval += GetChar();
      }
      else{
        token = tUndefined;
      }
      break;

    case '!':
      token = tNot;
      break;

    case '=':
    case '#':
      token = tRelOp;
      break;

    case '<':
      if(_in->peek() == '=') {
        tokval += GetChar();
        token = tRelOp;
      }
      else {
        token = tRelOp;
      }
      break;

    case '>':
      if(_in->peek() == '=') {
        tokval += GetChar();
        token = tRelOp;
      }
      else {
        token = tRelOp;
      }
      break;

    case ':':
      if (_in->peek() == '=') {
        tokval += GetChar();
        token = tAssign;
      }
      else{
        token = tColon;
      }
      break;

    case ';':
      token = tSemicolon;
      break;

    case '.':
      token = tDot;
      break;

    case ',':
      token = tComma;
      break;

    case '(':
      token = tLParen;
      break;

    case ')':
      token = tRParen;
      break;

    case '[':
      token = tLBracket;
      break;

    case ']':
      token = tRBracket;
      break;

    case '\'':
      {
        tokval = "";

        bool valid = true;
        int len = 0;

        for(;;){
          if(_in->good()){
            char t = _in->peek();
            if(t == EOF){ /// pick EOF not to put EOF in character
              token = tUndefined;
              break;
            }
            else t = GetChar();
            if(t == '\\'){ /// escape
              if(_in->good()){ /// check w.o.n next input stream is good
                t = _in->peek();
                switch(t){
                  case 'n':
                    GetChar(); t = '\n'; break;
                  case 't':
                    GetChar(); t = '\t'; break;
                  case '\'':
                    GetChar(); t = '\''; break;
                  case '\"':
                    GetChar(); t = '\"'; break;
                  case '\\':
                    GetChar(); t = '\\'; break;
                  case '0':
                    GetChar(); t = '\0'; break;
                  case EOF: /// pick EOF not to put EOF in character
                    valid = false; break;
                  default: // invalid
                    GetChar(); valid = false;
                    break;
                }
              }
              else{
                token = tUndefined; valid = false;
                break;
              }
            }
            else if(t == '\''){ // close quote
              if(len == 1 && valid){
                token = tCharacter;
              }
              else token = tUndefined;
              break;
            }
            else if(!IsAscii(t)){
              valid = false;
            }
            tokval += t;
            len++;
          }
          else{
            token = tUndefined;
            break;
          }
        }
        break;
      }

    case '\"':
      {
        tokval = "";

        bool valid = true;

        for(;;){
          if(_in->good()){
            char t = _in->peek();
            if(t == EOF){ /// pick EOF not to put EOF in string
              token = tUndefined;
              break;
            }
            else t = GetChar();
            if(t == '\\'){ // escape
              if(_in->good()){
                t = _in->peek();
                switch(t){
                  case 'n':
                    GetChar(); t = '\n'; break;
                  case 't':
                    GetChar(); t = '\t'; break;
                  case '\'':
                    GetChar(); t = '\''; break;
                  case '\"':
                    GetChar(); t = '\"'; break;
                  case '\\':
                    GetChar(); t = '\\'; break;
                  case '0':
                    GetChar(); t = '\0'; break;
                  case EOF: /// pick EOF not to put EOF in string
                    valid = false; break;
                  default: // invalid
                    GetChar(); valid = false;
                    break;
                }
              }
              else{
                token = tUndefined; valid = false;
                break;
              }
            }
            else if(t == '\"'){ // close quote
              if(valid){
                token = tString;
              }
              else token = tUndefined;
              break;
            }
            else if(!IsAscii(t)){
              valid = false;
            }
            tokval += t;
          }
          else{
            token = tUndefined;
            break;
          }
        }
        break;
      }

    default:
      if(IsDigit(c)){
        token = tNumber;

        char t = _in->peek();
        while(IsDigit(t)){
          tokval += GetChar();
          t = _in->peek();
        }
        break;
      }

      else if(IsLetter(c)){
        token = tIdent;

        char t = _in->peek();
        while (IsLetter(t) || IsDigit(t)){
          tokval += GetChar();
          t = _in->peek();
        }

        if(keywords.find(tokval) != keywords.end()) {
          token = keywords.find(tokval)->second;
        }
        else {
          token = tIdent;
          keywords[tokval] = token;
        }
        break;
      }

      else {
        tokval = "invalid character ("; // To make consistency, changed ''' to '('
        tokval += c;
        tokval += ")";
      }
      break;
  }

  return NewToken(token, tokval);
}

char CScanner::GetChar()
{
  char c = _in->get();
  if (c == '\n') { _line++; _char = 1; } else _char++;
  return c;
}

string CScanner::GetChar(int n)
{
  string str;
  for (int i=0; i<n; i++) str += GetChar();
  return str;
}

bool CScanner::IsDigit(char c) const {
  return (c >= '0' && c <= '9');
}

bool CScanner::IsLetter(char c) const {
  return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c == '_');
}

bool CScanner::IsAscii(char c) const{
  return ((0x00 <= c) && (c <= 0x7F));
}

bool CScanner::IsWhite(char c) const
{
  return ((c == ' ') || (c == '\n') || (c == '\t'));
}
