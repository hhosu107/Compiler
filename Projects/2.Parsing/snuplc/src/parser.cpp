//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/11/04 Bernhard Egger maintain unary '+' signs in the AST
/// 2016/04/01 Bernhard Egger adapted to SnuPL/1 (this is not a joke)
/// 2016/09/28 Bernhard Egger assignment 2: parser for SnuPL/-1
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

#include <limits.h>
#include <cassert>
#include <errno.h>
#include <cstdlib>
#include <vector>
#include <iostream>
#include <exception>

#include "parser.h"
using namespace std;


//------------------------------------------------------------------------------
// CParser
//
CParser::CParser(CScanner *scanner)
{
  _scanner = scanner;
  _module = NULL;
}

CAstNode* CParser::Parse(void)
{
  _abort = false;

  if (_module != NULL) { delete _module; _module = NULL; }

  try {
    if (_scanner != NULL) _module = module();

    if (_module != NULL) {
      CToken t;
      string msg;
      //if (!_module->TypeCheck(&t, &msg)) SetError(t, msg);
    }
  } catch (...) {
    _module = NULL;
  }

  return _module;
}

const CToken* CParser::GetErrorToken(void) const
{
  if (_abort) return &_error_token;
  else return NULL;
}

string CParser::GetErrorMessage(void) const
{
  if (_abort) return _message;
  else return "";
}

void CParser::SetError(CToken t, const string message)
{
  _error_token = t;
  _message = message;
  _abort = true;
  throw message;
}

bool CParser::Consume(EToken type, CToken *token)
{
  if (_abort) return false;

  CToken t = _scanner->Get();

  if (t.GetType() != type) {
    SetError(t, "expected '" + CToken::Name(type) + "', got '" +
             t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  return t.GetType() == type;
}

void CParser::InitSymbolTable(CSymtab *s)
{
  CTypeManager *tm = CTypeManager::Get();
  // below three lines are wrong. Edit it.
  //s->AddSymbol("DIM", stProcedure, /* how to put two parameters? */ ... );
  //s->AddSymbol("DOFS", stProcedure, /* ptr to array */ ...);
  //s->AddSymbol("ReadInt", stProcedure,
  // TODO: add predefined functions here
}

CAstModule* CParser::module(void)
{
  //
  // module ::= "module" ident ";" varDeclaration { subroutineDecl }
  //            "begin" statSequence "end" ident ".".
  //
  // TODO: Fill areas which are still just commented
  //
  CToken dummy;

  Consume(tModule);
  CToken id; // prepare ident token: either hard coded or function
  CSymtab *symbol;
  InitSymbolTable(symbol);
  CAstModule *m;
  /* check it is ident or not
  EToken tt = _scanner->Peek().GetType();
  if(tt != tIdent)
  {
    SetError(_scanner->Peek(), "module name expected");
    m = new CAstModule(_scanner->Peek(), "");
    return m;
  }
  else
  {
    id = _scanner->Get();
    m = new CAstModule(dummy, id.GetValue());
  } */ // Replace by ident recognizer with empty scope
  Consume(tSemicolon);
  // varDeclaration method -> update the symbol table of module
  /* do{
   * ... match with subroutineDecl
   * }while(!_abort); -> symbol table update
   */
  Consume(tBegin);

  CAstStatement *statseq = NULL;

  statseq = statSequence(m);

  m->SetStatementSequence(statseq);

  Consume(tEnd);

  CToken idEnd; // To match with id
  Consume(tIdent, &idEnd);

  if((id->GetValue()).compare(idEnd->GetValue()) != 0){
    SetError(idEnd, "module name is not equal to the original");
    delete m;
    m = net CAstModule(idEnd, "");
  }

  Consume(tDot);
  return m;
}

CAstStatement* CParser::statSequence(CAstScope *s)
{
  //
  // statSequence ::= [ statement { ";" statement } ].
  // statement ::= assignment | subroutineCall | ifStatement |
  //                whileStatement | returnStatement.
  // FIRST(statSequence) = { tIdent, tIf, tWhile, tReturn }
  // FOLLOW(statSequence) = { tElse, tEnd }
  //
  CAstStatement *head = NULL;

  EToken tt = _scanner->Peek().GetType();
  CSymtab *tbl = s->GetSymbolTable();
  if (!(tt == tElse || tt == tEnd)) {
    CAstStatement *tail = NULL;

    do {
      CToken t;
      CAstStatement *st = NULL;

      switch (tt) {
        // statement ::= assignment / subroutineCall
        case tIdent:
          if(tbl->FindSymbol(_scanner->Peek().GetValue())->GetSymbolType() == stProcedure)
            // Since a procedure has to be declared before it is called,
            // this statement is valid.
          {
            st = statementSubroutineCall(s);
          }
          else
          {
            st = assignment(s);
          }
          break;

        case tIf:
          st = ifStatement(s); // Make this function
          break;

        case tWhile:
          st = whileStatement(s); // Make this function
          break;

        case tReturn:
          st = returnStatement(s); // Make this function
          break;

        default:
          SetError(_scanner->Peek(), "statement expected.");
          break;
      }

      assert(st != NULL);
      if (head == NULL) head = st;
      else tail->SetNext(st);
      tail = st;

      tt = _scanner->Peek().GetType();
      if (tt != tSemicolon) break;

      Consume(tSemicolon);
    } while (!_abort);
  }

  return head;
}

CAstStatAssign* CParser::assignment(CAstScope *s)
{
  //
  // assignment ::= qualident ":=" expression.
  //
  // TODO: Have to edit more for the qualident
  //
  CToken t;

  CAstDesignator *lhs;
  lhs = qualident(s);
  Consume(tAssign);

  CAstExpression *rhs = expression(s);

  return new CAstStatAssign(t, lhs, rhs);
}

CAstExpression* CParser::expression(CAstScope* s)
{
  //
  // expression ::= simpleexpr [ relOp simpleexpr ].
  //
  CToken t;
  EOperation relop;
  CAstExpression *left = NULL, *right = NULL;

  left = simpleexpr(s);

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=")       relop = opEqual;
    else if (t.GetValue() == "#")  relop = opNotEqual;
    else if (t.GetValue() == "<")  relop = opLessThan;
    else if (t.GetValue() == ">")  relop = opBiggerThan;
    else if (t.GetValue() == "<=") relop = opLessEqual;
    else if (t.GetValue() == ">=") relop = opBiggerEqual;
    else SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

CAstExpression* CParser::simpleexpr(CAstScope *s)
{
  //
  // simpleexpr ::= ["+" | "-"] term { termOp term }.
  //
  CAstExpression *n = NULL;
  EToken tt = _scanner->Peek().GetType();
  if(tt == tPlusMinus)
  {
    CToken t;
    CAstExpression *r = NULL;
    Consume(tPlusMinus, &t);
    r = term(s);
    n = new CAstUnaryOp(t, t.GetValue() == "+" ? opPos : opNeg, r);
  }
  else
  {
    n = term(s);
  }

  tt = _scanner->Peek().GetType();
  while (tt == tPlusMinus || tt == tOr) {
    CToken t;
    CAstExpression *l = n, *r = NULL;
    switch(tt){
      case tPlusMinus:
        Consume(tPlusMinus, &t);
        r = term(s);
        n = new CAstBinaryOp(t, t.GetValue() == "+" ? opAdd : opSub, l, r);
        break;

      case tAndOr:
        // Have to check whether or not it is "&&" or "||"
        Consume(tAndOr, &t);
        r = term(s);
        n = new CAstBinaryOp(t, opOr, l, r);
        break;
    }
    tt = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression* CParser::term(CAstScope *s)
{
  //
  // term ::= factor { ("*" | "/" | "&&") factor }.
  //
  CAstExpression *n = NULL;

  n = factor(s);

  EToken tt = _scanner->Peek().GetType();

  while ((tt == tMulDiv) || (tt == tAnd)) {
    CToken t;
    CAstExpression *l = n, *r = NULL;

    switch(tt){
      case tMulDiv:
        Consume(tMulDiv, &t);
        r = factor(s);
        n = new CAstBinaryOp(t, t.GetValue() == "*" ? opMul : opDiv, l, r);
        break;

      case tAndOr:
        // Have to check whether or not it is "&&" or "||"
        Consume(tAndOr, &t);
        r = factor(s);
        n = new CAstBinaryOp(t, opAnd, l, r);
        break;
    }
    tt = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression* CParser::factor(CAstScope *s)
{
  //
  // factor ::= number | "(" expression ")" | boolean |
  //            character | string | "!" factor |
  //            qualident | subroutineCall
  //            (these two requires lookahead 2. Also thissubroutineCall calls
  //            another function which differes to statementSubroutineCall.
  //
  // FIRST(factor) = { tNumber, tBool, tCharacter, tString, tIdent, tLParen, tNot }
  //

  // TODO: Not Edited yet

  CToken t;
  EToken tt = _scanner->Peek().GetType();
  CAstExpression *unary = NULL, *n = NULL;

  switch (tt) {
    // factor ::= number
    case tNumber:
      n = number();
      break;

    // factor ::= "(" expression ")"
    case tLParen:
      Consume(tLParen);
      n = expression(s);
      Consume(tRParen);
      break;

    case tBool:
      n = boolean();
      break;

    case tChar:
      n = character();
      break;

    case tNot:
      Consume(tNot);
      CAstExpression *r = NULL;
      r = factor(s);
      n = new CAstUnaryOp(tNot, opNot, r);
      break;

    case tIdent:
      if(tbl->FindSymbol(_scanner->Peek().GetValue())->GetSymbolType() == stProcedure)
        // Since a procedure has to be declared before it is called,
        // this statement is valid.
      {
        st = expSubroutineCall(s);
      }
      else
      {
        st = qualident(s);
      }
      break;

    default:
      cout << "got " << _scanner->Peek() << endl;
      SetError(_scanner->Peek(), "factor expected.");
      break;
  }

  return n;
}

CAstProcedure* CParser::subroutineDecl(CAstScope *s)
{
  //
  // subroutineDecl = (procedureDecl | functionDecl) subroutineBody ident ";"
  // procedureDecl = "procedure" ident [formalParam] ";"
  // functionDecl = "function" ident [formalParam] ":" type ";"
  //
  // FIRST(subroutineDecl) = {tProcedure, tFunction}
  //
  // TODO: Fill commented areas

  CToken t; // error returning token
  EToken tt = _scanner->Peek().GetType();
  CSymProc *symbol; // make a new symtab and edit in below switch?
  InitSymbolTable(symbol);

  string procName; // contain procedure/function name
  // edit symbol table by formalPram
  switch (tt) {
    // subroutineCall ::= "procedure" ... [formalParam] ...
    case tProcedure:
      break;

    // subroutineCall ::= "function" ... [formalParam] ...
    case tFunction:
      break;

    default:
      cout << "got " << _scanner->Peek() << endl;
      SetError(_scanner->Peek(), "subroutineCall expected.");
      break;
  }

  CAstProcedure *m = new CAstProcedure(t, procName, s, symbol);

  // subroutineBody part.
  // subroutineBody = varDeclaration "begin" statSequence "end"
  // No proper CAstNode type exists for varDeclaration...
  // therefore, make CSymtab* varDeclaration function to make symbol table.
  // but statSequence has CAstStatement* type.

  tt = _scanner->Peek().GetType();
  switch (tt) {
    // varDeclaration is empty
    case tBegin:
      Consume(tBegin);
      break;

    // varDeclaration is not empty
    // edit symbol table by varDeclaration
    case tVar:
      // varDeclaration (gets symbol as symbol table and edit it)
      Consume(tBegin);
      break;

    default:
      cout << "got " << _scanner->Peek() << endl;
      SetError(_scanner->Peek(), "subroutineBody expected.");
      break;
  }

  Consume(tBegin);
  CAstStatement* statseq = statSequence(m); // Give original scope
  m->SetStatementSequence(statseq); // set statseq
  Consume(tEnd);
  // get ident and compare to procName
  Consume(tSemicolon);

  return m;
}

CAstConstant* CParser::number(void)
{
  //
  // number ::= digit { digit }.
  //
  // "digit { digit }" is scanned as one token (tNumber)
  //

  CToken t;

  Consume(tNumber, &t);

  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0) SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

pair<bool, long long int> strtobool(string s)
{
  pair<bool, long long int> retval(true, 0LL);
  if(s.compare("true") == 0)
  {
    retval.second = 1LL;
  }
  else if(s.compare("false") == 0)
  {
    retval.second = 0LL;
  }
  else
  {
    retval.first = false;
  }
  return retval;
}

CAstConstant* CParser::boolean(void)
{
  //
  // boolean = "true" | "false".
  //
  // " \"true\" | \"false\" " is scanned as one token (tBool)
  //

  CToken t;

  Consume(tBool, &t);

  errno = 0;
  pair<bool, long long int> b = strtobool(t.GetValue());
  if(!b.first) SetError(t, "invalid boolean.");

  return new CAstConstant(t, CTypeManager::Get()->GetBool(), b.second);
}

// TODO: string / null type / pointer type / array type.
