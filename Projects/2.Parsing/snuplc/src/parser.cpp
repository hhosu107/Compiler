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
#include <stack>
#include <iostream>
#include <exception>

#include "parser.h"
using namespace std;


//------------------------------------------------------------------------------
// CParser
//
// TODO: check each tokens before consume it.
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
  CToken dummy;
  CAstModule *m = NULL;
  CAstStatement *statseq = NULL;

  Consume(tModule);

  CToken idBegin, idEnd;
  Consume(tIdent, &idBegin);

  m = new CAstModule(dummy, idBegin.GetValue());

  Consume(tSemicolon);

  CSymtab *symbols;
  InitSymbolTable(symbols);
  symbols = varDeclaration(symbols, stGlobal);

  // TODO: subroutineDecl

  Consume(tBegin);

  statseq = statSequence(m);

  Consume(tEnd);

  Consume(tIdent, &idEnd);

  if(idBegin.GetValue() != idEnd.GetValue()){
    SetError(idEnd, "module name is not matched");
  }

  Consume(tDot);

  m->SetSymbolTable(symbols);
  m->SetStatementSequence(statseq);
  return m;
}

CSymtab* CParser::varDeclaration(CSymtab* symbols, ESymbolType s_type){
  EToken tt = _scanner->Peek().GetType();

  if(tt == tVar){
    Consume(tVar);

    symbols = varDeclSequence(symbols, s_type);

    Consume(tDot);
  }

  return symbols;
}

CSymtab* CParser::varDeclSequence(CSymtab* symbols, ESymbolType s_type){
  symbols = varDecl(symbols, s_type);

  for(;;){
    EToken tt = _scanner->Peek().GetType();
    if(tt != tSemicolon) break;

    Consume(tSemicolon);

    symbols = varDecl(symbols, s_type);
  }

  return symbols;
}

CSymtab* CParser::varDecl(CSymtab* symbols, ESymbolType s_type){
  vector<string> var_names;

  CToken id;
  Consume(tIdent, &id);
  var_names.push_back(id.GetValue());

  for(;;){
    EToken tt = _scanner->Peek().GetType();
    if(tt != tComma) break;

    Consume(tComma);

    Consume(tIdent, &id);
    var_names.push_back(id.GetValue());
  }

  Consume(tColon);

  CType *datatype;
  datatype = read_type(datatype);

  // CSymbol(name, ESymbolType symboltype, CType *datatype)
  for(string name : var_names){
    symbols->AddSymbol(new CSymbol(name, s_type, datatype));
  }
  return symbols;
}

CType* CParser::read_type(CType *datatype){
  // TODO : implement
  // basetype { "[" [number] "]" }

  EToken tt = _scanner->Peek().GetType();
  CToken typeToken;

  if(tt == tBoolean){
    Consume(tBoolean, &typeToken);
  }
  else if(tt == tChar){
    Consume(tChar, &typeToken);
  }
  else if(tt == tInteger){
    Consume(tInteger, &typeToken);
  }
  else{
    SetError(_scanner->Peek(), "basetype expected.");
    return NULL;
    // TODO: non-type error
  }

  stack<int> NElems;

  for(;;){ // array check
    tt = _scanner->Peek().GetType();

    if(tt == tLBracket){
      Consume(tLBracket);

      CToken intToken;
      Consume(tNumber, &intToken);
      NElems.push(atoi(intToken.GetValue().c_str()));

      Consume(tRBracket);
    }
    else break;
  }

  if(!(NElems.empty())){
    // TODO: make array type
  }

  return datatype;
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
          if(tbl->FindSymbol(_scanner->Peek().GetValue(), sGlobal)->GetSymbolType() == stProcedure)
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

CAstStatCall* CParser::statementSubroutineCall(CAstScope *s){
  // TODO: implement
  return NULL;
}
 
CAstStatIf* CParser::ifStatement(CAstScope *s){
  // TODO: implement
  return NULL;
}

CAstStatWhile* CParser::whileStatement(CAstScope *s){
  // TODO: implement
  return NULL;
}

CAstStatReturn* CParser::returnStatement(CAstScope *s){
  // TODO: implement
  return NULL;
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

CAstDesignator* CParser::qualident(CAstScope* s)
{
  //
  // qualident ::= ident { "[ expression "]" }.
  //

  CSymtab* symbols = s->GetSymbolTable();
  CToken id;

  /* We have to make ident(CAstScope *s) since it will be duplicated in many
   * ftns. */
  Consume(tIdent, &id);
  const CSymbol* idsym = symbols->FindSymbol(id.GetValue(), sLocal);
  if(idsym == NULL){
    idsym = symbols->FindSymbol(id.GetValue(), sGlobal);
  }
  if(idsym == NULL){
    SetError(id, "undeclared variable");
  }

  CAstDesignator* n;
  n = new CAstDesignator(id, idsym);
  /* ident(s) end */

  EToken tt = _scanner->Peek().GetType();
  if(tt != tLBracket) return n;

  /* get tLBracket */

  CAstArrayDesignator *an = new CAstArrayDesignator(id, idsym);
  tt = _scanner->Peek().GetType();

  do{
    Consume(tLBracket);
    CAstExpression *idx = expression(s);
    an->AddIndex(idx);
    Consume(tRBracket);
    tt = _scanner->Peek().GetType();
  }while(tt == tLBracket);

  return an;
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
  while (tt == tPlusMinus || tt == tAndOr) {
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

  while ((tt == tMulDiv) || (tt == tAndOr)) {
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
    {
      Consume(tNot, &t);
      CAstExpression *r = NULL;
      r = factor(s);
      n = new CAstUnaryOp(t, opNot, r);
    }
      break;

    case tIdent:
      if(tbl->FindSymbol(_scanner->Peek().GetValue())->GetSymbolType() == stProcedure)
        // Since a procedure has to be declared before it is called,
        // this statement is valid.
      {
        n = expSubroutineCall(s);
      }
      else
      {
        n = qualident(s);
      }
      break;

    default:
      cout << "got " << _scanner->Peek() << endl;
      SetError(_scanner->Peek(), "factor expected.");
      break;
  }

  return n;
}

CAstFunctionCall* CParser::expSubroutineCall(CAstScope *s){
  // TODO: implement
  return NULL;
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

  EToken tt = _scanner->Peek().GetType();

  bool isProcedure = false;

  // subroutineDecl -> "procedure" ...
  if(tt == tProcedure){
    Consume(tProcedure);
    isProcedure = true;
  }

  // subroutineDecl -> "function" ...
  else if(tt == tFunction){
    Consume(tFunction);
  }

  else {
    SetError(_scanner->Peek(), "procedure/function expected");
  }

  CToken dummy;
  CAstProcedure *m = NULL; // will contain Procedure AST
  CToken idBegin, idEnd; // to check these two are the same
  CSymtab *symbols; //
  CType *return_type = NULL;
  InitSymbolTable(symbols);

  // subroutineDecl -> ... ident ...
  tt = _scanner->Peek().GetType();
  if(tt != tIdent){
    SetError(_scanner->Peek(), "ident expected");
  }

  Consume(tIdent, &idBegin);
  const string &idName = idBegin.GetName();
  if(s->GetSymbolTable()->FindSymbol(idName, sGlobal)){
    SetError(idBegin, "this proc/func is redeclared");
  }

  // subroutineDecl -> ... formalParam ... : start
  tt = _scanner->Peek().GetType();
  if(tt == tLParen) {
    // TODO: check w.o.n it is tLParen
    Consume(tLParen);

    tt = _scanner->Peek().GetType();

    if(tt == tIdent){
      varDeclSequence(symbols, stParam);
    }
    else{
      // TODO: check w.o.n. it is tRParen.
    }

    // subroutineDecl -> ... formalParam ... : end
    Consume(tRParen);

    // subroutineDecl -> ";" | ":" type ";"
    if(isProcedure){
      return_type = CNullType();
      // TODO: check w.o.n. it is tSemicolon.
      Consume(tSemicolon);
    }
    else{ // function
      // check w.o.n. it is tColon.
      Consume(tColon);

      return_type = read_type(return_type);
      // TODO: type

      // TODO: check w.o.n. it is tSemicolon.
      Consume(tSemicolon);
    }
  }
  else if(tt == tSemicolon){
    Consume(tSemicolon);
  }
  else{
    SetError(_scanner->Peek(), "tLParen or tSemicolon expected");
  }

  CSymProc *symproc;
  symproc = new CSymProc(idBegin.GetValue(), return_type);

  m = new CAstProcedure(idBegin, idBegin.GetValue(), s, symproc);
  // 3rd element sets parent scope. In here, it is s.
  // Still, its symbol table has to be set, in the below code.

  // subroutineDecl -> ... subroutineBody : varDeclaration
  symbols = varDeclaration(symbols, stLocal);

  // subroutineDecl -> ... subroutineBody : "begin" ...
  // TODO: check tBegin
  Consume(tBegin);

  // subroutineDecl -> ... subroutineBody : statSequence
  // TODO:
  CAstStatement *statseq = NULL;
  statseq = statSequence(m);

  // TODO: check tEnd
  Consume(tEnd);

  // TODO: check tIdent
  Consume(tIdent, &idEnd);

  if(idBegin.GetValue() != idEnd.GetValue()){
    SetError(idEnd, "module name is not mached");
  }

  // TODO: check tSemicolon
  Consume(tSemicolon);

  m->SetSymbolTable(symbols);
  m->SetStatementSequence(statseq);
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
