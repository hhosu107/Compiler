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
  CSymProc *func;

  // function DIM (arr: pointer to array; dim: integer): integer
  func = new CSymProc("DIM", tm->GetInt());
  func->AddParam(new CSymParam(0, "arr", tm->GetPointer(tm->GetNull())));
  func->AddParam(new CSymParam(1, "dim", tm->GetInt()));
  s->AddSymbol(func);

  // function DOFS (a: pointer to array): integer
  func = new CSymProc("DOFS", tm->GetInt());
  func->AddParam(new CSymParam(0, "a", tm->GetPointer(tm->GetNull())));
  s->AddSymbol(func);

  // funcion ReadInt (): integer
  func = new CSymProc("ReadInt", tm->GetInt());
  s->AddSymbol(func);

  // procedure WriteInt(i: integer)
  func = new CSymProc("WriteInt", tm->GetNull());
  func->AddParam(new CSymParam(0, "i", tm->GetInt()));
  s->AddSymbol(func);

  // procedure WriteChar(c: char)
  func = new CSymProc("WriteChar", tm->GetNull());
  func->AddParam(new CSymParam(0, "c", tm->GetChar()));
  s->AddSymbol(func);

  // procedure WriteString(str: char[])
  func = new CSymProc("WriteString", tm->GetNull());
  func->AddParam(new CSymParam(0, "str", tm->GetPointer(tm->GetArray(CArrayType::OPEN, tm->GetChar()))));
  s->AddSymbol(func);

  // procedure WriteLn()
  func = new CSymProc("WriteLn", tm->GetNull());
  s->AddSymbol(func);
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

  CSymtab *symbols = m->GetSymbolTable();
  InitSymbolTable(symbols);
  symbols = varDeclaration(symbols, stGlobal);

  while(subroutineDecl(m) != NULL) ;

  Consume(tBegin);

  statseq = statSequence(m);

  Consume(tEnd);

  Consume(tIdent, &idEnd);

  if(idBegin.GetValue() != idEnd.GetValue()){
    SetError(idEnd, "module name is not matched");
  }

  Consume(tDot);

  m->SetStatementSequence(statseq);
  return m;
}

CSymtab* CParser::varDeclaration(CSymtab* symbols, ESymbolType s_type){
  //
  // varDeclaration ::= [ "var" varDecl ";" { varDecl ";"} ]

  EToken tt = _scanner->Peek().GetType();

  if(tt == tVar){
    Consume(tVar);

    symbols = varDecl(symbols, s_type);
    Consume(tSemicolon);

    for(;;){
      tt = _scanner->Peek().GetType();
      if(tt != tIdent) break;

      symbols = varDecl(symbols, s_type);
      Consume(tSemicolon);
    }
  }

  return symbols;
}

/*
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
*/

CSymtab* CParser::varDecl(CSymtab* symbols, ESymbolType s_type){
  vector<string> var_names;

  CToken id;
  Consume(tIdent, &id);
  var_names.push_back(id.GetValue());

  for(;;){
    const CSymbol *duplicate;
    EToken tt = _scanner->Peek().GetType();
    if(tt != tComma) break;

    Consume(tComma);

    Consume(tIdent, &id);

    duplicate = symbols->FindSymbol(id.GetValue(), sLocal);
    if(duplicate != NULL){
      SetError(id, "ident was duplicated in local");
      return NULL;
    }

    for(string name : var_names){
      if(name == id.GetValue()){
        SetError(id, "ident was duplicated in varDecl statement");
        return NULL;
      }
    }
    var_names.push_back(id.GetValue());
  }

  Consume(tColon);

  const CType *datatype;
  datatype = read_type();

  // CSymbol(name, ESymbolType symboltype, CType *datatype)
  for(string name : var_names){
    symbols->AddSymbol(new CSymbol(name, s_type, datatype));
    // in this step, if it returns false, make an error
  }
  return symbols;
}

const CType* CParser::read_type(void){

  CTypeManager *tm = CTypeManager::Get();
  EToken tt = _scanner->Peek().GetType();
  CToken typeToken;
  const CType* datatype;

  if(tt == tBoolean){
    Consume(tBoolean, &typeToken);
    datatype = tm->GetBool();
  }
  else if(tt == tChar){
    Consume(tChar, &typeToken);
    datatype = tm->GetChar();
  }
  else if(tt == tInteger){
    Consume(tInteger, &typeToken);
    datatype = tm->GetInt();
  }
  else{
    SetError(_scanner->Peek(), "basetype expected.");
    return NULL;
  }
  stack<long long> NElems;

  for(;;){ // array check
    tt = _scanner->Peek().GetType();

    if(tt == tLBracket){
      Consume(tLBracket);

      tt = _scanner->Peek().GetType();
      if(tt == tNumber){
        CToken intToken;
        Consume(tNumber, &intToken);
        NElems.push(strtoll(intToken.GetValue().c_str(), NULL, 10));
      }
      else if(tt == tRBracket){
        NElems.push(-1LL);
      }
      else{
        SetError(_scanner->Peek(), "[number] \"]\" expected");
        return NULL;
      }

      Consume(tRBracket);
    }
    else break;
  }

  if(!(NElems.empty())){
    long long size = NElems.top();
    NElems.pop();
    if(size >= 0){
      datatype = tm->GetPointer(tm->GetArray(size, datatype));
    }
    else{
      datatype = tm->GetPointer(tm->GetArray(CArrayType::OPEN, datatype));
    }
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
  CAstStatement *head = NULL, *tail = NULL;

  EToken tt = _scanner->Peek().GetType();

  CAstStatement *st = NULL;

  if(tt == tIdent || tt == tIf || tt == tWhile || tt == tReturn){
    st = statement(s);
    head = tail = st;

    for(;;){
      tt = _scanner->Peek().GetType();
      if(tt != tSemicolon) break;

      Consume(tSemicolon);
      st = statement(s);

      tail->SetNext(st);
      tail = st;
    }
  }

  return head;
}

CAstStatement* CParser::statement(CAstScope *s){
  CToken t = _scanner->Peek();
  EToken tt = t.GetType();

  CAstStatement *st = NULL;

  CSymtab *symbols = s->GetSymbolTable();

  if(tt == tIdent){ // assignment, subroutineCall
    const CSymbol* found = symbols->FindSymbol(t.GetValue(), sGlobal);
    if(found == NULL){
      cout << t.GetValue() << endl;
      symbols->print(cout, 0);
      SetError(t, "not declared from statement");
    }
    if(found->GetSymbolType() == stProcedure){
      st = statementSubroutineCall(s);
    }
    else{
      st = assignment(s);
    }
  }
  else if(tt == tIf){
    st = ifStatement(s);
  }
  else if(tt == tWhile){
    st = whileStatement(s);
  }
  else if(tt == tReturn){
    st = returnStatement(s);
  }
  else{
    SetError(t, "not statement");
  }

  return st;
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
  Consume(tWhile);
  Consume(tLParen);

  CAstExpression *cond_expr = expression(s);

  Consume(tRParen);
  Consume(tDo);

  CAstStatement *statseq = statSequence(s);

  Consume(tEnd);

  CToken dummy;
  CAstStatWhile* ret = new CAstStatWhile(dummy, cond_expr, statseq);
  return ret;
}

CAstStatReturn* CParser::returnStatement(CAstScope *s){
  CToken dummy;

  Consume(tReturn);

  CAstExpression *ret_expr = NULL;

  EToken tt;
  tt = _scanner->Peek().GetType();

  if(tt == tPlusMinus || tt == tIdent || tt == tNumber || tt == tBool || tt == tCharacter || tt == tString || tt == tLParen || tt == tNot){ // expression
    ret_expr = expression(s);
  }

  CAstStatReturn *ret = new CAstStatReturn(dummy, s, ret_expr);

  return ret;
}

CAstStatAssign* CParser::assignment(CAstScope *s)
{
  //
  // assignment ::= qualident ":=" expression.
  //
  // TODO: Have to edit more for the qualident
  //

  // cout << "assignment" << endl;

  CToken dummy;

  CAstDesignator *lhs;
  lhs = qualident(s);

  Consume(tAssign);

  CAstExpression *rhs = expression(s);

  return new CAstStatAssign(dummy, lhs, rhs);
}

CAstDesignator* CParser::qualident(CAstScope* s)
{
  //
  // qualident ::= ident { "[ expression "]" }.
  //

  CSymtab* symbols = s->GetSymbolTable();
   // symbols->print(cout, 0);

 CToken id;

  /* We have to make ident(CAstScope *s) since it will be duplicated in many
   * ftns. */
  Consume(tIdent, &id);

  // cout << id.GetValue() << endl;

  const CSymbol* idsym = symbols->FindSymbol(id.GetValue(), sGlobal);
  if(idsym == NULL){
    SetError(id, "undeclared variable from qualident");
  }

  CAstDesignator* n;
  n = new CAstDesignator(id, idsym);
  /* ident(s) end */

  EToken tt = _scanner->Peek().GetType();
  if(tt != tLBracket) return n;

  /* get tLBracket */

  CAstArrayDesignator *an = new CAstArrayDesignator(id, idsym);

  for(;;){
    tt = _scanner->Peek().GetType();
    if(tt != tLBracket) break;

    Consume(tLBracket);

    CAstExpression* idx = expression(s);
    an->AddIndex(idx);

    Consume(tRBracket);
  }

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
  // FIRST(factor) = { tNumber, tBool, tCharacter, tString, tIdent, tLParen, tNot, tPlusMinus }
  //

  // TODO: Not Edited yet

  CToken t;
  EToken tt = _scanner->Peek().GetType();
  CAstExpression *unary = NULL, *n = NULL;
  CSymtab *symbols = s->GetSymbolTable();
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
      if(symbols->FindSymbol(_scanner->Peek().GetValue(), sGlobal)->GetSymbolType() == stProcedure)
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
    return NULL;
  }

  CToken idBegin, idEnd;
  CSymtab* symbols = new CSymtab();
  CAstProcedure *m = NULL;
  const CType* return_type;
  CTypeManager *tm = CTypeManager::Get();

  Consume(tIdent, &idBegin);
  if(s->GetSymbolTable()->FindSymbol(idBegin.GetValue(), sGlobal)){
    SetError(idBegin, "this proc/func is redeclared");
  }

  // subroutineDecl -> ... formalParam ... : start
  tt = _scanner->Peek().GetType();
  if(tt == tLParen) { // formalParam
    Consume(tLParen);

    tt = _scanner->Peek().GetType();

    if(tt == tIdent){
      symbols = varDecl(symbols, stParam);
      for(;;){
        tt = _scanner->Peek().GetType();
        if(tt != tSemicolon) break;
        Consume(tSemicolon);
        symbols = varDecl(symbols, stParam);
      }
    }

    Consume(tRParen);
  }

  if(isProcedure){
    return_type = tm->GetNull();
    Consume(tSemicolon);
  }
  else{
    Consume(tColon);
    return_type = read_type();
    Consume(tSemicolon);
  }

  CSymProc *symproc;
  symproc = new CSymProc(idBegin.GetValue(), return_type);

  m = new CAstProcedure(idBegin, idBegin.GetValue(), s, symproc);
  // 3rd element sets parent scope. In here, it is s.
  // Still, its symbol table has to be set, in the below code.

  // subroutineDecl -> ... subroutineBody : varDeclaration
  symbols = varDeclaration(symbols, stLocal);
  // TODO: make vector<CSymbol*> symbolList
  vector<CSymbol*> symbolList = symbols->GetSymbols();
  for(int i=0; i<symbolList.size(); i++){
    (m->GetSymbolTable())->AddSymbol(symbolList[i]);
  }
  // subroutineDecl -> ... subroutineBody : "begin" ...
  Consume(tBegin);

  // subroutineDecl -> ... subroutineBody : statSequence
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

CAstConstant* CParser::character(void)
{
  //
  // character = ...
  // char = "'" character "'"
  //
  // char is scanned as one token (tCharacter)
  //

  CToken t;

  Consume(tCharacter, &t);
  errno = 0;
  long long c = (long long) CToken::unescape(t.GetValue())[0];
  return new CAstConstant(t, CTypeManager::Get()->GetChar(), c);
}
// TODO: string / null type / pointer type / array type.
