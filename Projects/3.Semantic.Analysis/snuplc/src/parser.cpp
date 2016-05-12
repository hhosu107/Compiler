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
      if (!_module->TypeCheck(&t, &msg)) SetError(t, msg);
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

  // procedure WriteStr(str: char[])
  func = new CSymProc("WriteStr", tm->GetNull());
  func->AddParam(new CSymParam(0, "str", tm->GetPointer(tm->GetArray(CArrayType::OPEN, tm->GetChar()))));
  s->AddSymbol(func);

  // procedure WriteLn()
  func = new CSymProc("WriteLn", tm->GetNull());
  s->AddSymbol(func);
}

// module
CAstModule* CParser::module(void)
{
  //
  // module ::= "module" ident ";" varDeclaration { subroutineDecl }
  //            "begin" statSequence "end" ident ".".
  //
  CToken dummy;
  CAstModule *m = NULL;
  CAstStatement *statseq = NULL;

  // module -> "module" ideBegin ";" ...
  Consume(tModule, &dummy);

  CToken idBegin, idEnd;
  Consume(tIdent, &idBegin);

  m = new CAstModule(dummy, idBegin.GetValue());

  Consume(tSemicolon);

  // module -> ... varDeclaration ...
  CSymtab *symbols = m->GetSymbolTable();
  InitSymbolTable(symbols);
  symbols = varDeclaration(symbols, stGlobal, false);

  // module -> ... { subroutineDecl }
  // m will be edited in subroutineDecl,
  // so we don't have to save each return value of subroutineDecl.
  while(subroutineDecl(m) != NULL) ;

  // module -> ... "begin" statSequence "end" ...
  Consume(tBegin);

  statseq = statSequence(m);

  Consume(tEnd);

  // module -> ... idEnd "."
  Consume(tIdent, &idEnd);

  if(idBegin.GetValue() != idEnd.GetValue()){
    SetError(idEnd, "module name is not matched");
  }

  Consume(tDot);

  m->SetStatementSequence(statseq);
  return m;
}

// varDeclaration
CSymtab* CParser::varDeclaration(CSymtab* symbols, ESymbolType sType, bool openArray){
  //
  // varDeclaration ::= [ "var" varDecl ";" { varDecl ";" } ].

  EToken tt = _scanner->Peek().GetType();

  // varDeclaration -> [ "var" ...
  // Is it an empty declaration or not?
  if(tt == tVar){
    Consume(tVar);

    // varDeclaration -> [ ... varDecl ";" ... ]
    symbols = varDecl(symbols, sType, NULL, openArray);
    Consume(tSemicolon);

    // varDeclaration -> [ ... { varDecl ";" } ]
    for(;;){
      tt = _scanner->Peek().GetType();
      if(tt != tIdent) break;

      symbols = varDecl(symbols, sType, NULL, openArray);
      Consume(tSemicolon);
    }
  }

  return symbols;
}

// varDecl
CSymtab* CParser::varDecl(CSymtab* symbols, ESymbolType sType, vector<CSymParam*> *params, bool openArray){
  //
  // varDecl : ident { "," ident } ":" type.
  //
  vector<string> varNames;
  CTypeManager *tm = CTypeManager::Get();

  // varDecl -> ident ...
  CToken id;
  Consume(tIdent, &id);
  if(symbols->FindSymbol(id.GetValue(), sLocal) != NULL){
    SetError(id, "duplicate variable declaration \'" + id.GetValue() + "\'.");
    return NULL;
  }

  varNames.push_back(id.GetValue());

  // varDecl -> ... { "," identNext } ":" ...
  for(;;){
    EToken tt = _scanner->Peek().GetType();
    if(tt != tComma) break;

    Consume(tComma);

    Consume(tIdent, &id);

    if(symbols->FindSymbol(id.GetValue(), sLocal) != NULL){
      SetError(id, "duplicate variable declaration \'" + id.GetValue() + "\'.");
      return NULL;
    }

    for(string name : varNames){
      if(name == id.GetValue()){
        SetError(id, "duplicate variable declaration \'" + id.GetValue() + "\'.");
        return NULL;
      }
    }
    varNames.push_back(id.GetValue());
  }

  Consume(tColon);

  // varDecl : .. type
  const CType *datatype;
  datatype = ReadType(openArray, true)->GetType();
  // CSymbol(name, ESymbolType symboltype, CType *datatype)
  // have to give each methods refer to sType
  for(string name : varNames){
    if(sType == stGlobal){
      symbols->AddSymbol(new CSymGlobal(name, datatype));
    }
    else if(sType == stLocal){
      symbols->AddSymbol(new CSymLocal(name, datatype));
    }
    else if(sType == stProcedure){
      symbols->AddSymbol(new CSymProc(name, datatype));
    }
    else if(sType == stParam && params != NULL){
      // for the formalParam definition, each declaration has also to be saved
      // into the procedure parameter list, so save it seperatedly
      int size = params->size();
      const CType* ptrtype;

      // only if it is an array as a parameter, it has to be a pointer
      if(datatype->IsArray()){
        ptrtype = tm->GetPointer(datatype);
      }
      else{
        ptrtype = datatype;
      }

      symbols->AddSymbol(new CSymParam(size, name, ptrtype));
      params->push_back(new CSymParam(size, name, ptrtype));
    }
    else{
      symbols->AddSymbol(new CSymbol(name, sType, datatype));
    }
  }
  return symbols;
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

  EToken tt = _scanner->Peek().GetType();

  CToken dummy;
  bool isProcedure = false;

  // subroutineDecl -> "procedure" ...
  if(tt == tProcedure){
    Consume(tProcedure, &dummy);
    isProcedure = true;
  }

  // subroutineDecl -> "function" ...
  else if(tt == tFunction){
    Consume(tFunction, &dummy);
  }

  else {
    return NULL;
  }

  CToken idBegin, idEnd;
  CSymtab* symbols = new CSymtab();
  CAstProcedure *m = NULL;
  const CType* retType;
  CTypeManager *tm = CTypeManager::Get();

  Consume(tIdent, &idBegin);
  if(s->GetSymbolTable()->FindSymbol(idBegin.GetValue(), sGlobal)){
    SetError(idBegin, "this proc/func is redeclared");
  }

  // subroutineDecl -> ... [ formalParam ]  ...
  // formalParam ::= "(" [ varDecl { ";" varDecl } ] ")"
  vector<CSymParam*> params;
  tt = _scanner->Peek().GetType();
  if(tt == tLParen) { // formalParam
    // formalParam -> "(" ...
    Consume(tLParen);

    // formalParam -> ... [ varDecl ...
    tt = _scanner->Peek().GetType();

    // Does a varDecl exist or not?
    // Here, since it is for the formalParam,
    // we have to save each formal parameters sequentially.
    if(tt == tIdent){
      // formalParam -> ... [ varDecl ...
      symbols = varDecl(symbols, stParam, &params, true);

      // formalParam -> ... [ ... { ";" varDecl } ] ...
      for(;;){
        tt = _scanner->Peek().GetType();
        if(tt != tSemicolon) break;
        Consume(tSemicolon);
        symbols = varDecl(symbols, stParam, &params, true);
      }
    }

    // formalParam -> ... ")"
    Consume(tRParen);
  }

  if(isProcedure){
    // subroutineDecl -> ... ";" ...
    retType = tm->GetNull();
    Consume(tSemicolon);
  }
  else{
    // subroutineDecl -> ... ":" type ";" ...
    Consume(tColon);
    retType = ReadType(false, false)->GetType();
    Consume(tSemicolon);
  }

  // make CSymProc* for this function and add it into parent symtab
  CSymProc *symproc;
  symproc = new CSymProc(idBegin.GetValue(), retType);
  int idx = 0;
  for(CSymParam* param : params){
    symproc->AddParam(param);
  }
  (s->GetSymbolTable())->AddSymbol(symproc);

  m = new CAstProcedure(dummy, idBegin.GetValue(), s, symproc);
  // 3rd element sets parent scope. In here, it is s.
  // Still, its symbol table has to be set, in the below code.

  // subroutineDecl -> ... subroutineBody ...
  // subroutineBody ::= varDeclaration "begin" statSequence "end"

  // subroutineBody -> varDeclaration ...
  symbols = varDeclaration(symbols, stLocal, false);
  vector<CSymbol*> symbolList = symbols->GetSymbols();
  for(int i=0; i<symbolList.size(); i++){
    (m->GetSymbolTable())->AddSymbol(symbolList[i]);
  }

  // subroutineBody -> ... "begin" ...
  Consume(tBegin);

  // subroutineDecl -> ... statSequence "end"
  CAstStatement *statseq = NULL;
  statseq = statSequence(m);
  Consume(tEnd);

  // subroutineDecl -> ... ident ";"
  Consume(tIdent, &idEnd);

  if(idBegin.GetValue() != idEnd.GetValue()){
    SetError(idEnd, "module name is not mached");
  }

  Consume(tSemicolon);

  m->SetStatementSequence(statseq);

  // Add this symbol into function itself to able recursive call?
  // In the reference parser, it is not given to its symbol table.
  // (m->GetSymbolTable())->AddSymbol(symproc);

  return m;
}

// ReadType
const CAstType* CParser::ReadType(bool openArray, bool beArray){
  //
  // type ::= basetype | type "[" [ number ] "]".
  //  <=>
  // type ::= basetype type'
  // type' ::= epsilon | "[" [ number ] "]" type'
  //
  CTypeManager *tm = CTypeManager::Get();
  EToken tt = _scanner->Peek().GetType();
  CToken typeToken;
  const CType* datatype;

  // type -> basetype
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

  // type -> ... type'
  // we used stack since we have to make CType* node from the last dimension
  stack<long long> NElems;

  for(;;){ // array check
    tt = _scanner->Peek().GetType();

    if(tt == tLBracket){
      Consume(tLBracket);

      tt = _scanner->Peek().GetType();
      if(!openArray || tt == tNumber){
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
  // cout << endl;

  if(!beArray && !(NElems.empty())){
    SetError(typeToken, "invalid composite type for function.");
  }

  // make CType* node iteratively
  while(!(NElems.empty())){
    long long size = NElems.top();
    NElems.pop();
    if(size >= 0){
      datatype = tm->GetArray(size, datatype);
    }
    else{
      datatype = tm->GetArray(CArrayType::OPEN, datatype);
    }
  }

  return new CAstType(typeToken, datatype);
}

// statSequence
CAstStatement* CParser::statSequence(CAstScope *s)
{
  //
  // statSequence ::= [ statement { ";" statement } ].
  // statement ::= assignment | subroutineCall | ifStatement |
  //                whileStatement | returnStatement.
  // FIRST(statSequence) = { tIdent, tIf, tWhile, tReturn }
  // FOLLOW(statSequence) = { tElse, tEnd }
  //
  // this subroutineCall will call subroutineCall()
  // which is a closure of expSubroutineCall().
  //
  CAstStatement *head = NULL, *tail = NULL;

  EToken tt = _scanner->Peek().GetType();

  CAstStatement *st = NULL;

  // statSequence -> [ statement ...
  // Is it empty or not?
  if(tt == tIdent || tt == tIf || tt == tWhile || tt == tReturn){
    // statSequence -> [ statement ...
    st = statement(s);
    head = tail = st;

    // statSequence -> [ ... { ";" statement } ]
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

// statement
CAstStatement* CParser::statement(CAstScope *s){
  //
  // statement ::= assignment | subroutineCall | ifStatement | whileStatement
  //              | returnStatement
  //
  CToken t = _scanner->Peek();
  EToken tt = t.GetType();

  CAstStatement *st = NULL;

  CSymtab *symbols = s->GetSymbolTable();

  // check first tokens of each candidates
  if(tt == tIdent){ // assignment, subroutineCall
    const CSymbol* found = symbols->FindSymbol(t.GetValue(), sGlobal);
    if(found == NULL){
      cout << t.GetValue() << endl;
      symbols->print(cout, 0);
      SetError(t, "not declared from statement");
    }
    if(found->GetSymbolType() == stProcedure){
      st = subroutineCall(s);
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
    SetError(t, "statement expected.");
  }

  return st;
}

// assignment
CAstStatAssign* CParser::assignment(CAstScope *s){
  //
  // assignment ::= qualident ":=" expression.
  //
  CToken dummy;

  CAstDesignator *lhs;
  lhs = qualident(s);

  Consume(tAssign, &dummy);

  CAstExpression *rhs = expression(s);
  return new CAstStatAssign(dummy, lhs, rhs);
}

// subroutineCall
CAstStatCall* CParser::subroutineCall(CAstScope *s){
  //
  // subroutineCall ::= ident "(" [ expression { "," expression } ] ")"
  //
  CSymtab* symbols = s->GetSymbolTable();

  // subroutineCall -> ident "(" ...
  CToken id;
  Consume(tIdent, &id);

  const CSymbol* symbol = symbols->FindSymbol(id.GetValue(), sGlobal);
  if(symbol == NULL){
    SetError(id, "no such subroutine");
  }

  // make a CAstFunctionCall* node to enclose it
  CAstFunctionCall* fc = new CAstFunctionCall(id, (CSymProc*)symbol);

  Consume(tLParen);

  // subroutineCall -> ... [ expression ...
  // Does an expression exist or not?
  CToken t = _scanner->Peek();
  EToken tt = _scanner->Peek().GetType();
  if(tt == tPlusMinus || tt == tIdent || tt == tNumber || tt == tBool || tt == tCharacter || tt == tString || tt == tLParen || tt == tNot){ // expression
    // subroutineCall -> ... [ expression ...
    CAstExpression *ex = expression(s);
    if(ex->GetType()->IsArray())
      {
        fc->AddArg(new CAstSpecialOp(t, opAddress, ex, NULL));
      }
      else fc->AddArg(ex);

    // subroutineCall -> ... [ ... { "," expression } ] ...
    for(;;){
      EToken tt = _scanner->Peek().GetType();
      if(tt != tComma) break;

      Consume(tComma);
      t = _scanner->Peek();
      ex = expression(s);
      if(ex->GetType()->IsArray())
      {
        fc->AddArg(new CAstSpecialOp(t, opAddress, ex, NULL));
      }
      else fc->AddArg(ex);
    }
  }
  // subroutineCall -> ... ")"
  Consume(tRParen);

  return new CAstStatCall(id, fc);
}

// ifStatement
CAstStatIf* CParser::ifStatement(CAstScope *s){
  //
  // ifStatement ::= "if" "(" expression ")" "then" statSequence
  //                  [ "else" statSequence ] "end".
  //
  CToken dummy;

  // ifStatement -> if ...
  Consume(tIf, &dummy);

  // ifStatement -> ... "(" expression ")" ...
  Consume(tLParen);
  CAstExpression *ifcond = expression(s);
  Consume(tRParen);

  // ifStatement -> ... "then" statSequence1 ...
  Consume(tThen);
  CAstStatement *ifstat = statSequence(s);

  // ifStatement -> ... [ "else" statSequence2 ] "end"
  EToken tt;
  CAstStatement *elsestat = NULL;
  tt = _scanner->Peek().GetType();
  if(tt == tElse){
    Consume(tElse);
    elsestat = statSequence(s);
  }
  Consume(tEnd);

  return new CAstStatIf(dummy, ifcond, ifstat, elsestat);
}

// whileStatement
CAstStatWhile* CParser::whileStatement(CAstScope *s){
  //
  // whileStatement ::= "while" "(" expression ")" "then" statSequence "end".
  //
  CToken dummy;

  Consume(tWhile, &dummy);
  Consume(tLParen);

  CAstExpression *condExpr = expression(s);

  Consume(tRParen);
  Consume(tDo);

  CAstStatement *statseq = statSequence(s);

  Consume(tEnd);

  CAstStatWhile* ret = new CAstStatWhile(dummy, condExpr, statseq);
  return ret;
}

// returnStatement
CAstStatReturn* CParser::returnStatement(CAstScope *s){
  //
  // returnStatement ::= "return" [ expression ]
  //
  CToken dummy;

  // returnStatement -> "return" ...
  Consume(tReturn, &dummy);

  CAstExpression *retExpr = NULL;

  // returnStatement -> ... [ expression ...
  // Does an expression exist or not?
  EToken tt;
  tt = _scanner->Peek().GetType();

  if(tt == tPlusMinus || tt == tIdent || tt == tNumber || tt == tBool || tt == tCharacter || tt == tString || tt == tLParen || tt == tNot){ // expression
    // returnStatement -> ... [ expression ...
    retExpr = expression(s);
  }

  CAstStatReturn *ret = new CAstStatReturn(dummy, s, retExpr);

  return ret;
}
// factor
CAstExpression* CParser::factor(CAstScope *s)
{
  //
  // factor ::= number | "(" expression ")" | boolean |
  //            character | string | "!" factor |
  //            qualident | expSubroutineCall
  //
  // FIRST(factor) = { tNumber, tBool, tCharacter, tString, tIdent, tLParen, tNot, tPlusMinus }
  //

  CToken t;
  EToken tt = _scanner->Peek().GetType();
  CAstExpression *unary = NULL, *n = NULL;
  CSymtab *symbols = s->GetSymbolTable();

  // match to the first of each factor
  switch (tt) {
    // factor -> number
    case tNumber:
      n = number();
      break;

      // factor -> "(" expression ")"
    case tLParen:
      Consume(tLParen);
      n = expression(s);
      Consume(tRParen);
      break;

      // factor -> boolean
    case tBool:
      n = boolean();
      break;

      // factor -> character
    case tCharacter:
      n = character();
      break;

      // factor -> string
    case tString:
      n = stringConst(s);
      break;

      // factor -> "!" factor
    case tNot:
      {
        Consume(tNot, &t);
        CAstExpression *r = NULL;
        r = factor(s);
        n = new CAstUnaryOp(t, opNot, r);
      }
      break;

      // factor -> qualident | expSubroutineCall
    case tIdent:
      {
        // Is the symbol we want declared or not?
        const CSymbol* sym = symbols->FindSymbol(_scanner->Peek().GetValue(), sGlobal);
        if(sym){
          if(sym->GetSymbolType() == stProcedure)
          {
            n = expSubroutineCall(s);
          }
          else
          {
            n = qualident(s);
          }
        }
        else{
          SetError(_scanner->Peek(), "undefined identifier.");
        }
      }
      break;

    default:
      SetError(_scanner->Peek(), "factor expected.");
      break;
  }
  return n;
}

// qualident
CAstDesignator* CParser::qualident(CAstScope* s){
  //
  // qualident ::= ident { "[" expression "]" }.
  //
  CSymtab* symbols = s->GetSymbolTable();

  // qualident -> ident ...
  CToken id;
  Consume(tIdent, &id);

  // check w.o.n it is declared or not
  const CSymbol* idsym = symbols->FindSymbol(id.GetValue(), sGlobal);
  if(idsym == NULL){
    SetError(id, "undeclared variable from qualident");
  }

  CAstDesignator* n;
  n = new CAstDesignator(id, idsym);

  // qualident -> ... { "[" ...
  // Is it an array or not?
  EToken tt = _scanner->Peek().GetType();
  if(tt != tLBracket) return n;

  /* get tLBracket */
  // qualident -> ... { "[" ... expression "]" }
  CAstArrayDesignator *an = new CAstArrayDesignator(id, idsym);

  for(;;){
    tt = _scanner->Peek().GetType();
    if(tt != tLBracket) break;

    Consume(tLBracket);

    CAstExpression* idx = expression(s);
    an->AddIndex(idx);

    Consume(tRBracket);
  }

  an->IndicesComplete();
  return an;
}

// expSubroutineCall
CAstFunctionCall* CParser::expSubroutineCall(CAstScope *s){
  // expSubroutineCall ::= same definition as subroutineCall
  CSymtab* symbols = s->GetSymbolTable();

  CToken id;
  Consume(tIdent, &id);

  const CSymbol* symbol = symbols->FindSymbol(id.GetValue(), sGlobal);
  if(symbol == NULL){
    SetError(id, "no such subroutine");
  }

  CAstFunctionCall* fc = new CAstFunctionCall(id, (CSymProc*)symbol);

  Consume(tLParen);
  EToken tt = _scanner->Peek().GetType();
  if(tt == tPlusMinus || tt == tIdent || tt == tNumber || tt == tBool ||
      tt == tCharacter || tt == tString || tt == tLParen || tt == tNot){ // expression

    CAstExpression *ex = expression(s);
    fc->AddArg(ex);

    for(;;){
      EToken tt = _scanner->Peek().GetType();
      if(tt != tComma) break;

      Consume(tComma);
      ex = expression(s);
      fc->AddArg(ex);
    }
  }

  Consume(tRParen);

  return fc;
}

// term
CAstExpression* CParser::term(CAstScope *s)
{
  //
  // term ::= factor { factOp factor }.
  //
  CAstExpression *n = NULL;

  // term ::= factor ...
  n = factor(s);

  EToken tt;

  // term ::= ... { factOp factor }
  // Does a factOp exist or not?
  while ((_scanner->Peek().GetType() == tMulDiv) || (_scanner->Peek().GetType() == tAndOr && _scanner->Peek().GetValue() == "&&")) {
    CToken t;
    CAstExpression *l = n, *r = NULL;
    tt = _scanner->Peek().GetType();
    switch(tt){
      case tMulDiv:
        Consume(tMulDiv, &t);
        r = factor(s);
        n = new CAstBinaryOp(t, t.GetValue() == "*" ? opMul : opDiv, l, r);
        break;

      case tAndOr:
        Consume(tAndOr, &t);
        r = factor(s);
        n = new CAstBinaryOp(t, opAnd, l, r);
        break;
    }
  }

  return n;
}
// simpleexpr
CAstExpression* CParser::simpleexpr(CAstScope *s)
{
  //
  // simpleexpr ::= [ "+" | "-" ] term { termOp term }.
  //
  CAstExpression *n = NULL;

  // simpleexpr -> [ "+" | "-" ] term ...
  // Is it signed or not?
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

  // simpleexpr -> ... { termOp term }
  // Does a termOp exist or not?
  CToken t = _scanner->Peek();
  tt = t.GetType();
  while (tt == tPlusMinus || (tt == tAndOr && t.GetValue() == "||")) {
    CToken t;
    CAstExpression *l = n, *r = NULL;
    switch(tt){
      case tPlusMinus:
        Consume(tPlusMinus, &t);
        r = term(s);
        n = new CAstBinaryOp(t, t.GetValue() == "+" ? opAdd : opSub, l, r);
        break;

      case tAndOr:
        Consume(tAndOr, &t);
        r = term(s);
        n = new CAstBinaryOp(t, opOr, l, r);
        break;
    }
    tt = _scanner->Peek().GetType();
  }

  return n;
}

// expression
CAstExpression* CParser::expression(CAstScope* s)
{
  //
  // expression ::= simpleexpr [ relOp simpleexpr ].
  //
  CToken t;
  EOperation relop;
  CAstExpression *left = NULL, *right = NULL;

  // expression -> simpleexpr ...
  left = simpleexpr(s);

  // Does a relative operation exist or not?
  if (_scanner->Peek().GetType() == tRelOp) {
    // expression -> ... [ relOp simpleexpr ]
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=")       relop = opEqual;
    else if (t.GetValue() == "#")  relop = opNotEqual;
    else if (t.GetValue() == "<")  relop = opLessThan;
    else if (t.GetValue() == ">")  relop = opBiggerThan;
    else if (t.GetValue() == "<=") relop = opLessEqual;
    else if (t.GetValue() == ">=") relop = opBiggerEqual;
    else {
      cout << "got " << t << endl;
      SetError(t, "invalid relation.");
    }

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

// number
CAstConstant* CParser::number(void){
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
  long long absv = (v > 0 ? v : (-v));
  if(absv > (1LL << 31)) SetError(t, "integer constant outside valid range.");

  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

pair<bool, long long int> strtobool(string s){
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

// boolean
CAstConstant* CParser::boolean(void){
  //
  // boolean ::= "true" | "false".
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

// character
CAstConstant* CParser::character(void){
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

// string
CAstStringConstant* CParser::stringConst(CAstScope *s){
  //
  // string ::= '"' { character } '"'
  //
  // string is red by one token tString.
  CToken t;

  Consume(tString, &t);

  return new CAstStringConstant(t, t.GetValue(), s);
}
