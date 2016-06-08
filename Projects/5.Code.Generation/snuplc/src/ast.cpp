//------------------------------------------------------------------------------
/// @brief SnuPL abstract syntax tree
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/05/22 Bernhard Egger reimplemented TAC generation
/// 2013/11/04 Bernhard Egger added typechecks for unary '+' operators
/// 2016/03/12 Bernhard Egger adapted to SnuPL/1
/// 2014/04/08 Bernhard Egger assignment 2: AST for SnuPL/-1
///
/// @section license_section License
/// Copyright (c) 2012-2016 Bernhard Egger
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
#include <cassert>
#include <cstring>

#include <typeinfo>

#include "ast.h"
using namespace std;


//------------------------------------------------------------------------------
// CAstNode
//
int CAstNode::_global_id = 0;

CAstNode::CAstNode(CToken token)
  : _token(token), _addr(NULL)
{
  _id = _global_id++;
}

CAstNode::~CAstNode(void)
{
  if (_addr != NULL) delete _addr;
}

int CAstNode::GetID(void) const
{
  return _id;
}

CToken CAstNode::GetToken(void) const
{
  return _token;
}

const CType* CAstNode::GetType(void) const
{
  return CTypeManager::Get()->GetNull();
}

string CAstNode::dotID(void) const
{
  ostringstream out;
  out << "node" << dec << _id;
  return out.str();
}

string CAstNode::dotAttr(void) const
{
  return " [label=\"" + dotID() + "\"]";
}

void CAstNode::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << dotID() << dotAttr() << ";" << endl;
}

CTacAddr* CAstNode::GetTacAddr(void) const
{
  return _addr;
}

ostream& operator<<(ostream &out, const CAstNode &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CAstNode *t)
{
  return t->print(out);
}

//------------------------------------------------------------------------------
// CAstScope
//
CAstScope::CAstScope(CToken t, const string name, CAstScope *parent)
  : CAstNode(t), _name(name), _symtab(NULL), _parent(parent), _statseq(NULL),
    _cb(NULL)
{
  if (_parent != NULL) _parent->AddChild(this);
}

CAstScope::~CAstScope(void)
{
  delete _symtab;
  delete _statseq;
  delete _cb;
}

const string CAstScope::GetName(void) const
{
  return _name;
}

CAstScope* CAstScope::GetParent(void) const
{
  return _parent;
}

size_t CAstScope::GetNumChildren(void) const
{
  return _children.size();
}

CAstScope* CAstScope::GetChild(size_t i) const
{
  assert(i < _children.size());
  return _children[i];
}

CSymtab* CAstScope::GetSymbolTable(void) const
{
  assert(_symtab != NULL);
  return _symtab;
}

void CAstScope::SetStatementSequence(CAstStatement *statseq)
{
  _statseq = statseq;
}

CAstStatement* CAstScope::GetStatementSequence(void) const
{
  return _statseq;
}

bool CAstScope::TypeCheck(CToken *t, string *msg) const
{
  bool res = true;
  try{
    CAstStatement *s = _statseq;
    while(res && (s != NULL)){
      if(!s->TypeCheck(t, msg)) res = false;
      s = s->GetNext();
    }

    vector<CAstScope*>::const_iterator it = _children.begin();
    while(it != _children.end()){
      if(!(*it)->TypeCheck(t, msg)) res = false;
      it++;
    }
  } catch (...) {
    // if(t != NULL) *t = GetToken();
    // if(msg != NULL) *msg = "unexpected error.";
    res = false;
  }

  return res;
}

ostream& CAstScope::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstScope: '" << _name << "'" << endl;
  out << ind << "  symbol table:" << endl;
  _symtab->print(out, indent+4);
  out << ind << "  statement list:" << endl;
  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    do {
      s->print(out, indent+4);
      s = s->GetNext();
    } while (s != NULL);
  } else {
    out << ind << "    empty." << endl;
  }

  out << ind << "  nested scopes:" << endl;
  if (_children.size() > 0) {
    for (size_t i=0; i<_children.size(); i++) {
      _children[i]->print(out, indent+4);
    }
  } else {
    out << ind << "    empty." << endl;
  }
  out << ind << endl;

  return out;
}

void CAstScope::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    string prev = dotID();
    do {
      s->toDot(out, indent);
      out << ind << prev << " -> " << s->dotID() << " [style=dotted];" << endl;
      prev = s->dotID();
      s = s->GetNext();
    } while (s != NULL);
  }

  vector<CAstScope*>::const_iterator it = _children.begin();
  while (it != _children.end()) {
    CAstScope *s = *it++;
    s->toDot(out, indent);
    out << ind << dotID() << " -> " << s->dotID() << ";" << endl;
  }

}

// Just follow reference's implementation
CTacAddr* CAstScope::ToTac(CCodeBlock *cb)
{
  assert(cb != NULL);

  CAstStatement *s = GetStatementSequence();
  while (s != NULL) {
    CTacLabel *next = cb->CreateLabel();
    s->ToTac(cb, next);

    cb->AddInstr(next);
    s = s->GetNext();
  }

  cb->CleanupControlFlow();

  return NULL;
}

CCodeBlock* CAstScope::GetCodeBlock(void) const
{
  return _cb;
}

void CAstScope::SetSymbolTable(CSymtab *st)
{
  if (_symtab != NULL) delete _symtab;
  _symtab = st;
}

void CAstScope::AddChild(CAstScope *child)
{
  _children.push_back(child);
}


//------------------------------------------------------------------------------
// CAstModule
//
CAstModule::CAstModule(CToken t, const string name)
  : CAstScope(t, name, NULL)
{
  SetSymbolTable(new CSymtab());
}

CSymbol* CAstModule::CreateVar(const string ident, const CType *type)
{
  return new CSymGlobal(ident, type);
}

string CAstModule::dotAttr(void) const
{
  return " [label=\"m " + GetName() + "\",shape=box]";
}



//------------------------------------------------------------------------------
// CAstProcedure
//
CAstProcedure::CAstProcedure(CToken t, const string name,
                             CAstScope *parent, CSymProc *symbol)
  : CAstScope(t, name, parent), _symbol(symbol)
{
  assert(GetParent() != NULL);
  SetSymbolTable(new CSymtab(GetParent()->GetSymbolTable()));
  assert(_symbol != NULL);
}

CSymProc* CAstProcedure::GetSymbol(void) const
{
  return _symbol;
}

CSymbol* CAstProcedure::CreateVar(const string ident, const CType *type)
{
  return new CSymLocal(ident, type);
}

const CType* CAstProcedure::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

string CAstProcedure::dotAttr(void) const
{
  return " [label=\"p/f " + GetName() + "\",shape=box]";
}


//------------------------------------------------------------------------------
// CAstType
//
CAstType::CAstType(CToken t, const CType *type)
  : CAstNode(t), _type(type)
{
  assert(type != NULL);
}

const CType* CAstType::GetType(void) const
{
  return _type;
}

ostream& CAstType::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstType (" << _type << ")" << endl;
  return out;
}


//------------------------------------------------------------------------------
// CAstStatement
//
CAstStatement::CAstStatement(CToken token)
  : CAstNode(token), _next(NULL)
{
}

CAstStatement::~CAstStatement(void)
{
  delete _next;
}

void CAstStatement::SetNext(CAstStatement *next)
{
  _next = next;
}

CAstStatement* CAstStatement::GetNext(void) const
{
  return _next;
}

// We just implement ToTacs on below.
CTacAddr* CAstStatement::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatAssign
//
CAstStatAssign::CAstStatAssign(CToken t,
                               CAstDesignator *lhs, CAstExpression *rhs)
  : CAstStatement(t), _lhs(lhs), _rhs(rhs)
{
  assert(lhs != NULL);
  assert(rhs != NULL);
}

CAstDesignator* CAstStatAssign::GetLHS(void) const
{
  return _lhs;
}

CAstExpression* CAstStatAssign::GetRHS(void) const
{
  return _rhs;
}

// Check errors on lhs, rhs of assignment and match their types.
bool CAstStatAssign::TypeCheck(CToken *t, string *msg) const
{
  CAstExpression *lhs = GetLHS();
  CAstExpression *rhs = GetRHS();

  if(!lhs->TypeCheck(t, msg)) return false;
  if(lhs->GetType() == NULL){
    if(t != NULL) *t = GetToken();
    if(msg != NULL) *msg = "invalid types in assignment.";
    return false;
  }
  if(!rhs->TypeCheck(t, msg)) return false;
  if(rhs->GetType() == NULL){
    if(t != NULL) *t = GetToken();
    if(msg != NULL) *msg = "invalid types in assignment.";
    return false;
  }

  // We don;t allow assign from array to array(not ptr).
  // Below here, we used ostringstream to make a proper printing format.
  if(lhs->GetType()->IsArray() || rhs->GetType()->IsArray()){
    if(t != NULL) *t = GetToken();
    ostringstream os;
    os.clear();
    os << "assignments to compound types are not supported.\n";
    os << "  LHS: ";
    lhs->GetType()->print(os, 0);
    os << "\n";
    os << "  RHS: ";
    rhs->GetType()->print(os, 0);
    *msg = os.str().c_str();
    return false;
  }

  if(!lhs->GetType()->Match(rhs->GetType())){
    if(t != NULL) *t = GetToken();
    if(msg != NULL){
      ostringstream os;
      os.clear();
      os << "incompatible types in assignment:\n";
      os << "  LHS: ";
      lhs->GetType()->print(os, 0);
      os << "\n";
      os << "  RHS: ";
      rhs->GetType()->print(os, 0);

      *msg = os.str().c_str();
    }
    return false;
  }

  return true;
}

const CType* CAstStatAssign::GetType(void) const
{
  return _lhs->GetType();
}

ostream& CAstStatAssign::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << ":=" << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  _lhs->print(out, indent+2);
  _rhs->print(out, indent+2);

  return out;
}

string CAstStatAssign::dotAttr(void) const
{
  return " [label=\":=\",shape=box]";
}

void CAstStatAssign::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _lhs->toDot(out, indent);
  out << ind << dotID() << "->" << _lhs->dotID() << ";" << endl;
  _rhs->toDot(out, indent);
  out << ind << dotID() << "->" << _rhs->dotID() << ";" << endl;
}

CTacAddr* CAstStatAssign::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  // assert it.
  assert(cb != NULL);
  assert(next != NULL);
  CTacAddr* src = NULL;

  //Just call _rhs->ToTac(cb) even if it is CAstBinaryOp.
  src = _rhs->ToTac(cb);

  cb->AddInstr(new CTacInstr(opAssign, _lhs->ToTac(cb), src, NULL));
  cb->AddInstr(new CTacInstr(opGoto, next));

  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatCall
//
CAstStatCall::CAstStatCall(CToken t, CAstFunctionCall *call)
  : CAstStatement(t), _call(call)
{
  assert(call != NULL);
}

CAstFunctionCall* CAstStatCall::GetCall(void) const
{
  return _call;
}

// Since this class is wrapper of CAstFunctionCall, just perform into it.
bool CAstStatCall::TypeCheck(CToken *t, string *msg) const
{
  return GetCall()->TypeCheck(t, msg);
}

ostream& CAstStatCall::print(ostream &out, int indent) const
{
  _call->print(out, indent);

  return out;
}

string CAstStatCall::dotID(void) const
{
  return _call->dotID();
}

string CAstStatCall::dotAttr(void) const
{
  return _call->dotAttr();
}

void CAstStatCall::toDot(ostream &out, int indent) const
{
  _call->toDot(out, indent);
}

CTacAddr* CAstStatCall::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  assert(cb != NULL);
  assert(next != NULL);

  // CAstAtatCall is just a wrapper of _call(type CAstFunctionCall*).
  GetCall()->ToTac(cb);
  cb->AddInstr(new CTacInstr(opGoto, next));
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatReturn
//
CAstStatReturn::CAstStatReturn(CToken t, CAstScope *scope, CAstExpression *expr)
  : CAstStatement(t), _scope(scope), _expr(expr)
{
  assert(scope != NULL);
}

CAstScope* CAstStatReturn::GetScope(void) const
{
  return _scope;
}

CAstExpression* CAstStatReturn::GetExpression(void) const
{
  return _expr;
}

// match two types: function/procedure return type, type of expression of return
bool CAstStatReturn::TypeCheck(CToken *t, string *msg) const
{
  const CType *st = GetScope()->GetType();
  CAstExpression *e = GetExpression();

  if(st->Match(CTypeManager::Get()->GetNull())){
    if(e != NULL){
      if(t != NULL) *t = e->GetToken();
      if(msg != NULL) *msg = "superfluous expression after return.";
      return false;
    }
  }
  else{
    if(e == NULL){
      if(t != NULL) *t = GetToken();
      if(msg != NULL) *msg = "expression expected after return.";
      return false;
    }

    if(!e->TypeCheck(t, msg)) return false;

    if(!st->Match(e->GetType())){
      if(t != NULL) *t = e->GetToken();
      ostringstream os;
      os.clear();
      os << "return type mismatch." << '\n';
      st->print(os, 0);
      os  << " type expected, but it returns ";
      if(e->GetType() == NULL) os << "<INVALID>" << '\n';
      else {
        e->GetType()->print(os, 0);
        os << '\n';
      }
      if(msg != NULL) *msg = os.str().c_str();
      return false;
    }
  }

  return true;
}

const CType* CAstStatReturn::GetType(void) const
{
  const CType *t = NULL;

  if (GetExpression() != NULL) {
    t = GetExpression()->GetType();
  } else {
    t = CTypeManager::Get()->GetNull();
  }

  return t;
}

ostream& CAstStatReturn::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "return" << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  if (_expr != NULL) _expr->print(out, indent+2);

  return out;
}

string CAstStatReturn::dotAttr(void) const
{
  return " [label=\"return\",shape=box]";
}

void CAstStatReturn::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  if (_expr != NULL) {
    _expr->toDot(out, indent);
    out << ind << dotID() << "->" << _expr->dotID() << ";" << endl;
  }
}

CTacAddr* CAstStatReturn::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  assert(cb != NULL);
  assert(next != NULL);

  // Just calculate its operand if exists.
  if(_expr == NULL){
    cb->AddInstr(new CTacInstr(opReturn, NULL, NULL));
  }
  else{
    CTacAddr* oper = _expr->ToTac(cb);
    cb->AddInstr(new CTacInstr(opReturn, NULL, oper));
  }
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatIf
//
CAstStatIf::CAstStatIf(CToken t, CAstExpression *cond,
                       CAstStatement *ifBody, CAstStatement *elseBody)
  : CAstStatement(t), _cond(cond), _ifBody(ifBody), _elseBody(elseBody)
{
  assert(cond != NULL);
}

CAstExpression* CAstStatIf::GetCondition(void) const
{
  return _cond;
}

CAstStatement* CAstStatIf::GetIfBody(void) const
{
  return _ifBody;
}

CAstStatement* CAstStatIf::GetElseBody(void) const
{
  return _elseBody;
}

// Check validity of ifcond/truestat/falsestat and w.o.n the type of ifcond
// matches with boolean type.
bool CAstStatIf::TypeCheck(CToken *t, string *msg) const
{
  CAstExpression *cond = GetCondition();

  if(!cond->TypeCheck(t, msg)) return false;
  if(!cond->GetType()->Match(CTypeManager::Get()->GetBool())){
    if(t != NULL) *t = cond->GetToken();
    if(msg != NULL) *msg = "boolean expression expected.";
    return false;
  }

  CAstStatement *ifBody = GetIfBody();
  CAstStatement *elseBody = GetElseBody();

  while(ifBody){
    if(!ifBody->TypeCheck(t, msg)){
      return false;
    }
    ifBody = ifBody->GetNext();
  }
  while(elseBody){
    if(!elseBody->TypeCheck(t, msg)){
      return false;
    }
    elseBody = elseBody->GetNext();
  }

  return true;
}

ostream& CAstStatIf::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "if cond" << endl;
  _cond->print(out, indent+2);
  out << ind << "if-body" << endl;
  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;
  out << ind << "else-body" << endl;
  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;

  return out;
}

string CAstStatIf::dotAttr(void) const
{
  return " [label=\"if\",shape=box]";
}

void CAstStatIf::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }

  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatIf::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  assert(cb != NULL);
  assert(next != NULL);

  // make labels and calculate condition.
  CTacLabel *if_true = cb->CreateLabel("if_true");
  CTacLabel *if_false = cb->CreateLabel("if_false");
  _cond->ToTac(cb, if_true, if_false);

  // calculate if-body if it is non-empty.
  cb->AddInstr(if_true);
  if(_ifBody != NULL){
    CAstStatement *s = _ifBody;
    do{
      CTacLabel *nxt = cb->CreateLabel();
      s->ToTac(cb, nxt);
      cb->AddInstr(nxt);
      s = s->GetNext();
    } while(s != NULL);
  }
  cb->AddInstr(new CTacInstr(opGoto, next));

  // calculate else-body if it is non-empty.
  cb->AddInstr(if_false);
  if(_elseBody != NULL){
    CAstStatement *s = _elseBody;
    do{
      CTacLabel *nxt = cb->CreateLabel();
      s->ToTac(cb, nxt);
      cb->AddInstr(nxt);
      s = s->GetNext();
    } while(s != NULL);
  }

  cb->AddInstr(new CTacInstr(opGoto, next));

  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatWhile
//
CAstStatWhile::CAstStatWhile(CToken t,
                             CAstExpression *cond, CAstStatement *body)
  : CAstStatement(t), _cond(cond), _body(body)
{
  assert(cond != NULL);
}

CAstExpression* CAstStatWhile::GetCondition(void) const
{
  return _cond;
}

CAstStatement* CAstStatWhile::GetBody(void) const
{
  return _body;
}

// check validity of whilecond/whilebody, and match whilecond type with boolean
bool CAstStatWhile::TypeCheck(CToken *t, string *msg) const
{
  CAstExpression *cond = GetCondition();

  if(!cond->TypeCheck(t, msg)) return false;
  if(!cond->GetType()->Match(CTypeManager::Get()->GetBool())){
    if(t != NULL) *t = cond->GetToken();
    if(msg != NULL) *msg = "boolean expression expected.";
    return false;
  }
  CAstStatement *body = GetBody();
  while(body){
    if(!body->TypeCheck(t, msg)){
      return false;
    }
    body = body->GetNext();
  }

  return true;
}

ostream& CAstStatWhile::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "while cond" << endl;
  _cond->print(out, indent+2);
  out << ind << "while-body" << endl;
  if (_body != NULL) {
    CAstStatement *s = _body;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  }
  else out << ind << "  empty." << endl;

  return out;
}

string CAstStatWhile::dotAttr(void) const
{
  return " [label=\"while\",shape=box]";
}

void CAstStatWhile::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_body != NULL) {
    CAstStatement *s = _body;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatWhile::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  assert(cb != NULL);
  assert(next != NULL);

  // make labels and calculate condition.
  CTacLabel *while_cond = cb->CreateLabel("while_cond");
  CTacLabel *while_body = cb->CreateLabel("while_body");
  cb->AddInstr(while_cond);
  _cond->ToTac(cb, while_body, next);

  // calculate while body.
  cb->AddInstr(while_body);
  if(_body != NULL){
    CAstStatement *s = _body;
    do{
      CTacLabel *nxt = cb->CreateLabel();
      s->ToTac(cb, nxt);
      cb->AddInstr(nxt);
      s = s->GetNext();
    } while(s != NULL);
  }

  cb->AddInstr(new CTacInstr(opGoto, while_cond));

  return NULL;
}


//------------------------------------------------------------------------------
// CAstExpression
//
CAstExpression::CAstExpression(CToken t)
  : CAstNode(t)
{
}

CTacAddr* CAstExpression::ToTac(CCodeBlock *cb)
{
  return NULL;
}

CTacAddr* CAstExpression::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstOperation
//
CAstOperation::CAstOperation(CToken t, EOperation oper)
  : CAstExpression(t), _oper(oper)
{
}

EOperation CAstOperation::GetOperation(void) const
{
  return _oper;
}


//------------------------------------------------------------------------------
// CAstBinaryOp
//
CAstBinaryOp::CAstBinaryOp(CToken t, EOperation oper,
                           CAstExpression *l,CAstExpression *r)
  : CAstOperation(t, oper), _left(l), _right(r)
{
  // these are the only binary operation we support for now
  assert((oper == opAdd)        || (oper == opSub)         ||
         (oper == opMul)        || (oper == opDiv)         ||
         (oper == opAnd)        || (oper == opOr)          ||
         (oper == opEqual)      || (oper == opNotEqual)    ||
         (oper == opLessThan)   || (oper == opLessEqual)   ||
         (oper == opBiggerThan) || (oper == opBiggerEqual)
        );
  assert(l != NULL);
  assert(r != NULL);
}

CAstExpression* CAstBinaryOp::GetLeft(void) const
{
  return _left;
}

CAstExpression* CAstBinaryOp::GetRight(void) const
{
  return _right;
}

// check validity of lhs and rhs, match types of lhs, rhs, and
// match that type with the return type of given operation.
bool CAstBinaryOp::TypeCheck(CToken *t, string *msg) const
{
  CAstExpression *lhs = GetLeft();
  CAstExpression *rhs = GetRight();
  EOperation oper = GetOperation();

  if(!lhs->TypeCheck(t, msg)) return false;
  if(!rhs->TypeCheck(t, msg)) return false;

  if(oper == opAdd || oper == opSub || oper == opMul || oper == opDiv){
    if(!lhs->GetType()->Match(CTypeManager::Get()->GetInt()) || !rhs->GetType()->Match(CTypeManager::Get()->GetInt())){
      if(t != NULL) *t = GetToken();
      ostringstream os;
      os.clear();
      if(oper == opAdd) os << "add: ";
      if(oper == opSub) os << "sub: ";
      if(oper == opMul) os << "mul: ";
      if(oper == opDiv) os << "div: ";
      os << "type mismatch.";
      os << '\n' << "  left  operand: ";
      lhs->GetType()->print(os, 0);
      os << '\n' << "  right operand: ";
      rhs->GetType()->print(os, 0);

      if(msg != NULL) *msg = os.str().c_str();
      return false;
    }
  }
  else if(oper == opLessThan || oper == opLessEqual || oper == opBiggerThan || oper == opBiggerEqual){
    if((!lhs->GetType()->Match(CTypeManager::Get()->GetInt()) && !lhs->GetType()->Match(CTypeManager::Get()->GetChar()))
        || (!rhs->GetType()->Match(CTypeManager::Get()->GetInt()) && !rhs->GetType()->Match(CTypeManager::Get()->GetChar()))
        || !lhs->GetType()->Match(rhs->GetType())){
      if(t != NULL) *t = GetToken();
      ostringstream os;
      os.clear();
      if(oper == opLessThan) os << "<: ";
      if(oper == opLessEqual) os << "<=: ";
      if(oper == opBiggerThan) os << ">: ";
      if(oper == opBiggerEqual) os << ">=: ";
      os << "type mismatch.";
      os << '\n' << "  left  operand: ";
      lhs->GetType()->print(os, 0);
      os << '\n' << "  right operand: ";
      rhs->GetType()->print(os, 0);

      if(msg != NULL) *msg = os.str().c_str();
      return false;
    }
  }
  else if(oper == opAnd || oper == opOr){
    if(!lhs->GetType()->Match(CTypeManager::Get()->GetBool()) || !rhs->GetType()->Match(CTypeManager::Get()->GetBool())){
      if(t != NULL) *t = GetToken();
      ostringstream os;
      os.clear();
      if(oper == opAnd) os << "and: ";
      if(oper == opOr) os << "or: ";
      os << "type mismatch.";
      os << '\n' << "  left  operand: ";
      lhs->GetType()->print(os, 0);
      os << '\n' << "  right operand: ";
      rhs->GetType()->print(os, 0);

      if(msg != NULL) *msg = os.str().c_str();
      return false;
    }
  }
  else{
    if(!lhs->GetType()->IsScalar() || lhs->GetType()->IsPointer()
        || !rhs->GetType()->IsScalar() || rhs->GetType()->IsPointer()
        || !lhs->GetType()->Match(rhs->GetType())){
      if(t != NULL) *t = GetToken();
      ostringstream os;
      os.clear();
      if(oper == opEqual) os << "=: ";
      if(oper == opNotEqual) os << "#: ";
      os << "type mismatch.";
      os << '\n' << "  left  operand: ";
      lhs->GetType()->print(os, 0);
      os << '\n' << "  right operand: ";
      rhs->GetType()->print(os, 0);

      if(msg != NULL) *msg = os.str().c_str();
      return false;
    }
  }
  return true;
}

// binary operators return int(arithmetics) or bool(and/or) type.
const CType* CAstBinaryOp::GetType(void) const
{
  EOperation _oper = GetOperation();
  // +, -, *, /
  if(_oper == opAdd || _oper == opSub || _oper == opMul || _oper == opDiv){
    return CTypeManager::Get()->GetInt();
  }
  else{ // &&, ||, =, #, <, <=, >, >=
    return CTypeManager::Get()->GetBool();
  }
}

ostream& CAstBinaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  _left->print(out, indent+2);
  _right->print(out, indent+2);

  return out;
}

string CAstBinaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstBinaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _left->toDot(out, indent);
  out << ind << dotID() << "->" << _left->dotID() << ";" << endl;
  _right->toDot(out, indent);
  out << ind << dotID() << "->" << _right->dotID() << ";" << endl;
}

CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb)
{
  assert(cb != NULL);

  CTacAddr* _addr = NULL;

  // check whether or not it is a boolean type.
  // If it is, calculate boolean value and put it into the temporary variable.
  if(GetType()->Compare(CTypeManager::Get()->GetBool())){

    // make labels including next label and calculate
    // condition part(that is, a value of this operation itself.)
    CTacLabel *ltrue = cb->CreateLabel();
    CTacLabel *lfalse = cb->CreateLabel();
    CTacLabel *lnext = cb->CreateLabel();

    ToTac(cb, ltrue, lfalse);

    _addr = cb->CreateTemp(CTypeManager::Get()->GetBool());

    // Fill true-body to assign true into the temporary variable.
    cb->AddInstr(ltrue);
    cb->AddInstr(new CTacInstr(opAssign, _addr, new CTacConst(1), NULL));
    cb->AddInstr(new CTacInstr(opGoto, lnext));

    // Fill false-body to assign false into the temporary variable.
    cb->AddInstr(lfalse);
    cb->AddInstr(new CTacInstr(opAssign, _addr, new CTacConst(0), NULL));

    cb->AddInstr(lnext);
  }
  else{
    // Just calculate left and right and combine by the operation.
    CTacAddr *src1 = _left->ToTac(cb);
    CTacAddr *src2 = _right->ToTac(cb);
    EOperation _op = GetOperation();

    _addr = cb->CreateTemp(GetType());

    cb->AddInstr(new CTacInstr(_op, _addr, src1, src2));
  }

  return _addr;
}

CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb,
                              CTacLabel *ltrue, CTacLabel *lfalse)
{
  assert(cb != NULL);
  assert(ltrue != NULL);
  assert(lfalse != NULL);
  assert(GetType()->Compare(CTypeManager::Get()->GetBool()));

  // make next label.
  CTacLabel *nxt = cb->CreateLabel();
  EOperation _op = GetOperation();

  // And
  if(_op == opAnd){
    _left->ToTac(cb, nxt, lfalse);
    cb->AddInstr(nxt);
    _right->ToTac(cb, ltrue, lfalse);
  }

  // Or
  else if(_op == opOr){
    _left->ToTac(cb, ltrue, nxt);
    cb->AddInstr(nxt);
    _right->ToTac(cb, ltrue, lfalse);
  }

  // Others can be done by just calculating it.
  else{
    CTacAddr *src1 = _left->ToTac(cb);
    CTacAddr *src2 = _right->ToTac(cb);
    cb->AddInstr(new CTacInstr(_op, ltrue, src1, src2));
    cb->AddInstr(new CTacInstr(opGoto, lfalse));
 }

  return NULL;
}


//------------------------------------------------------------------------------
// CAstUnaryOp
//
CAstUnaryOp::CAstUnaryOp(CToken t, EOperation oper, CAstExpression *e)
  : CAstOperation(t, oper), _operand(e)
{
  assert((oper == opNeg) || (oper == opPos) || (oper == opNot));
  assert(e != NULL);
}

CAstExpression* CAstUnaryOp::GetOperand(void) const
{
  return _operand;
}

// check validity of expression next to the unary operation
// and match this type with the return type of the given operation
bool CAstUnaryOp::TypeCheck(CToken *t, string *msg) const
{
  EOperation oper = GetOperation();
  CAstExpression *e = GetOperand();

  if(!e->TypeCheck(t, msg)){
    if(oper == opNeg && dynamic_cast<CAstConstant*>(e) != NULL){
      // valid
    }
    else return false;
  }

  if(oper == opNot){
    if(!e->GetType()->Match(CTypeManager::Get()->GetBool())){
      if(t != NULL) *t = GetToken();
      ostringstream os;
      os.clear();
      os << "not: type mismatch.";
      os << '\n' << "  operand:       ";
      e->GetType()->print(os, 0);
      os << '\n';
      if(msg != NULL) *msg = os.str().c_str();
      return false;
    }
  }
  else if(oper == opPos || oper == opNeg){
    if(!e->GetType()->Match(CTypeManager::Get()->GetInt())){
      if(t != NULL) *t = GetToken();
      ostringstream os;
      os.clear();
      if(oper == opPos) os << "pos: ";
      else os << "neg: ";
      os << "type mismatch.";
      os << '\n' << "  operand:       ";
      e->GetType()->print(os, 0);
      os << '\n';
      if(msg != NULL) *msg = os.str().c_str();
      return false;
    }
  }
  return true;
}

// only not returns bool, otherwise int
const CType* CAstUnaryOp::GetType(void) const
{
  // !
  EOperation _oper = GetOperation();
  if(_oper == opNot){
    return CTypeManager::Get()->GetBool();
  }
  else{ // +, -
    return CTypeManager::Get()->GetInt();
  }
}

ostream& CAstUnaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  _operand->print(out, indent+2);

  return out;
}

string CAstUnaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstUnaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb)
{
  assert(cb != NULL);
  CTacAddr* _addr = NULL;

  // Similary, if unary's type is boolean, calculate it and push the actual
  // value of it.
  if(GetType()->Compare(CTypeManager::Get()->GetBool())){

    // Make labels
    CTacLabel *ltrue = cb->CreateLabel();
    CTacLabel *lfalse = cb->CreateLabel();
    CTacLabel *lnext = cb->CreateLabel();
    ToTac(cb, ltrue, lfalse);
    _addr = cb->CreateTemp(CTypeManager::Get()->GetBool());

    // Make true-body
    cb->AddInstr(ltrue);
    cb->AddInstr(new CTacInstr(opAssign, _addr, new CTacConst(1), NULL));
    cb->AddInstr(new CTacInstr(opGoto, lnext));

    // Make false-body
    cb->AddInstr(lfalse);
    cb->AddInstr(new CTacInstr(opAssign, _addr, new CTacConst(0), NULL));
    cb->AddInstr(new CTacInstr(opGoto, lnext));

    cb->AddInstr(lnext);
  }
  else{
    // Just calculate it.
    CTacAddr *src = _operand->ToTac(cb);
    EOperation _op = GetOperation();

    CTacConst* c = dynamic_cast<CTacConst*>(src);
    if(c != NULL){
      int cval = c->GetValue();
      if(_op == opNeg) cval = -cval;
      return new CTacConst(cval);
    }

    _addr = cb->CreateTemp(GetType());
    cb->AddInstr(new CTacInstr(_op, _addr, src, NULL));

  }

  return _addr;
}

CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb,
                             CTacLabel *ltrue, CTacLabel *lfalse)
{
  assert(cb != NULL);
  assert(ltrue != NULL);
  assert(lfalse != NULL);
  assert(GetType()->Compare(CTypeManager::Get()->GetBool()));

  // Since we have the only instruction Not as a boolean type,
  // it's operand must have a type boolean.
  assert(_operand->GetType()->Compare(CTypeManager::Get()->GetBool()));

  // Just swap true-label and false-label and calculate operand.
  _operand->ToTac(cb, lfalse, ltrue);
  return NULL;
}


//------------------------------------------------------------------------------
// CAstSpecialOp
//
CAstSpecialOp::CAstSpecialOp(CToken t, EOperation oper, CAstExpression *e,
                             const CType *type)
  : CAstOperation(t, oper), _operand(e), _type(type)
{
  assert((oper == opAddress) || (oper == opDeref) || (oper = opCast));
  assert(e != NULL);
  assert(((oper != opCast) && (type == NULL)) ||
         ((oper == opCast) && (type != NULL)));
}

CAstExpression* CAstSpecialOp::GetOperand(void) const
{
  return _operand;
}

// Just check validity of its operand. If operation were opDeref, operand must
// be a pointer.
bool CAstSpecialOp::TypeCheck(CToken *t, string *msg) const
{
  EOperation _oper = GetOperation();
  if(!_operand->TypeCheck(t, msg))
    return false;
  if(_oper == opDeref){
    if(!_operand->GetType() || !_operand->GetType()->IsPointer()){
      if(t != NULL) *t = _operand->GetToken();
      if(msg) *msg = "dereferencing non-pointer type is invalid.";
      return false;
    }
  }
  return true;
}

// Address operator returns pointer to it and it will resolve address to take
// real value. Dereferencing deletes one pointer wrapper. Otherwise, return the
// new(forced) type which is given.
const CType* CAstSpecialOp::GetType(void) const
{
  EOperation _oper = GetOperation();
  if(_oper == opAddress){
    return CTypeManager::Get()->GetPointer(GetOperand()->GetType());
  }
  else if(_oper == opDeref){
    if(GetOperand()->GetType()->IsPointer()){
      return dynamic_cast<const CPointerType*>(GetOperand()->GetType())->GetBaseType();
    }
    else return NULL;
  }
  else return _type;
}

ostream& CAstSpecialOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  _operand->print(out, indent+2);

  return out;
}

string CAstSpecialOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstSpecialOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

// Note that SpecialOp only acts on designators.
// Therefore we have to recognize when we have to attach reference operator.
// Also, we can only use opAddress.
// opDeref and opCast cannot be generated by correct SnuPL/1 source program.
// So we only perform opAddress case.
CTacAddr* CAstSpecialOp::ToTac(CCodeBlock *cb)
{

  assert(cb != NULL);

  EOperation _op = GetOperation();
  CTacAddr *src = NULL;
  CTacAddr* _addr = NULL;

  // Just give its type.
  src = _operand->ToTac(cb);
  _addr = cb->CreateTemp(CTypeManager::Get()->GetPointer(GetOperand()->GetType()));
  cb->AddInstr(new CTacInstr(opAddress, _addr, src, NULL));

  return _addr;
}


//------------------------------------------------------------------------------
// CAstFunctionCall
//
CAstFunctionCall::CAstFunctionCall(CToken t, const CSymProc *symbol)
  : CAstExpression(t), _symbol(symbol)
{
  assert(symbol != NULL);
}

const CSymProc* CAstFunctionCall::GetSymbol(void) const
{
  return _symbol;
}

void CAstFunctionCall::AddArg(CAstExpression *arg)
{
  _arg.push_back(arg);
}

int CAstFunctionCall::GetNArgs(void) const
{
  return (int)_arg.size();
}

CAstExpression* CAstFunctionCall::GetArg(int index) const
{
  assert((index >= 0) && (index < _arg.size()));
  return _arg[index];
}

// Many points have to be checked.
bool CAstFunctionCall::TypeCheck(CToken *t, string *msg) const
{
  const CSymProc *symbol = GetSymbol();
  int nArgs = GetNArgs();
  int nRealArgs = symbol->GetNParams();

  // Check Argument #
  if(nArgs > nRealArgs){
    if(t != NULL) *t = GetToken();
    if(msg != NULL) *msg = "too many arguments.";
    return false;
  }

  if(nArgs < nRealArgs){
    if(t != NULL) *t = GetToken();
    if(msg != NULL) *msg = "not enough arguments.";
    return false;
  }

  // Check types of each argument
  // We gave pointer wrapper to all parameter except primitive types. So it
  // encloses array as a pointer to array, and then perform type checking using
  // their basetypes.
  for(int i = 0; i < nArgs; i++){
    if(!GetArg(i)->TypeCheck(t, msg)) return false;

    const CType* exType = symbol->GetParam(i)->GetDataType();
    const CType* gotType = GetArg(i)->GetType();

    const CType* gotPointedType = (gotType->IsArray() ? CTypeManager::Get()->GetPointer(gotType) : gotType);

    ostringstream os;

    if(!exType->Match(gotPointedType)){
      if(t != NULL) *t = GetArg(i)->GetToken();
      ostringstream os;
      os.clear();
      os << "parameter " << (i + 1) << ": argument type mismatch.";
      os << '\n' << "  expected ";
      exType->print(os, 0);
      os << "\n";
      os << "  got      ";
      gotPointedType->print(os, 0);
      if(msg != NULL) *msg = os.str().c_str();
      return false;
    }
  }
  return true;
}

// DataType contains its return type.
const CType* CAstFunctionCall::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

ostream& CAstFunctionCall::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "call " << _symbol << " ";
  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  for (size_t i=0; i<_arg.size(); i++) {
    _arg[i]->print(out, indent+2);
  }

  return out;
}

string CAstFunctionCall::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"call " << _symbol->GetName() << "\",shape=box]";
  return out.str();
}

void CAstFunctionCall::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i=0; i<_arg.size(); i++) {
    _arg[i]->toDot(out, indent);
    out << ind << dotID() << "->" << _arg[i]->dotID() << ";" << endl;
  }
}

CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb)
{
  assert(cb != NULL);
  const CType* ret = GetType();

  // Since assembly puts argument in the reverse order(since it uses reversed stack),
  // we have to traverse the loop from the last parameter.
  int nArgs = GetNArgs();
  for(int i = nArgs - 1; i >= 0; i--){
    CTacAddr *argdst = NULL;
    CTacAddr *temp = NULL;
    // We performed attaching CTacReference on the variable on the other ToTacs.
    // We don't have to care about the type of the given parameter whether or
    // not it is enclosed by CTacReference.
    argdst = GetArg(i)->ToTac(cb);
    cb->AddInstr(new CTacInstr(opParam, new CTacConst(i), argdst));
  }

  // Just put destination if the return type is not NULL.
  CTacAddr *dst = (ret->IsNull() ? NULL : cb->CreateTemp(GetType()));
  cb->AddInstr(new CTacInstr(opCall, dst, new CTacName(_symbol)));

  return dst;
}

// First calculate its value and put it into the temp symbol.
// Here, since this temp symbol itself must be a designator,
// we just treat as we do on CAstDesignator::ToTac(cb, ltrue, lfalse).
CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb,
                                  CTacLabel *ltrue, CTacLabel *lfalse)
{
  assert(cb != NULL);
  assert(ltrue != NULL);
  assert(lfalse != NULL);
  assert(GetType()->Compare(CTypeManager::Get()->GetBool()));

  // Just compare the calculated value with true.
  cb->AddInstr(new CTacInstr(opEqual, ltrue, ToTac(cb), new CTacConst(1)));
  cb->AddInstr(new CTacInstr(opGoto, lfalse));
  return NULL;
}



//------------------------------------------------------------------------------
// CAstOperand
//
CAstOperand::CAstOperand(CToken t)
  : CAstExpression(t)
{
}


//------------------------------------------------------------------------------
// CAstDesignator
//
CAstDesignator::CAstDesignator(CToken t, const CSymbol *symbol)
  : CAstOperand(t), _symbol(symbol)
{
  assert(symbol != NULL);
}

const CSymbol* CAstDesignator::GetSymbol(void) const
{
  return _symbol;
}

// It is just a simple designator. If it has to return false, it is an error on
// parsing, not on type checking.
bool CAstDesignator::TypeCheck(CToken *t, string *msg) const
{
  if(GetType() == NULL || GetType()->IsNull()){
    if(t) *t = GetToken();
    if(msg) *msg = "invalid designator type.";
    return false;
  }
  return true;
}

const CType* CAstDesignator::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

ostream& CAstDesignator::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstDesignator::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << _symbol->GetName();
  out << "\",shape=ellipse]";
  return out.str();
}

void CAstDesignator::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);
}

CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb)
{
  assert(cb != NULL);

  // Designator is just a symbol.
  // Get symbol and return it by wrapping CTacName.
  return new CTacName(GetSymbol());
}

// This function can be called only if it itself is in condition statement.
// here, since we already computed its value on ToTac(cb), just bring it and
// compare with true.
CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  assert(cb != NULL);
  assert(ltrue != NULL);
  assert(lfalse != NULL);
  assert(GetType()->Compare(CTypeManager::Get()->GetBool()));

  // Just calculate its value and compare with true.
  cb->AddInstr(new CTacInstr(opEqual, ltrue, ToTac(cb), new CTacConst(1)));
  cb->AddInstr(new CTacInstr(opGoto, lfalse));
  return NULL;
}


//------------------------------------------------------------------------------
// CAstArrayDesignator
//
CAstArrayDesignator::CAstArrayDesignator(CToken t, const CSymbol *symbol)
  : CAstDesignator(t, symbol), _done(false), _offset(NULL)
{
}

void CAstArrayDesignator::AddIndex(CAstExpression *idx)
{
  assert(!_done);
  _idx.push_back(idx);
}

void CAstArrayDesignator::IndicesComplete(void)
{
  assert(!_done);
  _done = true;
}

int CAstArrayDesignator::GetNIndices(void) const
{
  return (int)_idx.size();
}

CAstExpression* CAstArrayDesignator::GetIndex(int index) const
{
  assert((index >= 0) && (index < _idx.size()));
  return _idx[index];
}

// Check its indices are the valid numbers. If the parameter was open, no
// problem. But, if such indices are not closed, we must check its actual value.
bool CAstArrayDesignator::TypeCheck(CToken *t, string *msg) const
{
  assert(_done);

  if(GetType() == NULL){
    if(t != NULL) *t = GetToken();
    if(msg != NULL) *msg = "invalid array expression.";
    return false;
  }

  // This is found after we start 5th phase, but we cannot find this on 4th phase.
  // If you cut our score at the 3rd phasebecause of this problem,
  // we will claim by this evidence.
  int nIndices = GetNIndices();
  const CType *originType = GetSymbol()->GetDataType();
  if(originType->IsPointer()){
    originType = dynamic_cast<const CPointerType*>(originType)->GetBaseType();
  }

  if(nIndices != (dynamic_cast<const CArrayType*>(originType))->GetNDim()){
    if(t != NULL) *t = GetToken();
    if(msg != NULL) *msg = "incomplete array expression (sub-arrays are not supported).";
    return false;
  }


  for(int i = 0; i < nIndices; i++){
    if(!GetIndex(i)->TypeCheck(t, msg)) return false;
    if(!GetIndex(i)->GetType()->Match(CTypeManager::Get()->GetInt())){
      if(t != NULL) *t = GetIndex(i)->GetToken();
      if(msg != NULL) *msg = "invalid array index expression.";
      return false;
    }
  }

  return true;
}
// Make its dimension lower when each indices are detected. If it opened indices
// too many, it is an error. We also accept pointer to array, by removing
// wrapper(pointer).
const CType* CAstArrayDesignator::GetType(void) const
{
  const CType *originType = GetSymbol()->GetDataType();
  if(originType->IsPointer()){
    originType = dynamic_cast<const CPointerType*>(originType)->GetBaseType();
  }
  if(!originType->IsArray()) return NULL;

  const CType *ret = originType;

  if(GetNIndices() > dynamic_cast<const CArrayType*>(ret)->GetNDim() ){
    return NULL;
  }

  for(int i=0; i<GetNIndices(); i++){
    if(!ret->IsArray()){ return NULL; break; }
    ret = dynamic_cast<const CArrayType*>(ret)->GetInnerType();
  }

  return ret;
}

ostream& CAstArrayDesignator::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  for (size_t i=0; i<_idx.size(); i++) {
    _idx[i]->print(out, indent+2);
  }

  return out;
}

string CAstArrayDesignator::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << _symbol->GetName() << "[]\",shape=ellipse]";
  return out.str();
}

void CAstArrayDesignator::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i=0; i<_idx.size(); i++) {
    _idx[i]->toDot(out, indent);
    out << ind << dotID() << "-> " << _idx[i]->dotID() << ";" << endl;
  }
}

CTacAddr* CAstArrayDesignator::ToTac(CCodeBlock *cb)
{
  assert(cb != NULL);

  // If the array is not a pointer,
  // since we have to use its address, enclose it with Address operator.
  int nIndices = GetNIndices();
  CTacAddr* t0 = NULL;
  if(!(GetSymbol()->GetDataType()->IsPointer())){
    t0 = cb->CreateTemp(CTypeManager::Get()->GetPointer(GetSymbol()->GetDataType()));
    cb->AddInstr(new CTacInstr(opAddress, t0, new CTacName(GetSymbol()), NULL));
  }
  else{
    t0 = new CTacName(GetSymbol());
  }

  // Actually, its # of indice must exceed 0,
  // but we used this to make it more clearly.
  CTacAddr *offset = NULL;
  if(nIndices > 0){
    // if we use b[i] when we defined b as b[I][J][K],
    // reference_ir treats it as b[i][0][0](...)....

    // DIM iterate to compute offset

    CTacAddr *t = NULL;
    int nDim = 0;
    // Get DIM
    if(GetSymbol()->GetDataType()->IsPointer()){
      nDim = dynamic_cast<const CArrayType*>(dynamic_cast<const CPointerType*>(GetSymbol()->GetDataType())->GetBaseType())->GetNDim();
    }
    else{
      nDim = dynamic_cast<const CArrayType*>(GetSymbol()->GetDataType())->GetNDim();
    }

    if(nIndices != nDim){
    }
    // Run the loop by nDim. Even if its # indice is strictly smallet than dim.,
    // SnuPL/1 calculates it as full-indiced: by padding '0' index on the rest
    // of the indices.
    for(int i = 1; i <= nDim; i++){
      CTacTemp *t3 = NULL;
      if(i < nDim){
        CTacAddr* t4 = NULL;
        // If dimension is also 1, we only have information about the current index.
        // So we must compute this before we call DIM of the array.
        if(i == 1){
          t4 = GetIndex(i-1)->ToTac(cb);
        }
        // Take next dimension

        // put next dimension's index as second parameter of DIM
        cb->AddInstr(new CTacInstr(opParam, new CTacConst(1), new CTacConst(i+1)) );
        CTacAddr *t1 = NULL;

        // If it is not a pointer, wrap by address and use it as a parameter.
        // Otherwise, just use raw array as a parameter.
        if(!((GetSymbol()->GetDataType())->IsPointer())){
          t1 = cb->CreateTemp(CTypeManager::Get()->GetPointer(GetSymbol()->GetDataType()));
          cb->AddInstr(new CTacInstr(opAddress, t1, new CTacName(GetSymbol()), NULL));
          cb->AddInstr(new CTacInstr(opParam, new CTacConst(0), t1));
        }
        else{
          t1 = new CTacName(GetSymbol());
          cb->AddInstr(new CTacInstr(opParam, new CTacConst(0), t1));
        }

        // Call DIM
        CTacTemp *t2 = cb->CreateTemp(CTypeManager::Get()->GetInt());
        cb->AddInstr(new CTacInstr(opCall, t2,
              new CTacName(cb->GetOwner()->GetSymbolTable()->FindSymbol("DIM", sGlobal))));

        t3 = cb->CreateTemp(CTypeManager::Get()->GetInt());
        // If we are calculating on the first index,
        // we don't have any informations about the offset,
        // so we bring the calculated tac of the current index and put it.
        // this condition is identical to before, but we assert this for safety.
        if(i == 1){
          assert(t4 != NULL);
          cb->AddInstr(new CTacInstr(opMul, t3, t4, t2));
        }
        // Otherwise, we already have an information to calculate.
        else{
          cb->AddInstr(new CTacInstr(opMul, t3, t, t2));
        }
      }

      // Since we have an information about the next index if i < nIndices,
      // bring it and add on the current offset.
      if(i < nIndices){
        CTacAddr *tac = GetIndex(i)->ToTac(cb);
        t = cb->CreateTemp(CTypeManager::Get()->GetInt());
        // cb->AddInstr(new CTacInstr(opAdd, t, t3, GetIndex(i)->ToTac(cb)));
        cb->AddInstr(new CTacInstr(opAdd, t, t3, tac));
      }
      // Otherwise, we have to pad it as 0.
      else{
        // If dimension is also 1, we only have information about the current index.
        if(nDim == 1){
          t = GetIndex(i-1)->ToTac(cb);
        }
        // Otherwise, if current dimension is smaller than nDim,
        // Get the next index as 0 and add.
        else if(i < nDim){
          t = cb->CreateTemp(CTypeManager::Get()->GetInt());
          cb->AddInstr(new CTacInstr(opAdd, t, t3, new CTacConst(0)));
        }
      }
    }

    // Calculate real offset by multiplying with the type's size.
    offset = cb->CreateTemp(CTypeManager::Get()->GetInt());
    if(GetType()->IsArray()){
      cb->AddInstr(new CTacInstr(opMul, offset, t,
            new CTacConst(dynamic_cast<const CArrayType *>(GetType())->GetBaseType()->GetSize())));
    }
    else{
      cb->AddInstr(new CTacInstr(opMul, offset, t, new CTacConst(GetType()->GetSize())));
    }
  }
  // If it is an array itself, it's offset will be zero(but this part won't be
  // executed since it must be an array designator.
  else{
    offset = new CTacConst(0);
  }

  // Calculate DOFS
  CTacAddr *ts = NULL;
  // Similarly, we have to send array as a pointer when we call DOFS.
  if(!(GetSymbol()->GetDataType()->IsPointer())){
    ts = cb->CreateTemp(CTypeManager::Get()->GetPointer(GetSymbol()->GetDataType()));
    cb->AddInstr(new CTacInstr(opAddress, ts, new CTacName(GetSymbol()), NULL));
  }
  else{
    ts = new CTacName(GetSymbol());
  }
  cb->AddInstr(new CTacInstr(opParam, new CTacConst(0), ts));
  CTacTemp *ofs = cb->CreateTemp(CTypeManager::Get()->GetInt());
  cb->AddInstr(new CTacInstr(opCall, ofs,
          new CTacName(cb->GetOwner()->GetSymbolTable()->FindSymbol("DOFS", sGlobal))));

  // Add index offset and DOFS
  CTacTemp *sumofs = cb->CreateTemp(CTypeManager::Get()->GetInt());
  cb->AddInstr(new CTacInstr(opAdd, sumofs, offset, ofs));

  // Add total offset on the address of the array
  CTacTemp* res = cb->CreateTemp(CTypeManager::Get()->GetInt());
  cb->AddInstr(new CTacInstr(opAdd, res, t0, sumofs));

  // Since we calculated the address of the actual data,
  // we have to enclose it by CTacReference to get its real value.
  //
  // How can we put the right symbol?
  // Is it res->GetSymbol?
  return new CTacReference(res->GetSymbol(), GetSymbol());

}

CTacAddr* CAstArrayDesignator::ToTac(CCodeBlock *cb,
                                     CTacLabel *ltrue, CTacLabel *lfalse)
{
  assert(cb != NULL);
  assert(ltrue != NULL);
  assert(lfalse != NULL);
  assert(GetType()->Compare(CTypeManager::Get()->GetBool()));

  // We already know the value of the array designator is a temporary variable.
  // Bring it using ToTac(cb) and compare with true.
  cb->AddInstr(new CTacInstr(opEqual, ltrue, ToTac(cb), new CTacConst(1)));
  cb->AddInstr(new CTacInstr(opGoto, lfalse));
  return NULL;

}


//------------------------------------------------------------------------------
// CAstConstant
//
CAstConstant::CAstConstant(CToken t, const CType *type, long long value)
  : CAstOperand(t), _type(type), _value(value)
{
}

void CAstConstant::SetValue(long long value)
{
  _value = value;
}

long long CAstConstant::GetValue(void) const
{
  return _value;
}

string CAstConstant::GetValueStr(void) const
{
  ostringstream out;

  if (GetType() == CTypeManager::Get()->GetBool()) {
    out << (_value == 0 ? "false" : "true");
  } else {
    out << dec << _value;
  }

  return out.str();
}

// We used such a complex trick to make a valid boundary for number.
// If its token value is larger than or equal to 1LL << 31, return false. If
// unary operator takes it and its operator is opNeg, accept it only if its
// value was exactly 1LL << 31. Otherwise, check it false at there.
bool CAstConstant::TypeCheck(CToken *t, string *msg) const
{
  const CType *st = GetType();
  if(st->IsInt()){
    if(GetValue() == 2147483648LL){
      if(t != NULL) *t = GetToken();
      if(msg != NULL) *msg = "integer constant outside valid range.";
      return false;
    }
  }
  return true;
}

const CType* CAstConstant::GetType(void) const
{
  return _type;
}

ostream& CAstConstant::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetValueStr() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstConstant::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetValueStr() << "\",shape=ellipse]";
  return out.str();
}

CTacAddr* CAstConstant::ToTac(CCodeBlock *cb)
{
  assert(cb != NULL);

  // It is just a value, so return.
  return new CTacConst(GetValue());
}

CTacAddr* CAstConstant::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  assert(cb != NULL);
  assert(ltrue != NULL);
  assert(lfalse != NULL);
  assert(GetType()->Compare(CTypeManager::Get()->GetBool()));
  CTacAddr* src = ToTac(cb);
  assert(NULL != dynamic_cast<CTacConst*>(src));

  // Just calculate its value and compare with true.
  // If it is, just goto true label.
  // Otherwise, just goto false label.
  if((dynamic_cast<CTacConst*>(src))->GetValue() == 1){
    cb->AddInstr(new CTacInstr(opGoto, ltrue));
  }
  else{
    cb->AddInstr(new CTacInstr(opGoto, lfalse));
  }
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStringConstant
//
int CAstStringConstant::_idx = 0;

CAstStringConstant::CAstStringConstant(CToken t, const string value,
                                       CAstScope *s)
  : CAstOperand(t)
{
  CTypeManager *tm = CTypeManager::Get();

  _type = tm->GetArray(strlen(CToken::unescape(value).c_str())+1,
                       tm->GetChar());
  _value = new CDataInitString(value);

  ostringstream o;
  o << "_str_" << ++_idx;

  _sym = new CSymGlobal(o.str(), _type);
  _sym->SetData(_value);
  s->GetSymbolTable()->AddSymbol(_sym);
}

const string CAstStringConstant::GetValue(void) const
{
  return _value->GetData();
}

const string CAstStringConstant::GetValueStr(void) const
{
  return GetValue();
}

bool CAstStringConstant::TypeCheck(CToken *t, string *msg) const
{
  return true;
}

const CType* CAstStringConstant::GetType(void) const
{
  return CTypeManager::Get()->GetArray(CToken::unescape(GetValueStr()).size() + 1, CTypeManager::Get()->GetChar());
}

ostream& CAstStringConstant::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << '"' << GetValueStr() << '"' << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstStringConstant::dotAttr(void) const
{
  ostringstream out;
  // the string is already escaped, but dot requires double escaping
  out << " [label=\"\\\"" << CToken::escape(GetValueStr())
      << "\\\"\",shape=ellipse]";
  return out.str();
}

// Therefore just return the variable's name's tac.
CTacAddr* CAstStringConstant::ToTac(CCodeBlock *cb)
{
  assert(cb != NULL);

  // String is a global variable as a character array.
  // Therefore just return the variable's name's tac.
  return new CTacName(_sym);
  return NULL;
}

CTacAddr* CAstStringConstant::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  //assert(cb != NULL);
  //assert(ltrue != NULL);
  //assert(lfalse != NULL);
  // String cannot be boolean. Just assert(false).
  assert(false);
  return NULL;
}
