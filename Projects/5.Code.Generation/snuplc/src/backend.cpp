//------------------------------------------------------------------------------
/// @brief SnuPL backend
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/11/28 Bernhard Egger created
/// 2013/06/09 Bernhard Egger adapted to SnuPL/0
/// 2016/04/04 Bernhard Egger adapted to SnuPL/1
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

#include <fstream>
#include <sstream>
#include <iomanip>
#include <cassert>

#include "backend.h"
using namespace std;


//------------------------------------------------------------------------------
// CBackend
//
CBackend::CBackend(ostream &out)
  : _out(out)
{
}

CBackend::~CBackend(void)
{
}

bool CBackend::Emit(CModule *m)
{
  assert(m != NULL);
  _m = m;

  if (!_out.good()) return false;

  bool res = true;

  try {
    EmitHeader();
    EmitCode();
    EmitData();
    EmitFooter();

    res = _out.good();
  } catch (...) {
    res = false;
  }

  return res;
}

void CBackend::EmitHeader(void)
{
}

void CBackend::EmitCode(void)
{
}

void CBackend::EmitData(void)
{
}

void CBackend::EmitFooter(void)
{
}


//------------------------------------------------------------------------------
// CBackendx86
//
CBackendx86::CBackendx86(ostream &out)
  : CBackend(out), _curr_scope(NULL)
{
  _ind = string(4, ' ');
}

CBackendx86::~CBackendx86(void)
{
}

void CBackendx86::EmitHeader(void)
{
  _out << "##################################################" << endl
       << "# " << _m->GetName() << endl
       << "#" << endl
       << endl;
}

void CBackendx86::EmitCode(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# text section" << endl
       << _ind << "#" << endl
       << _ind << ".text" << endl
       << _ind << ".align 4" << endl
       << endl
       << _ind << "# entry point and pre-defined functions" << endl
       << _ind << ".global main" << endl
       << _ind << ".extern DIM" << endl
       << _ind << ".extern DOFS" << endl
       << _ind << ".extern ReadInt" << endl
       << _ind << ".extern WriteInt" << endl
       << _ind << ".extern WriteStr" << endl
       << _ind << ".extern WriteChar" << endl
       << _ind << ".extern WriteLn" << endl
       << endl;

  // TODO
  // forall s in subscopes do
  //  EmitScope(s)
  const vector<CScope*> &proc = _m->GetSubscopes();
  for(CScope* s : proc){
    EmitScope(s);
  }

  // EmitScope(program)
  EmitScope(_m);

  _out << _ind << "# end of text section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitData(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# global data section" << endl
       << _ind << "#" << endl
       << _ind << ".data" << endl
       << _ind << ".align 4" << endl
       << endl;

  EmitGlobalData(_m);

  _out << _ind << "# end of global data section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitFooter(void)
{
  _out << _ind << ".end" << endl
       << "##################################################" << endl;
}

void CBackendx86::SetScope(CScope *scope)
{
  _curr_scope = scope;
}

CScope* CBackendx86::GetScope(void) const
{
  return _curr_scope;
}

void CBackendx86::EmitScope(CScope *scope)
{
  assert(scope != NULL);

  SetScope(scope);

  string label;

  if (scope->GetParent() == NULL) label = "main";
  else label = scope->GetName();

  // label
  _out << _ind << "# scope " << scope->GetName() << endl
       << label << ":" << endl;

  // TODO
  // ComputeStackOffsets(scope)
  size_t stack_size = ComputeStackOffsets(scope->GetSymbolTable(), 8, -12);

  // emit function prologue
  _out << _ind << "# prologue" << endl;

  EmitInstruction("pushl", "%ebp");
  EmitInstruction("movl", "%esp, %ebp");
  EmitInstruction("pushl", "%ebx", "save callee saved registers");
  EmitInstruction("pushl", "%esi");
  EmitInstruction("pushl", "%edi");
  EmitInstruction("subl", Imm(stack_size) + ", %esp", "make room for locals");
  _out << endl;

  if(stack_size >= 20){
    EmitInstruction("cld", "", "memset local stack area to 0");
    EmitInstruction("xorl", "%eax, %eax");
    EmitInstruction("movl", Imm(stack_size / 4) + ", %ecx");
    EmitInstruction("mov", "%esp, %edi");
    EmitInstruction("rep", "stosl");

    _out << endl;
  }
  else if(stack_size > 0){
    EmitInstruction("xorl", "%eax, %eax", "memset local stack area to 0");
    for(int i = (int)stack_size - 4; i >= 0; i -= 4){
      EmitInstruction("movl", "%eax, " + to_string(i) + "(%esp)");
    }

    _out << endl;
  }

  // forall i in instructions do
  //   EmitInstruction(i)
  //
  _out << _ind << "# function body" << endl;

  EmitCodeBlock(scope->GetCodeBlock());

  _out << endl << Label("exit") << ":" << endl;

  // emit function epilogue
  _out << _ind << "# epilogue" << endl;

  EmitInstruction("addl", Imm(stack_size) + ", %esp", "remove locals");
  EmitInstruction("popl", "%edi");
  EmitInstruction("popl", "%esi");
  EmitInstruction("popl", "%ebx");
  EmitInstruction("popl", "%ebp");
  EmitInstruction("ret");

  _out << endl;
}

void CBackendx86::EmitGlobalData(CScope *scope)
{
  assert(scope != NULL);

  // emit the globals for the current scope
  CSymtab *st = scope->GetSymbolTable();
  assert(st != NULL);

  bool header = false;

  vector<CSymbol*> slist = st->GetSymbols();

  _out << dec;

  size_t size = 0;

  for (size_t i=0; i<slist.size(); i++) {
    CSymbol *s = slist[i];
    const CType *t = s->GetDataType();

    if (s->GetSymbolType() == stGlobal) {
      if (!header) {
        _out << _ind << "# scope: " << scope->GetName() << endl;
        header = true;
      }

      // insert alignment only when necessary
      if ((t->GetAlign() > 1) && (size % t->GetAlign() != 0)) {
        size += t->GetAlign() - size % t->GetAlign();
        _out << setw(4) << " " << ".align "
             << right << setw(3) << t->GetAlign() << endl;
      }

      _out << left << setw(36) << s->GetName() + ":" << "# " << t << endl;

      if (t->IsArray()) {
        const CArrayType *a = dynamic_cast<const CArrayType*>(t);
        assert(a != NULL);
        int dim = a->GetNDim();

        _out << setw(4) << " "
          << ".long " << right << setw(4) << dim << endl;

        for (int d=0; d<dim; d++) {
          assert(a != NULL);

          _out << setw(4) << " "
            << ".long " << right << setw(4) << a->GetNElem() << endl;

          a = dynamic_cast<const CArrayType*>(a->GetInnerType());
        }
      }

      const CDataInitializer *di = s->GetData();
      if (di != NULL) {
        const CDataInitString *sdi = dynamic_cast<const CDataInitString*>(di);
        assert(sdi != NULL);  // only support string data initializers for now

        _out << left << setw(4) << " "
          << ".asciz " << '"' << sdi->GetData() << '"' << endl;
      } else {
        _out  << left << setw(4) << " "
          << ".skip " << dec << right << setw(4) << t->GetDataSize()
          << endl;
      }

      size += t->GetSize();
    }
  }

  _out << endl;

  // emit globals in subscopes (necessary if we support static local variables)
  vector<CScope*>::const_iterator sit = scope->GetSubscopes().begin();
  while (sit != scope->GetSubscopes().end()) EmitGlobalData(*sit++);
}

void CBackendx86::EmitLocalData(CScope *scope)
{
  assert(scope != NULL);

  // TODO TODO!
}

void CBackendx86::EmitCodeBlock(CCodeBlock *cb)
{
  assert(cb != NULL);

  const list<CTacInstr*> &instr = cb->GetInstr();
  list<CTacInstr*>::const_iterator it = instr.begin();

  while (it != instr.end()) EmitInstruction(*it++);
}

void CBackendx86::EmitInstruction(CTacInstr *i)
{
  assert(i != NULL);

  ostringstream cmt;
  string mnm;
  cmt << i;

  EOperation op = i->GetOperation();

  switch (op) {
    // binary operators
    // dst = src1 op src2
    // opAdd, opSub, opMul, opDiv, opAnd, opOr
    case opAdd:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx");
      EmitInstruction("addl", "%ebx, %eax");
      Store(i->GetDest(), 'a');
      break;
    case opSub:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx");
      EmitInstruction("subl", "%ebx, %eax");
      Store(i->GetDest(), 'a');
      break;
    case opMul:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx");
      EmitInstruction("imull", "%ebx");
      Store(i->GetDest(), 'a');
      break;
    case opDiv:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx");
      EmitInstruction("idivl", "%ebx");
      Store(i->GetDest(), 'a');
      break;
    case opAnd:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx");
      EmitInstruction("andl", "%ebx, %eax");
      Store(i->GetDest(), 'a');
      break;
    case opOr:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx");
      EmitInstruction("orl", "%ebx, %eax");
      Store(i->GetDest(), 'a');
      break;

    // unary operators
    // dst = op src1
    // opNeg, opPos, opNot
    // TODO
    case opNeg:
      Load(i->GetSrc(1), "%eax", cmt.str());
      EmitInstruction("negl", "%eax");
      Store(i->GetDest(), 'a');
      break;

    // memory operations
    // dst = src1
    // opAssign
    case opAssign:
      // TODO: dest can be reference
      Load(i->GetSrc(1), "%eax", cmt.str());
      Store(i->GetDest(), 'a');
      break;

    // pointer operations
    // dst = &src1 // opAddress
    case opAddress:
      EmitInstruction("leal", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
      Store(i->GetDest(), 'a');
      break;

    // dst = *src1
    case opDeref:
      // opDeref not generated for now
      EmitInstruction("# opDeref", "not implemented", cmt.str());
      break;


    // unconditional branching
    // goto dst
    // opGoto
    case opGoto:
      EmitInstruction("jmp", Label(dynamic_cast<CTacLabel*>(i->GetDest())), cmt.str());
      break;

    // conditional branching
    // if src1 relOp src2 then goto dst
    // opEqual, opNotEqual, opLessThan, opLessEqual, opBiggerThan, opBiggerEqual
    // TODO
    case opEqual:
    case opNotEqual:
    case opLessThan:
    case opLessEqual:
    case opBiggerThan:
    case opBiggerEqual:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx");
      EmitInstruction("cmpl", "%ebx, %eax");
      EmitInstruction("j" + Condition(op), Label(dynamic_cast<CTacLabel*>(i->GetDest())));
      break;

    // function call-related operations
    // opCall, opReturn, opParam
    // TODO
    case opCall:
      {
        EmitInstruction("call", dynamic_cast<CTacName*>(i->GetSrc(1))->GetSymbol()->GetName(), cmt.str());

        // move %esp
        const CTacName *f = dynamic_cast<const CTacName*>(i->GetSrc(1));
        int NParam = dynamic_cast<const CSymProc*>(f->GetSymbol())->GetNParams();
        if(NParam != 0) EmitInstruction("addl", Imm(NParam * 4) + ", %esp");

        // return value
        if(i->GetDest()){
          Store(i->GetDest(), 'a');
        }
        break;
      }

    case opReturn:
      Load(i->GetSrc(1), "%eax", cmt.str());
      EmitInstruction("jmp", Label("exit"));
      break;

    case opParam:
      Load(i->GetSrc(1), "%eax", cmt.str());
      EmitInstruction("pushl", "%eax");
      break;

    // special
    case opLabel:
      _out << Label(dynamic_cast<CTacLabel*>(i)) << ":" << endl;
      break;

    case opNop:
      EmitInstruction("nop", "", cmt.str());
      break;


    default:
      EmitInstruction("# ???", "not implemented", cmt.str());
  }
}

void CBackendx86::EmitInstruction(string mnemonic, string args, string comment)
{
  _out << left
       << _ind
       << setw(7) << mnemonic << " "
       << setw(23) << args;
  if (comment != "") _out << " # " << comment;
  _out << endl;
}

void CBackendx86::Load(CTacAddr *src, string dst, string comment)
{
  assert(src != NULL);

  string mnm = "mov";
  string mod = "l";

  // set operator modifier based on the operand size
  switch (OperandSize(src)) {
    case 1: mod = "zbl"; break;
    case 2: mod = "zwl"; break;
    case 4: mod = "l"; break;
  }

  // emit the load instruction
  EmitInstruction(mnm + mod, Operand(src) + ", " + dst, comment);
}

void CBackendx86::Store(CTac *dst, char src_base, string comment)
{
  assert(dst != NULL);

  string mnm = "mov";
  string mod = "l";
  string src = "%";

  // compose the source register name based on the operand size
  switch (OperandSize(dst)) {
    case 1: mod = "b"; src += string(1, src_base) + "l"; break;
    case 2: mod = "w"; src += string(1, src_base) + "x"; break;
    case 4: mod = "l"; src += "e" + string(1, src_base) + "x"; break;
  }

  // emit the store instruction
  EmitInstruction(mnm + mod, src + ", " + Operand(dst), comment);
}

string CBackendx86::Operand(const CTac *op)
{
  string operand;

  // constant
  if(dynamic_cast<const CTacConst*>(op)){
    operand = Imm(dynamic_cast<const CTacConst*>(op)->GetValue());
  }

  // TODO: reference
  else if(dynamic_cast<const CTacReference*>(op)){
    const CSymbol *sym = dynamic_cast<const CTacReference*>(op)->GetDerefSymbol();

    string ref_operand = "";

     // global : just name
    if(dynamic_cast<const CSymGlobal*>(sym)){
      ref_operand = sym->GetName();
    }

    // else : register
    else{
      ref_operand = to_string(sym->GetOffset()) + "(" + sym->GetBaseRegister() + ")";
    }

    EmitInstruction("movl", ref_operand + ", %edi");

    operand = "(%edi)";
  }

  // normal name
  else if(dynamic_cast<const CTacName*>(op)){
    const CSymbol *sym = dynamic_cast<const CTacName*>(op)->GetSymbol();

    // global : just name
    if(dynamic_cast<const CSymGlobal*>(sym)){
      operand = sym->GetName();
    }

    // else : register
    else{
      operand = to_string(sym->GetOffset()) + "(" + sym->GetBaseRegister() + ")";
    }
  }

  // else: maybe cannot b operand
  else{
    operand = "";
  }

  return operand;
}

string CBackendx86::Imm(int value) const
{
  ostringstream o;
  o << "$" << dec << value;
  return o.str();
}

string CBackendx86::Label(const CTacLabel* label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  ostringstream o;
  o << "l_" << cs->GetName() << "_" << label->GetLabel();
  return o.str();
  return "l_" + cs->GetName() + "_" + label->GetLabel();
}

string CBackendx86::Label(string label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  return "l_" + cs->GetName() + "_" + label;
}

string CBackendx86::Condition(EOperation cond) const
{
  switch (cond) {
    case opEqual:       return "e";
    case opNotEqual:    return "ne";
    case opLessThan:    return "l";
    case opLessEqual:   return "le";
    case opBiggerThan:  return "g";
    case opBiggerEqual: return "ge";
    default:            assert(false); break;
  }
}

int CBackendx86::OperandSize(CTac *t) const
{
  int size = 4;

  // TODO
  // compute the size for operand t of type CTacName
  // Hint: you need to take special care of references (incl. references to pointers!)
  //       and arrays. Compare your output to that of the reference implementation
  //       if you are not sure.

  if(dynamic_cast<CTacConst*>(t)){
    // _out << "OperandSize - constant" << endl;
    size = 4;
  }
  else if(!dynamic_cast<CTacReference*>(t)){
    const CSymbol *sym = dynamic_cast<CTacName*>(t)->GetSymbol();
    // _out << sym->GetName() << " : ";
    // check array
    if(sym->GetDataType()->IsArray()){
      // _out << "OperandSize - not reference, array" << endl;
    }
    else if(sym->GetDataType()->IsPointer()){
      // _out << "OperandSize - not reference, pointer" << endl;
    }
    else if(sym->GetDataType()->IsInt()){
      // _out << "OperandSize - not reference, integer" << endl;
    }
    else if(sym->GetDataType()->IsChar()){
      // _out << "OperandSize - not reference, char" << endl;
    }
    else if(sym->GetDataType()->IsBoolean()){
      // _out << "OperandSize - not reference, boolean" << endl;
    }
    else{
      // _out << "OperandSize - not reference, something else" << endl;
    }
  }
  else{
    const CSymbol *sym = dynamic_cast<CTacReference*>(t)->GetDerefSymbol();
    // _out << sym->GetName() << " : ";
    if(sym->GetDataType()->IsArray()){
      // _out << "OperandSize - reference, array" << endl;
    }
    else if(sym->GetDataType()->IsPointer()){
      // _out << "OperandSize - reference, pointer" << endl;
    }
    else if(sym->GetDataType()->IsInt()){
      // _out << "OperandSize - reference, integer" << endl;
    }
    else if(sym->GetDataType()->IsChar()){
      // _out << "OperandSize - reference, char" << endl;
    }
    else if(sym->GetDataType()->IsBoolean()){
      // _out << "OperandSize - reference, boolean" << endl;
    }
    else{
      // _out << "OperandSize - reference, something else" << endl;
    }
  }

  return size;
}

size_t CBackendx86::ComputeStackOffsets(CSymtab *symtab,
                                        int param_ofs,int local_ofs)
{
  assert(symtab != NULL);
  vector<CSymbol*> slist = symtab->GetSymbols();

  size_t size = 0;

  // TODO
  for(CSymbol* sym : slist){
    // foreach parameter p in slist do
    if(dynamic_cast<CSymParam*>(sym)){
      CSymParam *p = dynamic_cast<CSymParam*>(sym);

      //   compute offset on stack and store in symbol p
      sym->SetOffset(param_ofs + (p->GetIndex()) * 4);

      //   set base register to %ebp
      sym->SetBaseRegister("%ebp");
    }
    // foreach local symbol l in slist do
    else if(dynamic_cast<CSymLocal*>(sym)){
      CSymLocal *l = dynamic_cast<CSymLocal*>(sym);
      int l_size = l->GetDataType()->GetDataSize();

      //   compute aligned offset on stack and store in symbol l
      local_ofs -= l_size; size += l_size;

      // boolean and character : 1 byte alignment
      // else : 4 bytes alignment
      if(!l->GetDataType()->IsBoolean() && !l->GetDataType()->IsChar()){
        while(local_ofs % 4 != 0){ local_ofs--; size++; }
      }

      sym->SetOffset(local_ofs);

      //   set base register to %ebp
      sym->SetBaseRegister("%ebp");
    }
 }

  // align size
  while(size % 4 != 0) size++;

  // dump stack frame to assembly file
  _out << _ind << "# stack offsets:" << endl;

  for(CSymbol* sym : slist){
    if(dynamic_cast<CSymLocal*>(sym) || dynamic_cast<CSymParam*>(sym)){
      _out << _ind << "# " << right << setw(6) << sym->GetOffset()
           << "(" << sym->GetBaseRegister() << ")"
           << right << setw(4) << sym->GetDataType()->GetDataSize()
           << "  " << sym << endl;
    }
  }
  _out << endl;

  return size;
}
