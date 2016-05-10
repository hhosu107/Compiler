//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2016/03/09 Bernhard Egger adapted to SnuPL/!
/// 2016/04/08 Bernhard Egger assignment 2: parser for SnuPL/-1
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

#ifndef __SnuPL_PARSER_H__
#define __SnuPL_PARSER_H__

#include "scanner.h"
#include "symtab.h"
#include "ast.h"


//------------------------------------------------------------------------------
/// @brief parser
///
/// parses a module
///
class CParser {
  public:
    /// @brief constructor
    ///
    /// @param scanner  CScanner from which the input stream is read
    CParser(CScanner *scanner);

    /// @brief parse a module
    /// @retval CAstNode program node
    CAstNode* Parse(void);

    /// @name error handling
    ///@{

    /// @brief indicates whether there was an error while parsing the source
    /// @retval true if the parser detected an error
    /// @retval false otherwise
    bool HasError(void) const { return _abort; };

    /// @brief returns the token that caused the error
    /// @retval CToken containing the error token
    const CToken* GetErrorToken(void) const;

    /// @brief returns a human-readable error message
    /// @retval error message
    string GetErrorMessage(void) const;
    ///@}

  private:
    /// @brief sets the token causing a parse error along with a message
    /// @param t token causing the error
    /// @param message human-readable error message
    void SetError(CToken t, const string message);

    /// @brief consume a token given type and optionally store the token
    /// @param type expected token type
    /// @param token If not null, the consumed token is stored in 'token'
    /// @retval true if a token has been consumed
    /// @retval false otherwise
    bool Consume(EToken type, CToken *token=NULL);

    /// @brief initialize symbol table
    /// @param s with predefined procedures and global variables
    void InitSymbolTable(CSymtab *s);

    /// @name methods for recursive-descent parsing
    /// @{

    /// @brief takes entire module and make AST node
    /// @retval CAstModule* node
    CAstModule*       module(void);

    /// @brief takes a set of variable declaration and add them into the given symbol table
    /// @param symbols a given symbol table
    /// @param s_type a symbol type depend on the location of variable declaration
    /// @retval CSymtab* symbols which saves newly declared symbols
    CSymtab* varDeclaration(CSymtab* symbols, ESymbolType s_type, bool openArray);

    /// @brief takes an one type of the variable declaration and add them into the given symbol table
    /// @param symbols a given symbol table
    /// @param s_type a symbol type depend on the location of variable declaration
    /// @param *params a pointer to vector<CSymParam*> which contains formal parameters of the subroutine, NULL otherwise
    /// @retval CSymtab* symbols which saves newly declared symbols
    /// @retval vector<CSymParam*>* as a parameter
    CSymtab* varDecl(CSymtab* symbols, ESymbolType s_type, vector<CSymParam*> *params, bool openArray);

    /// @brief takes a declaration part of subroutine and add it into global symbol table
    /// @param s parent scope
    /// @retval CAstProcedure node with its name, procedure symbol and its parent scope
    CAstProcedure*    subroutineDecl(CAstScope *s);

    /// @brief takes a type definition
    /// @retval CAstType* node with its type
    const CAstType* ReadType(bool openArray);

    /// @brief takes a sequence of statements to make a linked list of them
    /// @param s parent scope
    /// @retval head of linked list of statements if it is non-empty
    /// @retval NULL otherwise
    CAstStatement*    statSequence(CAstScope *s);

    /// @brief takes each statement
    /// @param s parent scope
    /// @retval CAstStatement* node if exists
    /// @retval NULL otherwise
    CAstStatement*    statement(CAstScope *s);

    /// @brief takes an assignment statement
    /// @param s parent scope
    /// @retval CAstStatAssign* node with lhs and rhs
    CAstStatAssign*   assignment(CAstScope *s);

    /// @brief takes a subroutine call statement
    /// @param s parent scope
    /// @retval CAstStatCall* node with an CAstFunctionCall* node fc
    CAstStatCall* subroutineCall(CAstScope *s);

    /// @brief takes an if statement
    /// @param s parent scope
    /// @retval CAstStatIf* node with ifcondition, ifstat and elsestat
    CAstStatIf* ifStatement(CAstScope *s);

    /// @brief takes a while loop statement
    /// @param s parent scope
    /// @retval CAstStatWhile* node with whilecondition, whilebody
    CAstStatWhile* whileStatement(CAstScope *s);

    /// @brief takes a return statement
    /// @param s parent scope
    /// @retval CAstStateReturn* node with a return statement
    CAstStatReturn* returnStatement(CAstScope *s);
    /// @brief takes a factor like constants, qualident, parenthesized expression, negated factor
    /// @param s
    /// @retval CAstExpression* node with a factor
    CAstExpression*   factor(CAstScope *s);

    /// @brief takes a qualident as ident { "[" [ number ] "]" }
    /// @param s parent scope
    /// @retval CAstDesignator* node with the qualident's symbol
    CAstDesignator* qualident(CAstScope* s);

    /// @brief takes a subroutine call expression
    /// @param s parent scope
    /// @retval CAstFunctionCall* node with a procedure symbol
    CAstFunctionCall* expSubroutineCall(CAstScope *s);

    /// @brief takes a term as "factor { termOp factor }"
    /// @param s parent scope
    /// @retval CAstExpression* node with a term
    CAstExpression*   term(CAstScope *s);

    /// @brief takes a simpleexpr as "[ + | - ] term { termOp term }"
    /// @param s parent scope
    /// @retval CAstExpression* node with a simpleexpr
    CAstExpression*   simpleexpr(CAstScope *s);

    /// @brief takes an expression as "simpleexpr [ relOp simpleexpr ]"
    /// @param s parent scope
    /// @retval CAstExpression* node with an expression
    CAstExpression*   expression(CAstScope *s);

    /// @brief takes a number constant and check w.o.n it exceeds a boundary
    /// @retval CAstConstant* node with integer type and value
    CAstConstant*     number(void);

    /// @brief takes "true" or "false"
    /// @retval CAstConstant* node with boolean type and 1 if true, 0 if false
    CAstConstant*     boolean(void);

    /// @brief takes a character constant and unescape it
    /// @retval CAstConstant* node with character type and its value
    CAstConstant*     character(void);

    /// @brief takes a string constant
    /// @param s parent scope
    /// @retval CAstStringConstant* node with its value and parent scope
    CAstStringConstant* stringConst(CAstScope *s);

    /// @}

    CScanner     *_scanner;       ///< CScanner instance
    CAstModule   *_module;        ///< root node of the program
    CToken        _token;         ///< current token

    /// @name error handling
    CToken        _error_token;   ///< error token
    string        _message;       ///< error message
    bool          _abort;         ///< error flag

};

#endif // __SnuPL_PARSER_H__
