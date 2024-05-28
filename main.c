#include "ad.h"
#include "at.h"
#include "mv.h"
#include "gc.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>

enum{
    //constante si ID
    ID, CT_INT, CT_REAL, CT_CHAR, CT_STRING,
    //cuvinte cheie
    BREAK, CHAR, DOUBLE, ELSE, IF, FOR, INT, RETURN, STRUCT, VOID, WHILE,
    //delimitatori
    COMMA, SEMICOLON, LPAR, RPAR, LBRACKET, RBRACKET, LACC, RACC, END,
    //operatori
    ADD, SUB, MUL, DIV, DOT, AND, OR, NOT, ASSIGN, EQUAL, NOTEQ, LESS, LESSEQ, GREATER, GREATEREQ
};

char *map[] = {
    "ID", "CT_INT", "CT_REAL", "CT_CHAR", "CT_STRING",
    "BREAK", "CHAR", "DOUBLE", "ELSE", "IF", "FOR", "INT", "RETURN", "STRUCT", "VOID", "WHILE",
    "COMMA", "SEMICOLON", "LPAR", "RPAR", "LBRACKET", "RBRACKET", "LACC", "RACC", "END",
    "ADD", "SUB", "MUL", "DIV", "DOT", "AND","OR", "NOT", "ASSIGN", "EQUAL", "NOTEQ", "LESS", "LESSEQ", "GREATER", "GREATEREQ"
};

bool structDef();
bool fnDef();
bool varDef();
bool typeBase(Type* t);
bool arrayDecl(Type* t);
bool expr(Ret *r);
bool fnParam();
bool stmCompound(bool newDomain);
bool stm();
bool exprAssign(Ret *r);
bool exprUnary(Ret *r);
bool exprOr(Ret *r);
bool exprOrPrim(Ret *r);
bool exprAnd(Ret *r);
bool exprAndPrim(Ret *r);
bool exprEq(Ret *r);
bool exprEqPrim(Ret *r);
bool exprRel(Ret *r);
bool exprRelPrim(Ret *r);
bool exprAdd(Ret *r);
bool exprAddPrim(Ret *r);
bool exprMul(Ret *r);
bool exprMulPrim(Ret *r);
bool exprCast(Ret *r);
bool exprPostfix(Ret *r);
bool exprPostfixPrim(Ret *r);
bool exprPrimary(Ret *r);
Symbol* owner;

void err(const char *fmt,...)
{
  va_list va;
  va_start(va,fmt);
  fprintf(stderr,"error: ");
  vfprintf(stderr,fmt,va);
  fputc('\n',stderr);
  va_end(va);
  exit(-1);
}

#define SAFEALLOC(var,Type) if((var=(Type*)malloc(sizeof(Type)))==NULL)err("Not enough memory");
#define SAFEALLOCN(var,Type,n) if((var=(Type*)malloc(n*sizeof(Type)))==NULL)err("Not enough memory");

int line;
char bufin[30000];
char *pCrtCh;

typedef struct _Token{
  int code; // codul (numele)
  union{
    char *text; // folosit pentru ID, CT_STRING (alocat dinamic)
    int i; // folosit pentru CT_INT, CT_CHAR
    double r; // folosit pentru CT_REAL
  };
  int line; // linia din fisierul de intrare
  struct _Token *next; // inlantuire la urmatorul AL
}Token;

void tkerr(const Token *tk,const char *fmt,...)
{
  va_list va;
  va_start(va,fmt);
  fprintf(stderr,"error in line %d: ",tk->line);
  vfprintf(stderr,fmt,va);
  fputc('\n',stderr);
  va_end(va);
  exit(-1);
}

Token *tokens, *lastToken, *iTk, *consumedTk;

Token *addTk(int code, char *data, int type)
{
    Token *tk;
    tk = (Token *)malloc(sizeof(Token));
    if (!tk)
        err("Not enough memory for token allocation");

    tk->code = code;
    tk->line = line;
    tk->next = NULL;

    if (type == 0)
    {
        tk->text = (char *)malloc(sizeof(char) * (strlen(data) + 1));
        strcpy(tk->text, data);
    }
    else if (type == 1)
        if (data[0] == '0' && data[1] == 'x')
        {
            tk->i = strtol(data, NULL, 16);
        }
        else if (data[0] == '0')
        {
            tk->i = strtol(data, NULL, 8);
        }
        else
            tk->i = atoi(data);
    else if (type == 2)
        tk->r = atof(data);
    if (lastToken)
    {
        lastToken->next = tk;
    }
    else
    {
        tokens = tk;
    }
    lastToken = tk;
    return tk;
}

char *createString(const char *pStartCh, const char *pCrtCh)
{
    size_t n = (pCrtCh - pStartCh);
    char *p;
    SAFEALLOCN(p, char, n+1);
    memcpy(p, pStartCh, n);
    p[n] = '\0';
    return p;
}

int getNextToken()
{
    int state = 0;
    const char *pStartCh;
    char *newString;

    while (1)
    {
        char ch = *pCrtCh;

        switch (state)
        {
        case 0:
            if (isalpha(ch) || ch == '_')
            {
                pStartCh = pCrtCh;
                pCrtCh++;
                state = 1;
            }
            else if (ch == ' ' || ch == '\r' || ch == '\t')
            {
                pCrtCh++;
            }
            else if (ch == '\n')
            {
                line++;
                pCrtCh++;
            }
            else if (ch == '0')
            {
                pStartCh = pCrtCh;
                pCrtCh++;
                state = 5;
            }
            else if (isdigit(ch))
            {
                pStartCh = pCrtCh;
                pCrtCh++;
                state = 3;
            }
            else if (ch == '\'')
            {
                pStartCh = pCrtCh;
                pCrtCh++;
                state = 19;
            }
            else if (ch == '"')
            {
                pStartCh = pCrtCh;
                pCrtCh++;
                state = 23;
            }
            else if (ch == 0)
            {
                addTk(END, NULL, 4);
                return END;
            }
            else if (ch == ',')
            {
                pCrtCh++;
                addTk(COMMA, ",", 0);
                return COMMA;
            }
            else if (ch == ';')
            {
                pCrtCh++;
                addTk(SEMICOLON, ";", 0);
                return SEMICOLON;
            }
            else if (ch == '(')
            {
                pCrtCh++;
                addTk(LPAR, "(", 0);
                return LPAR;
            }
            else if (ch == ')')
            {
                pCrtCh++;
                addTk(RPAR, ")", 0);
                return RPAR;
            }
            else if (ch == '[')
            {
                pCrtCh++;
                addTk(LBRACKET, "[", 0);
                return LBRACKET;
            }
            else if (ch == ']')
            {
                pCrtCh++;
                addTk(RBRACKET, "]", 0);
                return RBRACKET;
            }
            else if (ch == '{')
            {
                pCrtCh++;
                addTk(LACC, "{", 0);
                return LACC;
            }
            else if (ch == '}')
            {
                pCrtCh++;
                addTk(RACC, "}", 0);
                return RACC;
            }
            else if (ch == '+')
            {
                pCrtCh++;
                addTk(ADD, "+", 0);
                return ADD;
            }
            else if (ch == '-')
            {
                pCrtCh++;
                addTk(SUB, "-", 0);
                return SUB;
            }
            else if (ch == '*')
            {
                pCrtCh++;
                addTk(MUL, "*", 0);
                return MUL;
            }
            else if (ch == '/')
            {
                pCrtCh++;
                if (*pCrtCh == '/')
                {
                    state = 27;
                }
                else if (*pCrtCh == '*')
                {
                    state = 28;
                }
                else
                {
                    addTk(DIV, "/", 0);
                    return DIV;
                }
            }
            else if (ch == '.')
            {
                pCrtCh++;
                addTk(DOT, ".", 0);
                return DOT;
            }
            else if (ch == '&')
            {
                pCrtCh++;
                if (*pCrtCh == '&')
                {
                    pCrtCh++;
                    addTk(AND, "&&", 0);
                    return AND;
                }
                else
                {
                    tkerr(tokens, "Expected &&");
                }
            }
            else if (ch == '|')
            {
                pCrtCh++;
                if (*pCrtCh == '|')
                {
                    pCrtCh++;
                    addTk(OR, "||", 0);
                    return OR;
                }
                else
                {
                    tkerr(tokens, "Expected ||");
                }
            }
            else if (ch == '!')
            {
                pCrtCh++;
                addTk(NOT, "!", 0);
                return NOT;
            }
            else if (ch == '=')
            {
                pCrtCh++;
                if (*pCrtCh == '=')
                {
                    pCrtCh++;
                    addTk(EQUAL, "==", 0);
                    return EQUAL;
                }
                else
                {
                    addTk(ASSIGN, "=", 0);
                    return ASSIGN;
                }
            }
            else if (ch == '<')
            {
                pCrtCh++;
                if (*pCrtCh == '=')
                {
                    pCrtCh++;
                    addTk(LESSEQ, "<=", 0);
                    return LESSEQ;
                }
                else
                {
                    addTk(LESS, "<", 0);
                    return LESS;
                }
            }
            else if (ch == '>')
            {
                pCrtCh++;
                if (*pCrtCh == '=')
                {
                    pCrtCh++;
                    addTk(GREATEREQ, ">=", 0);
                    return GREATEREQ;
                }
                else
                {
                    addTk(GREATER, ">", 0);
                    return GREATER;
                }
            }
            else
            {
                printf("%c", ch);
                pCrtCh++;
                // err("invalid character");
            }
            break;

        case 1:
            if (isalnum(ch) || ch == '_')
            {
                pCrtCh++;
            }
            else
            {
                state = 2;
            }
            break;

        case 2:
            newString = (char *)malloc(sizeof(char) * (pCrtCh - pStartCh + 1));
            strncpy(newString, pStartCh, pCrtCh - pStartCh);
            newString[pCrtCh - pStartCh] = '\0';
            if (strcmp(newString, "break") == 0)
            {
                addTk(BREAK, newString, 0);
                return BREAK;
            }
            else if (strcmp(newString, "char") == 0)
            {
                addTk(CHAR, newString, 0);
                return CHAR;
            }
            else if (strcmp(newString, "double") == 0)
            {
                addTk(DOUBLE, newString, 0);
                return DOUBLE;
            }
            else if (strcmp(newString, "else") == 0)
            {
                addTk(ELSE, newString, 0);
                return ELSE;
            }
            else if (strcmp(newString, "for") == 0)
            {
                addTk(FOR, newString, 0);
                return FOR;
            }
            else if (strcmp(newString, "if") == 0)
            {
                addTk(IF, newString, 0);
                return IF;
            }
            else if (strcmp(newString, "int") == 0)
            {
                addTk(INT, newString, 0);
                return INT;
            }
            else if (strcmp(newString, "return") == 0)
            {
                addTk(RETURN, newString, 0);
                return RETURN;
            }
            else if (strcmp(newString, "struct") == 0)
            {
                addTk(STRUCT, newString, 0);
                return STRUCT;
            }
            else if (strcmp(newString, "void") == 0)
            {
                addTk(VOID, newString, 0);
                return VOID;
            }
            else if (strcmp(newString, "while") == 0)
            {
                addTk(WHILE, newString, 0);
                return WHILE;
            }
            else
            {
                addTk(ID, newString, 0);
                return ID;
            }

        case 3:
            if (isdigit(ch))
            {
                pCrtCh++;
                state = 3;
            }
            else if (ch == 'e' || ch == 'E')
            {
                pCrtCh++;
                state = 14;
            }
            else if (ch == '.')
            {
                pCrtCh++;
                state = 7;
            }
            else
            {
                state = 4;
            }
            break;
        case 4:
            newString = (char *)malloc(sizeof(char) * (pCrtCh - pStartCh + 1));
            strncpy(newString, pStartCh, pCrtCh - pStartCh);
            newString[pCrtCh - pStartCh] = '\0';
            addTk(CT_INT, newString, 1);
            return CT_INT;
        case 5:
            if (ch == '8' || ch == '9')
            {
                pCrtCh++;
                state = 3;
            }
            else if (isdigit(ch))
            {
                pCrtCh++;
                state = 5;
            }
            else if (ch == 'x' || ch == 'X')
            {
                pCrtCh++;
                state = 6;
            }
            else if (ch == '.')
            {
                pCrtCh++;
                state = 7;
            }
            else if (ch == 'e' || ch == 'E')
            {
                pCrtCh++;
                state = 14;
            }
            else
            {
                state = 6;
            }
            break;
        case 6:
            if (isdigit(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'))
            {
                pCrtCh++;
                state = 6;
            }
            else
            {
                newString = (char *)malloc(sizeof(char) * (pCrtCh - pStartCh + 1));
                strncpy(newString, pStartCh, pCrtCh - pStartCh);
                newString[pCrtCh - pStartCh] = '\0';
                addTk(CT_INT, newString, 1);
                return CT_INT;
            }
            break;
        case 7:
            if (isdigit(ch))
            {
                pCrtCh++;
                state = 8;
            }
            else
            {
                state = 0;
                err("invalid character");
            }
            break;
        case 8:
            if (isdigit(ch))
            {
                pCrtCh++;
                state = 8;
            }
            else if (ch == 'e' || ch == 'E')
            {
                pCrtCh++;
                state = 14;
            }
            else
            {
                pCrtCh++;
                state = 9;
            }
            break;
        case 9:
            newString = (char *)malloc(sizeof(char) * (pCrtCh - pStartCh + 1));
            strncpy(newString, pStartCh, pCrtCh - pStartCh);
            newString[pCrtCh - pStartCh] = '\0';
            addTk(CT_REAL, newString, 2);
            return CT_REAL;
        case 12:
            if (ch == 'a' || ch == 'b' || ch == 'f' || ch == 'n' || ch == 'r' || ch == 't' || ch == 'v' || ch == '\'' || ch == '?' || ch == '"' || ch == '\\' || ch == '0')
            {
                pCrtCh++;
                state = 13;
            }
            else
            {
                state = 0;
                err("invalid character");
            }
            break;
        case 13:
            if (ch == '\'')
            {
                pCrtCh++;
                state = 22;
            }
            else
            {
                state = 25;
            }
            break;
        case 14:
            if (ch == '+' || ch == '-')
            {
                pCrtCh++;
                state = 15;
            }
            else if (isdigit(ch))
            {
                pCrtCh++;
                state = 16;
            }
            else
            {
                state = 0;
                err("invalid character");
            }
            break;
        case 15:
            if (isdigit(ch))
            {
                pCrtCh++;
                state = 16;
            }
            else
            {
                state = 0;
                err("invalid character");
            }
            break;
        case 16:
            if (isdigit(ch))
            {
                pCrtCh++;
                state = 16;
            }
            else
            {
                state = 9;
            }
            break;
        case 19:
            if (ch == '\\')
            {
                pCrtCh++;
                state = 12;
            }
            else if (ch != '\'' && ch != '\\')
            {
                pCrtCh++;
                state = 21;
            }
            else
            {
                state = 0;
                err("invalid character");
            }
            break;
        case 21:
            if (ch == '\'')
            {
                pCrtCh++;
                state = 22;
            }
            else
            {
                state = 0;
                err("invalid character");
            }
            break;
        case 22:
            newString = (char *)malloc(sizeof(char) * (pCrtCh - pStartCh + 1));
            strncpy(newString, pStartCh, pCrtCh - pStartCh);
            newString[pCrtCh - pStartCh] = '\0';
            addTk(CT_CHAR, newString, 0);
            return CT_CHAR;
        case 23:
            if (ch == '\\')
            {
                pCrtCh++;
                state = 12;
            }
            else if (ch != '\'' && ch != '\\')
            {
                pCrtCh++;
                state = 25;
            }
            else
            {
                state = 0;
                err("invalid character");
            }
            break;
        case 25:
            if (ch == '"')
            {
                pCrtCh++;
                state = 26;
            }
            else if (ch == '\\')
            {
                pCrtCh++;
                state = 12;
            }
            else if (ch != '\"' && ch != '\\')
            {
                pCrtCh++;
                state = 25;
            }
            else
            {
                state = 0;
                err("invalid character");
            }
            break;
        case 26:
            newString = (char *)malloc(sizeof(char) * (pCrtCh - pStartCh + 1));
            strncpy(newString, pStartCh, pCrtCh - pStartCh);
            newString[pCrtCh - pStartCh] = '\0';
            addTk(CT_STRING, newString, 0);
            return CT_STRING;
        case 27:
            pCrtCh++;
            if (ch != '\n' && ch != '\r' && ch != '\0')
            {
                state = 27;
            }
            else
            {
                pCrtCh--;
                state = 0;
            }
            break;
        case 28:
            pCrtCh++;
            if (*pCrtCh == '*')
            {
                pCrtCh++;
                if (*pCrtCh == '/')
                {
                    pCrtCh++;
                    state = 0;
                }
                else
                {
                    pCrtCh--;
                }
            }
            else
            {
                state = 28;
            }
        }
    }
}

void showAtoms()
{
    Token *currentToken = tokens;
    while (currentToken != NULL)
    {
        printf("Token code: %s, ", map[currentToken->code]);
        printf("Line: %d, ", currentToken->line);
        switch (currentToken->code)
        {
        case ID:
            printf("Text: %s\n", currentToken->text);
            break;
        case CT_INT:
            printf("Value: %ld\n", currentToken->i);
            break;
        case CT_REAL:
            printf("Value: %lf\n", currentToken->r);
            break;
        case CT_CHAR:
            printf("Text: %s\n", currentToken->text);
            break;
        case CT_STRING:
            printf("Text: %s\n", currentToken->text);
            break;
        default:
            printf("Text: %s\n", currentToken->text);
            break;
        }
        currentToken = currentToken->next;
    }
    free(currentToken);
}

bool consume(int code){
    printf("consume(%s)",map[code]);
    if(iTk->code==code)
    {
        consumedTk=iTk;
        iTk=iTk->next;
        printf(" => consumed\n");
        return true;
    }
    printf(" => found %s\n",map[iTk->code]);
    return false;
}

bool unit() {
    Token* start = iTk;
    int startInstr = nInstructions;
    pushDomain();
    mvInit();
    mvDouble();
    mvChar();
    mvString();
    mvGetChar();
    mvGetInt();
    addInstr(OP_CALL);
    addInstr(OP_HALT);
    for(;;) {
        if(structDef()) {}
        else if(fnDef()) {}
        else if (varDef()) {}
        else break;
    }
    if(consume(END)) {
        showDomain(symTable,"global");
        Symbol* sm = findSymbol("main");
        if(!sm)
            tkerr(consumedTk, "undefined: main");
        instructions[0].arg.i = sm->fn.instrIdx;
        run();
        dropDomain();
        return true;
    }

    iTk = start;
    nInstructions = startInstr;

    return false;
}

bool structDef(){
    Token *start = iTk;
    int startInstr = nInstructions;
    if(consume(STRUCT)){
        if(consume(ID)){
            Token* tkName = consumedTk;
            if(consume(LACC)){
                Symbol *s=findSymbolInDomain(symTable,tkName->text);
                if(s)
                    tkerr(iTk,"symbol redefinition: %s",tkName->text);
                s=addSymbolToDomain(symTable,newSymbol(tkName->text,SK_STRUCT));
                s->type.tb=TB_STRUCT;
                s->type.s=s;
                s->type.n=-1;
                pushDomain();
                owner=s;
                for(;;){
                    if(varDef()){}
                    else break;
                }
                if(consume(RACC)){
                    if(consume(SEMICOLON)){
                        owner=NULL;
                        dropDomain();
                        return true;
                    }
                    else tkerr(iTk, "Need: ;");
                }
                else tkerr(iTk, "Need: }");
            }
        }
        else tkerr(iTk, "Need identifier after struct");
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}
bool commaBool = false; 
bool varDef(){
    Token *start = iTk;
    int startInstr = nInstructions;
    Type t;
    if(typeBase(&t)){
        if(consume(ID)){
            Token* tkName = consumedTk;
            if(arrayDecl(&t)){
                if(t.n==0)
                    tkerr(iTk,"a vector variable must have a specified dimension");
            }
            if(consume(SEMICOLON)){
                Symbol *var=findSymbolInDomain(symTable,tkName->text);
                if(var)
                    tkerr(iTk,"symbol redefinition: %s",tkName->text);
                var=newSymbol(tkName->text,SK_VAR);
                var->type=t;
                var->owner=owner;
                addSymbolToDomain(symTable,var);
                if(owner){
                    switch(owner->kind){
                        case SK_FN:
                            var->varIdx=symbolsLen(owner->fn.locals);
                            addSymbolToList(&owner->fn.locals,dupSymbol(var));
                            break;
                        case SK_STRUCT:
                            var->varIdx=typeSize(&owner->type);
                            addSymbolToList(&owner->structMembers,dupSymbol(var));
                            break;
                        }
                }else{
                    var->varIdx=allocInGlobalMemory(typeSize(&t));
                }
                return true;
            }
            else tkerr(iTk, "Need: ;");
        }
        else tkerr(iTk, "Need identifier after type declaration");
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}



bool typeBase(Type* t){
    Token *start = iTk;
    int startInstr = nInstructions;
    t->n=-1;
    if(consume(INT)){
        t->tb=TB_INT;
        return true;
    }
    if(consume(DOUBLE)){
        t->tb=TB_DOUBLE;
        return true;
    }
    if(consume(CHAR)){
        t->tb=TB_CHAR;
        return true;
    }
    if(consume(STRUCT)){
        if(consume(ID)){
            Token* tkName = consumedTk;
            t->tb=TB_STRUCT;
            t->s=findSymbol(tkName->text);
            if(!t->s)tkerr(iTk,"structura nedefinita: %s",tkName->text);
            return true;
        }
        else tkerr(iTk, "Need struct identifier");
    }

    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool arrayDecl(Type* t){
    Token *start = iTk;
    int startInstr = nInstructions;
    if(consume(LBRACKET)){
        if(consume(CT_INT)){
            Token* tkSize = consumedTk;
            t->n = tkSize->i;
        }
        if(consume(RBRACKET)){
            return true;
        }
        else tkerr(iTk, "Need: ]");
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool fnDefPrim(Type* t) {
    if(consume(ID)) {
        Token* tkName = consumedTk;
        if(consume(LPAR)) {
            Symbol* fn = findSymbolInDomain(symTable, tkName->text);
            if(fn)
                tkerr(iTk, "Symbol redefinition: %s", tkName->text);
            fn = newSymbol(tkName->text, SK_FN);
            fn->type = *t;
            addSymbolToDomain(symTable, fn);
            owner = fn;
            pushDomain();
            if(fnParam()) {
                for(;;) {
                    if(consume(COMMA)) {
                        if(fnParam()) {}
                        else {
                            tkerr(iTk, "Need function argument after ,");
                        }
                    } else
                        break;
                }
            }
            if(consume(RPAR)) {
                owner->fn.instrIdx=nInstructions;
                addInstr(OP_ENTER);
                if(stmCompound(false)) {
                    dropDomain();
                    instructions[owner->fn.instrIdx].arg.i=symbolsLen(owner->fn.locals);
                    if(owner->type.tb==TB_VOID)
                        addInstrWithInt(OP_RET_VOID,symbolsLen(owner->fn.params));
                    owner = NULL;
                    return true;
                }
            } else {
                tkerr(iTk, "Need: )");
            }
        }
    }
    return false;
}

bool fnDef() {
    Type t;
    Token* start = iTk;
    int startInstr = nInstructions;
    if(typeBase(&t)) {
        if(fnDefPrim(&t)) {
            return true;
        }
    } else if(consume(VOID)) {
        t.tb = TB_VOID;
        if(fnDefPrim(&t)) {
            return true;
        }
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool fnParam(){
    Token *start = iTk;
    int startInstr = nInstructions;
    Type t;
    if(typeBase(&t)){
        if(consume(ID)){
            Token* tkName = consumedTk;
            if(arrayDecl(&t)){
                t.n = 0;
            }

            Symbol *param=findSymbolInDomain(symTable,tkName->text);
            if(param)
                tkerr(iTk,"symbol redefinition: %s",tkName->text);
            param=newSymbol(tkName->text,SK_PARAM);
            param->type=t;
            param->owner=owner;
            param->paramIdx=symbolsLen(owner->fn.params);
            // parametrul este adaugat atat la domeniul curent, cat si la parametrii fn
            addSymbolToDomain(symTable,param);
            addSymbolToList(&owner->fn.params,dupSymbol(param));

            return true;
        }
        else tkerr(iTk, "Need type identifier");
    }

    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool returnStatementFound = false;
bool stm() {
    Ret rInit, rCond, rStep, rExpr;
    Token* start = iTk;
    int startInstr = nInstructions;
    if(stmCompound(true)) {
        return true;
    }
    if(consume(IF)) {
        if(consume(LPAR)) {
            if(expr(&rCond)) {
                if(!canBeScalar(&rCond))
                    tkerr(iTk, "the if condition must be a scalar value");
                if(consume(RPAR)) {
                    addRVal(rCond.lval,&rCond.type);
                    Type intType={TB_INT,NULL,-1};
                    insertConvIfNeeded(nInstructions,&rCond.type,&intType);
                    int posJF=addInstr(OP_JF);
                    if(stm()) {
                        if(consume(ELSE)) {
                            int posJMP=addInstr(OP_JMP);
                            instructions[posJF].arg.i=nInstructions;
                            if(stm()) {
                                instructions[posJMP].arg.i=nInstructions;
                                return true;
                            }
                            else {
                                tkerr(iTk, "Missing body for 'else'");
                            }
                        } else
                            instructions[posJF].arg.i=nInstructions;
                        return true;
                    } else {
                        tkerr(iTk, "Error inside 'if' body or missing body");
                    }
                } else {
                    tkerr(iTk, "Missing ) after condition in 'if'");
                }
            } else {
                tkerr(iTk, "Invalid condition for 'if'");
            }
        } else {
            tkerr(iTk, "Missing ( after 'if'");
        }
    }
    else if(consume(WHILE)) {
        int posCond=nInstructions;
        if(consume(LPAR)) {
            if(expr(&rCond)) {
                if(!canBeScalar(&rCond))
                    tkerr(iTk, "the while condition must be a scalar value");
                if(consume(RPAR)) {
                    addRVal(rCond.lval,&rCond.type);
                    Type intType={TB_INT,NULL,-1};
                    insertConvIfNeeded(nInstructions,&rCond.type,&intType);
                    int posJF=addInstr(OP_JF);
                    if(stm()) {
                        addInstrWithInt(OP_JMP,posCond);
                        instructions[posJF].arg.i=nInstructions;
                        return true;
                    } else {
                        tkerr(iTk, "Missing body for 'while'");
                    }
                } else {
                    tkerr(iTk, "Missing ) after condition in 'while'");
                }
            } else {
                tkerr(iTk, "Invalid condition for 'while'");
            }
        } else {
            tkerr(iTk, "Missing ( after 'while'");
        }
    }
    else if(consume(FOR)) {
        if(consume(LPAR)) {
            if(expr(&rInit)) {}
            if(consume(SEMICOLON)) {
                if(expr(&rCond)) {
                    if(!canBeScalar(&rCond))
                        tkerr(iTk, "the for condition must be a scalar value");
                }
                if(consume(SEMICOLON)) {
                    if(expr(&rStep)) {}
                    if(consume(RPAR)) {
                        if(stm()) {
                            return true;
                        } else {
                            tkerr(iTk, "Missing body for 'for'");
                        }
                    } else {
                        tkerr(iTk, "Missing ) after condition in 'for'");
                    }
                } else {
                    tkerr(consumedTk, "Missing ; or invalid condition in 'for'");
                }
            } else {
                tkerr(consumedTk, "Missing ; or invalid initialization in 'for'");
            }
        } else {
            tkerr(iTk, "Missing ( after 'for'");
        }
    }
    else if(consume(BREAK)) {
        if(consume(SEMICOLON)) {
            return true;
        } else {
            tkerr(consumedTk, "Missing ; after 'break'");
        }
    }
    else if(consume(RETURN)) {
        returnStatementFound = true;
        if(expr(&rExpr)) {
            if(owner->type.tb==TB_VOID)
                tkerr(iTk, "a void function cannot return a value");
            if(!canBeScalar(&rExpr))
                tkerr(iTk, "the return value must be a scalar value");
            if(!convTo(&rExpr.type,&owner->type))
                tkerr(iTk, "cannot convert the return expression type to the function return type");
            addRVal(rExpr.lval,&rExpr.type);
            insertConvIfNeeded(nInstructions,&rExpr.type,&owner->type);
            addInstrWithInt(OP_RET,symbolsLen(owner->fn.params));
        } else {
            if(owner->type.tb!=TB_VOID)
                tkerr(consumedTk,"a non-void function must return a value");
            addInstr(OP_RET_VOID);
        }
        if(consume(SEMICOLON)) {
            return true;
        } else {
            tkerr(consumedTk, "Missing ; after 'return'");
        }
    }
    else if(expr(&rExpr)) {
        if(rExpr.type.tb!=TB_VOID)
            addInstr(OP_DROP);
    }
    if(consume(SEMICOLON)) {
        return true;
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool stmCompound(bool newDomain){
    Token *start = iTk;
    int startInstr = nInstructions;
    if(consume(LACC)){
        if(newDomain)pushDomain();
        for(;;){
            if(varDef()){}
            else if(stm()){}
            else break;
        }
        if(consume(RACC)){
            if(owner->type.tb!=TB_VOID && returnStatementFound==false   ){
                tkerr(iTk,"a non-void function must return a value");
            }
            else if(returnStatementFound==true) returnStatementFound=false;

            if(newDomain)dropDomain();
            return true;
        }
        else tkerr(iTk, "Need: }");
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool expr(Ret *r){
    Token *start = iTk;
    int startInstr = nInstructions;
    if(exprAssign(r)){
        return true;
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool exprAssign(Ret *r) {
    Ret rDst;
    Token* start = iTk;
    int startInstr = nInstructions;
    if(exprUnary(&rDst)) {
        if(consume(ASSIGN)) {
            if(exprAssign(r)) {
                if(!rDst.lval)
                    tkerr(iTk, "the assign destination must be a left-value");
                if(rDst.ct)
                    tkerr(iTk, "the assign destination cannot be constant");
                if(!canBeScalar(&rDst))
                    tkerr(iTk, "the assign destination must be scalar");
                if(!canBeScalar(r))
                    tkerr(iTk, "the assign source must be scalar");
                if(!convTo(&r->type,&rDst.type))
                    tkerr(iTk, "the assign source cannot be converted to destination");
                r->lval=false;
                r->ct=true;
                addRVal(r->lval,&r->type);
                insertConvIfNeeded(nInstructions,&r->type,&rDst.type);
                switch(rDst.type.tb){
                    case TB_INT:
                        addInstr(OP_STORE_I);
                        break;
                    case TB_DOUBLE:
                        addInstr(OP_STORE_F);
                        break;
                    case TB_CHAR:
                        addInstr(OP_STORE_C);
                        break;
                }
                return true;
            } else {
                tkerr(iTk, "Invalid expression after '='");
            }
        }
        iTk = start;
        nInstructions = startInstr;
    }
    if(exprOr(r)) {
        return true;
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool exprOr(Ret *r){
    Token *start = iTk;
    int startInstr = nInstructions;
    if(exprAnd(r)){
        if(exprOrPrim(r)){
            return true;
        }
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool exprOrPrim(Ret *r) {
    if(consume(OR)) {
        Ret right;
        if(exprAnd(&right)) {
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst))
                tkerr(iTk, "invalid operand type for ||");
            *r=(Ret){{TB_INT,NULL,-1},false,true};
            if(exprOrPrim(r)) {
                return true;
            }
        }
    }
    return true;
}

bool exprAnd(Ret *r){
    Token *start = iTk;
    int startInstr = nInstructions;
    if(exprEq(r)){
        if(exprAndPrim(r)){
            return true;
        }
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool exprAndPrim(Ret *r) {
    if(consume(AND)) {
        Ret right;
        if(exprEq(&right)) {
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst))
                tkerr(iTk, "invalid operand type for &&");
            *r=(Ret){{TB_INT,NULL,-1},false,true};
            if(exprAndPrim(r)) {
                return true;
            }
        }
    }
    return true;
}

bool exprEq(Ret *r){
    Token *start = iTk;
    int startInstr = nInstructions;
    if(exprRel(r)){
        if(exprEqPrim(r)){
            return true;
        }
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool exprEqPrim(Ret *r) {
    if(consume(EQUAL) || consume(NOTEQ)) {
        Ret right;
        if(exprRel(&right)) {
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst))
                tkerr(iTk, "invalid operand type for == or !=");
            *r=(Ret){{TB_INT,NULL,-1},false,true};
            if(exprEqPrim(r)) {
                return true;
            }
        }
    }
    return true;
}

bool exprRel(Ret *r) {
    Token* start = iTk;
    int startInstr = nInstructions;
    if(exprAdd(r)) {
        if(exprRelPrim(r)) {
            return true;
        }
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool exprRelPrim(Ret *r) {
    if(consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ)) {
        Ret right;
        int code = consumedTk->code;
        int posLeft=nInstructions;
        addRVal(r->lval,&r->type);
        if(exprAdd(&right)) {
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst))
                tkerr(iTk, "invalid operand type for <, <=, >, >=");
            addRVal(right.lval,&right.type);
            insertConvIfNeeded(posLeft,&r->type,&tDst);
            insertConvIfNeeded(nInstructions,&right.type,&tDst);
            switch(code){
                case LESS:
                    switch(tDst.tb){
                        case TB_INT:
                            addInstr(OP_LESS_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(OP_LESS_F);
                            break;
                    }
                    break;
            }
            *r=(Ret){{TB_INT,NULL,-1},false,true};
            if(exprRelPrim(r)) {
                return true;
            }
        } else
            tkerr(iTk, "Invalid expression after %s", (code == LESS)?"<":((code == LESSEQ)?"<=":((code == GREATER)?">":">=")));
    }
    return true;
}

bool exprAdd(Ret *r) {
    Token* start = iTk;
    int startInstr = nInstructions;
    if(exprMul(r)) {
        if(exprAddPrim(r)) {
            return true;
        }
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool exprAddPrim(Ret *r) {
    if(consume(ADD) || consume(SUB)) {
        Ret right;
        int code = consumedTk->code;
        int posLeft=nInstructions;
        addRVal(r->lval,&r->type);
        if(exprMul(&right)) {
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst))
                tkerr(iTk,"invalid operand type for + or -");

            addRVal(right.lval,&right.type);
            insertConvIfNeeded(posLeft,&r->type,&tDst);
            insertConvIfNeeded(nInstructions,&right.type,&tDst);
            switch(code){
                case ADD:
                    switch(tDst.tb){
                        case TB_INT:
                            addInstr(OP_ADD_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(OP_ADD_F);
                            break;
                    }
                    break;
                case SUB:
                    switch(tDst.tb){
                        case TB_INT:
                            addInstr(OP_SUB_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(OP_SUB_F);
                            break;
                    }
                    break;
            }

            *r=(Ret){{TB_INT,NULL,-1},false,true};
            if(exprAddPrim(r)) {
                return true;
            }
        } else
            tkerr(iTk, "Invalid expression after %s", (code == ADD)?"+":"-");
    }
    return true;
}


bool exprMul(Ret *r) {
    Token* start = iTk;
    int startInstr = nInstructions;
    if(exprCast(r)) {
        if(exprMulPrim(r)) {
            return true;
        }
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool exprMulPrim(Ret *r) {
    if(consume(MUL) || consume(DIV)) {
        Ret right;
        int code = consumedTk->code;
        int posLeft=nInstructions;
        addRVal(r->lval,&r->type);
        if(exprCast(&right)) {
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst))
                tkerr(iTk,"invalid operand type for * or /");

            addRVal(right.lval,&right.type);
            insertConvIfNeeded(posLeft,&r->type,&tDst);
            insertConvIfNeeded(nInstructions,&right.type,&tDst);
            switch(code){
                case MUL:
                    switch(tDst.tb){
                        case TB_INT:
                            addInstr(OP_MUL_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(OP_MUL_F);
                            break;
                    }
                    break;
                case DIV:
                    switch(tDst.tb){
                        case TB_INT:
                            addInstr(OP_DIV_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(OP_DIV_F);
                            break;
                    }
                    break;
            }

            *r=(Ret){{TB_INT,NULL,-1},false,true};
            if(exprMulPrim(r)) {
                return true;
            }
        } else
            tkerr(iTk ,"Invalid expression after %s", (code == MUL)?"*":"/");
    }
    return true;
}

bool exprCast(Ret *r){
    Token *start = iTk;
    int startInstr = nInstructions;
    Type t;
    if(consume(LPAR)){
        Type t;
        Ret op;
        if(typeBase(&t)){
            if(arrayDecl(&t)){}
            if(consume(RPAR)){
                if(exprCast(&op)){
                    if(t.tb==TB_STRUCT)
                        tkerr(iTk,"cannot convert to a struct type");
                    if(op.type.tb==TB_STRUCT)
                        tkerr(iTk,"cannot convert a struct");
                    if(op.type.n>=0&&t.n<0)
                        tkerr(iTk,"an array can be converted only to another array");
                    if(op.type.n<0&&t.n>=0)
                        tkerr(iTk,"a scalar can be converted only to another scalar");
                    *r=(Ret){t,false,true};
                    return true;
                }
            }
            else tkerr(iTk, "Need: )");
        }
    }
    iTk = start;
    nInstructions = startInstr;
    if(exprUnary(r)){
        return true;
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool exprUnary(Ret *r) {
    Token* start = iTk;
    int startInstr = nInstructions;
    if(consume(SUB) || consume(NOT)) {
        if(exprUnary(r)) {
            if(!canBeScalar(r))
                tkerr(iTk,"unary - must have a scalar operand");
            r->lval=false;
            r->ct=true;
            return true;
        }
        iTk = start;
        nInstructions = startInstr;
    }
    else if(exprPostfix(r)) {
        return true;
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool exprPostfix(Ret *r){
    Token *start = iTk;
    int startInstr = nInstructions;
    if(exprPrimary(r)){
        if(exprPostfixPrim(r)){
            return true;
        }
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

bool exprPostfixPrim(Ret *r) {
    if(consume(LBRACKET)) {
        Ret idx;
        if(expr(&idx)) {
            if(consume(RBRACKET)) {
                if(r->type.n<0)
                    tkerr(iTk,"only an array can be indexed");
                Type tInt={TB_INT,NULL,-1};
                if(!convTo(&idx.type,&tInt))
                    tkerr(iTk,"the index is not convertible to int");
                r->type.n=-1;
                r->lval=true;
                r->ct=false;
                if(exprPostfixPrim(r)) {
                    return true;
                }
            }
        }
    } else if(consume(DOT)) {
        if(consume(ID)) {
            Token* tkName = consumedTk;
            if(r->type.tb!=TB_STRUCT)
                tkerr(iTk,"a field can only be selected from a struct");
            Symbol *s=findSymbolInList(r->type.s->structMembers,tkName->text);
            if(!s)
                tkerr(iTk,"the structure %s does not have a field %s",r->type.s->name,tkName->text);
            *r=(Ret){s->type,true,s->type.n>=0};
            if(exprPostfixPrim(r)) {
                return true;
            }
        }
    }
    return true;
}

bool exprPrimary(Ret* r) {
    Token* start = iTk;
    int startInstr = nInstructions;
    if(consume(ID)) {
        Token* tkName = consumedTk;
        Symbol *s=findSymbol(tkName->text);
        if(!s)
            tkerr(iTk,"undefined id: %s",tkName->text);
        if(consume(LPAR)) {
            if(s->kind!=SK_FN)
                tkerr(iTk,"only a function can be called");
            Ret rArg;
            Symbol *param=s->fn.params;
            if(expr(&rArg)) {
                if(!param)
                    tkerr(iTk,"too many arguments in function call");
                if(!convTo(&rArg.type,&param->type))
                    tkerr(iTk,"in call, cannot convert the argument type to the parameter type");
                addRVal(rArg.lval,&rArg.type);
                insertConvIfNeeded(nInstructions,&rArg.type,&param->type);
                param=param->next;
                for(;;) {
                    if(consume(COMMA)) {
                        if(expr(&rArg)) {
                            if(!param)
                                tkerr(iTk,"too many arguments in function call");
                            if(!convTo(&rArg.type,&param->type))
                                tkerr(iTk,"in call, cannot convert the argument type to the parameter type");
                            addRVal(rArg.lval,&rArg.type);
                            insertConvIfNeeded(nInstructions,&rArg.type,&param->type);
                            param=param->next;
                        } else
                            tkerr(iTk, "Need expression after ','!");
                    } else
                        break;
                }
            }
            if(consume(RPAR)) {
                if(param)
                    tkerr(iTk,"too few arguments in function call");
                *r=(Ret){s->type,false,true};
                if(s->fn.extFnPtr){
                    int posCallExt=addInstr(OP_CALL_EXT);
                    instructions[posCallExt].arg.extFnPtr=s->fn.extFnPtr;
                } else {
                    addInstrWithInt(OP_CALL,s->fn.instrIdx);
                }
                return true;
            }
        } else {
            if(s->kind==SK_FN)
                tkerr(iTk,"a function can only be called");
            *r=(Ret){s->type,true,s->type.n>=0};
            if(s->kind==SK_VAR){
                if(s->owner==NULL){ // variabile globale
                    addInstrWithInt(OP_ADDR,s->varIdx);
                } else { // variabile locale
                    switch(s->type.tb){
                        case TB_INT:
                            addInstrWithInt(OP_FPADDR_I,s->varIdx+1);
                            break;
                        case TB_DOUBLE:
                            addInstrWithInt(OP_FPADDR_F,s->varIdx+1);
                            break;
                        case TB_CHAR:
                            addInstrWithChar(OP_FPADDR_C,s->varIdx+1);
                    }
                }
            }
            if(s->kind==SK_PARAM){
                switch(s->type.tb){
                    case TB_INT:
                        //if(s->owner == NULL)
                        //    addInstrWithInt(OP_FPADDR_I,s->paramIdx-1);
                        //else
                            addInstrWithInt(OP_FPADDR_I,s->paramIdx-symbolsLen(s->owner->fn.params)-1);
                        break;
                    case TB_DOUBLE:
                        //if(s->owner == NULL)
                        //    addInstrWithInt(OP_FPADDR_I,s->paramIdx-1);
                        //else
                            addInstrWithInt(OP_FPADDR_F,s->paramIdx-symbolsLen(s->owner->fn.params)-1);
                        break;
                    case TB_CHAR:
                        //if(s->owner == NULL)
                        //    addInstrWithInt(OP_FPADDR_I,s->paramIdx-1);
                        //else
                            addInstrWithChar(OP_FPADDR_C,s->paramIdx-symbolsLen(s->owner->fn.params)-1);
                        break;
                }
            }
        }
        return true;
    }
    else if(consume(CT_INT)) {
        addInstrWithInt(OP_PUSH_I,consumedTk->i);
        *r=(Ret){{TB_INT,NULL,-1},false,true};
        return true;
    }
    else if(consume(CT_REAL)) {
        addInstrWithDouble(OP_PUSH_F,consumedTk->r);
        *r=(Ret){{TB_DOUBLE,NULL,-1},false,true};
        return true;
    }
    else if(consume(CT_CHAR)) {
        addInstrWithChar(OP_PUSH_C,consumedTk->i);
        *r=(Ret){{TB_CHAR,NULL,-1},false,true};
        return true;
    }
    else if(consume(CT_STRING)) {
        addInstrWithPtr(OP_PUSH_P, consumedTk->text);
        *r=(Ret){{TB_CHAR,NULL,0},false,true};
        return true;
    }
    else if(consume(LPAR)) {
        if(expr(r)) {
            if(consume(RPAR)) {
                return true;
            } else {
                tkerr(iTk, "Need ) after expression");
            }
        }
    }
    iTk = start;
    nInstructions = startInstr;
    return false;
}

int main()
{
  FILE *fis;
  fis = fopen("testGenerareCod.c", "rb");

  if (fis == NULL){
    printf("nu s-a putut deschide fisierul");
    return -1;
  }

  int n = fread(bufin, 1, 30000, fis);
  bufin[n] = '\0';
  fclose(fis);
  pCrtCh = bufin; // initializarea pch pe prima pozitie din buffin

  while(getNextToken() != END){}
  showAtoms();

    iTk=tokens;
    printf("\n%d",unit());
    // pushDomain();
    // showDomain(symTable,"global");
    // run();
    // dropDomain();
  free(iTk);
  free(consumedTk);
  free(tokens);
  //free(lastToken);

  //fclose(fis);
  return 0;
}
