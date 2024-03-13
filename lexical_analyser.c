#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

// Token codes
enum {
    ID, END, CT_INT, ASSIGN, SEMICOLON, BREAK, CHAR, DOUBLE, ELSE, FOR, IF, INT, RETURN, STRUCT, VOID, WHILE,
    CT_REAL, CT_CHAR, CT_STRING,
    COMMA, LPAR, RPAR, LBRACKET, RBRACKET, LACC, RACC,
    ADD, SUB, MUL, DIV, DOT, AND, OR, NOT,
    EQUAL, NOTEQ, LESS, LESSEQ, GREATER, GREATEREQ,
    SPACE, LINECOMMENT, COMMENT,
    INVALID
};

// Token structure
typedef struct _Token {
    int code;
    union {
        char *text;
        long int i;
        double r;
    };
    int line;
    struct _Token *next;
} Token;

// Global variables
const char *pCrtCh; // Pointer to the current character in the input
int line; // Current line number
Token *tokens = NULL; // Head of the linked list of tokens
Token *lastToken = NULL; // Last token in the linked list

// Function prototypes
void err(const char *fmt, ...);
void tkerr(const Token *tk, const char *fmt, ...);
Token *addTk(int code, char* data, int type);
int getNextToken();

int main() {
    // Test input string
    const char *input = "while char //hahahahahahaha\n struct void int double if(a == b && c!=d || e>=f) a=c; 2  2.3 ceva 22 \"ceva\" \'c\'";

    // Initialize pointers
    pCrtCh = input;
    line = 1;

    // Tokenize input
    int tokenCode;
    while ((tokenCode = getNextToken()) != END) {
        //printf("Token code: %d\n", tokenCode);
    }

    /*
    // Free memory allocated for tokens
    Token *currentToken = tokens;
    while (currentToken != NULL) {
        Token *temp = currentToken;
        currentToken = currentToken->next;
        free(temp->text); // Free memory allocated for token text
        free(temp); // Free token structure
    }
    */
    Token *currentToken = tokens;
    while (currentToken != NULL) {
        printf("Token code: %d, ", currentToken->code);
        printf("Line: %d, ", currentToken->line);
        switch (currentToken->code) {
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
    return 0;
}

int getNextToken() {
    int state = 0;
    const char *pStartCh;
    char* newString;

    while (1) {
        char ch = *pCrtCh;

        switch (state) {
            case 0:
                if (isalpha(ch) || ch == '_') {
                    pStartCh = pCrtCh;
                    pCrtCh++;
                    state = 1;
                } else if (ch == ' ' || ch == '\r' || ch == '\t') {
                    pCrtCh++;
                } else if (ch == '\n') {
                    line++;
                    pCrtCh++;  
                } else if (ch == '\t') {
                    pCrtCh++;
                } else if (ch == '\r') {
                    pCrtCh++;
                } else if (ch == '0') {
                    pStartCh = pCrtCh;
                    pCrtCh++;
                    state = 5;
                } else if (isdigit(ch)) {
                    pStartCh = pCrtCh;
                    pCrtCh++;
                    state = 3;
                } else if(ch == '\''){
                    pStartCh = pCrtCh;
                    pCrtCh++;
                    state = 19;
                } else if(ch == '"'){
                    pStartCh = pCrtCh;
                    pCrtCh++;
                    state = 23;
                }else if (ch == 0) {
                    addTk(END, NULL, 4);
                    return END;
                } else if (ch == ',') {
                    pCrtCh++;
                    addTk(COMMA, ",", 0);
                    return COMMA;
                } else if (ch == ';') {
                    pCrtCh++;
                    addTk(SEMICOLON, ";", 0);
                    return SEMICOLON;
                } else if (ch == '(') {
                    pCrtCh++;
                    addTk(LPAR, "(", 0);
                    return LPAR;
                } else if (ch == ')') {
                    pCrtCh++;
                    addTk(RPAR, ")", 0);
                    return RPAR;
                } else if (ch == '[') {
                    pCrtCh++;
                    addTk(LBRACKET, "[", 0);
                    return LBRACKET;
                } else if (ch == ']') {
                    pCrtCh++;
                    addTk(RBRACKET, "]", 0);
                    return RBRACKET;
                } else if (ch == '{') {
                    pCrtCh++;
                    addTk(LACC, "{", 0);
                    return LACC;
                } else if (ch == '}') {
                    pCrtCh++;
                    addTk(RACC, "}", 0);
                    return RACC;
                } else if (ch == '+') {
                    pCrtCh++;
                    addTk(ADD, "+", 0);
                    return ADD;
                } else if (ch == '-') {
                    pCrtCh++;
                    addTk(SUB, "-", 0);
                    return SUB;
                } else if (ch == '*') {
                    pCrtCh++;
                    addTk(MUL, "*", 0);
                    return MUL;
                } else if (ch == '/') {
                    pCrtCh++;
                    if(ch == '/')
                    {
                        pCrtCh++;
                        state = 27;
                    } else {
                        addTk(DIV, "/", 0);
                        return DIV;
                    }
                } else if (ch == '.') {
                    pCrtCh++;
                    addTk(DOT, ".", 0);
                    return DOT;
                } else if (ch == '&') {
                    pCrtCh++;
                    if (*pCrtCh == '&') {
                        pCrtCh++;
                        addTk(AND, "&&", 0);
                        return AND;
                    } else {
                        tkerr(tokens, "Expected &&");
                    }
                } else if (ch == '|') {
                    pCrtCh++;
                    if (*pCrtCh == '|') {
                        pCrtCh++;
                        addTk(OR, "||", 0);
                        return OR;
                    } else {
                        tkerr(tokens, "Expected ||");
                    }
                } else if (ch == '!') {
                    pCrtCh++;
                    addTk(NOT, "!", 0);
                    return NOT;
                } else if (ch == '=') {
                    pCrtCh++;
                    if (*pCrtCh == '=') {
                        pCrtCh++;
                        addTk(EQUAL, "==", 0);
                        return EQUAL;
                    } else {
                        addTk(ASSIGN, "=", 0);
                        return ASSIGN;
                    }
                } else if (ch == '<') {
                    pCrtCh++;
                    if (*pCrtCh == '=') {
                        pCrtCh++;
                        addTk(LESSEQ, "<=", 0);
                        return LESSEQ;
                    } else {
                        addTk(LESS, "<", 0);
                        return LESS;
                    }
                } else if (ch == '>') {
                    pCrtCh++;
                    if (*pCrtCh == '=') {
                        pCrtCh++;
                        addTk(GREATEREQ, ">=", 0);
                        return GREATEREQ;
                    } else {
                        addTk(GREATER, ">", 0);
                        return GREATER;
                    }
                } else if (isalpha(ch) || ch == '_') {
                    pStartCh = pCrtCh;
                    pCrtCh++;
                    state = 1;
                }else {
                    err("invalid character");
                }
                break;

            case 1:
                if (isalnum(ch) || ch == '_') {
                    pCrtCh++;
                } else {
                    state = 2;
                }
                break;

            case 2:
                newString = (char*) malloc(sizeof(char)*(pCrtCh - pStartCh+1));
                strncpy(newString, pStartCh, pCrtCh - pStartCh);
                newString[pCrtCh - pStartCh] = '\0';
                if(strcmp(newString, "break") == 0) {
                    addTk(BREAK, newString, 0);
                    return BREAK;
                } else if(strcmp(newString, "char") == 0) {
                    addTk(CHAR, newString, 0);
                    return CHAR;
                } else if(strcmp(newString, "double") == 0) {
                    addTk(DOUBLE, newString, 0);
                    return DOUBLE;
                } else if(strcmp(newString, "else") == 0) {
                    addTk(ELSE, newString, 0);
                    return ELSE;
                } else if(strcmp(newString, "for") == 0) {
                    addTk(FOR, newString, 0);
                    return FOR;
                } else if(strcmp(newString, "if") == 0) {
                    addTk(IF, newString, 0);
                    return IF;
                } else if(strcmp(newString, "int") == 0) {
                    addTk(INT, newString, 0);
                    return INT;
                } else if(strcmp(newString, "return") == 0) {
                    addTk(RETURN, newString, 0);
                    return RETURN;
                } else if(strcmp(newString, "struct") == 0) {
                    addTk(STRUCT, newString, 0);
                    return STRUCT;
                } else if(strcmp(newString, "void") == 0) {
                    addTk(VOID, newString, 0);
                    return VOID;
                } else if(strcmp(newString, "while") == 0) {
                    addTk(WHILE, newString, 0);
                    return WHILE;
                } else {
                    addTk(ID, newString, 0);
                    return ID;
                }
                
            case 3:
                if (isalpha(ch) || ch == '_') {
                    pCrtCh++;
                    state = 3;
                }else if(ch == '.'){
                    pCrtCh++;
                    state = 7;
                }else{
                    pCrtCh++;
                    state = 4;
                }
                break;
            case 4:
                newString = (char*) malloc(sizeof(char)*(pCrtCh - pStartCh+1));
                strncpy(newString, pStartCh, pCrtCh - pStartCh);
                newString[pCrtCh - pStartCh] = '\0';
                addTk(CT_INT, newString, 1);
                return CT_INT;
            case 5:
                if(ch == '8' || ch == '9'){
                    pCrtCh++;
                    state = 3;
                } else if (isdigit(ch)){
                    pCrtCh++;
                    state = 5;
                } else if(ch == 'x'){
                    pCrtCh++;
                    state = 6;
                } else if(ch == '.'){
                    pCrtCh++;
                    state = 7;
                } else if(ch == 'e' || ch == 'E'){
                    pCrtCh++;
                    state = 14;
                } else {
                    state = 0;
                    err("invalid character");
                }
                break;
            case 6:
                if(isdigit(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'))
                {
                    pCrtCh++;
                    state = 6;
                } else{
                    newString = (char*) malloc(sizeof(char)*(pCrtCh - pStartCh+1));
                    strncpy(newString, pStartCh, pCrtCh - pStartCh);
                    newString[pCrtCh - pStartCh] = '\0';
                    addTk(CT_INT, newString, 1);
                    return CT_INT;
                }
                break;
            case 7:
                if(isdigit(ch))
                {
                    pCrtCh++;
                    state = 8;
                } else {
                    state = 0;
                    err("invalid character");
                }
                break;
            case 8:
                if(isdigit(ch))
                {
                    pCrtCh++;
                    state = 8;
                } else if(ch == 'e' || ch == 'E'){
                    pCrtCh++;
                    state = 14;
                } else {
                    pCrtCh++;
                    state = 9; 
                }
                break;
            case 9:
                newString = (char*) malloc(sizeof(char)*(pCrtCh - pStartCh+1));
                strncpy(newString, pStartCh, pCrtCh - pStartCh);
                newString[pCrtCh - pStartCh] = '\0';
                addTk(CT_REAL, newString, 2);
                return CT_REAL;
            case 11:
                if(ch == '\\'){
                    pCrtCh++;
                    state = 12;
                } else {
                    state = 0;
                    err("invalid character");
                }
                break;
            case 12:
                if(ch == 'a' || ch == 'b' || ch == 'f' || ch == 'n' || ch == 'r' || ch == 't' || ch == 'v' || ch == '\'' || ch == '?' || ch == '"' || ch == '\\' || ch == '0'){
                    pCrtCh++;
                    state = 13;
                } else {
                    state = 0;
                    err("invalid character");
                }
                break;
            case 13:
                if(ch == '"'){
                    pCrtCh++;
                    state = 26; 
                } else if (ch == '\''){
                    pCrtCh++;
                    state = 22;
                } else {
                    state = 0;
                    err("invalid character");
                }
                break;
            case 14:
                if(ch == '+' || ch == '-'){
                    pCrtCh++;
                    state = 15;
                } else if(isdigit(ch)){
                    pCrtCh++;
                    state = 16;
                } else {
                    state = 0;
                    err("invalid character");
                }
                break;
            case 15: 
                if(isdigit(ch)){
                    pCrtCh++;
                    state = 16;
                } else {
                    state = 0;
                    err("invalid character");
                }
                break;
            case 16:
                if(isdigit(ch)){
                    pCrtCh++;
                    state = 16;
                } else{
                    state = 9;
                }
                break;
            case 19:
                if(ch == '\\'){
                    pCrtCh++;
                    state = 11;
                } else if(ch != '\'' && ch != '\\'){
                    pCrtCh++;
                    state = 21;
                } else {
                    state = 0;
                    err("invalid character");
                }
                break;
            case 21:
                if(ch == '\''){
                    pCrtCh++;
                    state = 22;
                } else {
                    state = 0;
                    err("invalid character");
                }
                break;
            case 22:
                newString = (char*) malloc(sizeof(char)*(pCrtCh - pStartCh+1));
                strncpy(newString, pStartCh, pCrtCh - pStartCh);
                newString[pCrtCh - pStartCh] = '\0';
                addTk(CT_CHAR, newString, 0);
                return CT_CHAR;
            case 23:
                if(ch == '\\'){
                    pCrtCh++;
                    state = 11;
                } else if(ch != '\'' && ch != '\\'){
                    pCrtCh++;
                    state = 25;
                } else {
                    state = 0;
                    err("invalid character");
                }
                break;
            case 25:
                if(ch == '"'){
                    pCrtCh++;
                    state = 26;
                } else if(ch != '\'' && ch != '\\'){
                    pCrtCh++;
                    state = 25;
                } else {
                    state = 0;
                    err("invalid character");
                }
                break;
            case 26:
                newString = (char*) malloc(sizeof(char)*(pCrtCh - pStartCh+1));
                strncpy(newString, pStartCh, pCrtCh - pStartCh);
                newString[pCrtCh - pStartCh] = '\0';
                addTk(CT_STRING, newString, 0);
                return CT_STRING;
            case 27:
                pCrtCh++;
                if(ch != '\n' && ch != '\r' && ch != '\0')
                {
                    state = 27;
                } else
                {
                    pCrtCh--;
                    state = 0;
                }
                break;
        }
    }
}

void err(const char *fmt, ...) {
    va_list va;
    va_start(va, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, va);
    fputc('\n', stderr);
    va_end(va);
    //exit(-1);
}

void tkerr(const Token *tk, const char *fmt, ...) {
    va_list va;
    va_start(va, fmt);
    fprintf(stderr, "error in line %d: ", tk->line);
    vfprintf(stderr, fmt, va);
    fputc('\n', stderr);
    va_end(va);
    exit(-1);
}

Token *addTk(int code, char* data, int type) {
    Token *tk;
    tk = (Token *)malloc(sizeof(Token));
    if (!tk)
        err("Not enough memory for token allocation");

    tk->code = code;
    tk->line = line;
    tk->next = NULL;


    if (type==0)
    {
        tk->text=(char*) malloc(sizeof(char)*(strlen(data)+1));
        strcpy(tk->text, data);
    }
    else if (type == 1)
        tk->i = atoi(data);
    else if (type == 2)
        tk->r = atof(data);
    if (lastToken) {
        lastToken->next = tk;
    } else {
        tokens = tk;
    }
    lastToken = tk;
    return tk;
}
