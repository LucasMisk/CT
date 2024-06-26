
#include "ad.h"
#include "mv.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>

Instr *instructions=NULL;
int nInstructions=0;

int addInstr(int op){
	Instr *p=(Instr*)realloc(instructions,(nInstructions+1)*sizeof(Instr));
	if(!p)err("not enough memory");
	instructions=p;
	Instr *i=instructions+nInstructions;
	i->op=op;
	return nInstructions++;
	}

int insertInstr(int pos,int op){
	Instr *p=(Instr*)realloc(instructions,(nInstructions+1)*sizeof(Instr));
	if(!p)err("not enough memory");
	instructions=p;
	Instr *i=instructions+pos;
	memmove(i+1,i,(nInstructions-pos)*sizeof(Instr));
	i->op=op;
	nInstructions++;
	return pos;
	}

int addInstrWithInt(int op,int i){
	int pos=addInstr(op);
	instructions[pos].arg.i=i;
	return pos;
	}

int addInstrWithDouble(int op,double d){
	int pos=addInstr(op);
	instructions[pos].arg.f=d;
	return pos;
	} 

int addInstrWithChar(int op,char c){
	int pos=addInstr(op);
	instructions[pos].arg.c=c;
	return pos;
	}

int addInstrWithPtr(int op,void* ptr){
	int pos=addInstr(op);
	instructions[pos].arg.p=ptr;
	return pos;
	}
Val stack[10000];		// stiva
Val *SP;		// Stack pointer - varful stivei - indica intotdeauna valoarea din varful stivei

void pushv(Val v){
	if(SP+1==stack+10000)err("trying to push into a full stack");
	*++SP=v;
	}

Val popv(){
	if(SP==stack-1)err("trying to pop from empty stack");
	return *SP--;
	}

void pushi(int i){
	if(SP+1==stack+10000)err("trying to push into a full stack");
	(++SP)->i=i;
	}

int popi(){
	if(SP==stack-1)err("trying to pop from empty stack");
	return SP--->i;
	}

void pushc(char c){
	if(SP+1==stack+10000)err("trying to push into a full stack");
	(++SP)->c=c;
	}

char popc(){
	if(SP==stack-1)err("trying to pop from empty stack");
	return SP--->c;
	}

void pushf(double f){
	if(SP+1==stack+10000)err("trying to push into a full stack");
	(++SP)->f=f;
	}

double popf(){
	if(SP==stack-1)err("trying to pop from empty stack");
	return SP--->f;
	}

void pushp(void *p){
	if(SP+1==stack+10000)err("trying to push into a full stack");
	(++SP)->p=p;
	}

void *popp(){
	if(SP==stack-1)err("trying to pop from empty stack");
	return SP--->p;
	}

void put_i(){
	printf("=> %d",popi());
	}
void put_d(){
	printf("=> %f",popf());
	}
void put_c(){
    printf("=> %c", popc());
	}
void put_s(){
    printf("=> %s", (char*)popp());
	}
char get_c() {
    char c;
    scanf(" %c", &c); // spatiu inainte de %c ca sa consumam spatiu de la inceput
	pushc(c);
    return c;
}
int get_i() {
    int i;
    scanf("%d", &i);
	pushi(i);
    return i;
}
void mvInit(){
	Symbol *fn=addExtFn("put_i",put_i,(Type){TB_VOID,NULL,-1});
	addFnParam(fn,"i",(Type){TB_INT,NULL,-1});
	}
void mvDouble(){
	Symbol *fn=addExtFn("put_d",put_d,(Type){TB_VOID,NULL,-1});
	addFnParam(fn,"d",(Type){TB_DOUBLE,NULL,-1});
	}
void mvChar(){
	Symbol *fn=addExtFn("put_c",put_c,(Type){TB_VOID,NULL,-1});
	addFnParam(fn,"c",(Type){TB_CHAR,NULL,-1});
}
void mvString(){
	Symbol *fn=addExtFn("put_s",put_s,(Type){TB_VOID,NULL,-1});
	addFnParam(fn,"p",(Type){TB_CHAR,NULL,0});
}
void mvGetInt(){
	Symbol *fn=addExtFn("get_i",get_i,(Type){TB_INT,NULL,-1});
}
void mvGetChar(){
	Symbol *fn=addExtFn("get_c",get_c,(Type){TB_CHAR,NULL,-1});
}



void run(){
	SP=stack-1;
	Val *FP=NULL;		// valoarea initiala nu conteaza
	Instr *IP=instructions;		// Instruction pointer - pointer la instructiunea curenta
	Val v;
	int iArg,iTop,iBefore;
	double fTop,fBefore;
	void *pTop;
	void(*extFnPtr)();
	for(;;){
		// afiseaza indexul instructiunii curente
		// si numarul de valori din stiva
		printf("%03d/%d\t",(int)(IP-instructions),(int)(SP-stack+1));
		switch(IP->op){
			case OP_HALT:
				printf("HALT");
				return;
			case OP_PUSH_I:
				printf("PUSH.i\t%d",IP->arg.i);
				pushi(IP->arg.i);
				IP++;
				break;
			case OP_PUSH_C:
				printf("PUSH.c\t%c",IP->arg.c);
				pushc(IP->arg.c);
				IP++;
				break;
			case OP_PUSH_P:
				printf("PUSH.p\t%s",IP->arg.p);
				pushp(IP->arg.p);
				IP++;
				break;
			case OP_CALL:
				pushp(IP+1);
				printf("CALL\t%d",IP->arg.i);
				IP=instructions+IP->arg.i;
				break;
			case OP_CALL_EXT:
				extFnPtr=IP->arg.extFnPtr;
				printf("CALL_EXT\t%p\n",extFnPtr);
				extFnPtr();
				IP++;
				break;
			case OP_ENTER:
				pushp(FP);
				FP=SP;
				SP+=IP->arg.i;
				printf("ENTER\t%d",IP->arg.i);
				IP++;
				break;
			case OP_RET_VOID:
				iArg=IP->arg.i;
				printf("RET_VOID\t%d",iArg);
				IP=FP[-1].p;
				SP=FP-iArg-2;
				FP=FP[0].p;
				break;
			case OP_JMP:
				printf("JMP\t%d",IP->arg.i);
				IP=instructions+IP->arg.i;
				break;
			case OP_JF:
				iTop=popi();
				printf("JF\t%d\t// %d",IP->arg.i,iTop);
				IP=iTop ? IP+1 : instructions+IP->arg.i;
				break;
			case OP_FPLOAD:
				v=FP[IP->arg.i];
				pushv(v);
				printf("FPLOAD\t%d\t// i:%d, f:%g",IP->arg.i,v.i,v.f);
				IP++;
				break;
			case OP_FPSTORE:
				v=popv();
				FP[IP->arg.i]=v;
				printf("FPSTORE\t%d\t// i:%d, f:%g",IP->arg.i,v.i,v.f);
				IP++;
				break;
			case OP_ADD_I:
				iTop=popi();
				iBefore=popi();
				pushi(iBefore+iTop);
				printf("ADD.i\t// %d+%d -> %d",iBefore,iTop,iBefore+iTop);
				IP++;
				break;
			case OP_ADD_F:
				fTop=popf();
				fBefore=popf();
				pushf(fBefore+fTop);
				printf("ADD.f\t// %f+%f -> %f",fBefore,fTop,fBefore+fTop);
				IP++;
				break;
			case OP_LESS_I:
				iTop=popi();
				iBefore=popi();
				pushi(iBefore<iTop);
				printf("LESS.i\t// %d<%d -> %d",iBefore,iTop,iBefore<iTop);
				IP++;
				break;
			case OP_LESS_F:
				fTop=popf();
				fBefore=popf();
				pushi(fBefore<fTop);
				printf("LESS.f\t// %f<%f -> %d",fBefore,fTop,fBefore<fTop);
				IP++;
				break;
			// instructiuni adaugate pentru generarea de cod
			case OP_RET:
				v=popv();
				iArg=IP->arg.i;
				printf("RET\t%d\t// i:%d, f:%g",iArg,v.i,v.f);
				IP=FP[-1].p;
				SP=FP-iArg-2;
				FP=FP[0].p;
				pushv(v);
				break;
			case OP_PUSH_F:
				printf("PUSH.f\t%g",IP->arg.f);
				pushf(IP->arg.f);
				IP++;
				break;
			case OP_CONV_F_I:
				fTop=popf();
				pushi((int)fTop);
				printf("CONV.f.i\t// %g -> %d",fTop,(int)fTop);
				IP++;
				break;
			case OP_LOAD_I:
				pTop=popp();
				pushi(*(int*)pTop);
				printf("LOAD.i\t// *(int*)%p -> %d",pTop,*(int*)pTop);
				IP++;
				break;
			case OP_LOAD_C:
				pTop=popp();
				pushc(*(char*)pTop);
				printf("LOAD.c\t// *(char*)%p -> %c",pTop,*(char*)pTop);
				IP++;
				break;
			case OP_LOAD_F:
				pTop=popp();
				pushf(*(float*)pTop);
				printf("LOAD.i\t// *(double*)%p -> %d",pTop,*(float*)pTop);
				IP++;
				break;
			case OP_STORE_I:
				iTop=popi();
				v=popv();
				*(int*)v.p=iTop;
				pushi(iTop);
				printf("STORE.i\t//*(int*)%p=%d",v.p,iTop);
				IP++;
				break;
			case OP_STORE_C:
				iTop=popc();
				v=popv();
				*(char*)v.p=iTop;
				pushc(iTop);
				printf("STORE.c\t//*(char*)%p=%c",v.p,iTop);
				IP++;
				break;
			case OP_FPADDR_I:
				pTop=&FP[IP->arg.i].i;
				pushp(pTop);
				printf("FPADDR\t%d\t// %p",IP->arg.i,pTop);
				IP++;
				break;
			case OP_FPADDR_C:
				pTop=&FP[IP->arg.c].c;
				pushp(pTop);
				printf("FPADDR\t%d\t// %p",IP->arg.c,pTop);
				IP++;
				break;
			case OP_SUB_I:
				iTop=popi();
				iBefore=popi();
				pushi(iBefore-iTop);
				printf("SUB.i\t// %d-%d -> %d",iBefore,iTop,iBefore-iTop);
				IP++;
				break;
			case OP_MUL_I:
				iTop=popi();
				iBefore=popi();
				pushi(iBefore*iTop);
				printf("MUL.i\t// %d*%d -> %d",iBefore,iTop,iBefore*iTop);
				IP++;
				break;
			case OP_DROP:
				popv();
				printf("DROP");
				IP++;
				break;
			default:err("run: instructiune neimplementata: %d",IP->op);
			}
		putchar('\n');
		}
	}

/* Programul codifica urmatorul cod sursa:
f(2.0);
void f(double n){
    double i;
    for(i=0.0;i<n;i=i+0.5){
        put_d(i);
    }
}
*/
void genTestProgram(){
	addInstrWithDouble(OP_PUSH_F,2.0);
	int callPos=addInstr(OP_CALL);
	addInstr(OP_HALT);
	instructions[callPos].arg.i=nInstructions;
	addInstrWithInt(OP_ENTER,1);
	// i=0;  (componentele lui for sunt implementate sub forma unui while)
	addInstrWithDouble(OP_PUSH_F,0.0);
	addInstrWithInt(OP_FPSTORE,1);
	// while(i<n){
	int whilePos=addInstrWithInt(OP_FPLOAD,1);
	addInstrWithInt(OP_FPLOAD,-2);
	addInstr(OP_LESS_F);
	int jfAfterFor=addInstr(OP_JF);
	// put_i(i);
	addInstrWithInt(OP_FPLOAD,1);
	Symbol *s=findSymbol("put_d");
	if(!s)err("undefined: put_d");
	int putiCall=addInstr(OP_CALL_EXT);
	instructions[putiCall].arg.extFnPtr=s->fn.extFnPtr;
	// i=i+1
	addInstrWithInt(OP_FPLOAD,1);
	addInstrWithDouble(OP_PUSH_F,0.5);
	addInstr(OP_ADD_F);
	addInstrWithInt(OP_FPSTORE,1);
	// } ( urmatoarea iteratie )
	addInstrWithInt(OP_JMP,whilePos);
	// revenire din functie
	instructions[jfAfterFor].arg.i=nInstructions;
	addInstrWithInt(OP_RET_VOID,1);
	}
