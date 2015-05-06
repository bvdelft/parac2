{-# LANGUAGE QuasiQuotes,TemplateHaskell #-}
module Larl where
import Language.LBNF
--{} means maybe

bnfc [lbnf|

CompilationUnit. CompilationUnit ::= {PackageDecl} [ImportDecl] [TypeDecl] ;

PackageDecl. PackageDecl ::= Name ;

ImportDecl. ImportDecl ::= Bool Name Bool ;

InterfaceTypeDecl. TypeDecl ::= InterfaceDecl ;
ClassTypeDecl. TypeDecl ::= ClassDecl ;

EnumDecl. ClassDecl ::= [Modifier] Ident [RefType] EnumBody ;
ClassDecl. ClassDecl ::= [Modifier] Ident [TypeParam] {RefType} [RefType] ClassBody ;

ClassBody. ClassBody ::= [Decl] ;

EnumBody. EnumBody ::= [EnumConstant] [Decl] ;

EnumConstant. EnumConstant ::= Ident [Exp] {ClassBody} ;

InterfaceDecl. InterfaceDecl ::= [Modifier] Ident [TypeParam] [RefType] InterfaceBody ;

InterfaceBody. InterfaceBody ::= [MemberDecl] ;

InitDecl. Decl ::= Bool Block ;
MemberDecl. Decl ::= MemberDecl ;

ExpDecl. MemberDecl ::= [Modifier] Ident Exp ;
LockDecl. MemberDecl ::= [Modifier] Ident [Ident] {LockProperties} ;
MemberInterfaceDecl. MemberDecl ::= InterfaceDecl ;
MemberClassDecl. MemberDecl ::= ClassDecl ;
ConstructorDecl. MemberDecl ::= [Modifier] [TypeParam] Ident [FormalParam] [ExceptionSpec] ConstructorBody ;
MethodDecl. MemberDecl ::= [Modifier] [TypeParam] {Type} Ident [FormalParam] [ExceptionSpec] MethodBody ;
FieldDecl. MemberDecl ::= [Modifier] Type [VarDecl] ;

VarDecl. VarDecl ::= VarDeclId {VarInit} ;

VarDeclArray. VarDeclId ::= VarDeclId ;
VarId. VarDeclId ::= Ident ;

InitArray. VarInit ::= ArrayInit ;
InitExp. VarInit ::= Exp ;

FormalParam. FormalParam ::= [Modifier] Type Bool VarDeclId ;

MethodBody. MethodBody ::= {Block} ;

ConstructorBody. ConstructorBody ::= {ExplConstrInv} [BlockStmt] ;

PrimarySuperInvoke. ExplConstrInv ::= Exp [NonWildTypeExp] [Exp] ;
SuperInvoke. ExplConstrInv ::= [NonWildTypeExp] [Exp] ;
ThisInvoke. ExplConstrInv ::= [NonWildTypeExp] [Exp] ;

Expects. Modifier ::= LockExp ;
Closes. Modifier ::= LockExp ;
Opens. Modifier ::= LockExp ;
Writes. Modifier ::= Exp ;
Reads. Modifier ::= Exp ;
Commutative. Modifier ::= ;
Transitive. Modifier ::= ;
Reflexive. Modifier ::= ;
Typemethod. Modifier ::= ;
Native. Modifier ::= ;
Volatile. Modifier ::= ;
Transient. Modifier ::= ;
StrictFP. Modifier ::= ;
Static. Modifier ::= ;
Final. Modifier ::= ;
Abstract. Modifier ::= ;
Protected. Modifier ::= ;
Private. Modifier ::= ;
Public. Modifier ::= ;

LockVar. LockExp ::= Ident ;
LockExp. LockExp ::= [Lock] ;

Block. Block ::= [BlockStmt] ;

LocalExp. BlockStmt ::= [Modifier] Ident Exp ;
LocalLock. BlockStmt ::= [Modifier] Ident [Ident] {LockProperties} ;
LocalVars. BlockStmt ::= [Modifier] Type [VarDecl] ;
LocalClass. BlockStmt ::= ClassDecl ;
BlockStmt. BlockStmt ::= Stmt ;

CloseBlock. Stmt ::= Lock Block ;
OpenBlock. Stmt ::= Lock Block ;
Close. Stmt ::= Lock ;
Open. Stmt ::= Lock ;
Labeled. Stmt ::= Ident Stmt ;
Try. Stmt ::= Block [Catch] {Block} ;
Throw. Stmt ::= Exp ;
Synchronized. Stmt ::= Exp Block ;
Return. Stmt ::= {Exp} ;
Continue. Stmt ::= {Ident} ;
Break. Stmt ::= {Ident} ;
Do. Stmt ::= Stmt Exp ;
Switch. Stmt ::= Exp [SwitchBlock] ;
Assert. Stmt ::= Exp {Exp} ;
ExpStmt. Stmt ::= Exp ;
Empty. Stmt ::= ;
EnhancedFor. Stmt ::= [Modifier] Type Ident Exp Stmt ;
BasicFor2. Stmt ::= {ForInit} {Exp} Stmt ;
BasicFor. Stmt ::= {ForInit} {Exp} [Exp] Stmt ;
While. Stmt ::= Exp Stmt ;
IfThenElse. Stmt ::= Exp Stmt Stmt ;
IfThen. Stmt ::= Exp Stmt ;
StmtBlock. Stmt ::= Block ;

Catch. Catch ::= FormalParam Block ;

SwitchBlock. SwitchBlock ::= SwitchLabel [BlockStmt] ;

Default. SwitchLabel ::= ;
SwitchCase. SwitchLabel ::= Exp ;

ForInitExps. ForInit ::= [Exp] ;
ForLocalVars. ForInit ::= [Modifier] Type [VarDecl] ;

ExceptionSpec. ExceptionSpec ::= [Modifier] ExceptionType ;


ExpExp. Exp ::= ExpLit ;
Assign. Exp ::= Lhs AssignOp Exp ;
Cond. Exp ::= Exp Exp Exp ;
InstanceOf. Exp ::= Exp RefType ;
BinOp. Exp ::= Exp Op Exp ;
Cast. Exp ::= Type Exp ;
PreNot. Exp ::= Exp ;
PreBitCompl. Exp ::= Exp ;
PreMinus. Exp ::= Exp ;
PrePlus. Exp ::= Exp ;
PreDecrement. Exp ::= Exp ;
PreIncrement. Exp ::= Exp ;
PostDecrement. Exp ::= Exp ;
PostIncrement. Exp ::= Exp ;
ExpName. Exp ::= Name ;
ArrayAccess. Exp ::= ArrayIndex ;
MethodInv. Exp ::= MethodInvocation ;
FieldAccess. Exp ::= FieldAccess ;
ArrayCreateInit. Exp ::= Type Int ArrayInit ;
ArrayCreate. Exp ::= Type [Exp] Int ;
QualInstanceCreation. Exp ::= Exp [TypeExp] Ident [Exp] {ClassBody} ;
InstanceCreation. Exp ::= [TypeExp] ClassType [Exp] {ClassBody} ;
Paren. Exp ::= Exp ;
ThisClass. Exp ::= Name ;
This. Exp ::= ;
ClassLit. Exp ::= {Type} ;
Lit. Exp ::= Literal ;

Null. Literal ::= ;
String. Literal ::= String ;
Char. Literal ::= Char ;
Boolean. Literal ::= Bool ;
Double. Literal ::= Double ;
Float. Literal ::= Double ;
Word. Literal ::= Integer ;
Int. Literal ::= Integer ;

COr. Op ::= ;
CAnd. Op ::= ;
Xor. Op ::= ;
Or. Op ::= ;
And. Op ::= ;
NotEq. Op ::= ;
Equal. Op ::= ;
GThanE. Op ::= ;
LThanE. Op ::= ;
GThan. Op ::= ;
LThan. Op ::= ;
RRShift. Op ::= ;
RShift. Op ::= ;
LShift. Op ::= ;
Sub. Op ::= ;
Add. Op ::= ;
Rem. Op ::= ;
Div. Op ::= ;
Mult. Op ::= ;

OrA. AssignOp ::= ;
XorA. AssignOp ::= ;
AndA. AssignOp ::= ;
RRShiftA. AssignOp ::= ;
RShiftA. AssignOp ::= ;
LShiftA. AssignOp ::= ;
SubA. AssignOp ::= ;
AddA. AssignOp ::= ;
RemA. AssignOp ::= ;
DivA. AssignOp ::= ;
MultA. AssignOp ::= ;
EqualA. AssignOp ::= ;

ArrayLhs. Lhs ::= ArrayIndex ;
FieldLhs. Lhs ::= FieldAccess ;
NameLhs. Lhs ::= Name ;

ArrayIndex. ArrayIndex ::= Exp Exp ;

ClassFieldAccess. FieldAccess ::= Name Ident ;
SuperFieldAccess. FieldAccess ::= Ident ;
PrimaryFieldAccess. FieldAccess ::= Exp Ident ;

TypeMethodCall. MethodInvocation ::= Name [NonWildTypeExp] Ident [Exp] ;
ClassMethodCall. MethodInvocation ::= Name [NonWildTypeExp] Ident [Exp] ;
SuperMethodCall. MethodInvocation ::= [NonWildTypeExp] Ident [Exp] ;
PrimaryMethodCall. MethodInvocation ::= Exp [NonWildTypeExp] Ident [Exp] ;
MethodCall. MethodInvocation ::= Name [Exp] ;

ArrayInit. ArrayInit ::= [VarInit] ;

RefType. Type ::= RefType ;
PrimType. Type ::= PrimType ;

ArrayType. RefType ::= Type ;
TypeVariable. RefType ::= Ident ;
ClassRefType. RefType ::= ClassType ;

ActualArg. TypeExp ::= NonWildTypeExp ;
Wildcard. TypeExp ::= {WildcardBound} ;

ActualLockState. NonWildTypeExp ::= LockExp ;
ActualType. NonWildTypeExp ::= RefType ;

SuperBound. WildcardBound ::= RefType ;
ExtendsBound. WildcardBound ::= RefType ;

ExpT. PrimType ::= ;
ActorT. PrimType ::= ;
DoubleT. PrimType ::= ;
FloatT. PrimType ::= ;
CharT. PrimType ::= ;
LongT. PrimType ::= ;
IntT. PrimType ::= ;
ShortT. PrimType ::= ;
ByteT. PrimType ::= ;
BooleanT. PrimType ::= ;

LockStateParam. TypeParam ::= Ident ;
ExpParam. TypeParam ::= Ident ;
ActorParam. TypeParam ::= Ident ;
TypeParam. TypeParam ::= Ident [RefType] ;


Var. Actor ::= Ident ;
Actor. Actor ::= Name ;

Atom. Atom ::= Name [Actor] ;

Lock. Lock ::= Name [Name] ;

Ident. Ident ::= String ;

Name. Name ::= [Ident] ;
      |]
