
import os.path
from os import path
import sys

par=1
flag=0


reservedWords=['program','declare','if','else','while','switchcase','forcase','incase','case','default','not','and','or','function','procedure','call','return','in','inout','input','print']


def givefilename():
     filen = sys.argv[1]
     if os.path.isfile(filen):
          if filen[len(filen)-3]!='.' or filen[len(filen)-2]!='c' or filen[len(filen)-1]!='i':
               raise NameError('invalid input! file exists but end of file should be .ci')
          else:
               return filen
     else:
          raise FileNotFoundError('file not found')
        
         
name=givefilename()
file=open(name,'r')


      

def lex_analyzer():
    global par
    global flag
    state="start"
    element_type=""
    error=""
    token_length=0
    p=0   
    x=[]
    while(state!="token"):
      char=file.read(1)
      if state=="start":
         if (char.isspace()):
             state="start"                           
         elif (char.isdigit()):
              state="digit"
              x.append(char)       
         elif (char>='a' and char<='z') or (char>='A' and char<='Z'):
              state="idk"
              x.append(char)
              token_length+=1
         elif (char=='+') or (char=='-'):
              state="addOperator"
              x.append(char)
              element_type=state
              state="token"
         elif (char=='*' or char=='/'):
              state="mullOperator"
              x.append(char)
              element_type=state
              state="token"
         elif (char=='{' or char=='}' or char=='(' or char==')' or char=='[' or char==']'):
              state="groupSymbol"
              x.append(char)
              element_type=state
              state="token"
         elif (char==';'or char==','):
              state="delimiter"
              x.append(char)
              element_type=state
              state="token"
         elif (char==':'):
              state="asgn"
              x.append(char)
         elif (char=='<'):
              state="smaller"
              x.append(char)
         elif (char=='>'):
              state="larger"
              x.append(char)
         elif (char=='='):
              state="token"
              x.append(char)
              element_type="relOperator"
         elif (char=='#'):
              state="rem"
              x.append(char)
         elif (char=='.'):
              x.append(char)
              state="token"
              element_type="finalOperator"
         elif (char==''):
              state="token"  
         else :      
              raise ValueError('a non-language character: '+char+' has appeared at line : '+str(par)+'')                 
      elif state=="digit":
         if  char.isdigit() :
                x.append(char)
                if int("".join(x))<-pow(2,32)+1 or int("".join(x))>pow(2,32)-1 :
                     raise ValueError('invalid number: '+"".join(x)+ ' out of bounds at line ' +str(par)+'')
         elif not char.isdigit() :   
               element_type=state
               state="token"
               file.seek(file.tell()-1)
               if(char=='\n'):
                    par=par-1          
      elif state=="idk":      
         if  char.isdigit() or (char>='a' and char<='z') or (char>'A' and char<='Z') :
                x.append(char)
                token_length+=1
                if token_length>30:
                     raise ValueError('length > 30 invalid '+"".join(x)+ ' has appeared at line: '+str(par)+'')
         else:           
                element_type="keyword"
                state="token"
                file.seek(file.tell()-1)
                if(char=='\n'):
                    par=par-1
      elif state=="asgn":
         if char=='=':
                x.append(char)
                state="token"
                element_type="assignment"
         else:
                raise ValueError('invalid value :'+"".join(x)+' at line: '+str(par)+' assignment is :=')
      elif state=="smaller":
         if char=='=' or char=='>':
                x.append(char)
                state="token"
                element_type="relOperator"
         else:
                element_type="relOperator"
                state="token"
                file.seek(file.tell()-1)
                if(char=='\n'):
                    par=par-1
      elif state=="larger":
         if char=='=':
                x.append(char)
                state="token"
                element_type="relOperator"
         else:
                element_type="relOperator"
                state="token"
                file.seek(file.tell()-1)
                if(char=='\n'):
                    par=par-1
      elif state=="rem":
         if char!='#':
                if char!='\n':
                   x.append(char)
                   state="rem"
         else:
                element_type="rem"
                x.append('#')
                x.clear()
                state="start"          
      if char=='\n':
           par+=1
      if state=="token":
           lineNo=par             
    if "".join(x) in  reservedWords:
         element_type="reservedWord"    
    if char=='':
         file.seek(0,0)
         par=1
    if ("".join(x)=="procedure" or "".join(x)=="function"):
         flag=1
    return "".join(x),lineNo,element_type




#while(1):
#    lex,line,e_type=lex_analyzer()
#    if  lex!='':
#         print("element: {:20s} line: {:15s}  element_type: {}".format(lex,str(line),e_type))
#   else:
#         print("\n")
#          break

num_quad=1
global quads_list
quads_list=[]
temp_vars=[]
T_k=1
global cF
global cFs

def nextquad():
    global num_quad
    return num_quad

def genquad(a_elem,b_elem,c_elem,d_elem):
    global quads_list,num_quad
    l5_elem=[]
    l5_elem.append(nextquad())
    l5_elem.append(a_elem)
    l5_elem.append(b_elem)
    l5_elem.append(c_elem)
    l5_elem.append(d_elem)
    quads_list.append(l5_elem)
    num_quad+=1
    return l5_elem

def newtemp():
    global T_k
    global temp_vars
    Tel=['T_']
    Tel.append(str(T_k))
    temp_var="".join(Tel)
    temp_vars.append(temp_var)
    T_k+=1
    entity=Entity()
    entity.name=temp_var
    entity.TempVar.offset=compute_offset()
    entity.type="TEMP"
    new_entity(entity)
    return temp_var

def emptylist():
    empty_quad=[]	
    return empty_quad

def makelist(x):
    quad1_el=[x]
    return quad1_el

def merge(l1, l2):
    merge_l=[]
    merge_l+=l1+l2
    return merge_l


def backpatch(list, z):
    global quads_list
    for i in range(len(list)):
	     for j in range(len(quads_list)):
	           if(list[i]==quads_list[j][0] and quads_list[j][4]=='_'):
		            quads_list[j][4]=z
		            break
    return


class Argument():
     def __init__(self):
          self.name=""
          self.parMode=""
          self.type="Int"


class Entity():
     def __init__(self):
          self.name=""
          self.type=""
          self.variable=self.Variable()
          self.parameter=self.Parameter()
          self.TempVar=self.TempVar()
          self.subprogram=self.SubProgram()
          
     class Variable:
          def __init__(self):
               self.offset=0
               self.type="Int"
    
     class Parameter:
          def __init__(self):
               self.mode=""
               self.offset=0

     class TempVar:
          def __init__(self):
               self.offset=0
               self.type="Int"

     class SubProgram:
          def __init__(self):
               self.argumentlist=[]
               self.startQuad=0
               self.frameLength=0
               self.type=""
class Scope():
     def __init__(self):
          self.entityList=[]
          self.nestingLevel=0
          self.enclosingScope=None
          self.name=""

def new_arg(scheme):
     global topScope
     topScope.entityList[-1].subprogram.argumentlist.append(scheme)         

def new_entity(scheme):
     global topScope
     topScope.entityList.append(scheme)

topScope=None

def new_scope(name):
     global topScope
     next_scope=Scope()
     next_scope.enclosingScope=topScope
     next_scope.name=name
     if(topScope!=None):
          next_scope.nestingLevel=topScope.nestingLevel+1
     else:
          next_scope.nestingLevel=0
     topScope=next_scope

def delete_scope():
     global topScope
     del_scope=topScope
     topScope=topScope.enclosingScope
     del del_scope




def compute_offset():
     sum=0
     global topScope
     if(topScope.entityList is not []):
       for entity in (topScope.entityList):
          if(entity.type=="VAR" or entity.type=="PARAM" or entity.type=="TEMP"):
               sum=sum+1
     offset=sum*4+12
     return offset

def compute_StartQuad():
     global topScope
     topScope.enclosingScope.entityList[-1].subprogram.startQuad=nextquad()

def compute_framelength():
     global topScope
     topScope.enclosingScope.entityList[-1].subprogram.frameLength=compute_offset()




def add_parameters():
     global topScope
     for arg in topScope.enclosingScope.entityList[-1].subprogram.argumentlist:
          entity=Entity()
          entity.name=arg.name
          entity.parameter.mode=arg.parMode
          entity.type="PARAM"
          entity.parameter.offset=compute_offset()
          new_entity(entity)

def Symbol_table():
     global cFs
     global topScope
     print('______________________________________________________________________________________________________________________________\n')
     scope=topScope
     while scope!=None:
          print("scope name is "+scope.name+" with nesting level: "+str(scope.nestingLevel))
          print("\tentities are :")
          for entity in scope.entityList:
               if(entity.type=="TEMP"):
                    cFs.write("\tentity name is "+entity.name+"\t with type: "+entity.type+"\t temporaly type: "+entity.TempVar.type+" \t offset: "+str(entity.TempVar.offset)+"\n")
                    print("\tentity name is "+entity.name+"\t with type: "+entity.type+"\t temporaly type: "+entity.TempVar.type+" \t offset: "+str(entity.TempVar.offset))
               elif(entity.type=="VAR"):
                    cFs.write("\tentity name is "+entity.name+"\t with type: "+entity.type+"\t variable type: "+entity.variable.type+" \t offset: "+str(entity.variable.offset)+"\n")
                    print("\tentity name is "+entity.name+"\t with type: "+entity.type+"\t variable type: "+entity.variable.type+" \t offset: "+str(entity.variable.offset))
               elif(entity.type=="SUBPR"):
                    if(entity.subprogram.type=="Procedure"):
                         cFs.write("\tentity name is "+entity.name+"\t with type: "+entity.type+"\t procedure type: "+entity.subprogram.type+" \t startQuad: "+str(entity.subprogram.startQuad)+" \t frame lenghth: "+str(entity.subprogram.frameLength)+"\n")
                         print("\tentity name is "+entity.name+"\t with type: "+entity.type+"\t procedure type: "+entity.subprogram.type+" \t startQuad: "+str(entity.subprogram.startQuad)+" \t frame lenghth: "+str(entity.subprogram.frameLength))
                         cFs.write("\t\targuments:\n")
                         print("\t\targuments:")
                         for argument in entity.subprogram.argumentlist:
                              cFs.write("\targument name is "+argument.name+"\t with type: "+argument.type+"\t par mode: "+argument.parMode+"\n")
                              print("\t\targument name is "+argument.name+"\t with type: "+argument.type+"\t par mode: "+argument.parMode)
                    elif(entity.subprogram.type=="Function"): 
                         cFs.write("\tentity name is "+entity.name+"\t with type: "+entity.type+"\t function type: "+entity.subprogram.type+" \t startQuad: "+str(entity.subprogram.startQuad)+" \t frame lenghth: "+str(entity.subprogram.frameLength)+"\n")
                         cFs.write("\t\targuments:\n")
                         print("\tentity name is "+entity.name+"\t with type: "+entity.type+"\t function type: "+entity.subprogram.type+" \t startQuad: "+str(entity.subprogram.startQuad)+" \t frame lenghth: "+str(entity.subprogram.frameLength))
                         print("\t\targuments:")
                         for argument in entity.subprogram.argumentlist:
                              cFs.write("\t\targument name is "+argument.name+"\t with type: "+argument.type+"\t par mode: "+argument.parMode+"\n")
                              print("\t\targument name is "+argument.name+"\t with type: "+argument.type+"\t par mode: "+argument.parMode)
               elif (entity.type=="PARAM"):
                    cFs.write("\t\tentity name is "+entity.name+"\t with type: "+entity.type+"\t par mode: "+entity.parameter.mode+" \toffset "+str(entity.parameter.offset)+"\n")
                    print("\t\tentity name is "+entity.name+"\t with type: "+entity.type+"\t par mode: "+entity.parameter.mode+" \toffset "+str(entity.parameter.offset))
          scope=scope.enclosingScope
     print('______________________________________________________________________________________________________________________________\n')


def syntax_analyzer():
        global ff
        global temp
        global par
        global lex
        global line
        global e_type 
        global flag
        
        ff=0   
        lex,line,e_type=lex_analyzer()
        par=line

        def program():
               global par
               global lex
               global line
               global e_type  
               if(lex=="program"):
                    twr=par
                    lex,line,e_type=lex_analyzer()
                    par=line
                    if(e_type=="keyword"):
                         wi=lex
                         lex,line,e_type=lex_analyzer()
                         par=line
                         block(wi,1)
                         if(lex=='.'):
                              lex,line,e_type=lex_analyzer()
                              par=line
                              return
                         else:
                              raise SyntaxError('invalid Syntax  missing curly brackets, to open or close the program { }.')
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(twr)+' please add name of the program after reserved word program')
               else:
                   raise SyntaxError('invalid Syntax at line : '+str(par)+' please add the reserved word program at the start of file')
         
         
        def block(name,fl):
               new_scope(name)
               if(fl!=1):
                    add_parameters()
               declarations()    
               subprograms() 
               
               genquad('begin_block',name,'_','_')
               if(fl!=1):
                    compute_StartQuad()
               statements()
               if fl==1:
                    genquad('halt','_','_','_')
               else:
                    compute_framelength()
               genquad('end_block',name,'_','_')
               Symbol_table()
               delete_scope()
               return


        def declarations():
              global cF
              global e_type
              global lex
              global line
              count=0
              while(lex=="declare"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    cF.write('int ')
                    if count==0 and lex==';':
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' declaration is empty')
                    count+=1     
                    varlist()  
                    cF.write(';\n\t')
                    if(lex==';'):
                         lex,line,e_type=lex_analyzer()
                         par=line         
                    else: 
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' maybe missing ; at the end of declaration but check please if missing , between 2 or more variables')
              return


        def varlist():
               global cF
               global e_type
               global lex
               global line 
               if(e_type=="keyword"):
                    cF.write(lex)
                    entity=Entity()
                    entity.name=lex
                    entity.type="VAR"
                    entity.variable.offset=compute_offset()
                    new_entity(entity)
                    lex,line,e_type=lex_analyzer()
                    par=line  
                    while(lex==','):
                         cF.write(lex)
                         lex,line,e_type=lex_analyzer()
                         par=line  
                         twr=line
                         if(e_type=="keyword"):
                              cF.write(lex)
                              entity=Entity()
                              entity.name=lex
                              entity.type="VAR"
                              entity.variable.offset=compute_offset()
                              new_entity(entity)
                              lex,line,e_type=lex_analyzer()
                              par=line
                         else: 
                              raise SyntaxError('invalid Syntax at line : '+str(twr)+' please check , between the elements')
               return

        def subprograms():
               global e_type
               global lex
               global line 
               while(lex=="function" or lex=="procedure"): 
                   subprogram()
               return
        
        def subprogram():
               global e_type
               global lex
               global line 
               global flag
               if(lex=="procedure"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    if(e_type=="keyword"):                        
                         wi=lex
                         name=lex
                         entity=Entity()
                         entity.name=name
                         entity.type="SUBPR"
                         entity.subprogram.type="Procedure"
                         new_entity(entity)
                         lex,line,e_type=lex_analyzer()
                         par=line
                         if(lex=='('):
                             lex,line,e_type=lex_analyzer()
                             par=line
                             formal_par_list()
                             if(lex==')'):
                                  lex,line,e_type=lex_analyzer()
                                  par=line
                                  block(wi,0)
                                  return
                             else:
                                  raise SyntaxError('invalid Syntax at line : '+str(par)+' please close right ) or check if missing reserved word before the variable or missing , between vars')
                         else:
                              raise SyntaxError('invalid Syntax at line : '+str(par)+' please open left (')
                    else:
                        raise SyntaxError('invalid Syntax at line : '+str(par)+' please add name for procedure')
               elif(lex=="function"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    if(e_type=="keyword"):
                         name=lex
                         wi=lex
                         entity=Entity()
                         entity.name=name
                         entity.type="SUBPR"
                         entity.subprogram.type="Function"
                         new_entity(entity)
                         lex,line,e_type=lex_analyzer()
                         par=line
                         if(lex=='('):
                              lex,line,e_type=lex_analyzer()
                              par=line
                              formal_par_list()
                              if(lex==')'):
                                   lex,line,e_type=lex_analyzer()
                                   par=line
                                   block(wi,0)
                                   return
                              else:
                                   raise SyntaxError('invalid Syntax at line : '+str(par-1)+' please close right ) after formalparlist or check if missing reserved word before the variable or missing , between vars')
                         else:
                              raise SyntaxError('invalid Syntax at line : '+str(par)+' please open left ( before formalparlist')
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' please add name for function')

        def formal_par_list():
               global e_type
               global lex
               global line 
               formal_par_item()
               while(lex==','):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    formal_par_item()
               return
     
        def formal_par_item():
               global e_type
               global lex
               global line 
               global par
               if(lex=="in"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    if(e_type=="keyword"):
                         argument=Argument()
                         argument.parMode="CV"
                         argument.name=lex
                         new_arg(argument)
                         lex,line,e_type=lex_analyzer()
                         par=line
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' please add name of parameter after in')         
               elif(lex=="inout"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    if(e_type=="keyword"):
                         argument=Argument()
                         argument.parMode="REF"
                         argument.name=lex
                         new_arg(argument)
                         lex,line,e_type=lex_analyzer()
                         par=line      
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' please add name of parameter after inout')       
               return


        def statements():
               global e_type
               global lex
               global line 
               global par
             
               if(lex=='{'):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    ep=line
                    statement()  
                    while(lex==';'):
                        lex,line,e_type=lex_analyzer()
                        par=line
                        ep=line
                        statement()  
                        
                    if(lex=='}'):
                        lex,line,e_type=lex_analyzer()
                        par=line
                        return
                    else:
                         raise SyntaxError('invalid Syntax please check at line '+str(par)+' missing close of block at statement')       
               else:
                    statement()
                    if(lex==';'):
                        lex,line,e_type=lex_analyzer()
                        par=line
                        return
                    else:
                         raise SyntaxError('invalid Syntax before line : '+str(par)+' please close correctly the statement ') 

        def statement():
               global e_type
               global lex
               global line 
               if(e_type=="keyword"):
                    assignment_stat()
               elif(lex=="if"):
                    if_stat()
               elif(lex=="while"):
                    while_stat()
               elif(lex=="switchcase"):
                    switchcase_stat()
               elif(lex=="forcase"):
                    forcase_stat()
               elif(lex=="incase"):
                    incase_stat()
               elif(lex=="call"):
                    call_stat()
               elif(lex=="return"):
                    return_stat()
               elif(lex=="input"):
                    input_stat()
               elif(lex=="print"):
                    print_stat()
               return 
        
        
        def assignment_stat():
               global e_type
               global lex
               global line 
               global temp
               global ff
               if(e_type=="keyword"):
                    kwd=lex
                    lex,line,e_type=lex_analyzer()
                    par=line
                    if(lex==":="):
                         lex,line,e_type=lex_analyzer()
                         par=line
                         E_place=expression()    
                         genquad(':=',E_place,'_',kwd)
                         return
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' please add := after variable, also please ckeck if you are on declaration block and missing declare') 
               else:
                    raise SyntaxError('invalid Syntax at line : '+str(par)+' no exists') 
          
     


        
        def if_stat():
               global e_type
               global lex
               global line 
               global par
               if(lex=="if"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    if(lex=='('):
                         lex,line,e_type=lex_analyzer()
                         par=line
                         cond=condition()
                         backpatch(cond[0],nextquad())
                         if(lex==')'):
                              lex,line,e_type=lex_analyzer()
                              par=line
                              statements()
                              lst=makelist(nextquad())
                              genquad("jump","_","_","_")
                              backpatch(cond[1],nextquad())
                              else_part()
                              backpatch(lst,nextquad())
                              return
                         else:
                              raise SyntaxError('invalid Syntax at line : '+str(par)+' please close ) in if condition ')        
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' please open ( in if condition') 
               else:
                    raise SyntaxError('invalid Syntax at line : '+str(par)+' please check start in if condition') 

        def else_part():
               global e_type
               global lex
               global line 
               global par
               if(lex=="else"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    statements()
               return
         
        def while_stat():
               global e_type
               global lex
               global line 
               global par
               if(lex=="while"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    if(lex=='('):
                         lex,line,e_type=lex_analyzer()
                         par=line
                         cond_4=nextquad()
                         cond=condition()
                         backpatch(cond[0],nextquad())
                         if(lex==')'):
                              lex,line,e_type=lex_analyzer()
                              par=line
                              statements()
                              genquad("jump","_","_",cond_4)
                              backpatch(cond[1],nextquad())
                              return
                         else:
                              raise SyntaxError('invalid Syntax at line : '+str(par)+'please close while condition') 
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(par)+'please please open ( in while condition') 
               else:
                    raise SyntaxError('invalid Syntax at line : '+str(par)+'please check while condition') 



        def switchcase_stat():
               global e_type
               global lex
               global line 
               global par
               if(lex=="switchcase"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    outl=emptylist()
                    while(lex=="case"):
                         lex,line,e_type=lex_analyzer()
                         par=line
                         if(lex=='('):
                              lex,line,e_type=lex_analyzer()
                              par=line
                              cond=condition()
                              backpatch(cond[0],nextquad())
                              if(lex==')'):
                                   lex,line,e_type=lex_analyzer()
                                   par=line
                                   statements()
                                   outJ=makelist(nextquad())
                                   genquad('jump','_','_','_')
                                   outl=merge(outl,outJ)
                                   backpatch(cond[1],nextquad())
                              else:
                                   raise SyntaxError('invalid Syntax at line : '+str(par)+' please add ) at switchcase condition') 
                         else:
                              raise SyntaxError('invalid Syntax at line : '+str(par)+' please add ( at switchcase condition') 
                    if(lex=="default"):
                         lex,line,e_type=lex_analyzer()
                         par=line
                         statements()
                         backpatch(outl,nextquad())
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' please check if missing default or case at switchcase condition') 
               else:
                    raise SyntaxError('invalid Syntax at line : '+str(par)+' check switchcase condition') 
          
        def forcase_stat():
               global e_type
               global lex
               global line 
               global par
               if(lex=="forcase"):
                    lex,line,e_type=lex_analyzer()
                    par=line   
                    q=nextquad() 
                    while(lex=="case"):
                         lex,line,e_type=lex_analyzer()
                         par=line        
                         if(lex=='('):
                              lex,line,e_type=lex_analyzer()
                              par=line  
                              cond=condition()
                              backpatch(cond[0],nextquad())
                              if(lex==')'):
                                   lex,line,e_type=lex_analyzer()
                                   par=line 
                                   statements()
                                   genquad('jump','_','_',q)
                                   backpatch(cond[1],nextquad())
                              else:
                                   raise SyntaxError('invalid Syntax at line : '+str(par)+' please add ) at forcase condition') 
                         else:
                              raise SyntaxError('invalid Syntax at line : '+str(par)+' please add ( at forcase condition') 
                    if(lex=="default"):
                         lex,line,e_type=lex_analyzer()
                         par=line 
                         statements()
                    else:
                          raise SyntaxError('invalid Syntax at line : '+str(par)+' please check if missing default or case at forcase condition') 
               else:
                    raise SyntaxError('invalid Syntax at line : '+str(par)+' check forcase condition') 
                


        def incase_stat():
               global e_type
               global lex
               global line 
               global par
               if(lex=="incase"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    q=nextquad()
                    nt=newtemp()
                    genquad(':=','0','_',nt)
                    while(lex=="case"):
                         lex,line,e_type=lex_analyzer()
                         par=line
                         if(lex=='('):
                              lex,line,e_type=lex_analyzer()
                              par=line
                              cond=condition()
                              backpatch(cond[0],nextquad())
                              if(lex==')'):
                                   lex,line,e_type=lex_analyzer()
                                   par=line
                                   statements()
                                   genquad(':=','1','_',nt)
                                   backpatch(cond[1],nextquad())
                              else:
                                   raise SyntaxError('invalid Syntax at line : '+str(par)+' please add ) at incase condition') 
                         else:
                              raise SyntaxError('invalid Syntax at line : '+str(par)+' please add ( at incase condition') 
                    genquad('=',nt,'1',q)
               else:
                    raise SyntaxError('invalid Syntax at line : '+str(par)+' check incase condition') 

        def return_stat():
               global e_type
               global lex
               global line 
               global par
               if(lex=="return"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    if(lex=='('):
                         lex,line,e_type=lex_analyzer()
                         par=line
                         E_place=expression()
                         if(lex==')'):
                              lex,line,e_type=lex_analyzer()
                              par=line
                              genquad('retv',E_place, '_', '_')
                              return
                         else:
                              raise SyntaxError('invalid Syntax at line : '+str(par)+' please close-add ) at return') 
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' please add ( at return') 

        def call_stat():
               global e_type
               global lex
               global line 
               global par
               if(lex=="call"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    if(e_type=="keyword"):
                         word=lex
                         lex,line,e_type=lex_analyzer()
                         par=line
                         if(lex=='('):
                              lex,line,e_type=lex_analyzer()
                              par=line
                              actual_par_list()
                              genquad("call",word,"_","_")
                              if(lex==')'):
                                   lex,line,e_type=lex_analyzer()
                                   par=line
                                   return
                              else:
                                   raise SyntaxError('invalid Syntax at line : '+str(par)+' please close-add ) at call') 
                         else:
                              raise SyntaxError('invalid Syntax at line : '+str(par)+' please open-add ( at call') 
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' please add keyword type at call') 
               else:
                    raise SyntaxError('invalid Syntax at line : '+str(par)+' please check call') 
               return
      
        def print_stat():
               global e_type
               global lex
               global line 
               global par
              
               if(lex=="print"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    kap=line
                    if(lex=='('):
                         lex,line,e_type=lex_analyzer()
                         par=line
                         E_place=expression()
                         if(lex==')'):
                              lex,line,e_type=lex_analyzer()
                              par=line   
                              genquad('out',E_place,'_','_')                      
                         else:
                              raise SyntaxError('invalid Syntax at line : '+str(kap)+' please close print expression missing ); at the end') 
                    else:
                          raise SyntaxError('invalid Syntax at line : '+str(par)+' please open-add ( at print expression') 
               else:
                    raise SyntaxError('invalid Syntax at line : '+str(par)+' please check print expression') 
               return

        def input_stat():
               global e_type
               global lex
               global line 
               global par
               if(lex=="input"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    if(lex=='('):
                         lex,line,e_type=lex_analyzer()
                         par=line
                         if(e_type=="keyword"):
                              wi=lex
                              genquad('inp',wi,'_','_')
                              lex,line,e_type=lex_analyzer()
                              par=line
                              if(lex==')'):
                                   lex,line,e_type=lex_analyzer()
                                   par=line
                                   return                          
                              else:
                                   raise SyntaxError('invalid Syntax at line : '+str(par)+' please close-add ) at input') 
                         else:
                              raise SyntaxError('invalid Syntax at line : '+str(par)+' please add keyword type at input') 
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' please open-add ( at input') 
               else:
                    raise SyntaxError('invalid Syntax at line : '+str(par)+' please check input') 
        

        def actual_par_list():
               global e_type
               global lex
               global line 
               global par
               actual_par_item()
               while(lex==','):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    actual_par_item()
               return

        def actual_par_item():
               global e_type
               global lex
               global line 
               global par
               if(lex=="in"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    expr=expression()
                    genquad("par",expr,"CV","_")
               elif(lex=="inout"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    wrn=lex
                    if(e_type=="keyword"):
                         lex,line,e_type=lex_analyzer()
                         par=line
                         genquad("par",wrn,"REF","_")
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' please add name of variable after inout') 
               else:
                    pass
                    

               return
         
        def condition():
               False_cond=[]
               True_cond=[]
               global e_type
               global lex
               global line 
               global par
               bool_T1=bool_term()
               False_cond=bool_T1[1]
               True_cond=bool_T1[0]
               while(lex=="or"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    backpatch(False_cond,nextquad())
                    bool_T2=bool_term()
                    True_cond=merge(True_cond,bool_T2[0])
                    False_cond=bool_T2[1]                    
               return True_cond,False_cond

        def bool_term():
               True_booltr=[]
               False_booltr=[]
               global e_type
               global lex
               global line 
               global par
               bool_F1=bool_factor()
               True_booltr=bool_F1[0]
               False_booltr=bool_F1[1]
               while(lex=="and"):
                    lex,line,e_type=lex_analyzer()
                    par=line 
                    backpatch(True_booltr,nextquad())
                    bool_F2=bool_factor()
                    False_booltr=merge(False_booltr,bool_F2[1])
                    True_booltr=bool_F2[0]
               return True_booltr,False_booltr

        def bool_factor():
               boolf_true=[]
               boolf_false=[]
               global e_type
               global lex
               global line 
               global par
               if(lex=="not"):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    if(lex=="["):
                         lex,line,e_type=lex_analyzer()
                         par=line
                         cond=condition()
                         if(lex==']'):
                              lex,line,e_type=lex_analyzer()
                              par=line
                              boolf_false=cond[0]
                              boolf_true=cond[1]
                         else:
                              raise SyntaxError('invalid Syntax at line : '+str(par)+' please close-add ] after boolfactor ') 
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' please open-add [ after not at boolfactor') 
               elif(lex=='['):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    cond=condition()
                    if(lex==']'):
                         lex,line,e_type=lex_analyzer()
                         par=line
                         boolf_false=cond[1]
                         boolf_true=cond[0]
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' please close-add ] after boolfactor ') 
               else: 
                    E_place_1=expression()
                    rlp=rel_op()
                    E_place_2=expression()
                    boolf_true=makelist(nextquad())
                    genquad(rlp,E_place_1,E_place_2,"_")
                    boolf_false=makelist(nextquad())
                    genquad("jump","_","_","_")
               return boolf_true,boolf_false



        def expression():
               global e_type
               global lex
               global line 
               global par
               optional_sign()
               T1_place=term()
               while(lex=='-' or lex=='+'):
                    add_sub=add_oper()
                    T2_place=term()
                    w=newtemp()
                    genquad(add_sub,T1_place,T2_place,w)     
                    T1_place=w
               E_place=T1_place
               return E_place
     
        
        def term():
               global e_type
               global lex
               global line 
               global par
               F1_place=factor()
               while(lex=='*' or lex=='/'):
                    mul_div=mul_oper()
                    F2_place=factor()
                    w=newtemp()
                    genquad(mul_div,F1_place,F2_place,w)
                    F1_place=w 
               T_place=F1_place
               return T_place

        def factor():
               global e_type
               global lex
               global line 
               global par
               if(e_type=="digit"):
                    fact=lex
                    lex,line,e_type=lex_analyzer()
                    par=line
               elif(lex=='('):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    E_place=expression()
                    fact=E_place
                    if(lex==')'):
                         lex,line,e_type=lex_analyzer()
                         par=line
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(par)+' please close-add ) after expression at factor') 
               elif(e_type=="keyword"):
                    fact1=lex
                    lex,line,e_type=lex_analyzer()
                    par=line
                    fact=idtail(fact1)
               else:
                     raise SyntaxError('invalid Syntax at line : '+str(par)+' please add expression or constant or variable at factor ') 
               return fact

        def idtail(wrd):
               global e_type
               global lex
               global line 
               global par
               if(lex=='('):
                    lex,line,e_type=lex_analyzer()
                    par=line
                    actual_par_list()
                    Nt=newtemp()
                    genquad("par",Nt,"RET","_")
                    genquad("call",wrd,"_","_")
                    ln=par
                    if(lex==')'):
                         lex,line,e_type=lex_analyzer()
                         par=line
                         return Nt
                    else:
                         raise SyntaxError('invalid Syntax at line : '+str(ln)+' please add ) to close actual_par_list ') 
               else:
                   return wrd
     
        def optional_sign():
               global e_type
               global lex
               global line 
               global par
               if(lex=='-' or lex=='+'):
                    add_oper()
               return



        def rel_op():
               global e_type
               global lex
               global line 
               global par
                
               if(lex=='='):
                    rel_token="="
                    lex,line,e_type=lex_analyzer()
                    par=line
               elif(lex=='<='):
                    rel_token="<="
                    lex,line,e_type=lex_analyzer()
                    par=line   
               elif(lex=='>='):
                    rel_token=">="
                    lex,line,e_type=lex_analyzer()
                    par=line     
               elif(lex=='>'):
                    rel_token=">"
                    lex,line,e_type=lex_analyzer()
                    par=line        
               elif(lex=='<'):
                    rel_token="<"
                    lex,line,e_type=lex_analyzer()
                    par=line   
               elif(lex=='<>'):
                    rel_token="<>"
                    lex,line,e_type=lex_analyzer()
                    par=line   
               else:
                     raise SyntaxError('invalid Syntax at line : '+str(par)+' no = or <= or >= or > or < or <>') 
               return rel_token
        
        def add_oper():
               global e_type
               global lex
               global line 
               global par
               if(lex=='-'):
                    add_op=lex
                    lex,line,e_type=lex_analyzer()
                    par=line   
               elif (lex=='+'):
                    add_op=lex
                    lex,line,e_type=lex_analyzer()
                    par=line   
               return add_op
        
        def mul_oper():
               global e_type
               global lex
               global line 
               global par
               if (lex=='*'):
                    mul_op=lex
                    lex,line,e_type=lex_analyzer()
                    par=line   
               elif (lex=='/'):
                    mul_op=lex
                    lex,line,e_type=lex_analyzer()
                    par=line   
               return mul_op
       
      

        program()
        return

        

def write_intc(F):
     for i in range(len(quads_list)):
          F.write(str(quads_list[i][0])+': '+str(quads_list[i][1])+' '+str(quads_list[i][2])+' '+str(quads_list[i][3])+' '+str(quads_list[i][4])+'\n')

def conv_c():
     global temp_vars
     if(len(temp_vars)!=0):
          cF.write('int ')
     for i in range(len(temp_vars)):
          cF.write(temp_vars[i])
          l=i+1
          if(len(temp_vars)!=l):
               cF.write(",")
          else:
               cF.write(";\n\n\t")

     for j in range(len(quads_list)):
          if quads_list[j][1]=='begin_block':
               cF.write('L'+str(j+1)+':\n\t')
          elif quads_list[j][1]=='halt':
               cF.write('L'+str(j+1)+': {}\n\t')
          elif quads_list[j][1]=='out' :
               cF.write('L'+str(j+1)+': '+'printf(\"'+quads_list[j][2]+'= %d\", '+quads_list[j][2]+');\n\t')
          elif quads_list[j][1]=='jump':
               cF.write('L_'+str(j+1)+': '+'goto L_'+str(quads_list[j][4])+';\n\t')
          elif quads_list[j][1]==':=':
               cF.write('L_'+str(j+1)+': '+quads_list[j][4]+'='+quads_list[j][2]+';\n\t')
          elif quads_list[j][1]=='=':
               cF.write('L_'+str(j+1)+': '+'if ('+quads_list[j][2]+'=='+quads_list[j][3]+') goto L_'+str(quads_list[j][4])+';\n\t')
          elif quads_list[j][1]=='<>':
               cF.write('L_'+str(j+1)+': '+'if ('+quads_list[j][2]+'!='+quads_list[j][3]+') goto L_'+str(quads_list[j][4])+';\n\t')
          elif quads_list[j][1]=='>':
               cF.write('L_'+str(j+1)+': '+'if ('+quads_list[j][2]+'>'+quads_list[j][3]+') goto L_'+str(quads_list[j][4])+';\n\t')
          elif quads_list[j][1]=='<':
               cF.write('L_'+str(j+1)+': '+'if ('+quads_list[j][2]+'<'+quads_list[j][3]+') goto L_'+str(quads_list[j][4])+';\n\t')
          elif quads_list[j][1]=='>=':
               cF.write('L_'+str(j+1)+': '+'if ('+quads_list[j][2]+'>='+quads_list[j][3]+') goto L_'+str(quads_list[j][4])+';\n\t')
          elif quads_list[j][1]=='<=':
               cF.write('L_'+str(j+1)+': '+'if ('+quads_list[j][2]+'<='+quads_list[j][3]+') goto L_'+str(quads_list[j][4])+';\n\t')
          elif quads_list[j][1]=='+':
               cF.write('L_'+str(j+1)+': '+quads_list[j][4]+'='+quads_list[j][2]+'+'+quads_list[j][3]+';\n\t')
          elif quads_list[j][1]=='-':
               cF.write('L_'+str(j+1)+': '+quads_list[j][4]+'='+quads_list[j][2]+'-'+quads_list[j][3]+';\n\t')
          elif quads_list[j][1]=='/':
               cF.write('L_'+str(j+1)+': '+quads_list[j][4]+'='+quads_list[j][2]+'/'+quads_list[j][3]+';\n\t')
          elif quads_list[j][1]=='*':
               cF.write('L_'+str(j+1)+': '+quads_list[j][4]+'='+quads_list[j][2]+'*'+quads_list[j][3]+';\n\t')
          
def create_c():
     global cF
     int_F=open('int_F.int','w')
     cF=open('c_F.c','w')
     cF.write('#include<stdio.h>\n#include<stdlib.h>\n\n')
     cF.write('int main(){\n\t')
     syntax_analyzer()
     write_intc(int_F)
     conv_c()
     cF.write('\n}')
     cF.close()
     int_F.close()

def check_create():
     global flag
     if flag==0:
        create_c()



def printlist():
     for i in range(len(quads_list)):
          print(" {:1s}) {:5s} {:5s} {:5s} {}\n".format(str(quads_list[i][0]),str(quads_list[i][1]),str(quads_list[i][2]),str(quads_list[i][3]),str(quads_list[i][4])))
       

def create_symbol_table():
     global cFs
     cFs=open('symbols.txt','w')
     check_create()
     cFs.close()


create_symbol_table()


                
                
                   

                    
     
                         

                         














  

   
 

    
