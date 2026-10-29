#include <cctype>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>
#include <algorithm>

namespace fy {

struct Value;
using List = std::vector<Value>;
using Dict = std::map<std::string, Value>;

struct NoneType {};
struct SourceLine { int indent=0, number=0; std::string text; };
struct Function {
    std::vector<std::string> params;
    std::vector<SourceLine> code;
    std::string name;
    bool builtin=false;
};
struct Value {
    using Data = std::variant<NoneType, bool, double, std::string,
                              std::shared_ptr<List>, std::shared_ptr<Dict>,
                              std::shared_ptr<Function>>;
    Data data;
    Value() : data(NoneType{}) {}
    Value(std::nullptr_t) : data(NoneType{}) {}
    Value(bool x) : data(x) {}
    Value(double x) : data(x) {}
    Value(int x) : data(double(x)) {}
    Value(const char* x) : data(std::string(x)) {}
    Value(std::string x) : data(std::move(x)) {}
    Value(List x) : data(std::make_shared<List>(std::move(x))) {}
    Value(Dict x) : data(std::make_shared<Dict>(std::move(x))) {}
    Value(std::shared_ptr<Function> x) : data(std::move(x)) {}
};

static std::string trim(const std::string& s) {
    size_t a = s.find_first_not_of(" \t\r\n");
    if (a == std::string::npos) return "";
    size_t b = s.find_last_not_of(" \t\r\n");
    return s.substr(a, b-a+1);
}
static std::string reprNumber(double d) {
    if(std::isnan(d))return "nan";
    if(std::isinf(d))return d<0?"-inf":"inf";
    if(d==0)return std::signbit(d)?"-0":"0";
    if(d==std::floor(d)&&std::fabs(d)<9e18)return std::to_string((long long)d);
    char buf[64];int p=1;
    for(;p<=17;++p){std::snprintf(buf,sizeof buf,"%.*e",p-1,d);if(std::strtod(buf,nullptr)==d)break;}
    std::string s=buf;bool negative=s[0]=='-';if(negative)s=s.substr(1);
    size_t e=s.find('e');int exponent=std::atoi(s.c_str()+e+1);std::string digits,result;
    for(size_t k=0;k<e;++k)if(std::isdigit((unsigned char)s[k]))digits+=s[k];
    if(exponent<-4||exponent>=16){
        result=digits.substr(0,1);if(digits.size()>1)result+="."+digits.substr(1);
        char eb[16];std::snprintf(eb,sizeof eb,"e%c%02d",exponent<0?'-':'+',std::abs(exponent));result+=eb;
    } else if(exponent<0)result="0."+std::string(-exponent-1,'0')+digits;
    else if((int)digits.size()<=exponent+1)result=digits+std::string(exponent+1-digits.size(),'0');
    else result=digits.substr(0,exponent+1)+"."+digits.substr(exponent+1);
    return negative?"-"+result:result;
}
static bool truthy(const Value& v) {
    if (std::holds_alternative<NoneType>(v.data)) return false;
    if (auto p=std::get_if<bool>(&v.data)) return *p;
    if (auto p=std::get_if<double>(&v.data)) return *p != 0.0;
    if (auto p=std::get_if<std::string>(&v.data)) return !p->empty();
    if (auto p=std::get_if<std::shared_ptr<List>>(&v.data)) return !(*p)->empty();
    if (auto p=std::get_if<std::shared_ptr<Dict>>(&v.data)) return !(*p)->empty();
    return true;
}
static std::string repr(const Value& v) {
    if (std::holds_alternative<NoneType>(v.data)) return "None";
    if (auto p=std::get_if<bool>(&v.data)) return *p ? "True" : "False";
    if (auto p=std::get_if<double>(&v.data)) return reprNumber(*p);
    if (auto p=std::get_if<std::string>(&v.data)) return *p;
    if (auto p=std::get_if<std::shared_ptr<List>>(&v.data)) {
        std::string s="[";
        for (size_t i=0;i<(*p)->size();++i) { if(i) s += ", "; s += repr((**p)[i]); }
        return s+"]";
    }
    if (auto p=std::get_if<std::shared_ptr<Dict>>(&v.data)) {
        std::string s="{";
        bool first=true;
        for (const auto& kv:**p) { if(!first) s += ", "; first=false; s += "'" + kv.first + "': " + repr(kv.second); }
        return s+"}";
    }
    return "<function>";
}

enum class Tok { End, Number, String, Ident, LParen, RParen, LBracket, RBracket,
                 LBrace, RBrace, Comma, Colon, Dot, Plus, Minus, Star, Slash,
                 Pow, FloorSlash, Percent, Eq, EqEq, NotEq, Lt, Lte, Gt, Gte,
                 And, Or, Not, In, NotIn };
struct Token { Tok type; std::string text; double number=0; };

class Lexer {
    std::string s; size_t i=0;
public:
    explicit Lexer(std::string x):s(std::move(x)){}
    std::vector<Token> scan() {
        std::vector<Token> out;
        out.reserve(s.size()/2+1);
        while(i<s.size()) {
            char c=s[i];
            if(std::isspace((unsigned char)c)){++i;continue;}
            if(c=='#') break;
            if(std::isdigit((unsigned char)c) || (c=='.' && i+1<s.size() && std::isdigit((unsigned char)s[i+1]))) {
                size_t st=i++;
                while(i<s.size() && (std::isdigit((unsigned char)s[i]) || s[i]=='.')) ++i;
                std::string n=s.substr(st,i-st); out.push_back({Tok::Number,n,std::stod(n)}); continue;
            }
            if(c=='\'' || c=='"') {
                char q=c; ++i; std::string val;
                while(i<s.size() && s[i]!=q) {
                    if(s[i]=='\\' && i+1<s.size()) {
                        ++i; char e=s[i++];
                        if(e=='n') val+='\n'; else if(e=='t') val+='\t'; else val+=e;
                    } else val+=s[i++];
                }
                if(i>=s.size()) throw std::runtime_error("SyntaxError: unterminated string");
                ++i; out.push_back({Tok::String,val}); continue;
            }
            if(std::isalpha((unsigned char)c)||c=='_') {
                size_t st=i++;
                while(i<s.size() && (std::isalnum((unsigned char)s[i])||s[i]=='_')) ++i;
                std::string id=s.substr(st,i-st);
                Tok t=Tok::Ident;
                if(id=="and")t=Tok::And; else if(id=="or")t=Tok::Or;
                else if(id=="not")t=Tok::Not; else if(id=="in")t=Tok::In;
                out.push_back({t,id}); continue;
            }
            auto two = [&](char a,char b,Tok yes,Tok no) {
                if(i+1<s.size() && s[i]==a && s[i+1]==b){out.push_back({yes,s.substr(i,2)});i+=2;}
                else {out.push_back({no,s.substr(i,1)});++i;}
            };
            switch(c) {
                case '(':out.push_back({Tok::LParen,"("});++i;break;
                case ')':out.push_back({Tok::RParen,")"});++i;break;
                case '[':out.push_back({Tok::LBracket,"["});++i;break;
                case ']':out.push_back({Tok::RBracket,"]"});++i;break;
                case '{':out.push_back({Tok::LBrace,"{"});++i;break;
                case '}':out.push_back({Tok::RBrace,"}"});++i;break;
                case ',':out.push_back({Tok::Comma,","});++i;break;
                case ':':out.push_back({Tok::Colon,":"});++i;break;
                case '.':out.push_back({Tok::Dot,"."});++i;break;
                case '+':out.push_back({Tok::Plus,"+"});++i;break;
                case '-':out.push_back({Tok::Minus,"-"});++i;break;
                case '*':if(i+1<s.size()&&s[i+1]=='*'){out.push_back({Tok::Pow,"**"});i+=2;}else{out.push_back({Tok::Star,"*"});++i;}break;
                case '/': if(i+1<s.size()&&s[i+1]=='/') {out.push_back({Tok::FloorSlash,"//"});i+=2;} else {out.push_back({Tok::Slash,"/"});++i;} break;
                case '%':out.push_back({Tok::Percent,"%"});++i;break;
                case '=':two('=','=',Tok::EqEq,Tok::Eq);break;
                case '!': if(i+1<s.size()&&s[i+1]=='='){out.push_back({Tok::NotEq,"!="});i+=2;}else throw std::runtime_error("SyntaxError: unexpected !");
                    break;
                case '<':two('<','=',Tok::Lte,Tok::Lt);break;
                case '>':two('>','=',Tok::Gte,Tok::Gt);break;
                default: throw std::runtime_error(std::string("SyntaxError: unexpected character '")+c+"'");
            }
        }
        out.push_back({Tok::End,""}); return out;
    }
};

enum class Op { Push, Load, Store, Pop, Add, Sub, Mul, Div, FloorDiv, Mod, Neg,
                Pow, Eq, Ne, Lt, Le, Gt, Ge, In, NotIn, Not, And, Or, MakeList, Index, Call,
                Jump, JumpFalse, JumpFalseKeep, JumpTrueKeep, Return };
struct Ins { Op op; Value value; std::string name; int arg=0; };
struct Code { std::vector<Ins> ins; };

class Compiler {
    std::vector<Token> t; size_t i=0; Code code;
    Token cur(){return t[i];}
    bool match(Tok x){if(cur().type==x){++i;return true;}return false;}
    Token take(Tok x,const std::string& msg){if(cur().type!=x)throw std::runtime_error("SyntaxError: "+msg);return t[i++];}
    void emit(Op op, Value v={}, std::string name="", int arg=0){code.ins.push_back({op,std::move(v),std::move(name),arg});}
    int precedence(Tok x) {
        switch(x){case Tok::Or:return 1;case Tok::And:return 2;
        case Tok::EqEq:case Tok::NotEq:case Tok::Lt:case Tok::Lte:case Tok::Gt:case Tok::Gte:case Tok::In:case Tok::NotIn:return 3;
        case Tok::Plus:case Tok::Minus:return 4;case Tok::Star:case Tok::Slash:case Tok::FloorSlash:case Tok::Percent:return 5;
        case Tok::Pow:return 6;default:return 0;}
    }
    void expression(int minp=1) {
        if(match(Tok::Minus)){expression(6);emit(Op::Neg);}
        else if(match(Tok::Not)){expression(6);emit(Op::Not);}
        else if(match(Tok::Number)){emit(Op::Push,Value(t[i-1].number));}
        else if(match(Tok::String)){emit(Op::Push,Value(t[i-1].text));}
        else if(match(Tok::LParen)){expression();take(Tok::RParen,"expected ')'");}
        else if(match(Tok::LBracket)){
            int n=0;
            if(cur().type!=Tok::RBracket){do{expression();++n;}while(match(Tok::Comma)&&cur().type!=Tok::RBracket);}
            take(Tok::RBracket,"expected ']'");
            emit(Op::MakeList,{}, "",n);
        }
        else if(match(Tok::Ident)){
            std::string id=t[i-1].text;
            if(id=="True")emit(Op::Push,Value(true));
            else if(id=="False")emit(Op::Push,Value(false));
            else if(id=="None")emit(Op::Push,Value());
            else emit(Op::Load,{},id);
        } else throw std::runtime_error("SyntaxError: expected expression");
        while(true) {
            if(cur().type==Tok::LParen) {
                ++i; int n=0;
                if(cur().type!=Tok::RParen){do{expression();++n;}while(match(Tok::Comma)&&cur().type!=Tok::RParen);}
                take(Tok::RParen,"expected ')'");
                emit(Op::Call,{}, "",n); continue;
            }
            if(cur().type==Tok::LBracket) {
                ++i; expression(); take(Tok::RBracket,"expected ']'");
                emit(Op::Index); continue;
            }
            bool notIn=cur().type==Tok::Not&&i+1<t.size()&&t[i+1].type==Tok::In;
            int p=notIn?3:precedence(cur().type); if(p<minp || p==0) break;
            Tok op;
            if(notIn){i+=2;op=Tok::NotIn;}else op=t[i++].type;
            if(op==Tok::And||op==Tok::Or){
                int jump=(int)code.ins.size();
                emit(op==Tok::And?Op::JumpFalseKeep:Op::JumpTrueKeep);
                expression(p+1);
                code.ins[jump].arg=(int)code.ins.size();
                continue;
            }
            expression(op==Tok::Pow?p:p+1);
            switch(op){
                case Tok::Plus:emit(Op::Add);break;case Tok::Minus:emit(Op::Sub);break;
                case Tok::Star:emit(Op::Mul);break;case Tok::Slash:emit(Op::Div);break;
                case Tok::FloorSlash:emit(Op::FloorDiv);break;case Tok::Percent:emit(Op::Mod);break;case Tok::Pow:emit(Op::Pow);break;
                case Tok::EqEq:emit(Op::Eq);break;case Tok::NotEq:emit(Op::Ne);break;
                case Tok::Lt:emit(Op::Lt);break;case Tok::Lte:emit(Op::Le);break;
                case Tok::Gt:emit(Op::Gt);break;case Tok::Gte:emit(Op::Ge);break;
                case Tok::In:emit(Op::In);break;case Tok::NotIn:emit(Op::NotIn);break;
                default:break;
            }
        }
    }
public:
    explicit Compiler(std::string line):t(Lexer(std::move(line)).scan()) { code.ins.reserve(t.size()); }
    Code compileValue() {
        expression();
        if(cur().type!=Tok::End) throw std::runtime_error("SyntaxError: unexpected token '"+cur().text+"'");
        return code;
    }
    Code compile() {
        if(cur().type==Tok::Ident && cur().text=="print") {
            ++i; emit(Op::Load,{},"__print"); take(Tok::LParen,"expected '(' after print"); int n=0;
            if(cur().type!=Tok::RParen){do{expression();++n;}while(match(Tok::Comma)&&cur().type!=Tok::RParen);}
            take(Tok::RParen,"expected ')' after print"); emit(Op::Call,{},"",-n);
        } else if(cur().type==Tok::Ident && i+1<t.size() && t[i+1].type==Tok::Eq) {
            std::string name=t[i++].text; ++i; expression(); emit(Op::Store,{},name);
        } else if(cur().type==Tok::Ident && cur().text=="pass") {
            ++i;
        } else if(cur().type==Tok::Ident && cur().text=="return") {
            ++i; if(cur().type!=Tok::End) expression(); else emit(Op::Push,Value());
            emit(Op::Return);
        } else {
            expression(); emit(Op::Pop);
        }
        if(cur().type!=Tok::End) throw std::runtime_error("SyntaxError: unexpected token '"+cur().text+"'");
        return code;
    }
};

class VM {
    std::unordered_map<std::string,Value> globals;
    std::vector<std::unordered_map<std::string,Value>> scopes;
    std::vector<Value> stack;
    std::unordered_map<std::string,std::shared_ptr<Code>> codeCache;
    enum class Flow { Normal, Break, Continue, Return };
    struct FlowResult { Flow flow=Flow::Normal; Value value; };
    static double num(const Value& v) {
        if(auto p=std::get_if<double>(&v.data))return *p;
        if(auto p=std::get_if<bool>(&v.data))return *p?1:0;
        throw std::runtime_error("TypeError: expected a number");
    }
    Value pop(){if(stack.empty())throw std::runtime_error("RuntimeError: stack underflow");Value v=std::move(stack.back());stack.pop_back();return v;}
    Value get(const std::string& name) {
        for(auto it=scopes.rbegin();it!=scopes.rend();++it){auto local=it->find(name);if(local!=it->end())return local->second;}
        auto it=globals.find(name); if(it==globals.end())throw std::runtime_error("NameError: name '"+name+"' is not defined");return it->second;
    }
    void store(const std::string& name,Value value) {
        if(scopes.empty())globals[name]=std::move(value);
        else scopes.back()[name]=std::move(value);
    }
    std::shared_ptr<Code> compiled(const std::string& source,bool value) {
        std::string key(1,value?'\1':'\2');key+=source;
        auto it=codeCache.find(key);
        if(it!=codeCache.end())return it->second;
        if(codeCache.size()>=4096)codeCache.clear();
        std::shared_ptr<Code> code=std::make_shared<Code>(value?Compiler(source).compileValue():Compiler(source).compile());
        codeCache.emplace(std::move(key),code);
        return code;
    }
    Value evaluate(const std::string& expression);
    Value callUser(const std::shared_ptr<Function>& fn,const std::vector<Value>& args);
    FlowResult runBlock(const std::vector<SourceLine>& lines,size_t& pos,int indent);
    static size_t blockEnd(const std::vector<SourceLine>& lines,size_t start,int parentIndent);
    static std::string blockValue(const std::string& text,const std::string& keyword);
    static bool equal(const Value&a,const Value&b) {
        if(a.data.index()!=b.data.index()) return false;
        if(auto p=std::get_if<double>(&a.data))return *p==std::get<double>(b.data);
        if(auto p=std::get_if<bool>(&a.data))return *p==std::get<bool>(b.data);
        if(auto p=std::get_if<std::string>(&a.data))return *p==std::get<std::string>(b.data);
        return repr(a)==repr(b);
    }
    static bool contains(const Value& container,const Value& item) {
        if(auto p=std::get_if<std::string>(&container.data)){
            auto q=std::get_if<std::string>(&item.data);
            if(!q)throw std::runtime_error("TypeError: 'in <string>' requires string as left operand");
            return p->find(*q)!=std::string::npos;
        }
        if(auto p=std::get_if<std::shared_ptr<List>>(&container.data)){
            for(const auto& value:**p)if(equal(value,item))return true;
            return false;
        }
        throw std::runtime_error("TypeError: argument of type is not iterable");
    }
public:
    VM() {
        globals.reserve(16);
        stack.reserve(256);
        codeCache.reserve(256);
        auto builtin=[](const std::string& name){return Value(std::make_shared<Function>(Function{{},{},name,true}));};
        globals["__print"]=builtin("print");globals["print"]=globals["__print"];
        globals["True"]=Value(true);globals["False"]=Value(false);globals["None"]=Value();
        globals["len"]=builtin("len");
        globals["range"]=builtin("range");
        globals["str"]=builtin("str");
        globals["int"]=builtin("int");
        globals["float"]=builtin("float");
        globals["type"]=builtin("type");
    }
    void execute(const Code& c) {
        size_t ip=0;
        while(ip<c.ins.size()) {
            const Ins& x=c.ins[ip++];
            switch(x.op) {
                case Op::Push:stack.push_back(x.value);break;
                case Op::Load:stack.push_back(get(x.name));break;
                case Op::Store:store(x.name,pop());break;
                case Op::Pop:pop();break;
                case Op::Neg:{auto a=pop();stack.emplace_back(-num(a));break;}
                case Op::Not:{auto a=pop();stack.emplace_back(!truthy(a));break;}
                case Op::Add:{auto b=pop(),a=pop();
                    if(auto p=std::get_if<std::string>(&a.data)){if(auto q=std::get_if<std::string>(&b.data))stack.emplace_back(*p+*q);else throw std::runtime_error("TypeError: can only concatenate str to str");}
                    else stack.emplace_back(num(a)+num(b));
                    break;}
                case Op::Sub:{auto b=pop(),a=pop();stack.emplace_back(num(a)-num(b));break;}
                case Op::Mul:{auto b=pop(),a=pop();stack.emplace_back(num(a)*num(b));break;}
                case Op::Div:{auto b=pop(),a=pop();if(num(b)==0)throw std::runtime_error("ZeroDivisionError: division by zero");stack.emplace_back(num(a)/num(b));break;}
                case Op::FloorDiv:{auto b=pop(),a=pop();if(num(b)==0)throw std::runtime_error("ZeroDivisionError: integer division or modulo by zero");stack.emplace_back(std::floor(num(a)/num(b)));break;}
                case Op::Mod:{auto b=pop(),a=pop();if(num(b)==0)throw std::runtime_error("ZeroDivisionError: integer division or modulo by zero");stack.emplace_back(std::fmod(num(a),num(b)));break;}
                case Op::Pow:{auto b=pop(),a=pop();if(num(a)==0&&num(b)<0)throw std::runtime_error("ZeroDivisionError: 0.0 cannot be raised to a negative power");stack.emplace_back(std::pow(num(a),num(b)));break;}
                case Op::Eq:case Op::Ne:{auto b=pop(),a=pop();bool v=equal(a,b);stack.emplace_back(x.op==Op::Eq?v:!v);break;}
                case Op::In:case Op::NotIn:{auto b=pop(),a=pop();bool v=contains(b,a);stack.emplace_back(x.op==Op::In?v:!v);break;}
                case Op::Lt:case Op::Le:case Op::Gt:case Op::Ge:{
                    auto b=pop(),a=pop(); bool v=false;
                    if(auto p=std::get_if<std::string>(&a.data)){auto q=std::get_if<std::string>(&b.data);if(!q)throw std::runtime_error("TypeError: comparison type mismatch");
                        if(x.op==Op::Lt)v=*p<*q;else if(x.op==Op::Le)v=*p<=*q;else if(x.op==Op::Gt)v=*p>*q;else v=*p>=*q;
                    } else {double aa=num(a),bb=num(b);if(x.op==Op::Lt)v=aa<bb;else if(x.op==Op::Le)v=aa<=bb;else if(x.op==Op::Gt)v=aa>bb;else v=aa>=bb;}
                    stack.emplace_back(v);break;
                }
                case Op::And:{auto b=pop(),a=pop();stack.emplace_back(truthy(a)&&truthy(b));break;}
                case Op::Or:{auto b=pop(),a=pop();stack.emplace_back(truthy(a)||truthy(b));break;}
                case Op::MakeList:{List l(x.arg);for(int i=x.arg-1;i>=0;--i)l[i]=pop();stack.emplace_back(std::move(l));break;}
                case Op::Index:{auto idx=pop(),obj=pop();int n=(int)num(idx);
                    if(auto p=std::get_if<std::shared_ptr<List>>(&obj.data)){int sz=(int)(*p)->size();if(n<0)n+=sz;if(n<0||n>=sz)throw std::runtime_error("IndexError: list index out of range");stack.push_back((**p)[n]);}
                    else if(auto p=std::get_if<std::string>(&obj.data)){int sz=(int)p->size();if(n<0)n+=sz;if(n<0||n>=sz)throw std::runtime_error("IndexError: string index out of range");stack.emplace_back(std::string(1,(*p)[n]));}
                    else throw std::runtime_error("TypeError: object is not subscriptable");
                    break;}
                case Op::Call:{
                    std::vector<Value> args;
                    args.reserve((size_t)std::abs(x.arg));
                    for(int i=0;i<std::abs(x.arg);++i)args.push_back(pop());
                    std::reverse(args.begin(),args.end()); Value fn=pop();
                    auto fp=std::get_if<std::shared_ptr<Function>>(&fn.data);
                    if(!fp)throw std::runtime_error("TypeError: object is not callable");
                    const std::string& name=(*fp)->name;
                    if(!(*fp)->builtin)stack.push_back(callUser(*fp,args));
                    else if(name=="print") {for(size_t i=0;i<args.size();++i){if(i)std::cout<<" ";std::cout<<repr(args[i]);}std::cout<<"\n";stack.emplace_back();}
                    else if(name=="len") {if(args.size()!=1)throw std::runtime_error("TypeError: len() takes one argument");if(auto p=std::get_if<std::string>(&args[0].data))stack.emplace_back((double)p->size());else if(auto p=std::get_if<std::shared_ptr<List>>(&args[0].data))stack.emplace_back((double)(*p)->size());else throw std::runtime_error("TypeError: object has no len()");}
                    else if(name=="str") {if(args.size()!=1)throw std::runtime_error("TypeError: str() takes one argument");stack.emplace_back(repr(args[0]));}
                    else if(name=="int"||name=="float") {if(args.size()!=1)throw std::runtime_error("TypeError: conversion takes one argument");double v=num(args[0]);stack.emplace_back(name=="int"?std::floor(v):v);}
                    else if(name=="type") {if(args.size()!=1)throw std::runtime_error("TypeError: type() takes one argument");std::string s="NoneType";auto& d=args[0].data;if(std::holds_alternative<bool>(d))s="bool";else if(std::holds_alternative<double>(d))s="int";else if(std::holds_alternative<std::string>(d))s="str";else if(std::holds_alternative<std::shared_ptr<List>>(d))s="list";else if(std::holds_alternative<std::shared_ptr<Dict>>(d))s="dict";stack.emplace_back("<class '"+s+"'>");}
                    else if(name=="range") {if(args.empty()||args.size()>3)throw std::runtime_error("TypeError: range expected 1 to 3 arguments");int start=0,end=0,step=1;if(args.size()==1)end=(int)num(args[0]);else {start=(int)num(args[0]);end=(int)num(args[1]);if(args.size()==3)step=(int)num(args[2]);}if(step==0)throw std::runtime_error("ValueError: range() arg 3 must not be zero");long long distance=step>0?(long long)end-start:(long long)start-end;long long stride=step>0?step:-(long long)step;long long count=distance>0?(distance+stride-1)/stride:0;if(count>1000000)throw std::runtime_error("RuntimeError: range too large");List l;l.reserve((size_t)count);for(long long n=start;(step>0?n<end:n>end);n+=step)l.emplace_back((int)n);stack.emplace_back(std::move(l));}
                    else throw std::runtime_error("NotImplementedError: user-defined functions are not implemented yet");
                    break;
                }
                case Op::Jump:ip=x.arg;break;
                case Op::JumpFalse:{auto v=pop();if(!truthy(v))ip=x.arg;break;}
                case Op::JumpFalseKeep:if(stack.empty())throw std::runtime_error("RuntimeError: stack underflow");if(!truthy(stack.back()))ip=x.arg;else stack.pop_back();break;
                case Op::JumpTrueKeep:if(stack.empty())throw std::runtime_error("RuntimeError: stack underflow");if(truthy(stack.back()))ip=x.arg;else stack.pop_back();break;
                case Op::Return:return;
            }
        }
    }
    void runLine(const std::string& line) { auto code=compiled(line,false);execute(*code); }
    void run(const std::vector<SourceLine>& lines) {
        size_t pos=0;
        FlowResult result=runBlock(lines,pos,0);
        if(result.flow==Flow::Break)throw std::runtime_error("SyntaxError: 'break' outside loop");
        if(result.flow==Flow::Continue)throw std::runtime_error("SyntaxError: 'continue' outside loop");
        if(result.flow==Flow::Return)throw std::runtime_error("SyntaxError: 'return' outside function");
    }
};

Value VM::evaluate(const std::string& expression) {
    size_t base=stack.size();
    auto code=compiled(expression,true);
    execute(*code);
    if(stack.size()!=base+1)throw std::runtime_error("RuntimeError: invalid expression result");
    Value value=std::move(stack.back());stack.pop_back();return value;
}

Value VM::callUser(const std::shared_ptr<Function>& fn,const std::vector<Value>& args) {
    if(args.size()!=fn->params.size())throw std::runtime_error("TypeError: "+fn->name+"() takes "+std::to_string(fn->params.size())+" arguments");
    std::unordered_map<std::string,Value> local;
    local.reserve(fn->params.size());
    for(size_t i=0;i<args.size();++i)local.emplace(fn->params[i],args[i]);
    scopes.push_back(std::move(local));
    FlowResult result;
    try{if(!fn->code.empty()){size_t pos=0;result=runBlock(fn->code,pos,fn->code.front().indent);}}
    catch(...){scopes.pop_back();throw;}
    scopes.pop_back();
    if(result.flow==Flow::Break)throw std::runtime_error("SyntaxError: 'break' outside loop");
    if(result.flow==Flow::Continue)throw std::runtime_error("SyntaxError: 'continue' outside loop");
    return result.flow==Flow::Return?result.value:Value();
}

size_t VM::blockEnd(const std::vector<SourceLine>& lines,size_t start,int parentIndent) {
    if(start>=lines.size()||lines[start].indent<=parentIndent)throw std::runtime_error("IndentationError: expected an indented block");
    size_t end=start+1;
    while(end<lines.size()&&lines[end].indent>parentIndent)++end;
    return end;
}

std::string VM::blockValue(const std::string& text,const std::string& keyword) {
    if(text.size()<=keyword.size()+1||text.compare(0,keyword.size(),keyword)!=0||
       !std::isspace((unsigned char)text[keyword.size()])||text.back()!=':')
        throw std::runtime_error("SyntaxError: invalid '"+keyword+"' statement");
    std::string value=trim(text.substr(keyword.size(),text.size()-keyword.size()-1));
    if(value.empty())throw std::runtime_error("SyntaxError: expected expression");
    return value;
}

VM::FlowResult VM::runBlock(const std::vector<SourceLine>& lines,size_t& pos,int indent) {
    while(pos<lines.size()){
        const SourceLine line=lines[pos];
        if(line.indent<indent)break;
        if(line.indent>indent)throw std::runtime_error("IndentationError: unexpected indent at line "+std::to_string(line.number));
        const std::string& text=line.text;
        auto fail=[&](const std::string& message)->void {
            throw std::runtime_error("SyntaxError at line "+std::to_string(line.number)+": "+message);
        };
        if(text=="break"){++pos;return {Flow::Break,Value()};}
        if(text=="continue"){++pos;return {Flow::Continue,Value()};}
        if(text=="return"||text.compare(0,7,"return ")==0){
            ++pos;
            Value value=text=="return"?Value():evaluate(trim(text.substr(7)));
            return {Flow::Return,std::move(value)};
        }
        if(text.compare(0,3,"if ")==0){
            bool matched=false;
            FlowResult result;
            bool first=true;
            while(pos<lines.size()&&lines[pos].indent==indent){
                const std::string branch=lines[pos].text;
                bool isElse=branch=="else:";
                bool isIf=branch.compare(0,first?3:5,first?"if ":"elif ")==0;
                if(!isElse&&!isIf)break;
                std::string condition;
                if(!isElse)condition=blockValue(branch,first?"if":"elif");
                size_t body=pos+1;
                size_t end=blockEnd(lines,body,indent);
                bool take=!matched&&(isElse||truthy(evaluate(condition)));
                if(take){
                    matched=true;
                    size_t child=body;
                    result=runBlock(lines,child,lines[body].indent);
                    if(result.flow!=Flow::Normal){pos=end;return result;}
                }
                pos=end;
                if(isElse)break;
                first=false;
            }
            continue;
        }
        if(text.compare(0,6,"while ")==0){
            std::string condition=blockValue(text,"while");
            size_t body=pos+1,end=blockEnd(lines,body,indent);
            int childIndent=lines[body].indent;
            pos=end;
            size_t iterations=0;
            while(truthy(evaluate(condition))){
                if(++iterations>10000000)throw std::runtime_error("RuntimeError: loop iteration limit exceeded");
                size_t child=body;
                FlowResult result=runBlock(lines,child,childIndent);
                if(result.flow==Flow::Return)return result;
                if(result.flow==Flow::Break)break;
            }
            continue;
        }
        if(text.compare(0,4,"for ")==0){
            std::string header=blockValue(text,"for");
            size_t in=header.find(" in ");
            if(in==std::string::npos)fail("expected 'in' in for statement");
            std::string name=trim(header.substr(0,in));
            if(name.empty()||!(std::isalpha((unsigned char)name[0])||name[0]=='_'))fail("invalid loop variable");
            for(char c:name)if(!(std::isalnum((unsigned char)c)||c=='_'))fail("invalid loop variable");
            Value iterable=evaluate(trim(header.substr(in+4)));
            std::vector<Value> values;
            if(auto list=std::get_if<std::shared_ptr<List>>(&iterable.data))values=**list;
            else if(auto string=std::get_if<std::string>(&iterable.data)){values.reserve(string->size());for(char c:*string)values.emplace_back(std::string(1,c));}
            else fail("for loop requires a list, string, or range");
            size_t body=pos+1,end=blockEnd(lines,body,indent);
            int childIndent=lines[body].indent;
            pos=end;
            size_t iterations=0;
            for(const Value& value:values){
                if(++iterations>10000000)throw std::runtime_error("RuntimeError: loop iteration limit exceeded");
                store(name,value);
                size_t child=body;
                FlowResult result=runBlock(lines,child,childIndent);
                if(result.flow==Flow::Return)return result;
                if(result.flow==Flow::Break)break;
            }
            continue;
        }
        if(text.compare(0,4,"def ")==0){
            std::string header=trim(text.substr(4));
            if(header.size()<4||header.back()!=':')fail("invalid function definition");
            header=trim(header.substr(0,header.size()-1));
            size_t open=header.find('('),close=header.rfind(')');
            if(open==std::string::npos||close!=header.size()-1||close<open)fail("invalid function definition");
            std::string name=trim(header.substr(0,open));
            if(name.empty()||!(std::isalpha((unsigned char)name[0])||name[0]=='_'))fail("invalid function name");
            for(char c:name)if(!(std::isalnum((unsigned char)c)||c=='_'))fail("invalid function name");
            auto fn=std::make_shared<Function>();fn->name=name;
            std::string params=trim(header.substr(open+1,close-open-1));
            if(!params.empty()){
                std::stringstream stream(params);std::string param;
                while(std::getline(stream,param,',')){
                    param=trim(param);
                    if(param.empty()||!(std::isalpha((unsigned char)param[0])||param[0]=='_'))fail("invalid parameter name");
                    for(char c:param)if(!(std::isalnum((unsigned char)c)||c=='_'))fail("invalid parameter name");
                    if(std::find(fn->params.begin(),fn->params.end(),param)!=fn->params.end())fail("duplicate parameter name");
                    fn->params.push_back(param);
                }
            }
            size_t body=pos+1,end=blockEnd(lines,body,indent);
            fn->code.assign(lines.begin()+body,lines.begin()+end);
            store(name,Value(std::move(fn)));
            pos=end;
            continue;
        }
        if(text=="else:"||text.compare(0,5,"elif ")==0)fail("unexpected branch");
        if(text.back()==':'&&(text.compare(0,3,"if ")==0||text.compare(0,6,"while ")==0||text.compare(0,4,"for ")==0||text.compare(0,4,"def ")==0))
            fail("unsupported or malformed block");
        try{runLine(text);}
        catch(const std::exception& error){throw std::runtime_error("line "+std::to_string(line.number)+": "+error.what());}
        ++pos;
    }
    return {};
}

static std::string stripComment(const std::string& s) {
    bool single=false,dbl=false,escape=false;
    for(size_t i=0;i<s.size();++i){char c=s[i];if(escape){escape=false;continue;}if(c=='\\'){escape=true;continue;}if(c=='\''&&!dbl)single=!single;else if(c=='"'&&!single)dbl=!dbl;else if(c=='#'&&!single&&!dbl)return s.substr(0,i);}
    return s;
}
} // namespace fy

int main(int argc,char** argv) {
    using namespace fy;
    VM vm;
    if(argc==1) {
        std::cout<<"FY 0.2.0 — Python-style bytecode language\n";
        std::cout<<"Type expressions, or 'exit' to quit.\n";
        std::string line;
        while(true){std::cout<<"fy>>> ";if(!std::getline(std::cin,line))break;line=trim(line);if(line=="exit"||line=="quit")break;if(line.empty())continue;try{vm.runLine(stripComment(line));}catch(const std::exception&e){std::cerr<<e.what()<<"\n";}}
        return 0;
    }
    std::string filename=argv[1];
    std::ifstream in(filename);
    if(!in){std::cerr<<"fy: cannot open file: "<<filename<<"\n";return 2;}
    std::vector<SourceLine> source;
    std::string line;int lineno=0;
    while(std::getline(in,line)){
        ++lineno;
        std::string clean=stripComment(line);
        size_t pos=0;int indent=0;
        while(pos<clean.size()&&(clean[pos]==' '||clean[pos]=='\t')){
            indent+=clean[pos]=='\t'?4:1;
            ++pos;
        }
        std::string text=trim(clean.substr(pos));
        if(!text.empty())source.push_back({indent,lineno,std::move(text)});
    }
    try {
        vm.run(source);
    } catch(const std::exception&e) {
        std::cerr<<filename<<": "<<e.what()<<"\n";return 1;
    }
    return 0;
}
