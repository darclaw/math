"use strict"
var math = require("mathjs");
var mod= function(a,n){
	if(a>=0){
		return a%n;
	}else{
		return n-(n-a)%n;
	}
}
//var mod = math.mod;

var print1 = function(name,f){
	console.log(name);
	var string ='';
	for(let i1=0;i1<size;i1++){
		string+=f(i1);
	}
	console.log(string);
}

var print2=function(name, f){
	console.log(name);
	for(let i1=0;i1<size;i1++){
		var string ="";
		for(let i2=0;i2<size;i2++){
			string+=f(i1,i2);
		}
		console.log(string);
	}
}

		

var compose= function(f,g){
	return (x,y)=>{
		return f(g(x,y));
	}
}
var compose2 = function(f,g,h){
	return (x,y)=>{
		return f(g(x,y),h(x,y));
	}
}

//takes a function that returns a function with multiple args
var memomizeFunc2 = function(f, ...args){
   var map = new Map();
   var bijOfArgsFunc = function(args){ return args.join();}
   var bijOfArgs = bijOfArgsFunc(args);
   if(map.has(bijOfArgs)){
      return map.get(bijOfArgs);
   }else{
      var result = f(...args);
      var resultMap = new Map();
      var resultMapFunction= function(...xs){
         var bijOfXs = bijOfArgsFunc(xs);
         if(resultMap.has(bijOfArgs)){
            return resultMap.get(bijOfArgs);
         }else{
            var resultFuncResult = result(...xs);
            map.set(bijOfXs, resultFuncResult);
            return resultFuncResult;
         }
      }
      map.set(bijOfArgs, resultMapFunction);
      return resultMapFunction;
   }

}


//size of U
var size=4;

//base function
var op=function(x,y){
	x=mod(x,size);
	y=mod(y,size);
	if(x==y){
		return mod(x-1,size);
	}
	else{
		return 0;
	}
}
//var op = memomizeFunc.bind({}, unMemop);

function rot(x){
	x=mod(x,size);
	return op(x,x);
}
//var rot = memomizeFunc.bind({}, unMemrot);

//n is the number of rot
function A(n){
	n=mod(n,size);
	if(n==0){
		return (a,b)=>{return a}
	}else{
		return compose(rot,A(n-1));
	}
}
//var A = memomizeFunc.bind({},umA);

function B(n){
	n=mod(n,size);
	if(n==0){
		return (a,b)=>{return b}
	}else{
		return compose(rot,B(n-1));
	}
}
//var B = memomizeFunc.bind({},umB);

function F(a,b){
	a=mod(a,size);
	b=mod(b,size);
	if(b==0){
		return compose2(op,A(a),B(a));
	}else{
		return compose(rot,F(a,mod(b-1,size)));
	}
}
var F = memomizeFunc2.bind({}, F);

//this is the \ovl{O,1,2},etc funcs
function N(n){
	n=mod(n,size);
	if(n==0){
		return compose2(op,A(0),A(1));
	}else{
		return compose(rot,N(n+1));
	}
}
var N = memomizeFunc2.bind({}, N);

//isolator, same as \ovl{(a,b,c)}
function iso(a,b,c){
	a=mod(a,size);
	b=mod(b,size);
	c=mod(c,size);
	if(c==0){
		return N(0);
	}else if(c!=size-1){
		return compose2(
			op,
			compose2(
				F(-2-c,0),
				A(a), B(b)
			),
			N(c+1)
		);
	}else{
		return compose2(op, 
			compose2( F(c-1,1),
				A(a),B(b)
			), N(0)
		);
	}
}
var iso = memomizeFunc2.bind({}, iso);

function S(a){
	a=mod(a,size);
	return compose2(
		F(a-1,-a),
		iso(0,a,a),iso(a,0,a)
	);
}
var S = memomizeFunc2.bind({}, S);

//errors //doesn't seem to
function AS(a,b){
	a=mod(a,size);
	b=mod(b,size);
	return compose2(
		F(b-1,-b),
		S(a),S(a+1)
	);
}
var AS = memomizeFunc2.bind({}, AS);

function umPCO(a){
	a=mod(a,size);
	if(a==0){
		return N(0);
	}else if(a==1){
		return S(1);
	}else if(a==2){
		return compose2(
			F(0,-1),
			S(2),AS(1,2)
		);
	}else{
      var PCOprev=PCO(2);
      for(let i=3;i<=a;i++){
         var base = i+1;
         var [t0,t1]=createNeededTablesForPCOnext(base);
         var t0func=Pcreate(i, t0);
         var t1func=Pcreate(i, t1);
			PCOprev=compose2(F(-2,0),t0func,t1func);
      }
		return PCOprev;
	}
}
var PCO = memomizeFunc2.bind({}, umPCO);

function createNeededTablesForPCOnext(base){
//create
//t0=[0,0,1,2,3,...a-1,0,0,...]
//t1=[1,1,1,2,3,..,a-1,1,1,...]
//going across and down, and zero else
   var t0=[];
   var t1=[];
	var i1=0;
	var i2=0;
   for(i1=0;i1<base;i1++){
      t0[i1]=[];
      t1[i1]=[];
      for(i2=0;i2<base;i2++){
         if(i1==0){
            if(i2==0){
               t0[i1][i2]=0;
               t1[i1][i2]=1;
            }else{
               t0[i1][i2]=i2-1;
               t1[i1][i2]=i2-1;
            }
         }else if(i2==0){
            //i1=0 should be taken care of
            t0[i1][i2]=i1-1;
            t1[i1][i2]=i1-1;
         }else{
            t0[i1][i2]=0;
            t1[i1][i2]=1;
         }
      }
   }
	//don't reset i1 and i2
	for(var i3=i1;i3<size;i3++){
		t0[i3]=[];
		t1[i3]=[];
		for(var i4=i2;i4<size;i4++){
			t0[i3][i4]=0;
			t1[i3][i4]=1;
		}
	}
   return [t0, t1];
}
//partial creater, n is the base of the table (highest needed number + 1)
function Pcreate(n,table){
	var func=N(0);
	for(let i1=0;i1<size;i1++){
		for(let i2=0;i2<size;i2++){
			//console.log("/ogl3/Pcreate i1="+i1+" i2="+i2);
			var c=table[i1][i2];
			func=compose2(
				PCO(n-1),
				func,iso(i1,i2,c)
			);
		}
	}
	return func;
}

console.log("mod(-1,3)="+mod(-1,3));
print2("op",op);
print1("rot",rot);
print2("A(2)",A(2));
print2("F(1,1)",F(1,1));
print2("N(2)",N(2));
print2("iso(1,2,2)",iso(1,2,2));
print2("S(2)",S(2));
//errors
//print2("compose2(F(2-1,-2),S(1),S(1+1))",compose2(F(2-1,-2),S(1),S(1+1)));
print2("AS(1,2)",AS(1,2));
print2("PCO(3)",PCO(3));

var t0=[[0,1,0,3],
		  [0,1,2,3],
		  [1,0,2,1],
		  [0,3,3,2]]
        //[1,2,0,1,4]]
print2(t0,Pcreate(4,t0));

