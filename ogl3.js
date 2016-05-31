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



//size of U
var size=3;
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

function rot(x){
	x=mod(x,size);
	return op(x,x);
}

//n is the number of rot
function A(n){
	n=mod(n,size);
	if(n==0){
		return (a,b)=>{return a}
	}else{
		return compose(rot,A(n-1));
	}
}
function B(n){
	n=mod(n,size);
	if(n==0){
		return (a,b)=>{return b}
	}else{
		return compose(rot,B(n-1));
	}
}

function F(a,b){
	a=mod(a,size);
	b=mod(b,size);
	if(b==0){
		return (x,y)=>{
			return compose2(op,A(a),B(a))(x,y);
		}
	}else{
		return (x,y)=>{
			return compose(rot,F(a,mod(b-a,size)))(x,y);
		}
	}
}

//this is the \ovl{O,1,2},etc funcs
function N(n){
	n=mod(n,size);
	if(n==0){
		return (x,y)=>{
			return compose2(op,A(0),A(1))(x,y);
		}
	}else{
		return (x,y)=>{
			return compose(rot,N(n+1))(x,y);
		}
	}
}

//isolator, same as \ovl{(a,b,c)}
function iso(a,b,c){
	a=mod(a,size);
	b=mod(b,size);
	c=mod(c,size);
	if(c==0){
		return N(0);
	}else if(c!=size-1){
		return (x,y)=>{
			return compose2(
				op,
				compose2(
					F(-2-c,0),
					A(a), B(b)
				),
				N(c+1)
			)(x,y);
		}
	}else{
		return (x,y)=>{
			return compose2(op, 
				compose2( F(c-1,1),
					A(a),B(b)
				), N(0)
			)(x,y);
		}
	}
}

function S(a){
	a=mod(a,size);
	return (x,y)=>{
		return compose2(
			F(a-1,-a),
			iso(0,a,a),iso(a,0,a)
		)(x,y);
	}
}

//errors
function AS(a,b){
	a=mod(a,size);
	b=mod(b,size);
	return function ASret(x,y){
		return compose2(
			F(b-1,-b),
			S(a),S(a+1)
		)(x,y);
	}
}

function PCO(a){
	a=mod(a,size);
	if(a==0){
		return N(0);
	}else if(a==1){
		return S(1);
	}else if(a==2){
		return (x,y)=>{
			return compose2(
				F(0,-1),
				S(2),AS(1,2)
			);
		}
	}else{

	}
}

//partial creater, n is the base of the table (highest needed number)
function Pcreate(n,table){
	var func=N(0);
	for(let i1=0;i1<size;i1++){
		for(let i2=0;i2<size;i2++){
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
print2("iso(1,2,1)",iso(1,2,1));
print2("S(2)",S(2));
//errors
//print2("compose2(F(2-1,-2),S(1),S(1+1))",compose2(F(2-1,-2),S(1),S(1+1)));
//print2("AS(1,2)",AS(1,2));

var t0=[[0,0,1],
		  [0,1,2],
		  [1,0,2]]

print2(t0,Pcreate(2,t0));


