"use strict";
var extend = require('util')._extend;
function det(array){
	console.log("det array length "+array.length);
	if(array.length==1){
		return array[0][0];
	}else{
		let sum=0;
		for(let i1=0;i1<array.length;i1++){
			let mul=0;
			if(i1%2==0){
				mul=1;
			}else{
				mul=-1;
			}
			let narray=reduceArray(array,i1,0);
			console.log("det i1 "+i1+" narray "+narray);
			sum+=mul*array[0][i1]*det(narray);
		}
		return sum;
	}
}

//array must be square
function reduceArray(array, row, col){
	let b=[];
	let bi=0; //index of b
	for(let i=0;i<array.length;i++){
		if(i==row){}
		else{
			let c=[];
			let ci=0;
			for(let j=0;j<array[i].length;j++){
				if(j==col){}
				else{
					c[ci]=array[i][j];
					ci++;
				}
			}
			b[bi]=c;
			bi++;
		}
	}
	console.log(b);
	return b;
}
function replaceCol(array, colVect, col){
	var narray=extend({},array);
	for(let i=0;i<array.length;i++){
		narray[i][col]=colVect[i];
	}
	return narray;
}

function generateAnswerColVect(size,start){
	var ansColVect=new Array(size);
	for(let i=0;i<size;i++){
		let sum=0;
		for(let a=0;a<size;a++){
			sum+=Math.pow((start+i),a)/factorial(start+i);
		}
		ansColVect[i]=sum;
	}
	return ansColVect;
}

function generateCoefArray(size,start,r){
	var CoefArray=new Array(size);
	let col=new Array(size);
	for(let i=0;i<size;i++){
		for(let a=0;a<size;a++){
			col[a]=Math.pow((start+i),a-r);
		}
		CoefArray[i]=col;
	}
	return CoefArray;
}

function factorial(num){
	var prod;
	if(num==0 || num==1) return 1;
	for(let i=2;i<num;i++){
		prod=prod*i;
	}
	return prod;
}
function calcSol(coefArray,ansColVect){
	let solArray=new Array(coefArray.length);
	let detCoefArray=det(coefArray);
	for(let i=0;i<coefArray.length;i++){
		solArray[i]=det(replaceCol(coefArray,ansColVect,i))/detCoefArray;
	}
	return solArray;
}

var array=[[1,2,3],[4,5,6],[7,8,9]];
//console.log("coefArray "+generateCoefArray(10,1,1/2));

function testIt(size,start,r){
	let coefArray=generateCoefArray(size,start,r);
	console.log(coefArray);
	let ansColVect=generateAnswerColVect(size,start);
	console.log(ansColVect);
	//let solArray=calcSol(coefArray,ansColVect);
	//console.log("solutions array "+solArray);
}

//testIt(4,1,1/2);
testIt2();
function testIt2(){
	var matrix=[[1,2],[3,4]];
	console.log(det(matrix));
}
