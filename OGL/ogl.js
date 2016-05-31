"use strict"
var math=require("mathjs");
function OGLStuff(size){
	//internal globals
		//int size, const
		//table[] combinations
		//table[] uniqueCombinations
		//table tA, const
		//table tB, const
	//internal functions
		//int op(int a,int b)
		//table createTable(table t1,table t2,func func)
		//bool testIfUnique(table t)
	//datatypes
		//table a collection the outputs of a function(a,b)	of form
			//  a:	0	,1,...
			//b:0:f(0,0),f(1,0),..
			//	1:f(0,1),f(1,1),..
	
	//internal globals defin
	var combinations=[]; //all the tables that are created
	var uniqueCombinations=[]; //tables that unique
	//the table A is square with form: 
	//[0,1,2,...,size]
	//[0,1,2,...,size]
	//...
	//coresponds to the default input A
	const tA= Table("tA", 
		function tAgenerator(){
			var table=[];
			for(let i1=0;i1<size;i1++){
				table[i1]=[];
				for(let i2=0;i2<size;i2++){
					table[i1][i2]=i2;
				}
			}
			return table;
		}
	);
	//corresponds to default input B
	//is square, has form
	//[0,0,0,...]
	//[1,1,1,...]
	//....
	//[size,size,size,...]
	const tB= Table("tB",
		function tBgenerator(){
			var table=[];
			for(let i1=0;i1<size;i1++){
				table[i1]=[];
				for(let i2=0;i2<size;i2++){
					table[i1][i2]=i1;
				}
			}
			table.name="tB";
			return table;
		}
	);
	//end globals



	//funcition definitions
	
	//the base funtion that is composed
	function op(a,b){
		if(a==b){
			return math.mod((a-1),size);
		}else{
			return 0;
		}
	}
	function opA(aa,ba){
		if(aa.length!=ba.length){
			return new Error("arrays are of wrong dimensions");
		}
		let array=[];
		for(let i1=0;i1<aa.length;i1++){
			array[i1]=[];
			if(aa[i1].length!=ba[i1].length){
				return new Error("arrays are of wrong dimentions");
			}
			for(let i2=0;i2<aa[i1].length;i2++){
				array[i1][i2]=op(aa[i1][i2],ba[i1][i2]);
			}
		}
		return array;
	}

	//creates the table object of size size
	//generator is eather a function that returns a 2d array or a stored table
	//{array:[][],name:string,equals:function}
	function Table(name,generator,size){
		//public functions
			//bool equals(table)
			//string fancyString()
			//{name: string, array:[][]} storeable()
		let me={};
		me.array=[];
		//generate array part of me
		if(!generator){
			for(let i1=0;i1<size;i1++){
				me.array[i1]=[];
				for(let i2=0;i2<size;i2++){
					me.array[i1][i2]=Number.NaN;
				}
			}
		}else{
			if(typeof(generator)==="function"){
				me.array=generator(size);
			}else if(Array.isArray(generator)){
				me.array=generator;
			}else{
				me.unstore(generator);
			}
		}
		me.name=name;
		me.equals=function checkIfEqual(table){
			if(table.array.length!=me.array.length){
				return false;
			}
			for(let i1=0;i1<table.array.length;i1++){
				if(table.array[i1].length != me.array[i1].length){
					return false;
				}
				for(let i2=0;i2<table.array[i1].length;i2++){
					if(table.array[i1][i2] != me.array[i1][i2].length){
						return false;
					}
				}
			}
			return true;
		}
		me.fancyString=function StringifyMe(){
			let str=me.name+"\n";
			for(let i1=0;i1<me.array.length;i1++){
				str +="[ ";
				for(let i2=0;i2<me.array[i1].length;i2++){
					str += me.array[i1][i2]+" , ";
				}
				str+="],\n";
			}
			return str;
		}
		me.storeable=function objectifyMe(){
			storable={};
			storable.name=me.name;
			storable.array=(function(){
				array=[];
				for(let i1=0;i1<me.array.length;i1++){
					array[i1]=[];
					for(let i2=0;i2<me.array[i1].length;i2++){
						array[i1][i2]=me.array[i1][i2];
					}
				}
			})();
			return storable;
		}
		//private
		me.unstore=function(storeable){
			//must set array before anything else
			me.array=storeable.array;
			me.name=storeable.name;
		}

		return me;
	}
	//creates a table of func(t1,t2)
	//tables and function must have a name
	function createTable(t1,t2,func){
		var table = Table(
			func.name+"("+t1.name+","+t2.name+")",
			func(t1.array,t2.array)
		);
		return table;
	}
	

	function testIfUnique(table){
		for(let i1=0;i<uniqueCombinations.length;i++){
			//is unique
			if(!table.equals(uniqueCombinations[i])){
				uniqueCombinations.push(table);
			}
		}
	}

	//testing stuff delete me at end
	console.log("tB fancy\n"+tB.fancyString());
	console.log("tA fancy\n"+tA.fancyString());
	console.log(opA(tA.array,tB.array));
	//let opAB=createTable(tA,tB,opA);
	//console.log("opAB fancy\n"+opAB.fancyString());


	return{
		op:op
	}
}


OGLStuff(4);
