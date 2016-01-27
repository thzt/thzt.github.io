---
layout: post
categories: Lisp
title: Y combinator in javascript
description: javascript中推导Y组合子。
---

本文借助阶乘函数，在javascript中推导了Y combinator。

<br/>

## **原始的阶乘函数定义**

var fact=function(n){

&nbsp;&nbsp;&nbsp;&nbsp;return n==0?1:n*fact(n-1);

};

<br/>

console.assert(fact(5)==120);

<br/>

## **把递归函数作为参数传递**

var highFact=function(f){

&nbsp;&nbsp;&nbsp;&nbsp;return function(n){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return n==0?1:n*f(f)(n-1);

&nbsp;&nbsp;&nbsp;&nbsp;};

};

<br/>

console.assert(highFact(highFact)(5)==120);

<br/>

## **借助辅助函数消除对自身的调用**

var fact=(function(g){

&nbsp;&nbsp;&nbsp;&nbsp;return g(g);

}(function(f){

&nbsp;&nbsp;&nbsp;&nbsp;return function(n){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return n==0?1:n*f(f)(n-1);

&nbsp;&nbsp;&nbsp;&nbsp;};

}));

<br/>

console.assert(fact(5)==120);

<br/>

## **构造递归原型**

var fact=(function(g){

&nbsp;&nbsp;&nbsp;&nbsp;return g(g);

}(function(f){

&nbsp;&nbsp;&nbsp;&nbsp;return function(n){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;var factProto=function(h){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return function(x){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return x==0?1:x*h(x-1);

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;};

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;};

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return factProto(f(f))(n);

&nbsp;&nbsp;&nbsp;&nbsp;};

}));

<br/>

console.assert(fact(5)==120);

<br/>

## **将递归原型提取出来**

var factProto=function(h){

&nbsp;&nbsp;&nbsp;&nbsp;return function(x){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return x==0?1:x*h(x-1);

&nbsp;&nbsp;&nbsp;&nbsp;};

};

<br/>

var fact=(function(g){

&nbsp;&nbsp;&nbsp;&nbsp;return g(g);

}(function(f){

&nbsp;&nbsp;&nbsp;&nbsp;return function(n){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return factProto(f(f))(n);

&nbsp;&nbsp;&nbsp;&nbsp;};

}));

<br/>

console.assert(fact(5)==120);

<br/>

## **把递归原型变成形参**

var fact=(function(k){

&nbsp;&nbsp;&nbsp;&nbsp;return (function(g){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return g(g);

&nbsp;&nbsp;&nbsp;&nbsp;}(function(f){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return function(n){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return k(f(f))(n);

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;};

&nbsp;&nbsp;&nbsp;&nbsp;}));

}(factProto));

<br/>

console.assert(fact(5)==120);

<br/>

## **得到Y combinator**

var yCombinator=function(k){

&nbsp;&nbsp;&nbsp;&nbsp;return (function(g){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return g(g);

&nbsp;&nbsp;&nbsp;&nbsp;}(function(f){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return function(n){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return k(f(f))(n);

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;};

&nbsp;&nbsp;&nbsp;&nbsp;}));

};

<br/>

console.assert(yCombinator(factProto)(5)==120);

<br/>

## **扩展为多参数形式**

var yCombinator=function(k){

&nbsp;&nbsp;&nbsp;&nbsp;return (function(g){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return g(g);

&nbsp;&nbsp;&nbsp;&nbsp;}(function(f){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return function(){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return k(f(f)).apply(null,arguments);

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;};

&nbsp;&nbsp;&nbsp;&nbsp;}));

};

<br/>

console.assert(yCombinator(factProto)(5)==120);



