---
layout: post
categories: Logic
title: 可计算性理论名词释义
description: 
---

<script src="/thirdpart/jQuery/jquery-1.11.1.js"></script>
<link href="/thirdpart/KaTeX/katex.min.css" rel="stylesheet"/>
<script src="/thirdpart/KaTeX/katex.min.js"></script>
<script src="/javascripts/katex.js"></script>

## **部分数论函数**

**二元关系**

集合<span data-katex="S"></span>和<span data-katex="T"></span>，

<span data-katex="S\times T"></span>的元素<span data-katex="(a,b)"></span>称为有序对，

<span data-katex="S\times T"></span>的子集称为从<span data-katex="S"></span>到<span data-katex="T"></span>的二元关系，

从<span data-katex="S"></span>到<span data-katex="S"></span>的二元关系，称为<span data-katex="S"></span>上的二元关系。

<br/>

**定义域和值域**

设<span data-katex="R"></span>是从<span data-katex="S"></span>到<span data-katex="T"></span>的二元关系，

则称<span data-katex="R"></span>的定义域为<span data-katex="dom\ R=\{a|\exists b\ (a,b)\in R\}"></span>，

<span data-katex="R"></span>的值域为<span data-katex="ran\ R=\{b|\exists a\ (a,b)\in R\}"></span>。

<br/>

**象**

<span data-katex="A\subseteq S"></span>，则称<span data-katex="A"></span>在<span data-katex="R"></span>下的象为，

<span data-katex="R(A)=\{b|\exists a\ (a\in A \wedge (a,b)\in R)\}"></span>，

特别的，如果<span data-katex="a\in A"></span>，那么把<span data-katex="\{a\}"></span>在<span data-katex="R"></span>下的象简记为<span data-katex="a"></span>在<span data-katex="R"></span>下的象，记为<span data-katex="R(a)"></span>。

即，<span data-katex="R(a)=\{b|(a,b)\in R\}"></span>

<br/>

**部分函数**

如果<span data-katex="f"></span>是从<span data-katex="S"></span>到<span data-katex="T"></span>的二元关系，且<span data-katex="\forall a\in S"></span>，<span data-katex="f(a)=\varnothing"></span>或<span data-katex="\{b\}"></span>，

则称<span data-katex="f"></span>是从<span data-katex="S"></span>到<span data-katex="T"></span>的部分函数，或<span data-katex="S"></span>上的部分函数。

若<span data-katex="f(a)=\{b\}"></span>，则称<span data-katex="f(a)"></span>有定义，记为<span data-katex="f(a)\downarrow"></span>，

<span data-katex="b"></span>称为<span data-katex="f"></span>在<span data-katex="a"></span>点的函数值，记为<span data-katex="f(a)=b"></span>，

若<span data-katex="f(a)=\varnothing"></span>，则称<span data-katex="f(a)"></span>无定义，记为<span data-katex="f(a)\uparrow"></span>。

<br/>

**全函数**

如果<span data-katex="\forall a\in S"></span>都有<span data-katex="f(a)\downarrow"></span>，即<span data-katex="dom\ f=S"></span>，则称<span data-katex="f"></span>是<span data-katex="S"></span>上的全函数，

此时，可以记为<span data-katex="f:S\rightarrow T"></span>。

<br/>

**<span data-katex="n"></span>元部分函数**

设<span data-katex="f"></span>是笛卡尔积<span data-katex="S_1\times S_2\times \cdots \times S_n"></span>上的部分函数，

则通常把<span data-katex="f((a_1,a_2,\cdots ,a_n))"></span>，简记为<span data-katex="f(a_1,a_2,\cdots ,a_n)"></span>。

<br/>

集合<span data-katex="S^n=S\times S\times \cdots \times S"></span>上的部分函数，称为<span data-katex="S"></span>上的<span data-katex="n"></span>元部分函数。

自然数集<span data-katex="N=\{0,1,2,\cdots \}"></span>，从<span data-katex="N^n"></span>到<span data-katex="N"></span>的部分函数，称为<span data-katex="n"></span>元部分数论函数。

下文中提到的函数，默认为数论函数。

<br/>

**字函数**

字母表是一个非空有穷集合，

设<span data-katex="A"></span>是一个字母表，<span data-katex="A"></span>中元素的有穷序列<span data-katex="w=(a_1,a_2,\cdots ,a_m)"></span>称为<span data-katex="A"></span>上的字符串或字，

简记为<span data-katex="w=a_1a_2\cdots a_m"></span>。

<span data-katex="w"></span>中符号的个数，称为字符串的长度，记为<span data-katex="|w|"></span>。

我们用<span data-katex="\epsilon"></span>表示空串，它不包含任何符号，是唯一的长度为<span data-katex="0"></span>的字符串。

<br/>

<span data-katex="A"></span>上字符串的全体，记为<span data-katex="A^*"></span>。

设<span data-katex="u,v\in A^*"></span>，把<span data-katex="v"></span>连接在<span data-katex="u"></span>后面得到的字符串记为<span data-katex="uv"></span>。

<br/>

设<span data-katex="u\in A^*"></span>，规定，<span data-katex="u^0=\epsilon"></span>，<span data-katex="u^{n+1}=u^n u\ ,n\in N"></span>，

当<span data-katex="n>0"></span>时，<span data-katex="u^n"></span>等于<span data-katex="n"></span>个<span data-katex="u"></span>连接在一起。

<br/>

<span data-katex="(A^*)^n"></span>到<span data-katex="A^*"></span>的部分函数，称为<span data-katex="A"></span>上的<span data-katex="n"></span>元部分字函数。

<br/>

## **程序设计语言<span data-katex="\mathscr{S}"></span>**

**变量**

语言<span data-katex="\mathscr{S}"></span>使用三种变量，

输入变量<span data-katex="X_1,X_2,\cdots"></span>，输出变量<span data-katex="Y"></span>，中间变量<span data-katex="Z_1,Z_2,\cdots"></span>，

变量可以取任何自然数值<span data-katex="n\in N"></span>。

语言还可以使用标号<span data-katex="A_1,A_2,\cdots"></span>。

当下标为<span data-katex="1"></span>时，可以略去。例如，<span data-katex="X_1"></span>和<span data-katex="X"></span>表示同一个变量。

<br/>

**语句**

语言<span data-katex="\mathscr{S}"></span>有三种类型的语句，

（1）增量语句<span data-katex="V\leftarrow V+1"></span>，表示变量V的值加<span data-katex="1"></span>

（2）减量语句<span data-katex="V\leftarrow V-1"></span>，若变量<span data-katex="V"></span>的当前值为<span data-katex="0"></span>，则<span data-katex="V"></span>的值保持不变，否则<span data-katex="V"></span>的值减<span data-katex="1"></span>。

（3）条件转移语句<span data-katex="IF\ V\neq 0\ GOTO\ L"></span>，如果变量<span data-katex="V"></span>的值不等于<span data-katex="0"></span>，则下一步执行带标号<span data-katex="L"></span>的指令，否则顺序执行下一条指令。

<br/>

**执行**

开始执行程序时，中间变量和输出变量的值都为<span data-katex="0"></span>，

从第一条指令开始，一条一条的顺序执行，除非遇到条件转移语句，

当程序没有指令可执行时，计算结束，

此时<span data-katex="Y"></span>的值为程序的输出值。

<br/>

例如，

<span data-katex="[A]\ X\leftarrow X-1"></span>

<span data-katex="\ \ \ \ Y\leftarrow Y+1"></span>

<span data-katex="\ \ \ \ IF\ X\neq 0\ GOTO\ A"></span>

这里<span data-katex="A"></span>是第一条指令的标号，我们可以看到，这个程序计算的函数是，

<span data-katex="f(x)=x,\ if\ x>0"></span>

<span data-katex="f(x)=1,\ else"></span>

<br/>

**状态**

设<span data-katex="\sigma"></span>是形如等式<span data-katex="V=m"></span>的有穷集合，其中<span data-katex="V"></span>是一个变量，<span data-katex="m"></span>是一个数。

如果，

（1）对于每一个变量<span data-katex="V"></span>，<span data-katex="\sigma"></span>中至多含有一个等式<span data-katex="V=m"></span>

（2）如果在程序<span data-katex="\mathscr{P}"></span>中出现变量<span data-katex="V"></span>，则<span data-katex="\sigma"></span>中必含有等式<span data-katex="V=m"></span>

那么，称<span data-katex="\sigma"></span>是程序<span data-katex="\mathscr{P}"></span>的一个状态。

<br/>

状态描述程序在执行的某一步各个变量的值，

我们约定，如果<span data-katex="\sigma"></span>中不含关于<span data-katex="V"></span>的等式，则变量<span data-katex="V"></span>的值自动取<span data-katex="0"></span>。

<br/>

**快相**

程序的一个快相，是一个有序对<span data-katex="(i,\sigma )"></span>，

表示程序的当前状态为<span data-katex="\sigma"></span>，即将执行第<span data-katex="i"></span>条指令。

<span data-katex="1\leqslant i \leqslant q"></span>，其中，<span data-katex="q"></span>是程序的长度，

如果<span data-katex="i=q+1"></span>，就表示程序结束，<span data-katex="(q+1,\sigma )"></span>称为程序的终点快相。

<br/>

除了输入变量外，所有变量值为0的状态称为初始状态，

如果<span data-katex="\sigma"></span>是初始状态，则称<span data-katex="(1,\sigma )"></span>是初始快相。

<br/>

**程序的计算**

设<span data-katex="s_1,s_2,\cdots"></span>是程序<span data-katex="\mathscr{P}"></span>的一个快相序列，长度为<span data-katex="k"></span>，

如果，

（1）<span data-katex="s_1"></span>是初始快相

（2）对于每一个<span data-katex="i(1\leqslant i<k)"></span>，<span data-katex="s_{i+1}"></span>是<span data-katex="s_i"></span>的后继

（3）当<span data-katex="k<\infty"></span>时，<span data-katex="s_k"></span>是终点快相

则称该序列是<span data-katex="\mathscr{P}"></span>的一个计算。

<br/>

## **函数的可计算性**

**程序计算的函数**

设<span data-katex="\mathscr{P}"></span>是语言<span data-katex="\mathscr{S}"></span>的一个程序，<span data-katex="n"></span>是一个正整数，

称函数<span data-katex="\psi (x_1,x_2,\cdots ,x_n)"></span>为程序<span data-katex="\mathscr{P}"></span>计算的<span data-katex="n"></span>元部分函数，

如果，s_1"></span>是初始快相，其中输入变量为<span data-katex="X_1=x_1,X_2=x_2,\cdots ,X_n=x_n"></span>，输出变量为<span data-katex="Y"></span>，

（1）从<span data-katex="s_1"></span>开始的计算是有穷序列<span data-katex="s_1,s_2,\cdots ,s_k"></span>，则<span data-katex="\psi (x_1,x_2,\cdots ,x_n)"></span>等于<span data-katex="Y"></span>在<span data-katex="s_k"></span>中的值

（2）从<span data-katex="s_1"></span>开始的计算是无穷序列<span data-katex="s_1,s_2,\cdots"></span>，则<span data-katex="\psi (x_1,x_2,\cdots ,x_n)\uparrow"></span>

<br/>

**部分可计算性与可计算性**

设<span data-katex="f(x_1,x_2,\cdots ,x_n)"></span>是一个部分函数，如果存在程序<span data-katex="\mathscr{P}"></span>计算<span data-katex="f"></span>，

则称<span data-katex="f"></span>是部分可计算的。

<br/>

如果一个函数，既是部分可计算的，又是全函数，则称这个函数是可计算的。

<br/>

**谓词的可计算性**

我们可以把谓词看作取值为<span data-katex="0"></span>或<span data-katex="1"></span>的全函数，

并把真值等同于<span data-katex="1"></span>，假值等同于<span data-katex="0"></span>。

<br/>

如果谓词<span data-katex="P(x_1,x_2,\cdots ,x_n)"></span>作为一个全函数是可计算的，

则称该谓词是可计算的。

<br/>

## **函数的递归性**

**合成运算**

设<span data-katex="f"></span>是<span data-katex="k"></span>元部分函数，<span data-katex="g_1,g_2,\cdots ,g_k"></span>是<span data-katex="k"></span>个<span data-katex="n"></span>元部分递归函数，

<span data-katex="h(x_1,\cdots ,x_n)=f(g_1(x_1,\cdots ,x_n),\cdots ,g_k(x_1,\cdots ,x_n))"></span>

则称<span data-katex="h"></span>是由<span data-katex="f"></span>和<span data-katex="g_1,g_2,\cdots ,g_k"></span>，经过合成运算得到的。

<br/>

可证，如果<span data-katex="h"></span>是由（部分）可计算函数<span data-katex="f"></span>和<span data-katex="g_1,g_2,\cdots ,g_k"></span>合成得到的，

则<span data-katex="h"></span>也是（部分）可计算函数。

<br/>

**原始递归运算**

设<span data-katex="g"></span>是一个<span data-katex="2"></span>元全函数，<span data-katex="k"></span>是一个常数，

<span data-katex="h(0)=k"></span>

<span data-katex="h(t+1)=g(t,h(t))"></span>

则称<span data-katex="h"></span>是由<span data-katex="g"></span>经过原始递归运算得到的。

<br/>

可证，如果<span data-katex="g"></span>是可计算的，则<span data-katex="h"></span>是可计算的。

<br/>

设<span data-katex="f"></span>是一个<span data-katex="n"></span>元全函数，<span data-katex="g"></span>是<span data-katex="n+2"></span>元全函数，

<span data-katex="h(x_1,\cdots ,x_n,0)=f(x_1,\cdots ,x_n)"></span>

<span data-katex="h(x_1,\cdots ,x_n,t+1)=g(t,h(x_1,\cdots ,x_n,t),x_1,\cdots ,x_n)"></span>

则称<span data-katex="h"></span>是由<span data-katex="f"></span>和<span data-katex="g"></span>经过原始递归运算得到的。

<br/>

可证，如果<span data-katex="f"></span>和<span data-katex="g"></span>都是可计算的，则<span data-katex="h"></span>是可计算的。

<br/>

**原始递归函数类**

设初始函数包括，

（1）零函数<span data-katex="n(x)=0"></span>

（2）后继函数<span data-katex="s(x)=x+1"></span>

（3）投影函数<span data-katex="u^n_i(x_1,\cdots ,x_n)=x_i"></span>，<span data-katex="i\leqslant i\leqslant n"></span>

则，由初始函数经过有限次合成运算和原始递归运算得到的函数，称为原始递归函数。

<br/>

可证，由原始递归函数经过合成运算或原始递归运算，得到的函数仍为原始递归函数。

每一个原始递归函数都是可计算的。

<br/>

常用原始递归函数举例，

常数<span data-katex="k"></span>，<span data-katex="x"></span>，<span data-katex="x+y"></span>，<span data-katex="x\cdot y"></span>，<span data-katex="x!"></span>，<span data-katex="x^y"></span>，前驱函数<span data-katex="p(x)"></span>，<span data-katex="|x-y|"></span>

<br/>

**原始递归谓词**

如果一个谓词看作取值为<span data-katex="0"></span>或<span data-katex="1"></span>的全函数是原始递归的，则称该谓词是原始递归的。

可证，如果<span data-katex="P"></span>和<span data-katex="Q"></span>是原始递归谓词，则<span data-katex="\neg P"></span>，<span data-katex="P\wedge Q"></span>和<span data-katex="P\vee Q"></span>也是原始递归谓词。

<br/>

**极小化运算**

设<span data-katex="P(x_1,\cdots ,x_n,t)"></span>是一个谓词，

<span data-katex="f(x_1,\cdots ,x_n)=min\ P(x_1,\cdots ,x_n,t)"></span>，

<span data-katex="f(x_1,\cdots ,x_n)"></span>的值，或者是使<span data-katex="P(x_1,\cdots ,x_n,t)"></span>为真的<span data-katex="t"></span>的最小值，

或者无定义，如果不存在<span data-katex="t"></span>使得<span data-katex="P(x_1,\cdots ,x_n,t)"></span>为真。

其中，<span data-katex="min"></span>为极小化运算，

也称部分函数<span data-katex="f"></span>，是由谓词<span data-katex="P"></span>经过极小化运算得到的。

<br/>

设<span data-katex="g(x_1,\cdots ,x_n,t)"></span>是一个<span data-katex="n+1"></span>元全函数，

<span data-katex="f(x_1,\cdots ,x_n)=min\ {g(x_1,\cdots ,x_n,t)=0}"></span>，

则称<span data-katex="f"></span>是由函数<span data-katex="g"></span>经过极小化运算得到的。

<br/>

**递归函数类**

由初始函数经过有限次合成运算，原始递归运算和极小化运算，得到的函数称为部分递归函数，

部分递归的全函数称为递归函数。

<br/>

如果一个谓词看作取值为<span data-katex="0"></span>或<span data-katex="1"></span>的全函数是递归的，则称该谓词是递归的。

<br/>

**递归性与可计算性**

可证，部分递归函数是部分可计算函数。

递归函数是可计算函数，递归谓词是可计算谓词。

<br/>

可证，原始递归函数类是可计算函数类的真子集。

<br/>

因为，至少存在一个Ackermann函数<span data-katex="A(k,x)"></span>是可计算的，但不是原始递归的。

<span data-katex="A(0,x)=x+1"></span>

<span data-katex="A(k+1,0)=A(k,1)"></span>

<span data-katex="A(K+1,x+1)=A(k,A(k+1,x))"></span>

<br/>

## **集合和语言的递归性**

**集合识别问题**

所谓集合识别问题，是指对于给定的集合，任给一个元素，问这个元素是否属于该集合，

也称为集合的成员资格问题。

<br/>

**集合特征函数**

设<span data-katex="B\subseteq N"></span>，<span data-katex="B"></span>的特征函数<span data-katex="\chi _B"></span>是一个谓词，定义为，

<span data-katex="\chi _B\equiv x\in B,\ \forall x\in N"></span>，

集合<span data-katex="B"></span>可以用它的特征函数表示为，<span data-katex="B=\{x\in N|\chi _B(x)\}"></span>。

<br/>

如果特征函数<span data-katex="\chi _B"></span>是可计算的，则成集合<span data-katex="B"></span>是递归的。

如果存在部分可计算函数<span data-katex="g"></span>使得<span data-katex="B=\{x\in N|g(x)\downarrow \}"></span>，

则称集合<span data-katex="B"></span>是递归可枚举的。

<br/>

可证，如果集合<span data-katex="B"></span>和<span data-katex="C"></span>都是递归的，则集合<span data-katex="\bar{B}"></span>，<span data-katex="B\cap C"></span>和<span data-katex="B\cup C"></span>都是递归的。

递归集一定是递归可枚举的。

集合<span data-katex="B"></span>是递归的，当且仅当<span data-katex="B"></span>和<span data-katex="\bar{B}"></span>都是递归可枚举的。

如果集合<span data-katex="B"></span>和<span data-katex="C"></span>是递归可枚举的，则集合<span data-katex="B\cap C"></span>和<span data-katex="B\cup C"></span>也是递归可枚举的。

<br/>

**语言识别问题**

设字母表<span data-katex="A=\{s_1,s_2,\cdots ,s_n\}"></span>，<span data-katex="A^*"></span>的任何子集<span data-katex="L"></span>称为<span data-katex="A"></span>上的语言。

<span data-katex="A^*"></span>上集合的识别问题，有称为<span data-katex="A"></span>上的语言识别问题。

<br/>

语言的特征函数定义为，<span data-katex="\chi _L\equiv w\in L,\ \forall w\in A^*"></span>，

如果语言<span data-katex="L"></span>的特征函数<span data-katex="\chi _L"></span>是可计算的，则称语言<span data-katex="L"></span>是递归的。

如果存在<span data-katex="A"></span>上的部分可计算函数<span data-katex="g"></span>使得，<span data-katex="L=\{w\in A^*|g(w)\downarrow \}"></span>，

则称语言<span data-katex="L"></span>是递归可枚举的。

<br/>

**递归可枚举集**

设<span data-katex="B\subseteq N"></span>，且<span data-katex="B"></span>是递归可枚举的，

则存在原始递归谓词<span data-katex="R(x,t)"></span>使得，<span data-katex="B=\{x|\exists t\ R(x,t)\}"></span>

<br/>

设<span data-katex="B"></span>是一个非空递归可枚举集，则存在原始递归函数<span data-katex="f(x)"></span>使得，

<span data-katex="B=\{f(x)|x\in N\}"></span>

<br/>

集合<span data-katex="B"></span>是递归可枚举的，当且仅当存在部分可计算函数<span data-katex="f(x)"></span>，使得，

<span data-katex="B=\{f(x)|f(x)\downarrow \}"></span>

<br/>

总之，如果<span data-katex="B"></span>非空，则以下命题是等价的，

（1）<span data-katex="B"></span>是递归可枚举的，即<span data-katex="B"></span>是一个部分可计算函数的定义域，

（2）<span data-katex="B"></span>是第一个原始递归函数的值域，

（3）<span data-katex="B"></span>是一个可计算函数的值域，

（4）<span data-katex="B"></span>是一个部分可计算函数的值域。

<br/>

## **可判定性与半可判定性**

<span data-katex="\mathscr{S}"></span>程序设计语言，递归函数，Turing机，文法等计算模型，

可以证明它们是等价的，即计算相同的函数类——部分可计算函数。

它们可以识别相同的语言类，递归可枚举语言。

<br/>

如果语言<span data-katex="L"></span>是递归的，那么存在一台总停机的DTM（确定型图灵机）<span data-katex="\mathscr{M}"></span>识别<span data-katex="L"></span>，

任给一个字符串<span data-katex="x"></span>，<span data-katex="\mathscr{M}"></span>总能在有限步内回答<span data-katex="x\in L"></span>还是<span data-katex="x\notin L"></span>，

因而，我们说<span data-katex="L"></span>为可判定的。

<br/>

如果<span data-katex="L"></span>的递归可枚举的，情况就不同了，识别<span data-katex="L"></span>的DTM可能永不停机，

只有当<span data-katex="x\in L"></span>时，<span data-katex="\mathscr{M}"></span>才能保证一定能在有限步内停机并接受<span data-katex="x"></span>，

而当<span data-katex="x\notin L"></span>时，<span data-katex="\mathscr{M}"></span>可能永不停机，

在这种情况下，我们不知道是暂时没有停机，还是永不停机，

这时，我们说<span data-katex="L"></span>是半可判定的。

<br/>

因此，递归语言是可判定的，而递归可枚举语言是半可判定的，

同样的，直观可计算函数是可计算函数，而部分可计算函数是半可计算的，

可计算谓词是可判定的，它定义的集合是递归的，

如果谓词定义的集合是递归可枚举的，则该谓词是半可判定的。

## **参考**

[可计算性与计算复杂性导引](https://book.douban.com/subject/1310925/)