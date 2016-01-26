---
layout: post
categories: Logic
title: 数理逻辑定义汇总
description: 数理逻辑是证明论和模型论的基础，学好它有助于从数学和逻辑学角度认识程序理论。
---

<script src="/thirdpart/jQuery/jquery-1.11.1.js"></script>
<link href="/thirdpart/KaTeX/katex.min.css" rel="stylesheet"/>
<script src="/thirdpart/KaTeX/katex.min.js"></script>
<script src="/javascripts/katex.js"></script>

**逻辑学**真是博大精深，

first-order logic，propositional logic，predicate logic，

mathematical logic，second-order Logic，intuitionistic logic，

modal logic，free logic，plural logic...

<br/>

所涉及的内容也很广，

set theory，proof theory，model theory，recursion theory，

theory of computation，computability and decidability...

<br/>

学习它，对**数学**，**计算机科学**或其他学科都有指导意义。

<br/>

例如，哥德尔协调性定理指出了公理化方法的局限性，

它告诉我们，**在理论上就不能通过逻辑推理解决所有的问题**，

必要时，要通过构造模型来进行检验。

对软件进行测试，就是这样的一个例子。

<br/>

例如，**类型系统**，相当于加在程序语言语法层面上的(谓词)逻辑，

类型系统的**可靠性**保证了语法正确的程序，

语义上也是满足规范的。

<br/>

这样的例子还有很多，

实际工作中，只有**见多识广**，站在更高的角度，

才能做到庖丁解牛，**游刃有余**。

<br/>

到此，我们还是从一阶谓词逻辑开始，慢慢打好基础吧。

以下摘自《[数理逻辑](http://book.douban.com/subject/2364517/)》——李未

<br/>

## **一阶语言的定义**

每个一阶语言的字符集由两类符号集合组成。

一类称为逻辑符号集合，另一类称为非逻辑符号集合。

<br/>

**逻辑符号集合**包括：

<span data-katex="V"></span>：变元符号集合，<span data-katex="x_1, x_2, ..., x_n, ..."></span>

<span data-katex="C"></span>：逻辑连接词符号集合，<span data-katex="\neg , \wedge , \vee , \rightarrow , \leftrightarrow"></span>

<span data-katex="Q"></span>：量词符号集合，<span data-katex="\forall , \exists"></span>

<span data-katex="E"></span>：等词符号集合，<span data-katex="\doteq"></span>

<span data-katex="P"></span>：括号集合，<span data-katex="(, )"></span>

<br/>

**非逻辑符号集合**包括：

<span data-katex="\mathscr{L}_c"></span>：常元符号集合，<span data-katex="c_1, c_2, ..."></span>

<span data-katex="\mathscr{L}_f"></span>：函数符号集合，<span data-katex="f_1, f_2, ..."></span>

<span data-katex="\mathscr{L}_P"></span>：谓词符号集合，<span data-katex="P_1, P_2, ..."></span>

<br/>

**例子：**初等算术语言<span data-katex="\mathscr{A}"></span>

初等算术语言是一个一阶语言，

它的常元符号集为<span data-katex="\{0\}"></span>，

函数符号集为<span data-katex="\{S, +, \cdot \}"></span>，

谓词符号集合为<span data-katex="\{<\}"></span>。

<br/>

## **项**

一阶语言<span data-katex="\mathscr{L}"></span>中的项被下述三个规则归纳的定义：

<span data-katex="T_1"></span>：每一个常元是一个项

<span data-katex="T_2"></span>：每一个变元是一个项

<span data-katex="T_3"></span>：如果<span data-katex="t_1, ..., t_n"></span>是项，而f是一个n元函数符号，那么，<span data-katex="ft_1\cdot \cdot \cdot t_n"></span>是一个项

<br/>

此定义也可以表述成下述形式：

<span data-katex="t ::= c | x | ft_1\cdot \cdot \cdot t_n"></span>

<br/>

**例子：**<span data-katex="\mathscr{A}"></span>的项

<span data-katex="S0, Sx_1, +S0SSx, \cdot x_1+Sx_1x_2"></span>

<br/>

## **逻辑公式**

语言<span data-katex="\mathscr{L}"></span>中的逻辑公式，简称**公式**，用大写字母<span data-katex="A, B, ..."></span>表示，并用下述五条规则归纳的定义：

<br/>

<span data-katex="F_1"></span>：如果<span data-katex="t_1"></span>和<span data-katex="t_2"></span>为项，那么<span data-katex="t_1\doteq t_2"></span>是公式

<span data-katex="F_2"></span>：如果<span data-katex="t_1, ..., t_n"></span>为项，而<span data-katex="R"></span>是一个n元谓词，那么<span data-katex="Rt_1\cdot \cdot \cdot t_n"></span>是公式

<span data-katex="F_3"></span>：如果<span data-katex="A"></span>是公式，则<span data-katex="\neg A"></span>是公式

<span data-katex="F_4"></span>：若<span data-katex="A, B"></span>是公式，则<span data-katex="A\wedge B, A\vee B, A\rightarrow B, A\leftrightarrow B"></span>都是公式

<span data-katex="F_5"></span>：若<span data-katex="A"></span>是公式并且<span data-katex="x"></span>是一个变元，那么<span data-katex="\forall xA"></span>和<span data-katex="\exists xA"></span>也是公式，<span data-katex="x"></span>称为**约束变元**

<br/>

上述结构归纳定义的Backus范式为：

<span data-katex="A ::= t_1\doteq t_2 | Rt_1\cdot \cdot \cdot t_n | \neg A | A\wedge B | A\vee B | A\rightarrow B | A\leftrightarrow B | \forall xA | \exists xA"></span>

<br/>

**例子：**<span data-katex="\mathscr{A}"></span>的公式

<span data-katex="\forall x\neg (Sx\doteq 0), \forall x\forall y(< xy\rightarrow(\exists (y\doteq +xz)))"></span>

<br/>

## **<span data-katex="\mathscr{L}"></span>的结构**

一阶语言<span data-katex="\mathscr{L}"></span>的**结构**<span data-katex="M"></span>是一个偶对，记为<span data-katex="M=(\mathbb{M}, I)"></span>，其中，

（1）<span data-katex="\mathbb{M}"></span>是一个非空集合，称为**论域**

（2）<span data-katex="I"></span>是从<span data-katex="\mathscr{L}"></span>到<span data-katex="\mathbb{M}"></span>的映射，称为**解释**，记为<span data-katex="I:\mathscr{L} \rightarrow \mathbb{M}"></span>，它满足下面三个条件

a) 对<span data-katex="\mathscr{L}"></span>中的每一个常元符号<span data-katex="c"></span>，<span data-katex="I(c)"></span>是<span data-katex="\mathbb{M}"></span>中的元素

b) 对<span data-katex="\mathscr{L}"></span>中的每一个n元函数符号<span data-katex="f"></span>，<span data-katex="I(f)"></span>是<span data-katex="\mathbb{M}"></span>上的n元函数

c) 对<span data-katex="\mathscr{L}"></span>中的每一个n元谓词符号<span data-katex="P"></span>，<span data-katex="I(P)"></span>是<span data-katex="\mathbb{M}"></span>上的一个n元关系

<br/>

**例子：**<span data-katex="\mathscr{A}"></span>的结构

<span data-katex="\mathscr{A}"></span>的常元符号为<span data-katex="0"></span>，

函数符号有<span data-katex="\{S, +, \cdot \}"></span>，

谓词符号只有一个，它是<span data-katex="<"></span>。

<br/>

我们定义偶对<span data-katex="N=(\mathbb{N}, I)"></span>，其中论域<span data-katex="\mathbb{N}"></span>为自然数系。

令<span data-katex="s"></span>为<span data-katex="\mathbb{N}"></span>上的加1函数，即<span data-katex="s(x)=x+1"></span>，

<span data-katex="+, \cdot"></span>代表<span data-katex="\mathbb{N}"></span>上的加法和乘法，

<span data-katex="<"></span>为<span data-katex="\mathbb{N}"></span>上的小于关系。

<br/>

我们定义解释映射<span data-katex="I"></span>如下：

<span data-katex="I(0)=0, I(S)=s, I(+)=+, I(\cdot )=\cdot , I(<)=<"></span>

<br/>

解释映射<span data-katex="I"></span>将常元符号<span data-katex="0"></span>解释为自然数<span data-katex="0"></span>，

将一元函数符号<span data-katex="S"></span>解释为自然数集合上的加1运算<span data-katex="s"></span>，

将二元函数符号<span data-katex="+"></span>和<span data-katex="\cdot"></span>分别解释为自然数集合上的加法和乘法，

将二元谓词符号<span data-katex="<"></span>解释为自然数集合上的小于关系，

而<span data-katex="N"></span>是初等算术语言<span data-katex="\mathscr{A}"></span>的一个结构。

<br/>

## **赋值**

赋值<span data-katex="\sigma"></span>是一个定义域为变元集合<span data-katex="V"></span>，值域为<span data-katex="\mathbb{M}"></span>的一个映射，记为<span data-katex="\sigma :V\rightarrow \mathbb{M}"></span>。

赋值<span data-katex="\sigma"></span>把<span data-katex="\mathscr{L}"></span>中的每一个变元<span data-katex="x"></span>，赋以论域<span data-katex="\mathbb{M}"></span>中的一个元素<span data-katex="a\in \mathbb{M}"></span>，

记为<span data-katex="\sigma (x)=a"></span>。

<br/>

## **模型**

给定一阶语言<span data-katex="\mathscr{L}"></span>，以及它的结构<span data-katex="M"></span>和赋值<span data-katex="\sigma"></span>，

偶对<span data-katex="(M, \sigma )"></span>称为<span data-katex="\mathscr{L}"></span>的一个**模型**。

<br/>

## **项的语义**

给定一阶语言<span data-katex="\mathscr{L}"></span>，结构<span data-katex="M=(\mathbb{M}, I)"></span>和赋值<span data-katex="\sigma :V\rightarrow \mathbb{M}"></span>。

在模型<span data-katex="(M, \sigma )"></span>下，项<span data-katex="t"></span>的语义是<span data-katex="\mathbb{M}"></span>中的一个元素，它用<span data-katex="t_{M[\sigma]}"></span>表示，并被归纳的定义：

（1）<span data-katex="x_{M[\sigma ]}=\sigma (x)"></span>，<span data-katex="x"></span>为变元符号

（2）<span data-katex="c_{M[\sigma ]}=c_M"></span>，<span data-katex="c"></span>为常元符号

（3）<span data-katex="(ft_1\cdot \cdot \cdot t_n)_{M[\sigma ]}=f_M((t_1)_{M[\sigma ]},\cdot \cdot \cdot (t_n)_{M[\sigma ]})"></span>

<br/>

**例子：**<span data-katex="\mathscr{A}"></span>项的语义

<span data-katex="(+x_1Sx_7)_{N[\sigma ]}=(x_1)_{N[\sigma ]}+(Sx_7)_{N[\sigma ]}=1+((x_7)_{N[\sigma ]}+1)=1+(7+1)=9"></span>

<br/>

## **逻辑连接词符号的语义**

为了避免逻辑连接词符号的多义性，我们把每一个逻辑连接词符号的语义都定义为一个真值函数，

此函数的定义域是一个真值集合或两个真值集合的笛卡尔积，而函数值是一个真假值。

对于一阶语言而言，逻辑连接词符号<span data-katex="\neg"></span>的真值函数为<span data-katex="B_\neg"></span>，

其自变量是<span data-katex="X"></span>，<span data-katex="X"></span>只能取<span data-katex="T"></span>和<span data-katex="F"></span>，

而函数值<span data-katex="B_\neg (X)"></span>由下述真值表定义：

<span data-katex="B_\neg (T)=F, B_\neg (F)=T"></span>

<br/>

二元函数<span data-katex="B_\wedge, B_\vee, B_\rightarrow, B_\leftrightarrow"></span>分别为逻辑连接词符号<span data-katex="\wedge, \vee, \rightarrow, \leftrightarrow"></span>的真值函数。

<br/>

## **公式的语义**

设<span data-katex="M"></span>和<span data-katex="\sigma"></span>分别为一阶语言<span data-katex="\mathscr{L}"></span>的结构和赋值，而<span data-katex="A"></span>为<span data-katex="\mathscr{L}"></span>的公式。

公式<span data-katex="A"></span>在模型<span data-katex="(M, \sigma )"></span>下的语义是一个真假值，用<span data-katex="A_{M[\sigma ]}"></span>表示，被归纳的定义如下：

（1）<span data-katex="(Pt_1\cdot \cdot \cdot t_n)_{M[\sigma ]}=P_M((t_1)_{M[\sigma ]},\cdot \cdot \cdot ,(t_n)_{M[\sigma ]})"></span>

（2）<span data-katex="(t_1\doteq t_2)_{M[\sigma ]}=\begin{cases}T,&\text{if }(t_1)_{M[\sigma ]}=(t_2)_{M[\sigma ]}\\F,&\text{otherwise}\end{cases}"></span> 

（3）<span data-katex="(\neg A)_{M[\sigma ]}=B_\neg (A_{M[\sigma ]})"></span>

（4）<span data-katex="(A\vee B)_{M[\sigma ]}=B_\vee (A_{M[\sigma ]}, B_{M[\sigma ]})"></span>

（5）<span data-katex="(A\wedge B)_{M[\sigma ]}=B_\wedge (A_{M[\sigma ]}, B_{M[\sigma ]})"></span>

（6）<span data-katex="(A\rightarrow B)_{M[\sigma ]}=B_\rightarrow (A_{M[\sigma ]}, B_{M[\sigma ]})"></span>

（7）<span data-katex="(A\leftrightarrow B)_{M[\sigma ]}=B_\leftrightarrow (A_{M[\sigma ]}, B_{M[\sigma ]})"></span>

（8）<span data-katex="(\forall x_iA)_{M[\sigma ]}=\begin{cases}T,&\forall a\in M, A_{M[\sigma [x_i:=a]]}=T\\F,&\text{otherwise}\end{cases}"></span>

（9）<span data-katex="(\exists x_iA)_{M[\sigma ]}=\begin{cases}T,&\exists a\in M, A_{M[\sigma [x_i:=a]]}=T\\F,&\text{otherwise}\end{cases}"></span>

<br/>

## **可满足性**

给定一阶语言<span data-katex="\mathscr{L}"></span>和它的公式<span data-katex="A"></span>以及公式集合<span data-katex="\Gamma"></span>。

如果存在模型<span data-katex="(M, \sigma )"></span>，使得<span data-katex="A_{M[\sigma ]}=T"></span>成立，

那么称公式<span data-katex="A"></span>关于模型<span data-katex="(M, \sigma )"></span>是**可满足的**，

简称<span data-katex="A"></span>可满足，也称为模型<span data-katex="(M, \sigma )"></span>满足<span data-katex="A"></span>，记为<span data-katex="M\models _\sigma A"></span>。

如果<span data-katex="A"></span>是一个语句，那么记为<span data-katex="A"></span>，记为<span data-katex="M\models A"></span>

<br/>

如果<span data-katex="\Gamma"></span>中的每一个公式关于模型<span data-katex="(M, \sigma )"></span>都是可满足的，即，

<span data-katex="M\models _\sigma A"></span>对于任意<span data-katex="A\in \Gamma"></span>成立，

那么称为公式集合<span data-katex="\Gamma"></span>关于模型<span data-katex="(M, \sigma )"></span>可满足，

简称公式集合<span data-katex="\Gamma"></span>可满足，

也称模型<span data-katex="(M, \sigma )"></span>满足公式集合<span data-katex="\Gamma"></span>，或<span data-katex="(M, \sigma )"></span>是<span data-katex="\Gamma"></span>的模型，记为<span data-katex="M\models _\sigma \Gamma"></span>。

如果<span data-katex="\Gamma"></span>是由语句组成的集合，那么记为<span data-katex="M\models \Gamma"></span>。

<br/>

## **永真性**

称公式<span data-katex="A"></span>是**永真的**或有效的，如果<span data-katex="A"></span>对<span data-katex="\mathscr{L}"></span>的任意模型<span data-katex="(M, \sigma )"></span>均可满足，

即，对任意结构<span data-katex="M"></span>和赋值<span data-katex="\sigma"></span>，<span data-katex="M\models _\sigma A"></span>成立，记为<span data-katex="\models A"></span>。

称公式集合<span data-katex="\Gamma"></span>是永真的或有效的，如果<span data-katex="\Gamma"></span>中的每一个公式<span data-katex="A"></span>都是永真的，记为<span data-katex="\models \Gamma"></span>

永真公式，也称为重言式，是与模型无关的公式，它们在任何模型下都为真。

<br/>

**例子：**重言式

<span data-katex="A\vee \neg A, \forall x(x\doteq x)"></span>

<br/>

## **逻辑结论**

设<span data-katex="A"></span>为公式，<span data-katex="\Gamma"></span>为公式集合，如果<span data-katex="M"></span>为任意结构，<span data-katex="\sigma"></span>为任意赋值，并且，

如果<span data-katex="M\models _\sigma \Gamma"></span>成立，则有<span data-katex="M\models _\sigma A"></span>成立，

那么称<span data-katex="A"></span>是<span data-katex="\Gamma"></span>的**逻辑结论**或语义结论，记为<span data-katex="\Gamma \models A"></span>，也称<span data-katex="\Gamma \models A"></span>有效。

<br/>

**注：**符号<span data-katex="\models"></span>可以出现在4种不同类型的语义关系式中，它们是，

<span data-katex="M\models _\sigma A, M\models A, \models A, \Gamma \models A"></span>

<span data-katex="\models"></span>在每种语义关系式中的含义不同，

区别这些关系式的简单办法是，

当<span data-katex="M"></span>和<span data-katex="\sigma"></span>同时出现时，表示此式仅对给定的<span data-katex="M"></span>和<span data-katex="\sigma"></span>成立，

当<span data-katex="\sigma"></span>不出现时，表示此式对任意<span data-katex="\sigma"></span>成立，

当<span data-katex="M"></span>及<span data-katex="\sigma"></span>均不出现时，表示此式对任意<span data-katex="M"></span>和任意<span data-katex="\sigma"></span>成立。

<br/>

<span data-katex="\Gamma \models A"></span>也是一个语义关系式，它表示对任意<span data-katex="M"></span>和任意<span data-katex="\sigma"></span>，

如果<span data-katex="\Gamma"></span>为真，那么<span data-katex="A"></span>也为真。

<br/>

## **序贯**

设<span data-katex="\Gamma ,\Delta"></span>为公式的有穷集合，<span data-katex="\Gamma \vdash \Delta"></span>称为**序贯**。

<span data-katex="\Gamma"></span>称为序贯的前提，<span data-katex="\Delta"></span>称为序贯的结论。

<br/>

## **公理**

设<span data-katex="\Gamma ,\Delta ,\Lambda ,\Theta"></span>为有穷公式集合，<span data-katex="A"></span>为公式，

则序贯<span data-katex="\Gamma ,A,\Delta \vdash \Lambda ,A,\Theta"></span>称为**公理**。

<br/>

**注：**公理序贯之所以成立，是因为证明结论中至少有一个公式包含在公理序贯的前提之中。

<br/>

## **G推理系统**

（1）<span data-katex="\neg"></span>规则

<span data-katex="\neg -L:\frac{\Gamma ,\Delta \vdash A,\Lambda }{\Gamma ,\neg A,\Delta \vdash \Lambda}"></span>

<span data-katex="\neg -R:\frac{A,\Gamma \vdash \Lambda ,\Delta }{\Gamma \vdash \Lambda ,\neg A,\Delta }"></span>

（2）<span data-katex="\vee"></span>规则

<span data-katex="\vee -L:\frac{\Gamma ,A,\Delta \vdash \Lambda \quad \Gamma ,B,\Delta \vdash \Lambda }{\Gamma ,A\vee B,\Delta \vdash \Lambda }"></span>

<span data-katex="\vee -R:\frac{\Gamma \vdash \Lambda ,A,B,\Theta }{\Gamma \vdash \Lambda ,A\vee B,\Theta }"></span>

（3）<span data-katex="\wedge"></span>规则

<span data-katex="\wedge -L:\frac{\Gamma ,A,B,\Delta \vdash \Lambda }{\Gamma ,A\wedge B,\Delta \vdash \Lambda }"></span>

<span data-katex="\wedge -R:\frac{\Gamma \vdash \Lambda ,A,\Theta \quad \Gamma \vdash \Lambda ,B,\Theta }{\Gamma \vdash \Lambda ,A\wedge B,\Theta }"></span>

（4）<span data-katex="\rightarrow"></span>规则

<span data-katex="\rightarrow -L:\frac{\Gamma ,\Delta \vdash A,\Lambda \quad B,\Gamma ,\Delta \vdash \Lambda }{\Gamma ,A\rightarrow B,\Delta \vdash \Lambda }"></span>

<span data-katex="\rightarrow -R:\frac{A,\Gamma \vdash B,\Lambda ,\Theta }{\Gamma \vdash \Lambda ,A\rightarrow B,\Theta }"></span>

（5）<span data-katex="\forall"></span>规则

<span data-katex="\forall -L:\frac{\Gamma ,A[t/x],\forall xA(x),\Delta \vdash \Lambda }{\Gamma ,\forall xA(x),\Delta \vdash \Lambda }"></span>

<span data-katex="\forall -R:\frac{\Gamma \vdash \Lambda ,A[y/x],\Theta }{\Gamma \vdash \Lambda ,\forall xA(x),\Theta }"></span>

（6）<span data-katex="\exists"></span>规则

<span data-katex="\exists -L:\frac{\Gamma ,A[y/x],\Delta \vdash \Lambda }{\Gamma ,\exists xA(x),\Delta \vdash \Lambda }"></span>

<span data-katex="\exists -L:\frac{\Gamma ,A[y/x],\Delta \vdash \Lambda }{\Gamma ,\exists xA(x),\Delta \vdash \Lambda }"></span>

<br/>

## **可靠性，紧致性，协调性，完全性**

**可靠性**

如果序贯<span data-katex="\Gamma \vdash \Lambda"></span>可证，那么<span data-katex="\Gamma \models \Lambda"></span>成立。

<br/>

**紧致性**

如果<span data-katex="\Gamma"></span>是一个公式集合，<span data-katex="A"></span>是一个公式，并且序贯<span data-katex="\Gamma \vdash A"></span>可证，

那么必然存在有穷公式集合<span data-katex="\Delta"></span>，使得<span data-katex="\Delta \subseteq \Gamma"></span>并且<span data-katex="\Delta \vdash A"></span>可证。

<br/>

**协调性**

设<span data-katex="\Gamma"></span>为公式集合，如果不存在一个公式<span data-katex="A"></span>使得序贯<span data-katex="\Gamma \vdash A"></span>与<span data-katex="\Gamma \vdash \neg A"></span>均可证，

那么称<span data-katex="\Gamma"></span>是协调的。

<br/>

**完全性**

令<span data-katex="\Gamma"></span>为一个公式集合，<span data-katex="A"></span>为一个公式，

如果<span data-katex="\Gamma \models A"></span>成立，那么<span data-katex="\Gamma \vdash A"></span>可证。

<br/>

**定理：**令<span data-katex="\Gamma"></span>为一个公式集合，<span data-katex="A"></span>为一个公式，

（1）<span data-katex="\Gamma \models A"></span>有效，当且仅当<span data-katex="\Gamma \vdash A"></span>

（2）<span data-katex="\Gamma"></span>可满足，当且仅当<span data-katex="\Gamma"></span>协调

<br/>

## **形式理论**

设<span data-katex="\Gamma"></span>是一阶语言<span data-katex="\mathscr{L}"></span>的有穷或可数无穷的语句集合，

如果<span data-katex="\Gamma"></span>协调，则称<span data-katex="\Gamma"></span>是一阶语言的形式理论，简称**形式理论**。

而称<span data-katex="\Gamma"></span>中的语句为<span data-katex="\Gamma"></span>的**公理**。

<br/>

如果<span data-katex="\Gamma"></span>是一个形式理论，

那么称语句集合，<span data-katex="Th(\Gamma )=\{A|A"></span>是<span data-katex="\mathscr{L}"></span>的语句，并且<span data-katex="\Gamma \vdash A"></span>可证<span data-katex="\}"></span>，

为<span data-katex="\Gamma"></span>的**理论闭包**。

<br/>

如果<span data-katex="\Gamma =\emptyset"></span>，那么，<span data-katex="Th(\emptyset )=\{A|A"></span>是<span data-katex="\mathscr{L}"></span>的语句，并且<span data-katex="\vdash A"></span>可证<span data-katex="\}"></span>，

是由全体重言式组成的集合。

<br/>

如果<span data-katex="M"></span>是<span data-katex="\mathscr{L}"></span>的模型，并且<span data-katex="M\models \Gamma"></span>，那么称<span data-katex="M"></span>是<span data-katex="\Gamma"></span>的模型。

<br/>

## **关于模型的形式理论**

如果<span data-katex="M"></span>是一阶语言<span data-katex="\mathscr{L}"></span>的模型，那么称语句集合，

<span data-katex="Th(M)=\{A|A"></span>是<span data-katex="\mathscr{L}"></span>的语句，并且<span data-katex="M\models A\}"></span>

为<span data-katex="\mathscr{L}"></span>关于模型<span data-katex="M"></span>的形式理论。

<br/>

## **形式理论的完全性**

称形式理论<span data-katex="\Gamma"></span>是完全的，如果对任意语句<span data-katex="A"></span>，

<span data-katex="\Gamma \vdash A"></span>及<span data-katex="\Gamma \vdash \neg A"></span>中必有一个可证。

<br/>

## **函数的可表示性**

设<span data-katex="f:\mathbb{N}^k\rightarrow \mathbb{N}"></span>是<span data-katex="\mathbb{N}"></span>上的<span data-katex="k"></span>元函数，

如果存在<span data-katex="\mathscr{A}"></span>公式<span data-katex="A(x_1,...,x_{k+1})"></span>，使得对任意自然数<span data-katex="n_1,...,n_{k+1}"></span>，

如果<span data-katex="f(n_1,...,n_k)=n_{k+1}"></span>，那么<span data-katex="\Pi \vdash A[S^{n_1}0,...,S^{n_{k+1}}0]"></span>可证

如果<span data-katex="f(n_1,...,n_k)\neq n_{k+1}"></span>，那么<span data-katex="\Pi \vdash \neg A[S^{n_1}0,...,S^{n_{k+1}}0]"></span>可证

在这种情况下，称函数<span data-katex="f"></span>在<span data-katex="\Pi"></span>中**可表示**，

并称公式<span data-katex="A(x_1,...,x_k,x_{k+1})"></span>是函数<span data-katex="f"></span>在<span data-katex="\Pi"></span>中的**表示**。

<br/>

**定理：**如果<span data-katex="f:\mathbb{N}^k\rightarrow \mathbb{N}"></span>是<span data-katex="\mathbb{N}"></span>上的<span data-katex="k"></span>元可计算函数，

那么函数<span data-katex="f"></span>在<span data-katex="\Pi"></span>中可表示。

<br/>

## **关系的可表示性**

设<span data-katex="r"></span>是<span data-katex="\mathbb{N}"></span>上的<span data-katex="k"></span>元关系，

如果存在<span data-katex="\mathscr{A}"></span>公式<span data-katex="A(x_1,...,x_{k+1})"></span>，使得对任意自然数<span data-katex="n_1,...,n_{k+1}"></span>，有

如果<span data-katex="r(n_1,...,n_k)=n_{k+1}"></span>，那么<span data-katex="\Pi \vdash A[S^{n_1}0,...,S^{n_{k+1}}0]"></span>可证

如果<span data-katex="r(n_1,...,n_k)\neq n_{k+1}"></span>，那么<span data-katex="\Pi \vdash \neg A[S^{n_1}0,...,S^{n_{k+1}}0]"></span>可证

在这种情况下，称关系<span data-katex="r"></span>在<span data-katex="\Pi"></span>中**可表示**，

并称公式<span data-katex="A(x_1,...,x_k,x_{k+1})"></span>在<span data-katex="\Pi"></span>中表示关系<span data-katex="r"></span>。

<br/>

**定理：**如果<span data-katex="r:\mathbb{N}^k\rightarrow \mathbb{N}"></span>是<span data-katex="\mathbb{N}"></span>上的<span data-katex="k"></span>元可判定关系，

那么<span data-katex="r"></span>在<span data-katex="\Pi"></span>中可表示。

<br/>

## **哥德尔定理**

**哥德尔不完全性定理**

如果<span data-katex="\Gamma"></span>是一个有穷并包含初等算术<span data-katex="\Pi"></span>的形式理论，

那么<span data-katex="\Gamma"></span>是一个不完全的形式理论。

<br/>

**哥德尔协调性定理**

如果形式理论<span data-katex="\Gamma"></span>包含初等算术<span data-katex="\Pi"></span>，

那么<span data-katex="\Pi"></span>的协调性不能在<span data-katex="\Gamma"></span>中被证明。

<br/>

## **结语**

以上，只是对谓词逻辑中用到的部分公式，进行了整理，

对建立**用证明论和模型论的观点来理解公理系统**，是很有帮助的。

然而，从更高的角度来看，有些观点很有可能就是**错误**的，

因此，此篇只是一个开始，督促我朝着更广阔的方向努力学习。

<br/>

**参考**

[数理逻辑](http://book.douban.com/subject/2364517/)

[Teach Yourself Logic 2015](http://www.logicmatters.net/tyl/)

[logic and structure](http://book.douban.com/subject/2878521/)
