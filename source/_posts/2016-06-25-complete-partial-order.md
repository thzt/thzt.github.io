---
layout: post
categories: Math
title: 不动点算子与完全偏序
description: 
---

<script src="/thirdpart/jQuery/jquery-1.11.1.js"></script>
<link href="/thirdpart/KaTeX/katex.min.css" rel="stylesheet"/>
<script src="/thirdpart/KaTeX/katex.min.js"></script>
<script src="/javascripts/katex.js"></script>

## **域论模型**

类型化lambda演算，有两个常用模型。

其一，域论模型。其二，递归函数论模型。

<br/>

在众多域论模型中，

主要关心的是一种具有完全偏序（complete partial order）结构的域，简称CPO。

研究它的主要原因是，它是带有不动点算子的模型，

而且它还提供了一种解释递归类型表达式的方法。

域论方法是递归函数论模型的基础。

<br/>

## **递归**

加入递归之后，对表达式进行归约就可能会无限的进行下去，

因此将出现没有范式（normal form）的表达式。

于是，把每个表达式指称为一个数值，这种想法就可能有问题了。

下面我们引入不动点算子（fixed-point operator），用来它定义递归。

<br/>

<span data-katex="letrec~f:\sigma=M~in~N"></span>

它表示<span data-katex="N"></span>，但是<span data-katex="N"></span>中f的值，是等式<span data-katex="f=M"></span>的解。

而<span data-katex="M"></span>中可能会包含<span data-katex="f"></span>。

<span data-katex="M"></span>的类型根据等式<span data-katex="f=M"></span>也是<span data-katex="\sigma"></span>。

<br/>

我们将看到，<span data-katex="letrec"></span>是<span data-katex="let"></span>与不动点算子的语法糖（syntactic sugar）。

首先，我们先用<span data-katex="letrec"></span>定义阶乘函数，来计算<span data-katex="5!"></span>。

<span data-katex="letrec~f:nat\rightarrow nat=\lambda y:nat.~(if~Eq?~y~0~then~1~else~y*f(y-1))~in~f~5"></span>

<br/>

其中<span data-katex="f"></span>是下列等式的解。

<span data-katex="f=\lambda y:nat.~if~Eq?~y~0~then~1~else~y*f(y-1)"></span>

<br/>

## **不动点算子**

从数学的角度来看，并不是所有形如<span data-katex="f:\sigma =M"></span>的等式都有解，

如果有多个解，也不知道选择哪个解。

我们先假设每个这样的等式都有解，为此我们增加一个不动点算子来得到这个解。

<br/>

一般的，如果<span data-katex="F:\sigma \rightarrow \sigma"></span>是某一类型到自身的函数。

那么<span data-katex="F"></span>的一个不动点，是使得<span data-katex="x=F(x)"></span>的值<span data-katex="x:\sigma"></span>。

<br/>

经过观察我们发现，阶乘函数<span data-katex="f"></span>是以下函数的<span data-katex="F"></span>的不动点，即满足<span data-katex="f=F(f)"></span>。

<span data-katex="F=_{def}\lambda f:nat\rightarrow nat.~\lambda y:nat.~if~Eq?~y~0~then~1~else~y*f(y-1)"></span>

其中，<span data-katex="f:nat\rightarrow nat"></span>，<span data-katex="F:(nat\rightarrow nat)\rightarrow (nat\rightarrow nat)"></span>

<br/>

我们定义，<span data-katex="fix_\sigma :(\sigma \rightarrow \sigma )\rightarrow \sigma"></span>，是对应于每个类型<span data-katex="\sigma"></span>的不动点算子。

满足的如下等式公理，

<span data-katex="fix_\sigma =\lambda f:\sigma \rightarrow \sigma .~f(fix_\sigma f)"></span>

<br/>

可知，对任意的<span data-katex="F:\sigma \rightarrow \sigma"></span>，<span data-katex="fix_\sigma F"></span>是<span data-katex="F"></span>的不动点，即，

<span data-katex="fix_\sigma F=F(fix_\sigma F)"></span>

<br/>

我们就可以用<span data-katex="let"></span>和<span data-katex="fix_\sigma"></span>表示<span data-katex="letrec"></span>了。

<span data-katex="letrec~f:\sigma =M~in~N=_{def}let~f:\sigma =(fix_\sigma \lambda f:\sigma .~M)~in~N"></span>

<br/>

## **归约**

我们以阶乘函数为例，来说明带有不动点算子的表达式是如何归约的。

为了行文方便，我们省略<span data-katex="fix_{nat\rightarrow nat}"></span>的下标，直接记为<span data-katex="fix"></span>。

定义阶乘函数<span data-katex="fact=_{def}fix~F"></span>，其中，

<span data-katex="F=_{def}\lambda f:nat\rightarrow nat.~\lambda y:nat.~if~Eq?~y~0~then~1~else~y*f(y-1)"></span>

<br/>

我们来计算<span data-katex="fact~n"></span>。

<span data-katex="fact~n=(fix~F)~n"></span>

<span data-katex="=((\lambda f:nat\rightarrow nat.~f(fix~f))~F)~n"></span>

<span data-katex="=(F~(fix~F))~n"></span>

<span data-katex="=((\lambda f:nat\rightarrow nat.~\lambda y:nat.~if~Eq?~y~0~then~1~else~y*f(y-1))~(fix~F))~n"></span>

<span data-katex="=(if~Eq?~n~0~then~1~else~n*(fix~F))~(n-1)"></span>

<br/>

## **无法终止的运算**

由于递归允许我们写出没有范式的表达式，

所以我们相应的必须给这样的表达式赋予含义。

<br/>

例如，

<span data-katex="letrec~f:nat\rightarrow nat=\lambda x:nat.~f(x+1)~in~f~3"></span>

尽管该表达式的类型是<span data-katex="nat"></span>，但是我们无法把它简化为一个数值。

所以该表达式的含义就不是一个自然数了。

<br/>

一方面，认为该表达式的类型是<span data-katex="nat"></span>是合理的，它是类型规则的推导结论。

另一方面，如果我们说该表达式的值是『未定义的』，

那么<span data-katex="f"></span>的语义就变成了部分函数（partial function）。

<br/>

我们不如给自然数集附加一个值<span data-katex="\perp _{nat}"></span>，

用来表示类型<span data-katex="nat"></span>上无法终止的运算（nonterminating computation）。

这给了我们一个把部分函数看成完全函数（total function）的方法。

<br/>

## **CPO**

**偏序**

一个偏序（partial order）<span data-katex="\left \langle D,\leqslant  \right \rangle"></span>是一个集合<span data-katex="D"></span>，以及集合上的一个关系（relation）<span data-katex="\leqslant"></span>，

这个关系具有自反性，反对称性，和传递性。

<br/>

若对于任意<span data-katex="d\in D"></span>有<span data-katex="d\leqslant d"></span>，则称<span data-katex="\leqslant"></span>具有自反性（reflexive）。

若<span data-katex="a\leqslant b"></span>且<span data-katex="b\leqslant a"></span>有<span data-katex="a=b"></span>，则称<span data-katex="\leqslant"></span>具有反对称性（anti-symmetric）。

若<span data-katex="a\leqslant b"></span>且<span data-katex="b\leqslant c"></span>有<span data-katex="a\leqslant c"></span>，则称<span data-katex="\leqslant"></span>具有传递性（transitive）。

<br/>

**上界与最小上界**

如果<span data-katex="\left \langle D,\leqslant  \right \rangle"></span>是一个偏序，则子集<span data-katex="S\subseteq D"></span>的上界（upper bound），

是<span data-katex="D"></span>中的一个元素<span data-katex="x\in D"></span>，使得对于任意的<span data-katex="y\in S"></span>有<span data-katex="y\leqslant x"></span>。

最小上界（least upper bound）是那个<span data-katex="\leqslant"></span>任何其它上界的元素。

<br/>

**有向集**

如果<span data-katex="\left \langle D,\leqslant  \right \rangle"></span>是一个偏序，称子集<span data-katex="S\subseteq D"></span>是有向的（directed），

如果<span data-katex="S"></span>的每一个有限子集<span data-katex="S_0\subseteq S"></span>在<span data-katex="S"></span>中都有上界。

有向集（directed set）的一个性质是，所有有向集都非空。

<br/>

**完全偏序**

完全偏序（complete partial order）简称CPO，它是一个偏序<span data-katex="\left \langle D,\leqslant  \right \rangle"></span>，

且每一个有向子集<span data-katex="S\subseteq D"></span>都有最小上界，我们把这个最小上界记为<span data-katex="\bigvee S"></span>。

可证，任何一个有限的偏序，都是完全偏序。

<br/>

一个不是CPO的例子是自然数集，自然数集<span data-katex="N"></span>本身是有向的，但没有最小上界。

如果我们加入一个比其他自然数都大的元素<span data-katex="\infty"></span>，我们就得到了一个CPO。

<br/>

## **CPO的提升**

**有奇点的CPO**

如果<span data-katex="\mathscr{D}=\left \langle D,\leqslant  \right \rangle"></span>是一个有最小元（least element）的偏序，

则称为<span data-katex="\mathscr{D}"></span>是有奇点（pointed）的，我们用<span data-katex="\perp _D"></span>表示<span data-katex="D"></span>的最小元。

<br/>

**提升集**

对于任意的集合<span data-katex="A"></span>，我们构建一个CPO，<span data-katex="A_\perp =\left \langle A\cup \{\perp \} ,\leqslant  \right \rangle"></span>，

其中，<span data-katex="x\leqslant y"></span>当且仅当<span data-katex="x=\perp"></span>或<span data-katex="x=y"></span>。

我们称<span data-katex="A_\perp"></span>为<span data-katex="A"></span>的提升集（lifted set）。

<br/>

用这个方法，我们可以提升任何一个CPO，<span data-katex="\mathscr{D}=\left \langle D,\leqslant _D  \right \rangle"></span>，

得到<span data-katex="\mathscr{D}_\perp =\left \langle D\cup \{\perp \},\leqslant \right \rangle"></span>，

其中<span data-katex="\perp"></span>与<span data-katex="D"></span>中的任何元素都不等，新的序关系<span data-katex="x\leqslant y"></span>当且仅当<span data-katex="x=\perp"></span>或<span data-katex="x\leqslant _D y"></span>。

<br/>

可证，如果<span data-katex="\mathscr{D}"></span>是一个CPO，则<span data-katex="\mathscr{D}_\perp"></span>是一个有奇点的CPO。

<br/>

## **连续函数**

**单调函数**

设<span data-katex="\mathscr{D}=\left \langle D,\leqslant _D \right \rangle"></span>和<span data-katex="\mathscr{E}=\left \langle E,\leqslant _E \right \rangle"></span>是CPO，

<span data-katex="f:D\rightarrow E"></span>是集合<span data-katex="D"></span>到<span data-katex="E"></span>的一个函数，我们说<span data-katex="f"></span>是单调的（monotonic），

如果<span data-katex="a\leqslant b"></span>就有<span data-katex="f(a)\leqslant f(b)"></span>。

<br/>

**连续函数**

一个单调函数<span data-katex="f"></span>是连续的（continuous），如果对于任意有向子集<span data-katex="S\subseteq D"></span>，有<span data-katex="f(\bigvee S)=\bigvee f(S)"></span>。

<br/>

**提升函数**

我们定义<span data-katex="f:D\rightarrow E"></span>对应的提升函数（lifted function）为<span data-katex="f_\perp =(D\cup\{ \perp \})\rightarrow (E\cup\{ \perp \})"></span>。

其中，如果<span data-katex="a\in D"></span>，则<span data-katex="f_\perp (a)=f(a)"></span>，否则<span data-katex="f_\perp (a)=\perp"></span>。

<br/>

**函数集构成CPO**

假设<span data-katex="\mathscr{D}=\left \langle D,\leqslant _D \right \rangle"></span>和<span data-katex="\mathscr{E}=\left \langle E,\leqslant _E \right \rangle"></span>是CPO，

对于连续函数<span data-katex="f,g:D\rightarrow E"></span>，我们称<span data-katex="f\leqslant_{D\rightarrow E} g"></span>，如果对于任意<span data-katex="d\in D"></span>，都有<span data-katex="f(d)\leqslant _E g(d)"></span>。

于是，所有这些连续函数构成了一个CPO，记为<span data-katex="\mathscr{D}\rightarrow \mathscr{E}=\left \langle D\rightarrow E,\leqslant _{D\rightarrow E} \right \rangle"></span>。

<br/>

## **最小不动点**

我们称<span data-katex="a"></span>是<span data-katex="f"></span>的最小不动点（least fixed point），

如果<span data-katex="a=f(a)"></span>且对于任意的<span data-katex="b=f(b)"></span>，我们有<span data-katex="a\leqslant b"></span>。

<br/>

如果<span data-katex="\mathscr{D}"></span>是一个有奇点的CPO，且<span data-katex="f:D\rightarrow D"></span>是连续的，则<span data-katex="f"></span>有最小不动点，

<span data-katex="fix_D f=\bigvee \{ f^n(\perp )~|~n\geqslant 0 \}"></span>，

且<span data-katex="fix_D"></span>是连续的。

<br/>

例如，设<span data-katex="\mathscr{D}=\left \langle D,\leqslant _D \right \rangle"></span>是有奇点的CPO，则恒等函数<span data-katex="id:D\rightarrow D"></span>的最小不动点是<span data-katex="\perp _D"></span>。

<span data-katex="fix_D~id=\bigvee \{ id^n(\perp _D)~|~n\geqslant 0 \}=\bigvee \{ \perp _D \}=\perp _D"></span>

## **结语**

初等数学中，某些函数是没有不动点的。

那么在什么情况下，形如<span data-katex="f:\sigma =M"></span>的表达式有解呢？

定义了递归之后，对类型化lambda演算的模型产生了什么影响呢？

<br/>

这是一直以来我心中是一个问题。

诚然，类型化lambda演算有不同的解释方式，但以上域论模型通俗易懂，

也算是告一段落吧，以后的路还长着呢。
