---
layout: post
categories: Math
title: 懵逼的上同调群
description: 
---

<script src="/thirdpart/jQuery/jquery-1.11.1.js"></script>
<link href="/thirdpart/KaTeX/katex.min.css" rel="stylesheet"/>
<script src="/thirdpart/KaTeX/katex.min.js"></script>
<script src="/javascripts/katex.js"></script>

## **代数结构**

**代数运算**

设<span data-katex="A"></span>为集合，函数<span data-katex="f:A\times A\rightarrow A"></span>称为<span data-katex="A"></span>上的一个二元代数运算，

简称二元运算。

<br/>

设<span data-katex="A"></span>为集合，<span data-katex="n"></span>为正整数，<span data-katex="A^n=A\times A\times \cdots \times A"></span>表示<span data-katex="A"></span>的<span data-katex="n"></span>阶笛卡尔积，

函数<span data-katex="f:A^n\rightarrow A"></span>称为<span data-katex="A"></span>上的一个<span data-katex="n"></span>元代数运算，简称<span data-katex="n"></span>元运算。

如果<span data-katex="f"></span>是<span data-katex="A"></span>上的运算，也可以称<span data-katex="A"></span>在运算<span data-katex="f"></span>下是封闭的。

<br/>

**单位元和零元**

设<span data-katex="\circ"></span>为集合<span data-katex="A"></span>上的二元运算，

若存在<span data-katex="e_l\in A"></span>（或<span data-katex="e_r\in A"></span>）使得<span data-katex="\forall x\in A"></span>都有<span data-katex="e_l\circ x=x"></span>（或<span data-katex="x\circ e_r=x"></span>），

则称<span data-katex="e_l"></span>（或<span data-katex="e_r"></span>）是<span data-katex="A"></span>中关于<span data-katex="\circ"></span>运算的左（或右）单位元。

若<span data-katex="e\in A"></span>关于<span data-katex="\circ"></span>运算既为左单位元又为右单位元，则称<span data-katex="e"></span>为<span data-katex="A"></span>中关于<span data-katex="\circ"></span>运算的单位元。

<br/>

若存在<span data-katex="\theta_l\in A"></span>（或<span data-katex="\theta_r\in A"></span>）使得<span data-katex="\forall x\in A"></span>都有<span data-katex="\theta_l\circ x=\theta_l"></span>（或<span data-katex="x\circ \theta_r=\theta_r"></span>），

则称<span data-katex="\theta_l"></span>（或<span data-katex="\theta_r"></span>）是<span data-katex="A"></span>中关于<span data-katex="\circ"></span>运算的左（或右）零元。

若<span data-katex="\theta\in A"></span>关于<span data-katex="\circ"></span>运算既为左零元又为右零元，则称<span data-katex="e"></span>为<span data-katex="A"></span>中关于<span data-katex="\circ"></span>运算的零元。

<br/>

可证，

如果集合<span data-katex="A"></span>中的任意元素，关于<span data-katex="\circ"></span>运算既有左单位元，又有右单位元，则左单位元等于右单位元，且是<span data-katex="A"></span>中唯一的单位元。

如果集合<span data-katex="A"></span>中的任意元素，关于<span data-katex="\circ"></span>运算既有左零元，又有右零元，则左零元等于右零元，且是<span data-katex="A"></span>中唯一的零元。

<br/>

**逆元**

设<span data-katex="\circ"></span>为集合<span data-katex="A"></span>上的二元运算，<span data-katex="e\in A"></span>是关于<span data-katex="\circ"></span>运算的单位元。

对于<span data-katex="x\in A"></span>，若存在<span data-katex="y_l\in A"></span>（或<span data-katex="y_r\in A"></span>）使得<span data-katex="y_l\circ x=e"></span>（或<span data-katex="x\circ y_r=e"></span>），

则称<span data-katex="y_l"></span>（或<span data-katex="y_r"></span>）是<span data-katex="x"></span>关于<span data-katex="\circ"></span>运算的左（或右）逆元。

若<span data-katex="y\in A"></span>既是<span data-katex="x"></span>关于<span data-katex="\circ"></span>运算的左逆元，又是<span data-katex="x"></span>关于<span data-katex="\circ"></span>运算的右逆元，

则称<span data-katex="y"></span>是<span data-katex="x"></span>关于<span data-katex="\circ"></span>运算的逆元。

<br/>

可证，如果集合<span data-katex="A"></span>中的任意元素，关于<span data-katex="\circ"></span>运算既有左逆元，又有右逆元，则左逆元等于右逆元，且是该元素唯一的逆元。

<br/>

**代数系统**

一个代数系统是一个三元组<span data-katex="V=\left \langle A,\Omega ,K \right \rangle"></span>，

其中<span data-katex="A"></span>是一个非空的对象集合，称为<span data-katex="V"></span>的载体，

<span data-katex="\Omega"></span>是一个非空的运算集合，即<span data-katex="\Omega=\;{\tiny\begin{matrix}\infty\\ \normalsize \cup \\ ^{\scriptsize j=1}\end{matrix}}\;\Omega_j"></span>，<span data-katex="\Omega_j"></span>是<span data-katex="A"></span>上所有<span data-katex="j"></span>元运算的集合，

<span data-katex="K\subseteq A"></span>是代数常数的集合。

<br/>

对于任何代数常数<span data-katex="k\in K"></span>，可以把<span data-katex="k"></span>看成<span data-katex="A"></span>上的零元运算，

这时可将代数系统<span data-katex="V"></span>写作<span data-katex="\left \langle A,\Omega \right \rangle"></span>，

这时<span data-katex="\Omega=\;{\tiny\begin{matrix}\infty\\ \normalsize \cup \\ ^{\scriptsize j=0}\end{matrix}}\;\Omega_j"></span>，<span data-katex="\Omega_0=K"></span>。

<br/>

**同类型的代数系统**

设<span data-katex="V_1=\left \langle A,o_1,o_2,\cdots,o_r \right \rangle"></span>，<span data-katex="V_2=\left \langle B,\overline{o_1},\overline{o_2},\cdots,\overline{o_r} \right \rangle"></span>是具有<span data-katex="r"></span>个运算的代数系统，<span data-katex="r\geqslant 1"></span>。

若对于<span data-katex="i=1,2,\cdots ,r"></span>，<span data-katex="o_i"></span>和<span data-katex="\overline{o_i}"></span>具有同样的元数，

则称<span data-katex="V_1"></span>和<span data-katex="V_2"></span>是同类型的代数系统。

<br/>

**子代数**

设<span data-katex="V=\left \langle A,o_1,o_2,\cdots,o_r \right \rangle"></span>是代数系统，<span data-katex="B"></span>是<span data-katex="A"></span>的非空子集，

若<span data-katex="B"></span>对<span data-katex="V"></span>中所有的运算封闭，则称<span data-katex="V'=\left \langle B,o_1,o_2,\cdots,o_r \right \rangle"></span>是<span data-katex="V"></span>的子代数系统，

简称子代数。

<br/>

当<span data-katex="B"></span>是<span data-katex="A"></span>的真子集时，称<span data-katex="V'"></span>是<span data-katex="V"></span>的真子代数。

<br/>

**同态与同构**

设<span data-katex="V_1=\left \langle A,o_1,o_2,\cdots,o_r \right \rangle"></span>，<span data-katex="V_2=\left \langle B,\overline{o_1},\overline{o_2},\cdots,\overline{o_r} \right \rangle"></span>是同类型的代数系统，

对于<span data-katex="i=1,2,\cdots ,r"></span>，<span data-katex="o_i"></span>和<span data-katex="\overline{o_i}"></span>是<span data-katex="k_i"></span>元运算，

函数<span data-katex="\varphi :A\rightarrow B"></span>对于所有的运算<span data-katex="o_i"></span>，<span data-katex="\overline{o_i}"></span>都有，

<span data-katex="\varphi(o_i(x_1,x_2,\cdots ,x_{k_i}))=\overline{o_i}(\varphi(x_1),\varphi(x_2),\cdots ,\varphi(x_{k_i}))"></span>

则称<span data-katex="\varphi"></span>是代数系统<span data-katex="V_1"></span>到<span data-katex="V_2"></span>的同态映射，简称同态。

<br/>

若<span data-katex="\varphi"></span>是满射，则称<span data-katex="\varphi"></span>是满同态，

若<span data-katex="\varphi"></span>是单射，则称<span data-katex="\varphi"></span>是单同态，

若<span data-katex="\varphi"></span>是双射，则称<span data-katex="\varphi"></span>是同构，

若<span data-katex="V_1=V_2"></span>，则称<span data-katex="\varphi"></span>是自同态，若<span data-katex="\varphi"></span>又是双射，则称<span data-katex="\varphi"></span>是自同构。

<br/>

**同态像**

设<span data-katex="V_1=\left \langle A,o_1,o_2,\cdots,o_r \right \rangle"></span>，<span data-katex="V_2=\left \langle B,\overline{o_1},\overline{o_2},\cdots,\overline{o_r} \right \rangle"></span>是同类型的代数系统，

对于<span data-katex="i=1,2,\cdots ,r"></span>，<span data-katex="o_i"></span>和<span data-katex="\overline{o_i}"></span>是<span data-katex="k_i"></span>元运算，

<span data-katex="\varphi :A\rightarrow B"></span>是<span data-katex="V_1"></span>到<span data-katex="V_2"></span>的同态，

则<span data-katex="\varphi(A)"></span>关于<span data-katex="V_2"></span>中的运算构成了一个代数系统，且是<span data-katex="V_2"></span>的子代数，

称为<span data-katex="V_1"></span>在<span data-katex="\varphi"></span>下的同态像。

<br/>

**同余关系**

设代数系统<span data-katex="V=\left \langle A,o_1,o_2,\cdots,o_r \right \rangle"></span>，其中<span data-katex="o_i"></span>是<span data-katex="k_i"></span>元运算，

关系<span data-katex="\sim"></span>是<span data-katex="A"></span>上的等价关系，

任取<span data-katex="A"></span>上<span data-katex="2k_i"></span>个元素，<span data-katex="a_1,a_2,\cdots ,a_{k_i}"></span>和<span data-katex="b_1,b_2,\cdots ,b_{k_i}"></span>，

如果对<span data-katex="j=1,2,\cdots ,k_i"></span>，<span data-katex="a_j\sim b_j"></span>成立，就有

<span data-katex="o_i(a_1,a_2,\cdots ,a_{k_i})\sim o_i(b_1,b_2,\cdots ,b_{k_i})"></span>

则称等价关系\sim"></span>对运算<span data-katex="o_i"></span>具有置换性质。

<br/>

如果等价关系<span data-katex="\sim"></span>对<span data-katex="V"></span>中的所有运算都具有置换性质，

则称关系<span data-katex="\sim"></span>是<span data-katex="V"></span>上的同余关系，

称<span data-katex="A"></span>中关于<span data-katex="\sim"></span>的等价类为<span data-katex="V"></span>上的同余类。

<br/>

**商代数**

设代数系统<span data-katex="V=\left \langle A,o_1,o_2,\cdots,o_r \right \rangle"></span>，其中<span data-katex="o_i"></span>是<span data-katex="k_i"></span>元运算，

关系<span data-katex="\sim"></span>是<span data-katex="V"></span>上的同余关系，

<span data-katex="V"></span>关于同余关系<span data-katex="\sim"></span>的商代数记作<span data-katex="V/\sim =\left \langle A/\sim ,\overline{o_1},\overline{o_2},\cdots,\overline{o_r} \right \rangle"></span>，

其中<span data-katex="A/\sim"></span>是<span data-katex="A"></span>关于同余关系<span data-katex="\sim"></span>的商集。

<br/>

对于<span data-katex="i=1,2,\cdots ,r"></span>，运算<span data-katex="\overline{o_i}"></span>规定为：

<span data-katex="\forall [a_1],[a_2],\cdots ,[a_{k_i}]\in A/\sim"></span>，有

<span data-katex="\overline{o_i}([a_1],[a_2],\cdots ,[a_{k_i}])=[o_i(a_1,a_2,\cdots ,a_{k_i})]"></span>。

<br/>

**同态导出的等价关系**

若<span data-katex="\varphi :A\rightarrow B"></span>是<span data-katex="V_1"></span>到<span data-katex="V_2"></span>的同态，

定义等价关系<span data-katex="x\sim y"></span>当且仅当<span data-katex="\varphi(x)=\varphi(y)"></span>，<span data-katex="\forall x,y\in A"></span>。

则称，该等价关系是同态<span data-katex="\varphi"></span>导出的等价关系。

<br/>

可证，同态<span data-katex="\varphi"></span>导出的等价关系是<span data-katex="V_1"></span>上的同余关系。

<br/>

**自然映射**

设<span data-katex="V=\left \langle A,o_1,o_2,\cdots,o_r \right \rangle"></span>是代数系统，

其中<span data-katex="o_i"></span>是<span data-katex="k_i"></span>元运算，<span data-katex="i=1,2,\cdots ,r"></span>，<span data-katex="\sim"></span>是<span data-katex="V"></span>上的同余关系，

则自然映射<span data-katex="g:A\rightarrow A/\sim"></span>，<span data-katex="\forall a\in A,\ g(a)=[a]"></span>，

是从<span data-katex="V"></span>到<span data-katex="V/\sim"></span>上的同态映射。

<br/>

**同态基本定理**

设<span data-katex="V_1=\left \langle A,o_1,o_2,\cdots,o_r \right \rangle"></span>，<span data-katex="V_2=\left \langle B,\overline{o_1},\overline{o_2},\cdots,\overline{o_r} \right \rangle"></span>是同类型的代数系统，

对于<span data-katex="i=1,2,\cdots ,r"></span>，<span data-katex="o_i"></span>和<span data-katex="\overline{o_i}"></span>是<span data-katex="k_i"></span>元运算，

<span data-katex="\varphi :A\rightarrow B"></span>是<span data-katex="V_1"></span>到<span data-katex="V_2"></span>的同态，

关系<span data-katex="\sim"></span>是<span data-katex="\varphi"></span>导出的<span data-katex="V_1"></span>上的同余关系，

则<span data-katex="V_1"></span>关于同余关系<span data-katex="\sim"></span>的商代数，同构于<span data-katex="V_1"></span>在<span data-katex="\varphi"></span>下的同态像，即

<span data-katex="V_1/\sim \ \cong \ \left \langle \varphi(A),\overline{o_1},\overline{o_2},\cdots ,\overline{o_r} \right \rangle"></span>

<br/>

同态基本定理告诉我们，任何代数系统<span data-katex="V"></span>的商代数是它的一个同态像。

反之，如果<span data-katex="V'"></span>是<span data-katex="V"></span>的同态像，则<span data-katex="V'"></span>与<span data-katex="V"></span>的一个商代数同构。

<br/>

## **群结构**

**半群**

设<span data-katex="\circ"></span>是集合<span data-katex="S"></span>上的二元运算，若<span data-katex="\circ"></span>运算在<span data-katex="S"></span>上是可结合的，

则称代数系统<span data-katex="V=\left \langle S,\circ \right \rangle"></span>是半群。

<br/>

**幺半群**

设<span data-katex="V=\left \langle S,\circ \right \rangle"></span>是半群，

若存在<span data-katex="e\in S"></span>为<span data-katex="V"></span>中关于运<span data-katex="\circ"></span>运算的单位元，

则称<span data-katex="V=\left \langle S,\circ ,e \right \rangle"></span>的幺半群。

<br/>

**群**

<span data-katex="\left \langle G,\circ \right \rangle"></span>是含有一个二元运算的代数系统，

如果满足以下条件：

（1）<span data-katex="\circ"></span>运算是可结合的，

（2）存在<span data-katex="e\in G"></span>是关于<span data-katex="\circ"></span>运算的单位元，

（3）任何<span data-katex="x\in G"></span>，<span data-katex="x"></span>关于<span data-katex="\circ"></span>运算的逆元<span data-katex="x^{-1}\in G"></span>。

则称<span data-katex="G"></span>是一个群。

<br/>

**交换群**

若群<span data-katex="G"></span>中运算满足交换律，则称<span data-katex="G"></span>为交换群，或Abel群。

<br/>

**子群**

<span data-katex="G"></span>是群，<span data-katex="H"></span>是<span data-katex="G"></span>的非空子集，若<span data-katex="H"></span>关于<span data-katex="G"></span>中的运算构成一个群，

则称<span data-katex="H"></span>是<span data-katex="G"></span>的子群，记为<span data-katex="H\leqslant G"></span>。

如果子群<span data-katex="H"></span>是<span data-katex="G"></span>的真子集，则称<span data-katex="H"></span>是<span data-katex="G"></span>的真子群，记为<span data-katex="H<G"></span>。

<br/>

若把群看做是具有一个可结合的二元运算，一个求逆元的一元运算，

和一个零元运算（二元运算的单位元）的代数系统，

可以证明，<span data-katex="G"></span>的子群就是代数系统<span data-katex="\left \langle G_1,\bullet,\ ^{-1},e_1 \right \rangle"></span>的子代数。

<br/>

**陪集分解**

<span data-katex="G"></span>是群，<span data-katex="H"></span>是<span data-katex="G"></span>的子群，<span data-katex="a\in G"></span>，定义

<span data-katex="Ha=\{ha|h\in H\}"></span>

则称<span data-katex="Ha"></span>是子群<span data-katex="H"></span>在<span data-katex="G"></span>中的一个右陪集。

<br/>

可以证明，

（1）<span data-katex="He=H"></span>，

（2）<span data-katex="\forall A\in G,\ a\in Ha"></span>，

（3）<span data-katex="a\in Hb \Leftrightarrow Ha=Hb \Leftrightarrow ab^{-1}\in H"></span>

<br/>

<span data-katex="G"></span>是群，<span data-katex="H"></span>是<span data-katex="G"></span>的子群，

在<span data-katex="G"></span>上定义二元关系<span data-katex="R"></span>，<span data-katex="\forall a,b\in G"></span>有

<span data-katex="aRb\Leftrightarrow ab^{-1}\in H"></span>

则<span data-katex="R"></span>为<span data-katex="G"></span>上的等价关系，且<span data-katex="[a]_R=Ha"></span>。

<br/>

<span data-katex="G"></span>是群，<span data-katex="H"></span>是<span data-katex="G"></span>的子群，则<span data-katex="\forall a,b\in G"></span>，

或者<span data-katex="Ha\cap Hb=\varnothing"></span>，或者<span data-katex="Ha=Hb"></span>，

且<span data-katex="\;{\tiny\begin{matrix}\\ \normalsize \cup \\ ^{\scriptsize a\in R}\end{matrix}}\;Ha=G"></span>

因此，子群<span data-katex="H"></span>的右陪集构成了群<span data-katex="G"></span>的一个划分。

<br/>

**正规子群**

<span data-katex="G"></span>是群，<span data-katex="H"></span>是<span data-katex="G"></span>的子群，若<span data-katex="\forall a\in G"></span>都有<span data-katex="Ha=aH"></span>，

则称<span data-katex="H"></span>是<span data-katex="G"></span>的正规子群，记为<span data-katex="H\trianglelefteq G"></span>。

<br/>

**商群**

<span data-katex="G"></span>是群，<span data-katex="H"></span>是<span data-katex="G"></span>的正规子群，

定义<span data-katex="G/H=\{Hg|g\in G\}"></span>为<span data-katex="H"></span>在<span data-katex="G"></span>中的所有右陪集构成的集合，

在<span data-katex="G/H"></span>上定义运算<span data-katex="\circ"></span>，对任意的<span data-katex="Ha,Hb\in G/H"></span>，

有<span data-katex="Ha\circ Hb=Hab"></span>。

则<span data-katex="G/H"></span>关于<span data-katex="\circ"></span>运算构成了一个群，称为<span data-katex="G"></span>的商群。

<br/>

注：如果<span data-katex="H"></span>不是正规子群，商仍可以得到，但结果将不是群，

而是[齐次空间](https://zh.wikipedia.org/wiki/%E9%BD%90%E6%80%A7%E7%A9%BA%E9%97%B4)。

<br/>

**群同态**

设<span data-katex="G_1"></span>和<span data-katex="G_2"></span>是群，<span data-katex="\varphi"></span>是<span data-katex="G_1"></span>到<span data-katex="G_2"></span>的映射，

若对于任意<span data-katex="x,y\in G"></span>有<span data-katex="\varphi(xy)=\varphi(x)\varphi(y)"></span>，

则称<span data-katex="\varphi"></span>是群<span data-katex="G_1"></span>到<span data-katex="G_2"></span>的同态映射，简称同态。

<br/>

若把群看做是具有一个可结合的二元运算，一个求逆元的一元运算，

和一个零元运算（二元运算的单位元）的代数系统，

则上述定义的群同态，就是代数系统<span data-katex="\left \langle G_1,\bullet,\ ^{-1},e_1 \right \rangle"></span>

到<span data-katex="\left \langle G_2,\bullet,\ ^{-1},e_2 \right \rangle"></span>的同态。

<br/>

**同态核与同态像**

设<span data-katex="\varphi :G_1\rightarrow G_2"></span>是群<span data-katex="G_1"></span>到<span data-katex="G_2"></span>的同态，则

<span data-katex="ker\ \varphi=\{x|x\in G\wedge \varphi(x)=e_2\}"></span>

<span data-katex="im\ \varphi=\{\varphi(x)|x\in G_1\}"></span>

<br/>

可证，同态核<span data-katex="ker\ \varphi"></span>是<span data-katex="G_1"></span>的正规子群，

而同态像<span data-katex="im\ \varphi"></span>是<span data-katex="G_2"></span>的子群。

<br/>

**群同态基本定理**

设<span data-katex="G"></span>是群，<span data-katex="H"></span>是<span data-katex="G"></span>的正规子群，则<span data-katex="G"></span>的商群<span data-katex="G/H"></span>是<span data-katex="G"></span>的同态像。

不难看出，群同态基本定理就是一般代数系统同态基本定理的特例。

<br/>

特别的，如果<span data-katex="\varphi :G_1\rightarrow G_2"></span>是群<span data-katex="G_1"></span>到<span data-katex="G_2"></span>的同态，

则<span data-katex="G_1/(ker\ \varphi)\ \cong\ im\ \varphi"></span>。

<br/>

## **链复形与同调群**

**链复形**

在数学上，同调代数领域中的一个链复形<span data-katex="(A_\bullet  ,d_\bullet )"></span>，

是一个交换群或者模的序列，<span data-katex="A_0,A_1,A_2,\cdots"></span>，

通过一系列同态<span data-katex="d_n:A_n\rightarrow A_{n-1}"></span>相连，

使得每两个接连的映射复合为零，即<span data-katex="\forall n,\ d_n\circ d_{n+1}=0"></span>。

<br/>

它们常写作如下形式：

<span data-katex="\cdots \longrightarrow A_{n+1}\;{\tiny\begin{matrix}d_{n+1}\\ \normalsize \longrightarrow \\ ^{\scriptsize }\end{matrix}}\;A_n\;{\tiny\begin{matrix}d_n\\ \normalsize \longrightarrow \\ ^{\scriptsize }\end{matrix}}\;\cdots\;{\tiny\begin{matrix}d_2\\ \normalsize \longrightarrow \\ ^{\scriptsize }\end{matrix}}\;A_1\;{\tiny\begin{matrix}d_1\\ \normalsize \longrightarrow \\ ^{\scriptsize }\end{matrix}}\;A_0\;{\tiny\begin{matrix}d_0\\ \normalsize \longrightarrow \\ ^{\scriptsize }\end{matrix}}\;0"></span>

<br/>

**同调群**

定义链复形的同调群为<span data-katex="H_n(A_\bullet):=Ker(d_n)/Im(d_{n+1})"></span>，

当所有同调群为零时，此链复形为正合的。

<br/>

**上链复形**

链复形概念的一个变种是上链复形。

一个上链复形<span data-katex="(A^\bullet  ,d^\bullet )"></span>，是一个交换群或者模的序列<span data-katex="A^0,A^1,A^2,\cdots"></span>，

由一系列同态<span data-katex="d^n:A^n\rightarrow A^{n+1}"></span>相连，

使得任何两个接连的映射复合为零，即<span data-katex="\forall n,\ d^{n+1}\circ d^n=0"></span>。

<span data-katex="0\longrightarrow A^0\;{\tiny\begin{matrix}d^0\\ \normalsize \longrightarrow \\ ^{\scriptsize }\end{matrix}}\;A^1\;{\tiny\begin{matrix}d^1\\ \normalsize \longrightarrow \\ ^{\scriptsize }\end{matrix}}\;\cdots\;{\tiny\begin{matrix}d^{n-1}\\ \normalsize \longrightarrow \\ ^{\scriptsize }\end{matrix}}\;A^n\;{\tiny\begin{matrix}d^n\\ \normalsize \longrightarrow \\ ^{\scriptsize }\end{matrix}}\;A^{n+1}\longrightarrow \cdots"></span>

<br/>

**上同调群**

定义上链复形的上同调群为<span data-katex="H^n(A^\bullet):=Ker(d^n)/Im(d^{n-1})"></span>，

当所有上同调群为零时，此上链复形正合。

<br/>

## **结语**

我们看到上同调群<span data-katex="H^n(A^\bullet)"></span>，

是<span data-katex="Ker(d^n)"></span>关于<span data-katex="Im(d^{n-1})"></span>的商群。

<br/>

下面我们证明<span data-katex="Im(d^{n-1})"></span>是<span data-katex="Ker(d^n)"></span>的正规子群。

（1）我们先证<span data-katex="Im(d^{n-1})"></span>是<span data-katex="Ker(d^n)"></span>的子群。

对于上链复形，任何两个接连的映射复合为零，说明<span data-katex="Im(d^{n-1})\subseteq Ker(d^n)"></span>。

且<span data-katex="Im(d^{n-1})"></span>是同态像，所以<span data-katex="Im(d^{n-1})\leqslant A^n"></span>，

因此<span data-katex="Im(d^{n-1})"></span>关于群运算封闭，

所以，根据子群的定义，<span data-katex="Im(d^{n-1})\leqslant Ker(d^n)"></span>。

<br/>

（2）我们再证交换群的子群是正规子群。

因为<span data-katex="A^n"></span>是交换群，所以它的子群也是交换群。

而交换群的任一子群<span data-katex="H"></span>都有，<span data-katex="Ha=\{ha|h\in H\}=\{ah|h\in H\}=aH"></span>，

因此交换群的任一子群都是正规子群。

<br/>

综上，<span data-katex="Im(d^{n-1})\trianglelefteq Ker(d^n)"></span>。

<br/>

所以，<span data-katex="H^n(A^\bullet):=Ker(d^n)/Im(d^{n-1})"></span>构成了一个群，

它的元素是<span data-katex="Im(d^{n-1})"></span>的陪集，这些陪集划分了<span data-katex="Ker(d^n)"></span>。

<br/>

## **参考**

[商群](https://zh.wikipedia.org/wiki/%E5%95%86%E7%BE%A4)

[群同态](https://zh.wikipedia.org/wiki/%E7%BE%A4%E5%90%8C%E6%85%8B)

[链复形](https://zh.wikipedia.org/wiki/%E9%93%BE%E5%A4%8D%E5%BD%A2)

[离散数学教程](https://book.douban.com/subject/1230394/)