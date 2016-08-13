---
layout: post
categories: Math
title: 拓扑学拾趣
description: 
---

<script src="/thirdpart/jQuery/jquery-1.11.1.js"></script>
<link href="/thirdpart/KaTeX/katex.min.css" rel="stylesheet"/>
<script src="/thirdpart/KaTeX/katex.min.js"></script>
<script src="/javascripts/katex.js"></script>

## **集合论的基础知识**

**幂集**

设<span data-katex="X"></span>是非空集合，

记<span data-katex="2^X"></span>是<span data-katex="X"></span>的全体子集（包括<span data-katex="X"></span>及<span data-katex="\varnothing"></span>）的集合，

称为<span data-katex="X"></span>的幂集。

<br/>

**包含关系**

<span data-katex="x\in A"></span>表示<span data-katex="x"></span>是集合<span data-katex="A"></span>中的一个元素。

<span data-katex="x\notin A"></span>表示<span data-katex="x"></span>不是集合<span data-katex="A"></span>的元素。

<span data-katex="A\subseteq B"></span>表示<span data-katex="A"></span>包含于<span data-katex="B"></span>（包含<span data-katex="A=B"></span>的情形）。

<span data-katex="A\nsubseteq"></span>表示<span data-katex="A"></span>不包含与<span data-katex="B"></span>，即<span data-katex="A"></span>中有不属于<span data-katex="B"></span>的元素。

<br/>

**交与并**

现在列出<span data-katex="2^X"></span>中的几种运算及它们的性质。

<span data-katex="\;{\tiny\begin{matrix} \\ \normalsize \cap \\ ^{\scriptsize \lambda \in \Lambda }\end{matrix}}\;A_\lambda"></span>表示集合族<span data-katex="\{ A_\lambda | \lambda \in \Lambda \}"></span>中所有集合之交。

<span data-katex="\;{\tiny\begin{matrix} \\ \normalsize \cup \\ ^{\scriptsize \lambda \in \Lambda }\end{matrix}}\;A_\lambda"></span>表示集合族<span data-katex="\{ A_\lambda | \lambda \in \Lambda \}"></span>中所有集合之并。

<br/>

**交换律，结合律和分配律**

交并运算各自都满足交换律和结合律。

交与并有分配律：

（1）<span data-katex="B\cup ( \;{\tiny\begin{matrix} \\ \normalsize \cap \\ ^{\scriptsize \lambda \in \Lambda }\end{matrix}}\;A_\lambda )=\;{\tiny\begin{matrix} \\ \normalsize \cap \\ ^{\scriptsize \lambda \in \Lambda }\end{matrix}}\;(B\cup A_\lambda )"></span>

（2）<span data-katex="B\cap ( \;{\tiny\begin{matrix} \\ \normalsize \cup \\ ^{\scriptsize \lambda \in \Lambda }\end{matrix}}\;A_\lambda )=\;{\tiny\begin{matrix} \\ \normalsize \cup \\ ^{\scriptsize \lambda \in \Lambda }\end{matrix}}\;(B\cap A_\lambda )"></span>

<br/>

**差和余**

差，<span data-katex="A\backslash B"></span>表示属于<span data-katex="A"></span>而不属于<span data-katex="B"></span>的元素的集合。

余集，<span data-katex="A^c:=X\backslash A"></span>。

因此，<span data-katex="A\backslash B=A\cap B^c"></span>

<br/>

**De Morgan公式**

（1）<span data-katex="B\backslash ( \;{\tiny\begin{matrix} \\ \normalsize \cup \\ ^{\scriptsize \lambda \in \Lambda }\end{matrix}}\;A_\lambda )=\;{\tiny\begin{matrix} \\ \normalsize \cap \\ ^{\scriptsize \lambda \in \Lambda }\end{matrix}}\;(B\backslash A_\lambda )"></span>

（2）<span data-katex="B\backslash ( \;{\tiny\begin{matrix} \\ \normalsize \cap \\ ^{\scriptsize \lambda \in \Lambda }\end{matrix}}\;A_\lambda )=\;{\tiny\begin{matrix} \\ \normalsize \cup \\ ^{\scriptsize \lambda \in \Lambda }\end{matrix}}\;(B\backslash A_\lambda )"></span>

<br/>

特别的当<span data-katex="B=X"></span>为全集时，

（3）<span data-katex="( \;{\tiny\begin{matrix} \\ \normalsize \cup \\ ^{\scriptsize \lambda \in \Lambda }\end{matrix}}\;A_\lambda )^c=\;{\tiny\begin{matrix} \\ \normalsize \cap \\ ^{\scriptsize \lambda \in \Lambda }\end{matrix}}\;A_\lambda^c"></span>

（4）<span data-katex="( \;{\tiny\begin{matrix} \\ \normalsize \cap \\ ^{\scriptsize \lambda \in \Lambda }\end{matrix}}\;A_\lambda )^c=\;{\tiny\begin{matrix} \\ \normalsize \cup \\ ^{\scriptsize \lambda \in \Lambda }\end{matrix}}\;A_\lambda^c"></span>

<br/>

**映射**

设<span data-katex="X"></span>和<span data-katex="Y"></span>都是集合，映射<span data-katex="f:X\rightarrow Y"></span>是一个对应关系，使得

<span data-katex="\forall x\in X"></span>，对应着<span data-katex="Y"></span>中的一点<span data-katex="f(x)"></span>（称为<span data-katex="x"></span>的像点）

<br/>

若<span data-katex="A\subseteq X"></span>，记<span data-katex="f(A:=\{f(x)|x\in A\}"></span>，是<span data-katex="Y"></span>的一个子集，

称为<span data-katex="A"></span>在<span data-katex="f"></span>下的像。

若<span data-katex="B\subseteq Y"></span>，记<span data-katex="f^{-1}(b):=\{x\in X|f(x)\in B\}"></span>，

称为<span data-katex="B"></span>在<span data-katex="f"></span>下的原像。

<br/>

当<span data-katex="f(X)=Y"></span>时，称<span data-katex="f"></span>是满的，

若<span data-katex="X"></span>中的不同点的像点也不同，称<span data-katex="f"></span>是单的。

既单又满的映射称为一一对应。

当<span data-katex="f"></span>是一一对应时，它就有一个逆映射，记作<span data-katex="f^{-1}"></span>。

<br/>

**映射的复合**

设<span data-katex="f:X\rightarrow Y"></span>和<span data-katex="g:Y\rightarrow Z"></span>都是映射，

<span data-katex="f"></span>和<span data-katex="g"></span>的复合（或称乘积）是<span data-katex="X"></span>到<span data-katex="Z"></span>的映射，记作<span data-katex="g\circ f:X\rightarrow Z"></span>，

规定为，<span data-katex="(g\circ f)(x)=g(f(x))"></span>。

<br/>

<span data-katex="\forall x\in X"></span>，则有，

（1）<span data-katex="(g\circ f)(A)=g(f(A))"></span>

（2）<span data-katex="(g\circ f)^{-1}(B)=f^{-1}(g^{-1}(B))"></span>

<br/>

**恒同映射，限制与包含映射**

集合<span data-katex="X"></span>到自身的恒同映射（保持每一点不变）记作<span data-katex="id_X:\rightarrow X"></span>（常简记为<span data-katex="id"></span>）。

若<span data-katex="f:X\rightarrow Y"></span>是映射，<span data-katex="A\subseteq X"></span>，规定<span data-katex="f"></span>在<span data-katex="A"></span>上的限制为<span data-katex="f|A:X\rightarrow Y"></span>，

<span data-katex="\forall x\in A"></span>，<span data-katex="(f|A)(x)=f(x)"></span>。

记<span data-katex="i:A\rightarrow X"></span>为包含映射，即<span data-katex="\forall x\in A"></span>，<span data-katex="i(x)=x"></span>，

于是，<span data-katex="i=id|A"></span>，<span data-katex="f|A=f\circ i"></span>。

<br/>

**笛卡尔积**

设<span data-katex="X_1"></span>和<span data-katex="X_2"></span>都是集合，称集合

<span data-katex="X_1\times X_2:=\{(x,y)|x\in X,y\in Y\}"></span>为<span data-katex="X_1"></span>与<span data-katex="X_2"></span>的笛卡尔积。

称<span data-katex="x"></span>和<span data-katex="y"></span>为有序偶<span data-katex="(x,y)"></span>的坐标。

<br/>

**等价关系**

集合<span data-katex="X"></span>上的一个关系<span data-katex="R"></span>是<span data-katex="X\times X"></span>的一个子集，

当<span data-katex="(x_1,x_2)\in R"></span>时，说<span data-katex="x_1"></span>与<span data-katex="x_2"></span>是<span data-katex="R"></span>相关的，记作<span data-katex="x_1Rx_2"></span>。

<br/>

集合<span data-katex="X"></span>的一个关系<span data-katex="R"></span>称为等价关系，如果满足：

（1）自反性：<span data-katex="\forall x\in X"></span>，<span data-katex="xRx"></span>

（2）对称性：若<span data-katex="x_1Rx_2"></span>，则<span data-katex="x_2Rx_1"></span>

（3）传递性：若<span data-katex="x_1Rx_2"></span>，<span data-katex="x_2Rx_3"></span>，则<span data-katex="x_1Rx_3"></span>

<br/>

等价关系常用<span data-katex="\sim"></span>表示，例如<span data-katex="x_1Rx_2"></span>，可记作<span data-katex="x_1\sim x_2"></span>，

称为<span data-katex="x_1"></span>等价于<span data-katex="x_2"></span>。

当<span data-katex="X"></span>上有等价关系<span data-katex="\sim"></span>时，可以把<span data-katex="X"></span>分成许多子集，

凡是互相等价的点属于同一子集。

称每个子集为一个<span data-katex="\sim"></span>等价类，记<span data-katex="X/\sim"></span>是全部等价类的集合，

称为<span data-katex="X"></span>关于<span data-katex="\sim"></span>的商集。

<span data-katex="\forall x\in X"></span>所在的等价类记作<span data-katex="\left \langle x \right \rangle"></span>，

于是，<span data-katex="X/\sim =\{\left \langle x \right \rangle |x\in X\}"></span>

<br/>

## **拓扑空间**

**子集族**

设<span data-katex="X"></span>是一个非空集合，<span data-katex="2^X"></span>是<span data-katex="X"></span>的幂集，

把<span data-katex="2^X"></span>的子集（即以<span data-katex="X"></span>的一部分子集为成员的集合）称为<span data-katex="X"></span>的子集族。

<br/>

**拓扑**

设<span data-katex="X"></span>是一个非空集合，<span data-katex="X"></span>的一个子集族<span data-katex="\tau"></span>称为<span data-katex="X"></span>的一个拓扑，

如果它满足

（1）<span data-katex="X"></span>和<span data-katex="\varnothing"></span>都包含在<span data-katex="\tau"></span>中

（2）<span data-katex="\tau"></span>中任意多个成员的并集仍在<span data-katex="\tau"></span>中

（3）<span data-katex="\tau"></span>中有限多个成员的交集仍在<span data-katex="\tau"></span>中

<br/>

集合<span data-katex="X"></span>和它的一个拓扑<span data-katex="\tau"></span>一起称为一个拓扑空间，记作<span data-katex="(X,\tau)"></span>。

称<span data-katex="\tau"></span>中的成员为这个拓扑空间的开集。

<br/>

从定义看出，给出集合的一个拓扑就是规定它的哪些子集是开集。

<br/>

**离散拓扑**

设<span data-katex="X"></span>是一个非空集合，<span data-katex="2^X"></span>构成<span data-katex="X"></span>上的一个拓扑，称为<span data-katex="X"></span>上的离散拓扑。

<span data-katex="\{X,\varnothing \}"></span>也是<span data-katex="X"></span>上的拓扑，称为<span data-katex="X"></span>上的平凡拓扑。

<br/>

**有限补拓扑，可数补拓扑**

设<span data-katex="X"></span>是无穷集合，<span data-katex="\tau =\{A| A^c \subseteq X\wedge A^c\ is\ finite\}\cup\{\varnothing \}"></span>，称为<span data-katex="X"></span>上的有限补拓扑。

设<span data-katex="X"></span>是不可数无穷集合，<span data-katex="\tau =\{A| A^c \subseteq X\wedge A^c\ is\ countable\}\cup\{\varnothing \}"></span>，称为<span data-katex="X"></span>上的可数补拓扑。

<br/>

**度量**

集合<span data-katex="X"></span>上的一个度量<span data-katex="d"></span>是一个映射<span data-katex="d:X\times X\rightarrow R"></span>，

它满足

（1）正定性：<span data-katex="d(x,x)=0"></span>，<span data-katex="\forall x\in X"></span>；<span data-katex="d(x,y)>0"></span>，当<span data-katex="X\neq Y"></span>

（2）对称性：<span data-katex="d(x,y)=d(y,x)"></span>，<span data-katex="\forall x,y\in X"></span>

（3）三角不等式：<span data-katex="d(x,x)\leqslant d(x,y)+d(y,z)"></span>，<span data-katex="\forall x,y,z\in X"></span>

<br/>

当集合<span data-katex="X"></span>上规定一个度量<span data-katex="d"></span>后，称为度量空间，记作<span data-katex="(X,d)"></span>。

<br/>

<span data-katex="R^n=\{(x_1,x_2,\cdots ,x_n)|x_i\in R,\ i=1,2,\cdots , n\}"></span>，

规定<span data-katex="R^n"></span>上的度量<span data-katex="d"></span>为，

<span data-katex="d((x_1,\cdots ,x_n),(y_1,\cdots ,y_n))=\sqrt{\sum_{x=1}^{n}(x_i-y_i)^2}"></span>

记<span data-katex="E^n=(R^n,d)"></span>，称为<span data-katex="n"></span>维欧氏空间。

<br/>

**球形邻域**

设<span data-katex="x_0\in X"></span>，<span data-katex="\varepsilon"></span>是一个正数，称<span data-katex="X"></span>的子集

<span data-katex="B(x_0,\varepsilon):=\{x\in X|d(x_0,x)<\varepsilon \}"></span>

为以<span data-katex="x_0"></span>为心，<span data-katex="\varepsilon"></span>为半径的球形邻域。

<br/>

如果规定<span data-katex="X"></span>的子集族<span data-katex="\tau"></span>是<span data-katex="X"></span>中若干球形邻域的并集，

那么<span data-katex="\tau"></span>构成了<span data-katex="X"></span>上的一个拓扑，

并称<span data-katex="\tau"></span>为<span data-katex="X"></span>上由度量<span data-katex="d"></span>决定的度量拓扑。

<br/>

因此，每个度量空间都可以自然的看成具有度量拓扑的拓扑空间。

<br/>

## **拓扑空间的几个基本概念**

**闭集**

拓扑空间<span data-katex="X"></span>的一个子集<span data-katex="A"></span>称为闭集，如果<span data-katex="A^c"></span>是开集。

也就是说，闭集就是开集的余集，开集也一定是一个闭集的余集。

<br/>

例如，在离散拓扑空间中，任何子集都是开集，从而任何子集也都是闭集。

在平凡拓扑空间中，只有两个闭集，<span data-katex="X=\varnothing ^c"></span>和<span data-katex="\varnothing =X^c"></span>。

<br/>

拓扑空间中的闭集满足：

（1）<span data-katex="X"></span>和<span data-katex="\varnothing"></span>都是闭集

（2）任意多个闭集的交集是闭集合

（3）有限个闭集的并集是闭集

<br/>

**内点，邻域和内部**

设<span data-katex="A"></span>是拓扑空间<span data-katex="X"></span>的一个子集，点<span data-katex="x\in A"></span>，

如果存在开集<span data-katex="U"></span>，使得<span data-katex="x\in U\subseteq A"></span>，

则称<span data-katex="x"></span>是<span data-katex="A"></span>的一个内点，<span data-katex="A"></span>是<span data-katex="x"></span>的一个邻域。

<br/>

<span data-katex="A"></span>的所有内点的集合称为<span data-katex="A"></span>的内部，记作<span data-katex="A^\circ"></span>。

（1）若<span data-katex="A\subseteq B"></span>，则<span data-katex="A^\circ \subseteq B^\circ"></span>

（2）<span data-katex="A^\circ"></span>是包含在<span data-katex="A"></span>中所有开集的并集，因此是包含在<span data-katex="A"></span>中的最大开集

（3）<span data-katex="A^\circ =A"></span>当且仅当<span data-katex="A"></span>是开集

（4）<span data-katex="(A\cap B)^\circ =A^\circ \cap B^\circ"></span>

（5）<span data-katex="(A\cup B)^\circ \supseteq A^\circ \cup B^\circ"></span>

<br/>

**聚点，导集和闭包**

设<span data-katex="A"></span>是拓扑空间<span data-katex="X"></span>的子集，<span data-katex="x\in X"></span>，

如果<span data-katex="x"></span>的每个邻域都含有<span data-katex="A\backslash\{x\}"></span>中的点，则称<span data-katex="x"></span>为<span data-katex="A"></span>的聚点。

<span data-katex="A"></span>的所有聚点集合称为<span data-katex="A"></span>的导集，记作<span data-katex="A'"></span>。

称集合<span data-katex="\overline{A}:=A\cup A'"></span>为<span data-katex="A"></span>的闭包。

<br/>

由定义不难看出，<span data-katex="x\in \overline{A}"></span>当且仅当<span data-katex="x"></span>的任一邻域与<span data-katex="A"></span>都有交点。

（1）若<span data-katex="A\subseteq B"></span>，则<span data-katex="\overline{A}\subseteq \overline{B}"></span>

（2）<span data-katex="\overline{A}"></span>是所有包含<span data-katex="A"></span>的闭集的交集，所以是包含<span data-katex="A"></span>的最小闭集。

（3）<span data-katex="\overline{A}=A"></span>当且仅当<span data-katex="A"></span>是闭集

（4）<span data-katex="\overline{A\cup B}=\overline{A}\cup \overline{B}"></span>

（5）<span data-katex="\overline{A\cap B} \subseteq \overline{A}\cap \overline{B}"></span>

<br/>

**可分拓扑空间**

拓扑空间<span data-katex="X"></span>的子集<span data-katex="A"></span>称为稠密的，如果<span data-katex="\overline{A}=X"></span>。

如果<span data-katex="X"></span>有可数的稠密子集，则称<span data-katex="X"></span>是可分拓扑空间。

<br/>

**子空间**

设<span data-katex="A"></span>是拓扑空间<span data-katex="(X,\tau)"></span>的一个非空子集，

规定<span data-katex="A"></span>的子集族为<span data-katex="\tau _A:=\{U\cap A|U\in \tau \}"></span>，

可证<span data-katex="\tau _A"></span>是<span data-katex="A"></span>上的一个拓扑，称为<span data-katex="\tau"></span>导出的<span data-katex="A"></span>上的子空间拓扑，

称<span data-katex="(A,\tau _A)"></span>为<span data-katex="(X,\tau)"></span>的子空间。

<br/>

以后对拓扑空间的子集都将看做拓扑空间，即子空间。

对于子空间<span data-katex="A"></span>的子集<span data-katex="U"></span>，笼统的说<span data-katex="U"></span>是不是开集意义就不明确了，

必须说明在<span data-katex="A"></span>中看还是在全空间中看，这两者是不同的。

<br/>

例如，<span data-katex="E^1"></span>是<span data-katex="E^2"></span>的子空间，开区间<span data-katex="(0,1)"></span>在<span data-katex="E^1"></span>中是开集，

而在<span data-katex="E^2"></span>中不是开集，因此开集的概念是相对的。

同样，闭集，邻域，内点，内部，聚点和闭包等等概念也都是相对概念。

<br/>

## **连续映射与同胚映射**

**连续映射**

设<span data-katex="X"></span>和<span data-katex="Y"></span>都是拓扑空间，<span data-katex="f:X\rightarrow Y"></span>是一个映射，

<span data-katex="x\in X"></span>，如果对于<span data-katex="Y"></span>中<span data-katex="f(x)"></span>的任一邻域<span data-katex="V"></span>，

<span data-katex="f^{-1}(V)"></span>总是<span data-katex="x"></span>的邻域，则说<span data-katex="f"></span>在<span data-katex="x"></span>处连续。

<br/>

如果把定义中的『任一邻域<span data-katex="V"></span>』改成『任一开邻域<span data-katex="V"></span>』，那么定义的意义不变，

因此，<span data-katex="f"></span>在点<span data-katex="x"></span>处连续，也就是，

对包含<span data-katex="f(x)"></span>的每个开集，比存在包含<span data-katex="x"></span>的开集<span data-katex="U"></span>，

使得，<span data-katex="f(U)\subseteq V"></span>。

<br/>

如果映射<span data-katex="f:X\rightarrow Y"></span>在任一点<span data-katex="x\in X"></span>都连续，

则说<span data-katex="f"></span>是连续映射。

（1）<span data-katex="Y"></span>的任一开集在<span data-katex="f"></span>下的原像是<span data-katex="X"></span>的开集

（2）<span data-katex="Y"></span>的任一闭集在<span data-katex="f"></span>下的原像是<span data-katex="X"></span>的闭集

<br/>

**同胚映射**

如果<span data-katex="f:X\rightarrow Y"></span>是一一对应，

并且<span data-katex="f"></span>及其逆<span data-katex="f"></span>{-1}:Y\rightarrow X"></span>都是连续的，

则称<span data-katex="f"></span>是一个同胚映射，简称同胚。

<br/>

当存在<span data-katex="X"></span>到<span data-katex="Y"></span>的同胚映射时，就称<span data-katex="X"></span>与<span data-katex="Y"></span>同胚，

记作<span data-katex="X \cong Y"></span>。

<br/>

值得注意的是，同胚映射中条件<span data-katex="f^{-1}"></span>连续不可忽视，

它不能从一一对应和<span data-katex="f"></span>的连续性推出。

<br/>

**拓扑不变性**

拓扑空间在同胚映射下保持不变的概念，称为拓扑概念，

在同胚映射下保持不变的性质，称为拓扑性质。

<br/>

当<span data-katex="f:X\rightarrow Y"></span>是同胚映射时，

<span data-katex="X"></span>的每个开集<span data-katex="U"></span>的像<span data-katex="f(U)"></span>，是<span data-katex="Y"></span>的开集，

而<span data-katex="Y"></span>的开集<span data-katex="V"></span>的<span data-katex="f"></span>原像是<span data-katex="X"></span>的开集，

因此开集概念在同胚映射下保持不变，它是拓扑概念。

由它规定的闭集，闭包，邻域，内点等等概念都是拓扑概念。

<br/>

用开集或其派生的拓扑概念来刻画的性质，都是拓扑性质。

<br/>

## **拓扑基**

**生成的子集族**

设<span data-katex="\mathscr{B}"></span>是<span data-katex="X"></span>的一个子集族，

规定新的子集族<span data-katex="\mathscr{B}'"></span>，它的元素为<span data-katex="\mathscr{B}"></span>中任意多个成员的并集。

称<span data-katex="\mathscr{B}'"></span>为<span data-katex="\mathscr{B}"></span>所生成的子集族，

显然有<span data-katex="\mathscr{B}\subseteq \mathscr{B}'"></span>，<span data-katex="\varnothing \in \mathscr{B}'"></span>。

<br/>

**投射**

设<span data-katex="X_1"></span>和<span data-katex="X_2"></span>是两个集合，记<span data-katex="X_1\times X_2"></span>为它们的笛卡尔积，

规定<span data-katex="j_1:X_1\times X_2\rightarrow X_i"></span>为<span data-katex="j_i(x_1,x_2)=x_i"></span>，<span data-katex="i=1,2"></span>，

称<span data-katex="j_i"></span>为<span data-katex="X_1\times X_2"></span>到<span data-katex="X_i"></span>的投射。

<br/>

**乘积空间**

设<span data-katex="(X_1,\tau_1)"></span>和<span data-katex="(X_2,\tau_2)"></span>是两个拓扑空间，

现在要在笛卡尔积<span data-katex="X_1\times X_2"></span>上规定一个与<span data-katex="\tau_1,\tau_2"></span>密切相关的拓扑<span data-katex="\tau"></span>，

使得<span data-katex="j_1"></span>和<span data-katex="j_2"></span>都连续，并且是满足此要求的最小拓扑。

<br/>

设<span data-katex="\mathscr{B}=\{U_1\times U_2|U_i\in \tau_i\}"></span>，

则由<span data-katex="\mathscr{B}"></span>生成的子集族<span data-katex="\mathscr{B}'"></span>，构成了<span data-katex="X_1\times X_2"></span>上的一个拓扑，

称为<span data-katex="X_1\times X_2"></span>上的乘积拓扑。

<br/>

称<span data-katex="(X_1\times X_2,\mathscr{B}')"></span>为<span data-katex="(X_1,\tau_1)"></span>和<span data-katex="(X_2,\tau_2)"></span>的乘积空间，

简记为<span data-katex="X_1\times X_2"></span>。

<br/>

可证，<span data-katex="\mathscr{B}'"></span>是使得<span data-katex="j_1"></span>和<span data-katex="j_2"></span>都连续的最小拓扑。

<br/>

**拓扑基**

称集合<span data-katex="X"></span>的子集族<span data-katex="\mathscr{B}"></span>为集合<span data-katex="X"></span>的拓扑基，

如果由<span data-katex="\mathscr{B}"></span>生成的子集族<span data-katex="\mathscr{B}'"></span>是<span data-katex="X"></span>的一个拓扑。

<br/>

称拓扑空间<span data-katex="(X,\tau)"></span>的子集族<span data-katex="\mathscr{B}"></span>为这个拓扑空间的拓扑基，

如果由<span data-katex="\mathscr{B}"></span>生成的子集族<span data-katex="\mathscr{B}'=\tau"></span>。

<br/>

这里提出了两个有联系的不同概念，集合的拓扑基和拓扑空间的拓扑基。

前者只要求<span data-katex="\mathscr{B}'"></span>是集合<span data-katex="X"></span>的一个拓扑，

而后者要求<span data-katex="\mathscr{B}'"></span>是<span data-katex="X"></span>原有的拓扑<span data-katex="\tau"></span>。

这两个概念的判断方法也是不一样的。

<br/>

<span data-katex="\mathscr{B}"></span>是集合<span data-katex="X"></span>的拓扑基的充分必要条件是：

（1）<span data-katex="\;{\tiny\begin{matrix} \\ \normalsize \cup \\ ^{\scriptsize B\in \mathscr{B} }\end{matrix}}\;=X"></span>

（2）若<span data-katex="B_1,B_2\in \mathscr{B}"></span>，则<span data-katex="B_1\cap B_2\in \mathscr{B}'"></span>

<br/>

<span data-katex="\mathscr{B}"></span>是拓扑空间<span data-katex="(X,\tau)"></span>的拓扑基的充分必要条件是：

（1）<span data-katex="\mathscr{B}\subseteq \tau"></span>（即<span data-katex="\mathscr{B}"></span>的成员都是开集）

（2）<span data-katex="\tau \subseteq \mathscr{B}'"></span>（即每个开集都是<span data-katex="\mathscr{B}"></span>中一些成员的并集）

<br/>

## **分离公理**

**<span data-katex="T_1"></span>公理**
任何两个不同的店<span data-katex="x"></span>与<span data-katex="y"></span>，<span data-katex="x"></span>有邻域不含<span data-katex="y"></span>，<span data-katex="y"></span>有邻域不含<span data-katex="x"></span>。

<br/>

**<span data-katex="T_2"></span>公理**

任何两个不同点有不相交邻域。

<br/>

这里的『邻域』可改成『开邻域』，而公理的含义不变。

满足<span data-katex="T_2"></span>也一定满足<span data-katex="T_1"></span>公理，但从<span data-katex="T_1"></span>公理推不出<span data-katex="T_2"></span>公理。

可证，<span data-katex="X"></span>满足<span data-katex="T_1"></span>公理，当且仅当<span data-katex="X"></span>的有限子集是闭集。

<br/>

<span data-katex="T_2"></span>公理是最重要的分离公理，满足<span data-katex="T_2"></span>公理的拓扑空间称为Hausdorff空间。

<br/>

**<span data-katex="T_3"></span>公理**

任意一点与不含它的任一闭集有不相交的（开）邻域。

<br/>

**<span data-katex="T_4"></span>公理**

任意两个不想交的闭集有不想交的（开）邻域。

（当<span data-katex="A\subseteq U^\circ"></span>时，说<span data-katex="U"></span>是集合<span data-katex="A"></span>的邻域）

<br/>

如果<span data-katex="X"></span>满足<span data-katex="T_1"></span>公理，则它的单点集是闭集，

因此<span data-katex="T_3"></span>公理推出<span data-katex="T_2"></span>公理，<span data-katex="T_4"></span>公理推出<span data-katex="T_3"></span>公理，

然而，没有<span data-katex="T_1"></span>公理的前提下，上述关系不成立。

<br/>

可证，度量空间<span data-katex="(X,d)"></span>满足<span data-katex="T_i"></span>公理（<span data-katex="i=1,2,3,4"></span>）。

<br/>

<span data-katex="X"></span>满足<span data-katex="T_3"></span>公理，当且仅当任意点<span data-katex="x"></span>和它的开邻域<span data-katex="W"></span>，存在<span data-katex="x"></span>的开邻域<span data-katex="U"></span>，

使得<span data-katex="\overline{U}\subseteq W"></span>。

<span data-katex="X"></span>满足<span data-katex="T_4"></span>公理，当且仅当任意闭集<span data-katex="A"></span>和它的开邻域<span data-katex="W"></span>，有<span data-katex="A"></span>的开邻域<span data-katex="U"></span>，

使得<span data-katex="\overline{U}\subseteq W"></span>。

<br/>

## **可数公理**

**邻域基**

设<span data-katex="x\in X"></span>，把<span data-katex="x"></span>的所有邻域的集合称为<span data-katex="x"></span>的邻域系，记作<span data-katex="\mathscr{N}(x)"></span>。

<span data-katex="\mathscr{N}(x)"></span>的一个子集（即<span data-katex="x"></span>的一族邻域）<span data-katex="\mathscr{U}"></span>称为<span data-katex="x"></span>的一个邻域基，

如果<span data-katex="x"></span>的每个邻域至少包含<span data-katex="\mathscr{U}"></span>中的一个成员。

<br/>

若<span data-katex="\mathscr{B}"></span>是拓扑空间<span data-katex="X"></span>的拓扑基，则<span data-katex="\mathscr{U}=\{B\in \mathscr{B}|x\in B\}"></span>也是<span data-katex="x"></span>的邻域基。

<br/>

**<span data-katex="C_1"></span>公理**

拓扑空间<span data-katex="X"></span>中的任一点都有可数的邻域基。

<br/>

**<span data-katex="C_2"></span>公理**

拓扑空间<span data-katex="X"></span>有可数拓扑基。

<br/>

<span data-katex="C_2"></span>空间称为完全可分空间，<span data-katex="C_2"></span>空间一定也是<span data-katex="C_1"></span>空间。

可证，可分度量空间是<span data-katex="C_2"></span>空间。

<br/>

## **紧致性**

**收敛**

设<span data-katex="x_0,x_2,\cdots ,x_n,\cdots"></span>（可简记为<span data-katex="\{x_n\}"></span>）是拓扑空间<span data-katex="X"></span>中的点的序列，

如果点<span data-katex="x_0\in X"></span>的任一邻域<span data-katex="U"></span>都包含<span data-katex="\{x_n\}"></span>中几乎所有项，

即，只有有限个<span data-katex="x_n"></span>不在<span data-katex="U"></span>中，

则说<span data-katex="\{x_n\}"></span>收敛到<span data-katex="x_0"></span>，记作<span data-katex="x_n\rightarrow x_0"></span>。

<br/>

**列紧性**

拓扑空间称为列紧的，如果它的每个序列有收敛的子序列。

<br/>

**覆盖**

设<span data-katex="\mathscr{C}\subseteq{2^X}"></span>是拓扑空间<span data-katex="X"></span>的子集族，

称<span data-katex="\mathscr{C}"></span>是<span data-katex="X"></span>的一个覆盖，如果<span data-katex="\;{\tiny\begin{matrix} \\ \normalsize \cup \\ ^{\scriptsize C\in \mathscr{C} }\end{matrix}}\;C=X"></span>。

如果覆盖<span data-katex="\mathscr{C}"></span>的每个成员都是开（闭）集，则称<span data-katex="\mathscr{C}"></span>为开（闭）覆盖。

如果覆盖<span data-katex="\mathscr{C}"></span>只包含有限个成员，则称<span data-katex="\mathscr{C}"></span>是一个有限覆盖。

如果<span data-katex="\mathscr{C}"></span>的一个子族<span data-katex="\mathscr{A}\subseteq \mathscr{C}"></span>本身也构成<span data-katex="X"></span>的覆盖，就称<span data-katex="\mathscr{A}"></span>是<span data-katex="\mathscr{C}"></span>的子覆盖。

<br/>

**粘接引理**

设<span data-katex="\{A_1,A_2,\cdots ,A_n\}"></span>是<span data-katex="X"></span>的一个有限闭覆盖，

如果映射<span data-katex="f:X\rightarrow Y"></span>在每个<span data-katex="A_i"></span>上的限制都是连续的，

则<span data-katex="f"></span>是连续的。

<br/>

**紧致性**

拓扑空间称为紧致的，如果它的每个开覆盖都有有限的子覆盖。

<br/>

可证，紧致<span data-katex="C_1"></span>空间是列紧的，列紧度量空间是紧致的。

对于度量空间来说，它是列紧的当且仅当它是紧致的。

<br/>

**紧致子集**

一个拓扑空间<span data-katex="X"></span>的子集<span data-katex="A"></span>如果作为子空间是紧致的，就称为<span data-katex="X"></span>的紧致子集。

可证，紧致空间的闭子集紧致，紧致空间在连续映射下的像也紧致。

定义在紧致空间上的连续函数有界。

<br/>

## **连通性和道路连通性**

**连通性**

拓扑空间<span data-katex="X"></span>称为连通的，如果它不能分解为两个非空不想交开集的并。

<br/>

连通空间在连续映射下的像也是连通的。

连通空间上的连续函数可以取到一切中间值。

<br/>

若<span data-katex="X"></span>有一个连通的稠密子集，则<span data-katex="X"></span>连通。

<br/>

如果<span data-katex="X"></span>有一个连通覆盖<span data-katex="\mathscr{C}"></span>（<span data-katex="\mathscr{C}"></span>中每个成员都连通），

并且<span data-katex="X"></span>有一个连通子集<span data-katex="A"></span>它与<span data-katex="\mathscr{C}"></span>中每个成员都相交，

则<span data-katex="X"></span>连通。

<br/>

**道路连通性**

设<span data-katex="X"></span>是拓扑空间，从单位闭区间<span data-katex="I=[0,1]"></span>到<span data-katex="X"></span>的一个连续映射<span data-katex="a:I\rightarrow X"></span>，

称为<span data-katex="X"></span>上的一条道路。

把点<span data-katex="a(0)"></span>和<span data-katex="a(1)"></span>分别称为<span data-katex="a"></span>的起点和终点，统称为端点。

<br/>

道路是指映射本身，而不是它的像集。

事实上，可能有许多不同道路，它们的像集完全相同。

<br/>

如果道路<span data-katex="a:I\rightarrow X"></span>是常值映射，即<span data-katex="a(I)"></span>是一个点，就称为点道路。

点道路完全被像点<span data-katex="x"></span>决定，记为<span data-katex="e_x"></span>。

起点与终点重合的道路，称为闭路，点道路是一个闭路。

<br/>

道路有两种运算：逆和乘积。

一条道路<span data-katex="a:I\rightarrow X"></span>的逆也是<span data-katex="X"></span>上的道路，记作<span data-katex="\overline{a}"></span>，

规定<span data-katex="\overline{a}(t)=a(1-t)"></span>，<span data-katex="\forall t\in I"></span>。

<span data-katex="X"></span>上的两条道路<span data-katex="a"></span>与<span data-katex="b"></span>，如果满足<span data-katex="a(1)=b(0)"></span>，则可规定它们的乘积<span data-katex="ab"></span>，

它也是<span data-katex="X"></span>上的道路，规定为

<span data-katex="ab(t)=a(2t)"></span>，<span data-katex="0\leqslant t\leqslant 1/2"></span>

<span data-katex="ab(t)=b(2t-1)"></span>，<span data-katex="1/2\leqslant t\leqslant 1"></span>

由粘接引理可证，<span data-katex="ab"></span>是连续的。

<br/>

拓扑空间<span data-katex="X"></span>称为道路连通的，

如果<span data-katex="\forall x,y\in X"></span>存在<span data-katex="X"></span>中分别以<span data-katex="x"></span>和<span data-katex="y"></span>为起点和终点的道路。

<br/>

道路连通空间一定连通。

道路连通空间在连续映射下的像也是道路连通的。

<br/>

## **商空间**

**商拓扑**

一般的，一个集合<span data-katex="X"></span>上如果有等价关系<span data-katex="\sim"></span>，相应的等价类的集合记作<span data-katex="X/\sim"></span>，

称为<span data-katex="X"></span>关于<span data-katex="\sim"></span>的商集。

把<span data-katex="X"></span>上的点，对应到它所在的等价类，得到映射<span data-katex="p:X\rightarrow X/\sim"></span>，称为粘合映射。

<br/>

设<span data-katex="(X,\tau)"></span>是一个拓扑空间，<span data-katex="\sim"></span>是集合<span data-katex="X"></span>上的一个等价关系，

规定商集<span data-katex="X/\sim"></span>上的子集族

<span data-katex="\tilde{\tau}:=\{V\subseteq X/\sim |\ p^{-1}(V)\in \tau \}"></span>

则可证<span data-katex="\tilde{\tau}"></span>是<span data-katex="X/\sim"></span>上的一个拓扑，

称为<span data-katex="\tau"></span>在<span data-katex="\sim"></span>下的商拓扑，

称<span data-katex="(X/\sim ,\tilde{\tau})"></span>是<span data-katex="(X,\tau )"></span>关于<span data-katex="\tau"></span>的商空间。

<br/>

**商映射**

设<span data-katex="X"></span>和<span data-katex="Y"></span>是拓扑空间，映射<span data-katex="f:X\rightarrow Y"></span>称为商映射，如果

（1）<span data-katex="f"></span>连续

（2）<span data-katex="f"></span>是满的

（3）设<span data-katex="B\subseteq Y"></span>，如果<span data-katex="f^{-1}(B)"></span>是<span data-katex="X"></span>的开集，则<span data-katex="B"></span>是<span data-katex="Y"></span>的开集

当<span data-katex="X/\sim"></span>是<span data-katex="X"></span>的一个商空间时，粘合映射<span data-katex="p:X\rightarrow X/\sim"></span>是一个商映射。

<br/>

任给映射<span data-katex="f:X\rightarrow Y"></span>，规定<span data-katex="X"></span>中的等价关系<span data-katex="\;{\tiny\begin{matrix} f \\ \normalsize \sim \\ ^{\scriptsize }\end{matrix}}\;"></span>如下：

<span data-katex="\forall x,x'\in X"></span>，若<span data-katex="f(x)=f(x')"></span>，就说<span data-katex="x"></span>与<span data-katex="x'"></span>在<span data-katex="\;{\tiny\begin{matrix} f \\ \normalsize \sim \\ ^{\scriptsize }\end{matrix}}\;"></span>意义下等价，

记作<span data-katex="x\;{\tiny\begin{matrix} f \\ \normalsize \sim \\ ^{\scriptsize }\end{matrix}}\;x'"></span>。

<br/>

可见，如果<span data-katex="f:X\rightarrow Y"></span>是商映射，则<span data-katex="X/\;{\tiny\begin{matrix} f \\ \normalsize \sim \\ ^{\scriptsize }\end{matrix}}\;\cong Y"></span>。

<br/>

连续的满映射<span data-katex="f:X\rightarrow Y"></span>，如果它还是开映射或闭映射，则它是商映射。

商映射的复合也是商映射。

<br/>

## **拓扑流形与闭曲面**

**拓扑流形**

一个Hausdorff空间<span data-katex="X"></span>称为<span data-katex="n"></span>维（拓扑）流形，

如果<span data-katex="X"></span>任一点都有一个同胚与<span data-katex="E^n"></span>或<span data-katex="E^n_+"></span>的开邻域。

这里<span data-katex="E^n_+"></span>是半个<span data-katex="n"></span>维欧氏空间，规定为

<span data-katex="E^n_+:=\{(x_1,x_2,\cdots ,x_n)\in E^n\ ,x_n\geqslant 0\}"></span>

<br/>

**边界点**

设<span data-katex="M"></span>是<span data-katex="n"></span>维流形，点<span data-katex="x\in M"></span>如果有同胚于<span data-katex="E^n"></span>的开邻域，

就称<span data-katex="x"></span>是<span data-katex="M"></span>的内点（注意，这里的内点与子集的内点不同），

否则就称为边界点。

全体内点的集合，称为<span data-katex="M"></span>的内部，它是<span data-katex="M"></span>的一个开集。

<br/>

**闭曲面**

二维流形称为曲面。

没有边界点的紧致连通曲面，称为闭曲面。

<br/>

## **参考**

[基础拓扑学讲义](https://book.douban.com/subject/1230382/)