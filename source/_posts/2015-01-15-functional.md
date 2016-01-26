---
layout: post
categories: Experience
title: 回调函数
description: 嵌套函数层数太深，不是函数式语言的弊端，而是程序员使用的方式不对。
---

人们对函数式编程的认识，一般分为两种：

函数的参数可以是另一个函数。

函数的调用没有副作用。

<br/>

副作用，这是关于引用透明性的。

即任何表达式所出现的位置上，都可以用它的值来替换。

而不产生其他影响。

<br/>

本文不讨论这个话题。

我们要讨论的是关于回调函数的。

如果函数的参数可以是另一个函数，这将是一个有趣的事情。

<br/>

## **闭包**

先写几个简单的小函数，来练练手。

看看函数是怎样作为参数传递的。

我们先定义一个高阶函数，它接受一个数字和函数作为参数。

(define (high-order-fn data fn)

&nbsp;&nbsp;(fn data))

<br/>

我们使用数字1和函数“-”作为参数调用高阶函数。

(high-order-fn 1 -)

=> -1

<br/>

我们使用数字1和一个lambda表达式作为参数调用。

(high-order-fn 1

&nbsp;&nbsp;(lambda (x) (+ 1 x)))
  
=> 2

<br/>

我们看一下函数的副作用。

(high-order-fn 1

&nbsp;&nbsp;(lambda (x) (display x)))
  
=> 1

<br/>

看起来很简单，不是吗。

只是需要注意，前两个例子的输出是表达式的值，

最后一个是表达式的副作用。

<br/>

那么，请猜一下：

(high-order-fn 1

&nbsp;&nbsp;(let [(y 2)]
  
&nbsp;&nbsp;&nbsp;&nbsp;(lambda (x) (+ x y))))

<br/>

答案是3，因为这看起来十分“显然”。

然而，这里面却藏有玄机。

<br/>

我们变换一下，将函数参数拿出来，并把它放在一个let表达式下面。

(define callback

&nbsp;&nbsp;(let [(y 2)]
  
&nbsp;&nbsp;&nbsp;&nbsp;(lambda (x) (+ x y))))

<br/>

然后我们用let表达式把高阶函数的调用包装起来。

(let ([y 3])

&nbsp;&nbsp;(high-order-fn 1 callback))
  
=> 3

<br/>

我们用另一个表达式把高阶函数的调用包装起来。

(let ([y 4])
 
&nbsp;&nbsp;(high-order-fn 1 callback))
  
=> 3

<br/>

这两个结果是相同的，

这说明callback函数体中(+ x y)里面，y是不会受外界影响的。

求值(+ x y)时，我们只会找在文本范围上最近的y。

<br/>

如果变量具有这样的查找方式，我们就称这个变量具有词法作用域，也叫静态作用域。

与之相应的还有动态作用域，这里就先不详细介绍了。

<br/>

再看上面的例子，(high-order-fn 1 callback)表达式中的callback，

它是一个函数，不太准确的可以表示为(lambda (x) (+ x y))。

重要的是，callback是带着自己定义时的环境传递给high-order-fn的，

以至于在调用callback的时候，y还是可以从callback携带的环境中找到y值。

<br/>

于是，专业一点的话，我们把函数体加上函数定义时的环境，称为一个闭包。

因为闭包包含了环境，所以是具有内部状态的。

如果你有面向对象的经验的话，可能瞬间会想到，这不就是一个“对象”吗？

<br/>

确实如此，一个闭包就是一个对象。

而在面向对象语言中，我们却要先建立一个类，再new一个对象。

如果想了解更多的对应关系，可以参考《Let Over Lambda》。

<br/>

## **嵌套回调函数**

很多人说自己讨厌函数式编程，

一方面，可能是觉得闭包太抽象了。

另一方面，可能是听别人说嵌套回调函数会导致逻辑混乱。

<br/>

让我们先看一下回调函数到底能混乱到什么程度吧。

为了不引入其他语言擅长的场景，这里虚构了一个小问题。

<br/>

首先，我们定义了一个小函数，它接受一个数字和两个函数作为参数。

如果数字小于10，就执行success回调函数，否则就执行error回调函数。

(define (async-task data success error)

&nbsp;&nbsp;(if (< data 10)
  
&nbsp;&nbsp;&nbsp;&nbsp;(success data)
	
&nbsp;&nbsp;&nbsp;&nbsp;(error)))

<br/>

然后，我们拿数字1来试试，并设置success显示这个值，success显示-1。

(async-task 1

&nbsp;&nbsp;(lambda (x) (display (list x)))
  
&nbsp;&nbsp;(lambda () (display -1)))
  
=> (1)

<br/>

现在，我们想这么做，

如果第一个async-task执行成功，我们就再拿2调用，

否则就直接失败(display -1)。

为了这样做，我们只能把async-task的第二次调用写在success回调函数中。

(async-task 1

&nbsp;&nbsp;(lambda (x) 
  
&nbsp;&nbsp;&nbsp;&nbsp;(async-task 2 
	
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda (y) (display (list x y))) 
	  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda () (display -1))))
	  
&nbsp;&nbsp;&nbsp;&nbsp;(lambda () (display -1)))
	
=> (1 2)

<br/>

逻辑也不是太混乱，可以理顺它。

别忙，紧接着我们要在第二次async-task执行成功后，再拿3调用。

那我们再写一层吧。

(async-task 1

&nbsp;&nbsp;(lambda (x)
  
&nbsp;&nbsp;&nbsp;&nbsp;(async-task 2
	
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda (y)
	  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(async-task 3
		
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda (z) (display (list x y z)))
		  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda () (display -1))))
		  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda () (display -1))))
	  
&nbsp;&nbsp;(lambda () (display -1)))

=> (1 2 3)

<br/>

看吧，人们所说的嵌套回调函数问题果然出现了。

一眼望去，根本不知道在做什么。

这样下去，我们想继续拿4,5...9,10调用，那要写多少层呀。

<br/>

很多人看到这里就断定，函数式编程是反人类的。

这也是情有可原的。

<br/>

让我们静一静，把急躁的心平静下来。

想一想有没有好的办法，而不是一味的抱怨。

<br/>

仔细观察后，我们发现，async-task这个函数我们会频繁调用。

我们何不写一个辅助函数来协助完成这件事呢？

于是，我们定义了一个辅助函数execute，这个函数实现很简单，暂可以略过，

我们只需要看看它怎么使用的。

(define (execute value-list fn)

&nbsp;&nbsp;(define (continue value-list result-list)
  
&nbsp;&nbsp;&nbsp;&nbsp;(fn value-list result-list continue))
	
&nbsp;&nbsp;(continue value-list '()))

<br/>

使用execute函数之后，我们就可以这样写了。

(execute '(1 2 3 4 5 6 7 8 9)

&nbsp;&nbsp;(lambda (values results continue)
  
&nbsp;&nbsp;&nbsp;&nbsp;(if (not (null? values))
	
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(async-task (car values)
	  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda (x)
		
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(let [(remainders (cdr values))]
		  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(set! results (append results (list x)))
			
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(if (null? remainders)
			
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(display results)
			  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(continue remainders results))))
			  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda () (display -1))))))
		
=> (1 2 3 4 5 6 7 8 9)

<br/>

太好了，我们就不用写很多层嵌套回调函数了。

这个函数的关键点是，我们巧妙的利用了一个continue函数，

用来循环执行同样的async-task。

<br/>

至于函数的解释，限于篇幅，还是请直接阅读源代码吧。

因为，源代码是最好的对计算过程的描述方式。

<br/>

## **总结**

以上回调函数的例子，

源于某次讨论中，有人问及如何连续使用JavaScript发送AJAX请求。

先有一些聪明的人说，async这个库实现了，去看吧。

再有一些大牛的人说，为了避免嵌套回调函数，还是用Promise来实现吧。

最后，有一些有工作经验的人总结道，函数式编程很垃圾。

<br/>

我那时忽然感到很惶恐，

是不是我眼中的世界太美好，使我看不到黑暗了？

于是，我就简单实现了一个AJAX控制器，消除了嵌套循环。

当然，实现方式不止我这一种。

我才发现，不是工具本身的问题，而是有些人不会制造工具来解决问题。

<br/>

我也明白了，一个人的盲区就在于那些他瞧不起的事物。

一旦有了成见，就很难再突破自我的限制了。

<br/>

对于这个问题来说，在Haskell中有类似的概念，称为monad。

它指定了一个函数“>>=”，将各个回调函数串联起来，用来实现顺序操作。

有兴趣的可以了解一下。

另外，我是不会告诉你，这种实现方式是和continuation有联系的，哈哈。

<br/>

正所谓，不识庐山真面目，只缘身在此山中。

不过，也可能那些聪明的/大牛们/有工作经验的人，说的是对的。

我不会限制自己，我还要继续努力来达到更高的水平。
