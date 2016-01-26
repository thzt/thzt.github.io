---
layout: post
categories: Haskell
title: 函数>>=的多态性
description: Monad类型类定义的>>=函数是一个多态函数，这保证了do notaion的灵活性。
---

Haskell中一些函数可以作用在多种类型上。

例如：

ghci> length [1, 2]

2

length :: [Int] -> Int

<br/>

ghci> length ["a", "b", "c"]

3

length :: [String] -> Int

<br/>

我们看length的类型，

length :: [a] -> Int

它类型签名中，包含类型变量，类型变量可以具体化为任意类型。

<br/>

像这样的函数，称为多态函数。

<br/>

## **函数>>=**

函数>>=定义在Monad类型类中，

class Monad m where

&nbsp;&nbsp;&nbsp;&nbsp;(>>=) :: m a -> (a -> m b) -> m b

<br/>

其中，m是Monad类型类的实例类型，它的kind是* -> *，

:k m = * -> *

类型m a是一个具体类型，该类型的值称为monad value。

<br/>

我们看到，在m确定的情况下，>>=的类型签名中仍然包含类型变量。

因此，对Monad类型类的某个实例来说，

\>>=可以操作相同m类型但是不同a类型的monad value :: m a。

<br/>

## **IO monad**

以Monad类型类的实例IO为例，对于IO来说，IO monad value称为IO action。

main = do

&nbsp;&nbsp;&nbsp;&nbsp;putStrLn "Please input: "

&nbsp;&nbsp;&nbsp;&nbsp;inpStr <- getLine

&nbsp;&nbsp;&nbsp;&nbsp;putStrLn $ "Hello " ++ inpStr
	
<br/>

其中，

putStrLn :: String -> IO ( )

getLine :: IO String

<br/>

我们来分析一下这3个IO action的类型，

putStrLn "Please input: " :: IO ( )

getLine :: IO String

putStrLn $ "Hello " ++ inpStr :: IO ( )

<br/>

它们的具体类型都是m a，

m相同，m = IO。而a不同，分别是( )，String，( )。

<br/>

在do notation中，如果某一行我们没有使用<-为monad value绑定值，

就相当于使用了函数>>，表示不需要这个绑定值。

(>>) :: x >> y = x >>= \\ \_ -> y

<br/>

这样的话，我们就可以将main函数还原成>>和>>=的串联形式了，

putStrLn "Please input: " >> getLine >>= \inpStr -> putStrLn $ "Hello " ++ inpStr

= putStrLn "Please input: " >>= \\ \_ -> getLine >>= \inpStr -> putStrLn $ "Hello " ++ inpStr

= putStrLn "Please input: " >>= ( \\ \_ -> getLine >>= ( \inpStr -> putStrLn $ "Hello " ++ inpStr ) )

<br/>

## **类型不同的>>=同时出现**

对于第一个>>=，我们能推断出它的大概类型，

\>>= :: IO ( ) -> (( ) -> IO ?) -> IO ?

其中“?”表示尚未确定的类型。

<br/>

而第二个>>=的类型，可以完全确定下来。

getLine >>= \inpStr -> putStrLn $ "Hello " ++ inpStr

\>>= :: IO String -> (String -> IO ( )) -> IO ( )

<br/>

那么，第一个>>=的类型也就可以完全确定下来了，

\>>= :: IO ( ) -> (( ) -> IO ( )) -> IO ( )

<br/>

由此可见，

第一个>>= :: IO ( ) -> (( ) -> IO ( )) -> IO ( )

第二个>>= :: IO String -> (String -> IO ( )) -> IO ( )

两个>>=的类型不同，它们同时出现了。

<br/>

## **结语**

do notation是>>和>>=串联形式的语法糖，IO action是 IO monad value，因此可以使用它。

认为do notation中每行的monad value类型必须相同，是不正确的。

事实上，他们的类型是m a，m是相同的，而a可以不同。

<br/>

相似的，Functor和Applicative类型类也定义了一些多态函数。

<br/>

此外，对于IO类型，《Programming in Haskell》P88说的最清楚

type IO a = World -> (a, World)

<br/>

World类型的值表示环境的当前状态。

World -> (a, World)类型的值是一个函数，

它接受环境状态作为参数，返回一个a类型的值，并影响了环境状态。

















