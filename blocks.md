# Block in Smalltalk


2022-09-29

《Deep Into Pharo》读书笔记。

Lexically-scoped block closures 是Smalltalk中的基础，用于分支、循环等的实现。Smalltalk中分支与循环是通过Library的方式实现的，而不是内置于语言实现的。

在运行时，block 可以访问的变量被bound 在`定义它的Context`，而不是运行它的Context。



- Context: 表示程序执行点的Object。
- Block 创建时捕获变量。
- block return 改变程序flow
- 程序如何执行，context，activation records，execution state
- exception

## Basic
Block: Lambda表达式 + 创建时的environment .

想象Block 是个匿名函数或方法，一段代码，执行被冻结，并可以通过消息执行。

### 创建Block：
```smalltalk
[1 + 2]
```

### 执行一个block： （使用 value, value: 。。。）

```smalltalk
[ :x | x + 3 ] value: 2 "5"
```

### 其它执行方法：
- cull:  可以多给参数
- valueWithPossibleArgs:  可以少给参数，少的填充 nil

### 性能相关
- bench 性能测试
- durationToRun  执行时长
- timeToRun


```smalltalk
[ 100 factorial  ] bench "'175411.753 per second'"
[ 10000 factorial ] bench. "'51.487 per second'"
[ 100000 factorial ] durationToRun  "0:00:00:03.686".
```

### Error handling
- ensure: terminationBlock  无论block执行结果如何都执行 terminationBlock
- ifCurtailed: onErrorBlock block执行不成功时执行 onErrorBlock
- on: exception do: catchBlock  类似 try/catch Exception
- on: exception fork: catchBlock  fork process 来处理异常，原有的进程继续执行，返回nil. context stack transfer to forked process。 （TODO）



```smalltalk
[ 1/0 ] ensure: [ Transcript show: 'done' ].
```
执行时会弹出 debugger ，关掉后会继续执行后面的 block。

```smalltalk
[ self error: 'err' ] on: Error fork: 
   [ :ex | Transcript show: 'handle in new process' ]. "nil"
```


#### on: exception fork: catchBlock 的详细分析

### process scheduling, concurrent
- fork  create and schedule a process
- forkAt: aPriority
- newProcess  create, not schedule a process



## Variable and blocks
local variable: Block 可以有自己的临时变量，在block每次执行时初始化，并且是block local 的。

non-local variable

A block will close over the external variables it uses. It means that even if the block is executed later in an environment that does not lexically contain the variables used by a block, the block will still have access to the variables during its execution. 

self, instance variables, method temporaries and arguments 私有变量是lexically scoped。

在运行时，block 可以访问的变量被bound 在`定义它的Context (block home context)`，而不是运行它的Context。


> At runtime, the variables that a block can access, are bound (get a value associated to them) in the context in which the block that contains them is defined, rather than the context in which the block is evaluated. 

### Context 的概念
表达程序的执行。在其它语言中有称为 Stack frame, Activation Record。
表达当前的执行步骤的信息：从哪个context来的，下一个 byte code，临时变量等。


#### 实验

##### Variable lookup

```smalltalk
Bexp>>setVarAndDefineBlock

	| t |
	t := 42.
	^ self evalBlock: [ t * 2 ]

Bexp>>evalBlock: aBlock

	| t |
	t := nil.
	^ aBlock value
```

执行：

```smalltalk
Bexp new setVarAndDefineBlock . "84"
```

setVarAndDefineBlock 时创建一个 block，此时block中的 t 指向的是创建时的变量 t，值为 42 。 

evalBlock: aBlock 执行时，临时变量 t := nil ，但并不影响 block 内的 t ，因此最后结果是 84.

![Block Context](./images/deepintopharo_block_context.png)

context, sender , setVarAndDefineBlock 被执行时，创建一个 context，在里面调用 evalBlock: ，此时 evalBlock： 的 context 对象中 sender指向之前调用它的context (setVarAndDefineBlock的)。

aBlock 的 home Context 指向创建它时的 context。



