Class {
	#name : #BeRichardsBenchmark,
	#superclass : #BeRiObject,
	#instVars : [
		'taskList',
		'currentTask',
		'currentTaskIdentity',
		'taskTable',
		'tracing',
		'layout',
		'queuePacketCount',
		'holdCount'
	],
	#category : #'Benchmarks-Richards'
}

{ #category : #'instance creation' }
BeRichardsBenchmark class >> report: string times: count run: aBlock [
        "Report the time required to execute the given block."

        | time |
        time := 0.
	count timesRepeat: [ time := time + aBlock value ].
        Transcript show: string, ' ', (time // count) printString, ' milliseconds'; cr.
]

{ #category : #'instance creation' }
BeRichardsBenchmark class >> standardBenchmark [
        "This the combined benchmark."
        "Planner standardBenchmark"

        self report: 'Richards simulations' times: 100 run: [ self start ]
]

{ #category : #'instance creation' }
BeRichardsBenchmark class >> start [
    "BeRichardsBenchmark start"

    ^self new start
]

{ #category : #'instance creation' }
BeRichardsBenchmark class >> start: trace [
    "BeRichardsBenchmark start: true"

    ^self new start: trace
]

{ #category : #creation }
BeRichardsBenchmark >> createDevice: identity priority: priority work: workX state: state [ 
    | dataX |
    dataX := BeRiDeviceTaskDataRecord create.
    self
	createTask: identity
	priority: priority
	work: workX
	state: state
	function: 
	    [:work :word | | data functionWork |
	    data := word.
	    functionWork := work.
	    functionWork isNil
		ifTrue:
		    [(functionWork := data pending) isNil
			ifTrue: [self wait]
			ifFalse: 
			    [data pending: nil.
			    self queuePacket: functionWork]]
		ifFalse: 
		    [data pending: functionWork.
		    tracing
			ifTrue: 
			    [self trace: functionWork datum].
		    self holdSelf]]
	data: dataX
]

{ #category : #creation }
BeRichardsBenchmark >> createHandler: identity priority: priority work: workX state: state [ 
    | dataX |
    dataX := BeRiHandlerTaskDataRecord create.
    self
	createTask: identity
	priority: priority
	work: workX
	state: state
	function: 
	    [:work :word | | data workPacket count devicePacket |
	    data := word.
	    work isNil
		ifFalse: [WorkPacketKind == work kind 
		    ifTrue: [data workInAdd: work]
		    ifFalse: [data deviceInAdd: work]].
	    (workPacket := data workIn) isNil
		ifTrue: [self wait]
		ifFalse: 
		    [count := workPacket datum.
		    count > 4
			ifTrue: 
			    [data workIn: workPacket link.
			    self queuePacket: workPacket]
			ifFalse:
			    [(devicePacket := data deviceIn) isNil
				ifTrue: [self wait]
				ifFalse: 
				    [data deviceIn: devicePacket link.
				    devicePacket datum: (workPacket data at: count).
				    workPacket datum: count + 1.
				    self queuePacket: devicePacket]]]]
	data: dataX
]

{ #category : #creation }
BeRichardsBenchmark >> createIdler: identity priority: priority work: workX state: state [ 
    | dataX |
    dataX := BeRiIdleTaskDataRecord create.
    self
	createTask: identity
	priority: priority
	work: workX
	state: state
	function: 
	    [:work :word | | data |
	    data := word.
	    data count: data count - 1.
	    0 = data count
		ifTrue: [self holdSelf]
		ifFalse:
		    [0 = (data control bitAnd: 1)
			ifTrue: 
			    [data control: data control // 2.
			    self release: DeviceA]
			ifFalse: 
			    [data control: (data control // 2 bitXor: 53256).
			    self release: DeviceB]]]
	data: dataX
]

{ #category : #creation }
BeRichardsBenchmark >> createPacket: link identity: identity kind: kind [ 
    ^BeRiPacket
	create: link
	identity: identity
	kind: kind
]

{ #category : #creation }
BeRichardsBenchmark >> createTask: identity priority: priority work: workX state: state function: aBlock data: data [ 
    | t |
    t := BeRiTaskControlBlock
		link: taskList
		create: identity
		priority: priority
		initialWorkQueue: workX
		initialState: state
		function: aBlock
		privateData: data.
    taskList := t.
    taskTable at: identity put: t
]

{ #category : #creation }
BeRichardsBenchmark >> createWorker: identity priority: priority work: workX state: state [ 
    | dataX |
    dataX := BeRiWorkerTaskDataRecord create.
    self
	createTask: identity
	priority: priority
	work: workX
	state: state
	function: 
	    [:work :word | | data |
	    data := word.
	    work isNil
		ifTrue: [self wait]
		ifFalse: 
		    [data destination: (HandlerA = data destination
			    ifTrue: [HandlerB]
			    ifFalse: [HandlerA]).
		    work identity: data destination.
		    work datum: 1.
		    1 to: 4 do: 
			[:i | 
			data count: data count + 1.
			data count > 26 ifTrue: [data count: 1].
			work data at: i put: $A asInteger + data count - 1].
		    self queuePacket: work]]
	data: dataX
]

{ #category : #private }
BeRichardsBenchmark >> findTask: identity [ 
    | t |
    t := taskTable at: identity.
    t isNil ifTrue: [self error: 'findTask failed'].
    ^t
]

{ #category : #private }
BeRichardsBenchmark >> holdSelf [
    holdCount := holdCount + 1.
    currentTask taskHolding: true.
    ^currentTask link
]

{ #category : #private }
BeRichardsBenchmark >> initScheduler [
    queuePacketCount := holdCount := 0.
    taskTable := Array new: 6.
    taskList := nil
]

{ #category : #private }
BeRichardsBenchmark >> initTrace: trace [
    tracing := trace.
    layout := 0
]

{ #category : #private }
BeRichardsBenchmark >> queuePacket: packet [ 
    | t |
    t := self findTask: packet identity.
    t isNil ifTrue: [^nil].
    queuePacketCount := queuePacketCount + 1.
    packet link: nil.
    packet identity: currentTaskIdentity.
    ^t addInput: packet checkPriority: currentTask
]

{ #category : #private }
BeRichardsBenchmark >> release: identity [ 
    | t |
    t := self findTask: identity.
    t isNil ifTrue: [^nil].
    t taskHolding: false.
    t priority > currentTask priority
	ifTrue: [^t]
	ifFalse: [^currentTask]
]

{ #category : #scheduling }
BeRichardsBenchmark >> schedule [
    currentTask := taskList. 
    [currentTask isNil]
	whileFalse: 
	    [currentTask isTaskHoldingOrWaiting
		ifTrue: [currentTask := currentTask link]
		ifFalse: 
		    [currentTaskIdentity := currentTask identity.
		    tracing ifTrue: [self trace: currentTaskIdentity].
		    currentTask := currentTask runTask]]
]

{ #category : #initialize }
BeRichardsBenchmark >> start [
    ^self start: false

]

{ #category : #initialize }
BeRichardsBenchmark >> start: trace [
    | workQ mark1 mark2 mark3 mark4 |
    self initTrace: trace; initScheduler.
    mark1 := Time microsecondClockValue.
    tracing ifTrue: [Transcript show: 'Bench mark starting'; cr].
    self
	createIdler: Idler
	priority: 0
	work: nil
	state: BeRiTaskState running.
    workQ := self
		createPacket: nil
		identity: Worker
		kind: WorkPacketKind.
    workQ := self
		createPacket: workQ
		identity: Worker
		kind: WorkPacketKind.
    self
	createWorker: Worker
	priority: 1000
	work: workQ
	state: BeRiTaskState waitingWithPacket.
    workQ := self
		createPacket: nil
		identity: DeviceA
		kind: DevicePacketKind.
    workQ := self
		createPacket: workQ
		identity: DeviceA
		kind: DevicePacketKind.
    workQ := self
		createPacket: workQ
		identity: DeviceA
		kind: DevicePacketKind.
    self
	createHandler: HandlerA
	priority: 2000
	work: workQ
	state: BeRiTaskState waitingWithPacket.
    workQ := self
		createPacket: nil
		identity: DeviceB
		kind: DevicePacketKind.
    workQ := self
		createPacket: workQ
		identity: DeviceB
		kind: DevicePacketKind.
    workQ := self
		createPacket: workQ
		identity: DeviceB
		kind: DevicePacketKind.
    self
	createHandler: HandlerB
	priority: 3000
	work: workQ
	state: BeRiTaskState waitingWithPacket.
    self
	createDevice: DeviceA
	priority: 4000
	work: nil
	state: BeRiTaskState waiting.
    self
	createDevice: DeviceB
	priority: 5000
	work: nil
	state: BeRiTaskState waiting.
    tracing ifTrue: [Transcript show: 'Starting'; cr].
    mark2 := Time microsecondClockValue.
    self schedule.
    mark3 := Time microsecondClockValue.
    tracing
	ifTrue: 
	    [Transcript show: 'Finished'; cr.
	    Transcript show: 'QueuePacket count = ' ;
		show: queuePacketCount printString;
		show: ' HoldCount = ';
		show: holdCount printString;
		cr.
	    Transcript cr; show: 'End of run'; cr].
    queuePacketCount = 23246 & (holdCount = 9297) ifFalse: [self error: 'wrong result'].
    mark4 := Time millisecondClockValue.
    tracing
	ifTrue:
	    [Transcript show: '***Scheduler time = ';
	        show: (mark3 - mark2) printString;
	        show: ' Total time = ';
	        show: (mark4 - mark1) printString;
	        cr].

  ^mark3 - mark2
]

{ #category : #private }
BeRichardsBenchmark >> trace: id [ 
    layout := layout - 1.
    0 >= layout
	ifTrue: 
	    [Transcript cr.
	    layout := 50].
    Transcript show: id printString
]

{ #category : #private }
BeRichardsBenchmark >> wait [ 
    currentTask taskWaiting: true.
    ^currentTask
]
