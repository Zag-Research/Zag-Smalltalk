Class {
	#name : #PjRbBenchmark,
	#superclass : #PjRbObject,
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
	#category : #'Zag-Benchmarking-Richards'
}

{ #category : #'instance creation' }
PjRbBenchmark class >> report: string times: count run: aBlock [
	<pharoJsSkip>
        "Report the time required to execute the given block."

        | time |
        time := 0.
	count timesRepeat: [ time := time + aBlock value ].
        Transcript show: string, ' ', (time // count) printString, ' milliseconds'; cr.
]

{ #category : #'instance creation' }
PjRbBenchmark class >> standardBenchmark [
	<pharoJsSkip>
        "This the combined benchmark."
        "PjRbBenchmark standardBenchmark"

        self report: 'Richards simulations' times: 100000 run: [ self start ]
]

{ #category : #'instance creation' }
PjRbBenchmark class >> start [
    "RichardsBenchmark start"

    ^self new start
]

{ #category : #'instance creation' }
PjRbBenchmark class >> start: trace [
    "RichardsBenchmark start: true"

    ^self new start: trace
]

{ #category : #creation }
PjRbBenchmark >> createDevice: identity priority: priority work: workX state: state [ 
    | dataX |
    dataX := PjRbDeviceTaskDataRecord create.
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
PjRbBenchmark >> createHandler: identity priority: priority work: workX state: state [ 
    | dataX |
    dataX := PjRbHandlerTaskDataRecord create.
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
PjRbBenchmark >> createIdler: identity priority: priority work: workX state: state [ 
    | dataX |
    dataX := PjRbIdleTaskDataRecord create.
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
PjRbBenchmark >> createPacket: link identity: identity kind: kind [ 
    ^PjRbPacket
	create: link
	identity: identity
	kind: kind
]

{ #category : #creation }
PjRbBenchmark >> createTask: identity priority: priority work: work state: state function: aBlock data: data [ 
    | t |
    t := PjRbTaskControlBlock
		link: taskList
		create: identity
		priority: priority
		initialWorkQueue: work
		initialState: state
		function: aBlock
		privateData: data.
    taskList := t.
    taskTable at: identity put: t
]

{ #category : #creation }
PjRbBenchmark >> createWorker: identity priority: priority work: workX state: state [ 
    | dataX |
    dataX := PjRbWorkerTaskDataRecord create.
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
PjRbBenchmark >> findTask: identity [ 
    | t |
    t := taskTable at: identity.
    t isNil ifTrue: [self error: 'findTask failed'].
    ^t
]

{ #category : #private }
PjRbBenchmark >> holdSelf [
    holdCount := holdCount + 1.
    currentTask taskHolding: true.
    ^currentTask link
]

{ #category : #private }
PjRbBenchmark >> initScheduler [
    queuePacketCount := holdCount := 0.
    taskTable := Array new: 6.
    taskList := nil
]

{ #category : #private }
PjRbBenchmark >> initTrace: trace [
    tracing := trace.
    layout := 0
]

{ #category : #private }
PjRbBenchmark >> queuePacket: packet [ 
    | t |
    t := self findTask: packet identity.
    t isNil ifTrue: [^nil].
    queuePacketCount := queuePacketCount + 1.
    packet link: nil.
    packet identity: currentTaskIdentity.
    ^t addInput: packet checkPriority: currentTask
]

{ #category : #private }
PjRbBenchmark >> release: identity [ 
    | t |
    t := self findTask: identity.
    t isNil ifTrue: [^nil].
    t taskHolding: false.
    t priority > currentTask priority
	ifTrue: [^t]
	ifFalse: [^currentTask]
]

{ #category : #scheduling }
PjRbBenchmark >> schedule [
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

{ #category : #initialization }
PjRbBenchmark >> start [
    ^self start: false
]

{ #category : #initialization }
PjRbBenchmark >> start: trace [
    | workQ mark1 mark2 mark3 mark4 |
    self initTrace: trace; initScheduler.
    mark1 := Time millisecondClockValue.
    tracing ifTrue: [self show: 'Bench mark starting'; cr].
    self
	createIdler: Idler
	priority: 0
	work: nil
	state: PjRbTaskState running.
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
	state: PjRbTaskState waitingWithPacket.
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
	state: PjRbTaskState waitingWithPacket.
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
	state: PjRbTaskState waitingWithPacket.
    self
	createDevice: DeviceA
	priority: 4000
	work: nil
	state: PjRbTaskState waiting.
    self
	createDevice: DeviceB
	priority: 5000
	work: nil
	state: PjRbTaskState waiting.
    tracing ifTrue: [self show: 'Starting'; cr].
    mark2 := Time millisecondClockValue.
    self schedule.
    mark3 := Time millisecondClockValue.
    tracing
	ifTrue: 
	    [self show: 'Finished'; cr.
	    self show: 'QueuePacket count = ', queuePacketCount printString;
		show: ' HoldCount = ', holdCount printString;
		cr.
	    self cr; show: 'End of run'; cr].
    queuePacketCount = 23246 & (holdCount = 9297) ifFalse: [self error: 'wrong result'].
    mark4 := Time millisecondClockValue.
    tracing
	ifTrue:
	    [self show: '***Scheduler time = ', (mark3 - mark2) printString;
	        show: ' Total time = ', (mark4 - mark1) printString;
	        cr].

  ^mark4 - mark1
]

{ #category : #private }
PjRbBenchmark >> trace: id [ 
    layout := layout - 1.
    0 >= layout
	ifTrue: 
	    [self cr.
	    layout := 50].
    self show: id printString
]

{ #category : #private }
PjRbBenchmark >> wait [ 
    currentTask taskWaiting: true.
    ^currentTask
]
