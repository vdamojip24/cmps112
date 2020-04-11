#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f


"Partner: Marian Vladoi (mvladoi@ucsc.edu)"
"Partner: Nicholas Pappas (nhpappas@ucsc.edu"
"Assignment 3 - hzip.st"
"CMPS-112"

execname := '$RCSfile: cmdline.st,v $' =~ '.*: (.*),v.*' at: 1.
nl := Character nl.





"Frequency table array object."
Object subclass: ZeroArray [
    |theArray|

    ZeroArray class >> new [
        self shouldNotImplement.
    ]

    ZeroArray class >> new: size [
        |result|
        result := super new.
        result init: size.
        ^ result
    ]

    init: size [
        theArray := Array new: size.
    ]

    size [
        ^ theArray size.
    ]

    at: index [
        ^ theArray at: index + 1.
    ]

    at: index put: value [
        ^ theArray at: index + 1 put: value.
    ]

    incr: index [
        (self at: index) isNil
         ifTrue: [ self at: index put: 0].
        self at: index put: (self at: index) + 1.
    ]

    keysAndValuesDo: aBlock [
        (0 to: (self size) - 1) do: [:index |
            aBlock value: index value: (self at: index).
        ]
    ]
]

"Leaf object."
Object subclass: Leaf [
    |char count|
    char [ ^ char ]
    count [ ^ count ]

    Leaf class >> new [
        self shouldNotImplement
    ]

    Leaf class >> new: aChar count: aCount [
        |result|
        result := super new.
        result setChar: aChar andCount: aCount.
        ^result
    ]

    setChar: aChar andCount: aCount [
        char := aChar.
        count := aCount.
    ]

    <= other [
        ^ (count < other count)
        | ((count = other count) & (char <= other char))
    ]

    printBase: aStream [
        ^ aStream << self class << '(' << char << ',' << count
    ]

    printOn: aStream [
        (self printBase: aStream) << ')'.
    ]

    inorder: visitor prefix: string [
        visitor value: char value: string.
    ]

    writeIn: outfile [
        outfile writeBit: (0 bitAt: 1).
        outfile charBit: self char.
    ]
]

"Tree object."
Leaf subclass: Tree [
    |left right|
    left [ ^ left ]
    right [ ^ right ]

    Tree class >> new: aChar count: aCount [
        self shouldNotImplement
    ]

    Tree class >> new: aChar count: aCount left: aLeft right: aRight [
        |result|
        result := super new: aChar count: aCount.
        result setLeft: aLeft andRight: aRight.
        ^ result
    ]

    setLeft: aLeft andRight: aRight [
        left := aLeft.
        right := aRight.
    ]

    printOn: aStream [
        (self printBase: aStream) << ',' << left << ',' << right << ')'.
    ]

    inorder: visitor prefix: string [
        left inorder: visitor prefix: string, '0'.
        right inorder: visitor prefix: string, '1'.
    ]

    writeIn: outfile [
        left writeIn: outfile.
        right writeIn: outfile.
        outfile writeBit: (1 bitAt: 1).
    ]
]

"BitStream class to handle IO"
Object subclass: BitStream [
   |bitIndex currentByte myStream|
   
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
   ]
  
   flushByte [
        bitIndex = 8 ifFalse: [
            myStream nextPut: (Character value: currentByte asInteger).
            bitIndex := 8.
            currentByte := 0.
        ]
    ]

    writeBit: bit [
        currentByte isNil ifTrue: 
        [bitIndex := 8. currentByte := 0.].
        bitIndex = 0 ifTrue: [self flushByte].
        currentByte := currentByte bitAt: bitIndex put: bit.
        bitIndex := bitIndex - 1.
    ]
    
    clearBits [
        bitIndex := 0.
    ]


    nextBit [
        |bit|
        bitIndex isNil ifTrue: [self clearBits].
        bitIndex = 0 ifTrue: 
        [currentByte := myStream next. bitIndex := 8.].
        currentByte isNil ifTrue: [ ^ nil ].
        bit := currentByte value bitAt: bitIndex.
        bitIndex := bitIndex - 1.
        ^ bit.
    ]

    charBit: char [
        |bit charVal bitIndex|
        charVal := char value.
        bitIndex := 8.
        [bitIndex ~= 0] whileTrue: [
            bit := char value bitAt: bitIndex.
            bitIndex := bitIndex - 1.
            self writeBit: bit.
        ].
        (charVal = 0) | (charVal = 256) ifTrue: [
            self writeBit: (charVal bitAt: 9).
        ].
    ]

   
   
   
]

"Extend the Character to output the character per project description"
Character extend [
    asBit [
        ^ self value bitAt: 1.
    ]

    isPrint [
        ^ ($! <= self) & (self <= $~)
    ]
    
     digits[
         ^ (self asInteger) > 15
         ]

visible [
        
      self isPrint 
      ifTrue: [^' ', self asString, ' ']             
      ifFalse: [ self digits
             ifTrue:[((self asInteger) = 256)
                    ifTrue:[ ^ 'EOF']ifFalse:[^ 'x',
                   ((self asInteger printStringRadix: 16)
                   copyFrom: 4 )]
                             ]
             ifFalse:[  ^ 'x0',
                        ((self asInteger printStringRadix: 16)
                         copyFrom: 4)]
                     ]           
   ]
]



d := false.
c := false.
t := false.
u := false.
   
 
argc:=0.
Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |

   (opt = $d) ifTrue: [ d := true. ].
   (opt = $t) ifTrue: [ t := true. ].
   (opt = $c) ifTrue: [ c := true. ].
   (opt = $u) ifTrue: [ u := true. ].
].  
   

"Set program arguments."
filenameIn := (Smalltalk getArgv: 2).
filenameOut := (Smalltalk getArgv: 3).

"Check program usage."
Smalltalk arguments size < 2
ifTrue: [ 
    stderr << 'Please use this optons:' << execname 
    << ' -dtcu inputfile [outputfile]' << nl.
    ObjectMemory quit.
].

"Create a new ZeroArray and count the frequency of characters."
freqtable := ZeroArray new: 257.
        |infile char|
        [infile := FileStream open: filenameIn
                            mode: FileStream read.
        [infile atEnd] whileFalse: [
            char := infile next asInteger.
            freqtable incr: char.
        ].
        char = nil ifFalse: [
            freqtable incr: 256.
        ].
        infile close.
        ] on: SystemExceptions.FileError do: [:signal |
        |errno|
        errno := File errno.
        stdout flush.
        stderr << execname << ': ' << filenameOut << ': '
              << signal messageText << nl.
        stderr flush.
    ].
        
"From the Frequency Table load the Priority Queue."
    pqueue := SortedCollection new.
    freqtable keysAndValuesDo: [:key :count |
        (count notNil and: [count > 0]) ifTrue: [
            pqueue add:(Leaf new: (key asCharacter) count: count).
        ]
    ].
    
"Unload priority queue into Huffman tree."
    [pqueue notEmpty] whileTrue: [
        tree := pqueue removeFirst.
        pqueue size = 0 ifTrue: [^ tree].
        rightChild := pqueue removeFirst.
        pqueue add: (Tree new: tree char 
        count: (tree count + rightChild count)
        left: tree right: rightChild).
    ].
    
"Traverse Huffman tree into encoding array."
    encodedCharacterdArray := ZeroArray new: 257.
    tree = nil ifFalse: [
        tree inorder: [:char :encoding |
            encodedCharacterdArray at: char asInteger put: encoding.
        ] prefix: ''.
    ].

"Debugging actions"
d ifTrue: [
     pqueue inspect.
     stdout << tree << nl.
].

"Print out frequency and encoding table."
t ifTrue: [
    freqtable keysAndValuesDo: [:key :value |
        (value notNil and: [value > 0]) ifTrue: [
             |character|
             character := (key asCharacter visible).
             character asString size to: 3 do: [:skip| stdout].
             stdout << character << ' '. 
             value asString size to: 6 do: [:skip| stdout << ' '].
             stdout << value << '  '.
             stdout << (encodedCharacterdArray at: key) << nl.
        ]
    ]
].

"Compress input file to the output file."
c ifTrue: [
    [   |infile outfile encodedCharacter outBits|
        infile := FileStream open: filenameIn
                            mode: FileStream read.
        filenameOut = nil
        ifTrue: [
            outfile := stdout.
        ]
        ifFalse: [
            outfile := FileStream open: filenameOut
                                mode: FileStream write.
        ].
        outBits := BitStream new: outfile.
        tree = nil ifFalse: [
            tree writeIn: outBits.
            outBits writeBit: (1 bitAt: 1).
            outBits flushByte.
            
        [infile atEnd not] whileTrue: [
            encodedCharacter := encodedCharacterdArray at: infile next value.
            encodedCharacter do: [:char |
                outBits writeBit: char asBit.
            ].
        ].
        encodedCharacter := encodedCharacterdArray at: 256.
        encodedCharacter do: [:char |
            outBits writeBit: char asBit.
        ].
        outBits flushByte.  
            
        ].
        outfile close.
        infile close.
    ] on: SystemExceptions.FileError do: [:signal |
        |errno|
        errno := File errno.
        stdout flush.
        stderr << execname << ': ' << filenameOut << ': '
              << signal messageText << nl.
        stderr flush.
    ].  
].

"Decompress input file to the output file."
u ifTrue: [
    [   |mybit myTree infile outfile stack inBits bl|
        infile := FileStream open: filenameIn
                            mode: FileStream read.
                                
        inBits := BitStream new: infile.
        stack := OrderedCollection new.
        bl:= true.
        
        [(infile atEnd not) & (bl == true)] whileTrue: [
            |preBit|
            preBit := inBits nextBit.
            preBit = 0 ifTrue: [
                |bit charBit bitIndex newLeaf|
                charBit := 0.
                bitIndex := 8.
                [bitIndex ~= 0] whileTrue: [
                   charBit := charBit bitAt: bitIndex put: inBits nextBit.
                   bitIndex := bitIndex - 1.
                ].
                newLeaf := Leaf new: (charBit asCharacter) count: 0.
                charBit = 0 ifTrue: [
                    bit := inBits nextBit.
                    bit = 1 ifTrue: [
                        newLeaf := Leaf new: 256 count: 0.
                    ].
                ].
                stack addFirst: newLeaf.
            ].
            preBit = 1 ifTrue: [
                |leftChild rightChild newTree|
                rightChild := stack removeFirst.
                stack size = 0 ifTrue: [
                    inBits clearBits.
                    stack addFirst: rightChild.
                    bl := false.
                ].
                bl ifTrue: [leftChild := stack removeFirst.
                newTree := Tree new: leftChild char 
                count: 0 left: leftChild right: rightChild.
                stack addFirst: newTree.
                ].
            ].
        ].
        (stack size ~= 0) ifTrue:[tree := stack removeFirst].
        tree = nil ifFalse: [
            filenameOut = nil
            ifTrue: [
                outfile := stdout.
            ]
            ifFalse: [
                outfile := FileStream open: filenameOut
                                    mode: FileStream write.
            ].      
        myTree := tree.
        mybit := 0.
        [mybit ~= nil] whileTrue: [
            mybit := inBits nextBit.
            mybit = 0 ifTrue: [
                myTree := myTree left.
            ].
            mybit = 1 ifTrue: [
                myTree := myTree right.
            ].
            (myTree isMemberOf: Leaf) ifTrue: [
                |char|
                myTree char = 256 ifTrue: [ ^ char ].
                char := Character value: myTree char asInteger.
                outfile nextPut: char.
                myTree := tree.
            ].
        ].
        
            outfile close.
            
        ].
        infile close.   
    ] on: SystemExceptions.FileError do: [:signal |
        |errno|
        errno := File errno.
        stdout flush.
        stderr << execname << ': ' << filenameOut << ': '
              << signal messageText << nl.
        stderr flush.
    ].  
].
