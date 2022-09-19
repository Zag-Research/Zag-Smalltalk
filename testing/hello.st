"
: Sep18 ; time ./run-test st hello.st 3>hello.sample 
: Sep18 ; tr <hello.sample -d '[*]' >hello.sample2
: Sep18 ; for i in I C F n t f;do echo $i `tr <hello.sample2 -d "[$i]"|wc -c`;done
: Sep18 ; ls -l hello.s*
"
{true not.
false not.
nil isNil}.
(Smalltalk at: #Transcript) show: 'hello world!!';cr.
