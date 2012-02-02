(let ([a 1] [aa 11] [aaa 111] [aaaa 1111])
  (let ([b 2] [bb 22] [bbb 222] [bbbb 2222] [bbbbb 22222])
    (let ([c 3] [cc 33] [ccc 333] [cccc 3333] [ccccc 33333])
      (let ([d 4] [dd 44] [ddd 444] [dddd 4444] [ddddd 44444])
	(let ([e 5] [ee 55] [eee 555])
	  (begin
	    (set! bbb 20)
	    (set! dddd 40)
	    (vector a bbb c dddd e)))))))

;;; expected result: #5(1 20 3 40 5)
