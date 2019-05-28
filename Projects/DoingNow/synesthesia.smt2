(set-logic BV)

; Sort typedefs
(define-sort bv8      (_ BitVec 8))
(define-sort bv10     (_ BitVec 10))
(define-sort bv32     (_ BitVec 32))
(define-sort bvstate  (_ BitVec 72))
(define-sort memstate Array bv32 bv8)

; Constants for flags (as a bitfield)
(define-const fmrepz #b0000000001) (define-const fmrep  #b0000000010)
(define-const fmop   #b0000000100) (define-const fmaddr #b0000001000)
(define-const fmcs   #b0000010000) (define-const fmss   #b0000100000)
(define-const fmds   #b0001000000) (define-const fmes   #b0010000000)
(define-const fmfs   #b0100000000) (define-const fmgs   #b1000000000)

; (define-const hasmodrm (_ BitVec 1024) #b...)
; (ite (= #0 ((_ extract 0 0) (bvlshr hasmodrm stem))) False True)
; 

; Existential instruction array
(declare-const instarr memstate)

; For instruction #0:  instruction pointer, prefixes initially blank
(declare-const instptr00 bv32)

(define-const flags01 
  (let ((flags00 #b0000000000))
  (let ((b00 (select instarr instptr00) ))
  (let ((flags01 
    (ite (= #xF2 b00) (bvor flags00 fmrepz ) (ite (= #xF3 b00) (bvor flags00 fmrep  )
    (ite (= #x66 b00) (bvor flags00 fmop   ) (ite (= #x67 b00) (bvor flags00 fmaddr )
    (ite (= #x2E b00) (bvor flags00 fmcs   ) (ite (= #x36 b00) (bvor flags00 fmss   )
    (ite (= #x3E b00) (bvor flags00 fmds   ) (ite (= #x26 b00) (bvor flags00 fmes   )
    (ite (= #x64 b00) (bvor flags00 fmfs   ) (ite (= #x65 b00) (bvor flags00 fmgs   ) flags00)))))))))))))
  (let ((instptr01 (ite (= flags01 flags00) instptr00 (bvadd instptr00 #x00000001))))
  (let ((b01 (select instarr instptr01) ))
  (let ((flags02 
    (ite (= #xF2 b01) (bvor flags01 fmrepz ) (ite (= #xF3 b01) (bvor flags01 fmrep  )
    (ite (= #x66 b01) (bvor flags01 fmop   ) (ite (= #x67 b01) (bvor flags01 fmaddr )
    (ite (= #x2E b01) (bvor flags01 fmcs   ) (ite (= #x36 b01) (bvor flags01 fmss   )
    (ite (= #x3E b01) (bvor flags01 fmds   ) (ite (= #x26 b01) (bvor flags01 fmes   )
    (ite (= #x64 b01) (bvor flags01 fmfs   ) (ite (= #x65 b01) (bvor flags01 fmgs   ) flags01)))))))))))))
  (let ((instptr02 (ite (= flags02 flags01) instptr01 (bvadd instptr01 #x00000001))))
  (let ((b02 (select instarr instptr02) ))
  (let ((flags03 
    (ite (= #xF2 b02) (bvor flags02 fmrepz ) (ite (= #xF3 b02) (bvor flags02 fmrep  )
    (ite (= #x66 b02) (bvor flags02 fmop   ) (ite (= #x67 b02) (bvor flags02 fmaddr )
    (ite (= #x2E b02) (bvor flags02 fmcs   ) (ite (= #x36 b02) (bvor flags02 fmss   )
    (ite (= #x3E b02) (bvor flags02 fmds   ) (ite (= #x26 b02) (bvor flags02 fmes   )
    (ite (= #x64 b02) (bvor flags02 fmfs   ) (ite (= #x65 b02) (bvor flags02 fmgs   ) flags02)))))))))))))
  (let ((instptr03 (ite (= flags03 flags02) instptr02 (bvadd instptr02 #x00000001))))
  (let ((b03 (select instarr instptr03) ))
  (let ((flags04 
    (ite (= #xF2 b03) (bvor flags03 fmrepz ) (ite (= #xF3 b03) (bvor flags03 fmrep  )
    (ite (= #x66 b03) (bvor flags03 fmop   ) (ite (= #x67 b03) (bvor flags03 fmaddr )
    (ite (= #x2E b03) (bvor flags03 fmcs   ) (ite (= #x36 b03) (bvor flags03 fmss   )
    (ite (= #x3E b03) (bvor flags03 fmds   ) (ite (= #x26 b03) (bvor flags03 fmes   )
    (ite (= #x64 b03) (bvor flags03 fmfs   ) (ite (= #x65 b03) (bvor flags03 fmgs   ) flags03)))))))))))))
  (let ((instptr04 (ite (= flags04 flags03) instptr03 (bvadd instptr03 #x00000001))))
  (let ((b04 (select instarr instptr04) ))
  (let ((flags05 
    (ite (= #xF2 b04) (bvor flags04 fmrepz ) (ite (= #xF3 b04) (bvor flags04 fmrep  )
    (ite (= #x66 b04) (bvor flags04 fmop   ) (ite (= #x67 b04) (bvor flags04 fmaddr )
    (ite (= #x2E b04) (bvor flags04 fmcs   ) (ite (= #x36 b04) (bvor flags04 fmss   )
    (ite (= #x3E b04) (bvor flags04 fmds   ) (ite (= #x26 b04) (bvor flags04 fmes   )
    (ite (= #x64 b04) (bvor flags04 fmfs   ) (ite (= #x65 b04) (bvor flags04 fmgs   ) flags04)))))))))))))
  (let ((instptr05 (ite (= flags05 flags04) instptr04 (bvadd instptr04 #x00000001))))
  (let ((b05 (select instarr instptr05) ))
  (let ((flags06 
    (ite (= #xF2 b05) (bvor flags05 fmrepz ) (ite (= #xF3 b05) (bvor flags05 fmrep  )
    (ite (= #x66 b05) (bvor flags05 fmop   ) (ite (= #x67 b05) (bvor flags05 fmaddr )
    (ite (= #x2E b05) (bvor flags05 fmcs   ) (ite (= #x36 b05) (bvor flags05 fmss   )
    (ite (= #x3E b05) (bvor flags05 fmds   ) (ite (= #x26 b05) (bvor flags05 fmes   )
    (ite (= #x64 b05) (bvor flags05 fmfs   ) (ite (= #x65 b05) (bvor flags05 fmgs   ) flags05)))))))))))))
  (let ((instptr06 (ite (= flags06 flags05) instptr05 (bvadd instptr05 #x00000001))))
  (let ((b06 (select instarr instptr06) ))
  (let ((flags07 
    (ite (= #xF2 b06) (bvor flags06 fmrepz ) (ite (= #xF3 b06) (bvor flags06 fmrep  )
    (ite (= #x66 b06) (bvor flags06 fmop   ) (ite (= #x67 b06) (bvor flags06 fmaddr )
    (ite (= #x2E b06) (bvor flags06 fmcs   ) (ite (= #x36 b06) (bvor flags06 fmss   )
    (ite (= #x3E b06) (bvor flags06 fmds   ) (ite (= #x26 b06) (bvor flags06 fmes   )
    (ite (= #x64 b06) (bvor flags06 fmfs   ) (ite (= #x65 b06) (bvor flags06 fmgs   ) flags06)))))))))))))
  (let ((instptr07 (ite (= flags07 flags06) instptr06 (bvadd instptr06 #x00000001))))
  (let ((b07 (select instarr instptr07) ))
  (let ((flags08 
    (ite (= #xF2 b07) (bvor flags07 fmrepz ) (ite (= #xF3 b07) (bvor flags07 fmrep  )
    (ite (= #x66 b07) (bvor flags07 fmop   ) (ite (= #x67 b07) (bvor flags07 fmaddr )
    (ite (= #x2E b07) (bvor flags07 fmcs   ) (ite (= #x36 b07) (bvor flags07 fmss   )
    (ite (= #x3E b07) (bvor flags07 fmds   ) (ite (= #x26 b07) (bvor flags07 fmes   )
    (ite (= #x64 b07) (bvor flags07 fmfs   ) (ite (= #x65 b07) (bvor flags07 fmgs   ) flags07)))))))))))))
  (let ((instptr08 (ite (= flags08 flags07) instptr07 (bvadd instptr07 #x00000001))))
  (let ((b08 (select instarr instptr08) ))
  (let ((flags09 
    (ite (= #xF2 b08) (bvor flags08 fmrepz ) (ite (= #xF3 b08) (bvor flags08 fmrep  )
    (ite (= #x66 b08) (bvor flags08 fmop   ) (ite (= #x67 b08) (bvor flags08 fmaddr )
    (ite (= #x2E b08) (bvor flags08 fmcs   ) (ite (= #x36 b08) (bvor flags08 fmss   )
    (ite (= #x3E b08) (bvor flags08 fmds   ) (ite (= #x26 b08) (bvor flags08 fmes   )
    (ite (= #x64 b08) (bvor flags08 fmfs   ) (ite (= #x65 b08) (bvor flags08 fmgs   ) flags08)))))))))))))
  (let ((instptr09 (ite (= flags09 flags08) instptr08 (bvadd instptr08 #x00000001))))
  (let ((b09 (select instarr instptr09) ))
  (let ((flags0A 
    (ite (= #xF2 b09) (bvor flags09 fmrepz ) (ite (= #xF3 b09) (bvor flags09 fmrep  )
    (ite (= #x66 b09) (bvor flags09 fmop   ) (ite (= #x67 b09) (bvor flags09 fmaddr )
    (ite (= #x2E b09) (bvor flags09 fmcs   ) (ite (= #x36 b09) (bvor flags09 fmss   )
    (ite (= #x3E b09) (bvor flags09 fmds   ) (ite (= #x26 b09) (bvor flags09 fmes   )
    (ite (= #x64 b09) (bvor flags09 fmfs   ) (ite (= #x65 b09) (bvor flags09 fmgs   ) flags09)))))))))))))
  (let ((instptr0Stem (ite (= flags0A flags09) instptr09 (bvadd instptr09 #x00000001))))

  ; Prefixes in flags0A, EIP in instptr0Stem

  (let ((stem0 
    (let ((b0 (select instarr instptr0Stem) ))
    (ite (not (= #x0F b0)) (concat #b00 b0)
    (let ((b1 (select instarr (bvadd #x00000001 instptr0Stem)) ))
    (ite (= #x38 b1) (concat #b10 (select instarr (bvadd #x00000002 instptr0Stem)))
    (ite (= #x3A b1) (concat #b11 (select instarr (bvadd #x00000002 instptr0Stem)))
    (concat #b01 b1))))))

  ; Stem in stem0
  
  (let ((instptr0ModRM 
    (let ((t ((_ extract 9 8) stem0)))
    (ite (= t #b00) (bvadd #x00000001 instptr0Stem)
    (ite (= t #b01) (bvadd #x00000002 instptr0Stem)
    (bvadd #x00000003 instptr0Stem))))))
  
  ; EIP in instptr0ModRM
  
  
  ; 4-bits for ModRM length
  ; 49-bit decoding for ModRM:
  ;    00: 32/16-bit
  ; 04-01: segment

  ; 32-BIT VARIANT
  ; 08-05: reg32 option
  ; 14-09: reg32*scale[2] option
  ; 48-15: disp32 option
  
  ; 16-BIT VARIANT
  ; 08-05: reg16 option
  ; 12-09: reg16 option
  ; 30-13: disp16 option
  ; 53 bits total
  
  
  (let ((lenplusmrm
    (ite (select modrm stem0)
      (ite (= #b0000000000 (bvand flags0A fmaddr))

        ; 32-bit ModRM, extract subparts
        (let ((b0 (select instarr instptr0ModRM)))
        (let ((mod ((_ extract 7 6) b0)) (gpart ((_ extract 5 3) b0)) (epart ((_ extract 2 0) b0)) 
        
        ; Is the address a plain DWORD in DS?
        (ite (and (= #b00 mod) (= #b101 epart)) 
        
          ; Yes.  Need to incorporate the segment information into the encoding.
          (bvshl 
            ((_ zero_extend 15) 
               (concat 
                 (concat 
                   (select instarr (bvadd instptr0ModRM #x00000004)) 
                   (select instarr (bvadd instptr0ModRM #x00000003)))
                 (concat 
                   (select instarr (bvadd instptr0ModRM #x00000002)) 
                   (select instarr (bvadd instptr0ModRM #x00000001)))))
            #x0F)
          
          

          #b00000000000000000000000000000000000000000000000000000
          
          ; Otherwise:  Is a SIB required?
          (ite (and (not (= #b11 mod)) (= epart #b100))
            
            ; Yes, SIB required
            (let ((b1 (x86dec-peek d0)) (d1 (x86dec-peek d0)))
              (let ((ss ((_ extract 7 6) b0)) (idx ((_ extract 5 3) b0)) (sreg ((_ extract 2 0) b0)))
              (ite (and (= sreg #b101) (= #b00 mod)) 
                ; add dword, but don't add EBP
              (ite (= sreg #b100) 
                ; no scaled component
                () ; otherwise, result is [sreg+idx*ss+(mod constant)]
              ))))

            ; No, SIB not required
          
          
          ; 16-bit ModRM

      ; Did not have ModRM
      #b00000000000000000000000000000000000000000000000000000)
      

; Look up stem in the "supported" table -- MUST BE SUPPORTED
; Look up stem in the ModR/M table -- if it has one, consume the ModRM and update the inst ptr
(assert (select supported stem0))
(define-const mrm0
  (
  
  
  
  
  
  
(declare-datatypes () ((X86MRM16 (mk-x86-mrm16 (base16 REG16) (index16 (Option REG16)) )) )
(declare-datatypes () ((X86MRM32 (mk-x86-mrm16 (base16 REG16) (index16 (Option REG16)) )) )

(declare-datatypes () 
 (
  (X86Modrm
    (ModRM16 (m16 X86MRM16))
    (ModRM32 (m32 X86MRM32))
  )
 )
)


(declare-datatypes () ((X86PFX (mk-x86-pfxs (opsize Bool) (addrsize Bool) (rep Bool) (repz Bool) (pfcs Bool) (pfss Bool) (pfds Bool) (pfes Bool) (pffs Bool) (pfgs Bool)) )) )
(declare-datatypes () ((X86Decoder (mk-x86-decoder (eip bv32) (decarray mem8to32) (pfx X86PFX)) )) )
(define-fun x86dec-update-eip   ((d X86Decoder) (e bv32))   X86Decoder (mk-x86-decoder e (decarray d) (pfx d)))
(define-fun x86dec-update-pfx   ((d X86Decoder) (p X86PFX)) X86Decoder (mk-x86-decoder (eip d) (decarray d) p))
(define-fun x86dec-set-opsize   ((p X86PFX)) (mk-x86-pfxs True (addrsize p) (rep p) (repz p) (pfcs p) (pfss p) (pfds p) (pfes p) (pffs p) (pfgs p)))
(define-fun x86dec-set-addrsize ((p X86PFX)) (mk-x86-pfxs (opsize p) True (rep p) (repz p) (pfcs p) (pfss p) (pfds p) (pfes p) (pffs p) (pfgs p)))
(define-fun x86dec-set-rep      ((p X86PFX)) (mk-x86-pfxs (opsize p) (addrsize p) True (repz p) (pfcs p) (pfss p) (pfds p) (pfes p) (pffs p) (pfgs p)))
(define-fun x86dec-set-repz     ((p X86PFX)) (mk-x86-pfxs (opsize p) (addrsize p) (rep p) True (pfcs p) (pfss p) (pfds p) (pfes p) (pffs p) (pfgs p)))
(define-fun x86dec-set-pfcs     ((p X86PFX)) (mk-x86-pfxs (opsize p) (addrsize p) (rep p) (repz p) True (pfss p) (pfds p) (pfes p) (pffs p) (pfgs p)))
(define-fun x86dec-set-pfss     ((p X86PFX)) (mk-x86-pfxs (opsize p) (addrsize p) (rep p) (repz p) (pfcs p) True (pfds p) (pfes p) (pffs p) (pfgs p)))
(define-fun x86dec-set-pfds     ((p X86PFX)) (mk-x86-pfxs (opsize p) (addrsize p) (rep p) (repz p) (pfcs p) (pfss p) True (pfes p) (pffs p) (pfgs p)))
(define-fun x86dec-set-pfes     ((p X86PFX)) (mk-x86-pfxs (opsize p) (addrsize p) (rep p) (repz p) (pfcs p) (pfss p) (pfds p) True (pffs p) (pfgs p)))
(define-fun x86dec-set-pffs     ((p X86PFX)) (mk-x86-pfxs (opsize p) (addrsize p) (rep p) (repz p) (pfcs p) (pfss p) (pfds p) (pfes p) True (pfgs p)))
(define-fun x86dec-set-pfgs     ((p X86PFX)) (mk-x86-pfxs (opsize p) (addrsize p) (rep p) (repz p) (pfcs p) (pfss p) (pfds p) (pfes p) (pffs p) True))

(define-fun x86dec-inc-eip ((d X86Decoder)) X86Decoder (x86dec-update-eip d (bvadd #x00000001 (eip d))))
(define-fun x86dec-peek    ((d X86Decoder)) bv8        (select (decarray d) (eip d)))

(define-fun x86dec-eat-pfx ((d X86Decoder)) X86Decoder 
  (let ((b  (x86dec-peek d) ))
  (ite (= #xF2 b) (x86dec-update-pfx (x86dec-inc-eip d) (x86dec-set-repz     (pfx d)))
  (ite (= #xF3 b) (x86dec-update-pfx (x86dec-inc-eip d) (x86dec-set-rep      (pfx d)))
  (ite (= #x66 b) (x86dec-update-pfx (x86dec-inc-eip d) (x86dec-set-opsize   (pfx d)))
  (ite (= #x67 b) (x86dec-update-pfx (x86dec-inc-eip d) (x86dec-set-addrsize (pfx d)))
  (ite (= #x2E b) (x86dec-update-pfx (x86dec-inc-eip d) (x86dec-set-pfcs     (pfx d)))
  (ite (= #x36 b) (x86dec-update-pfx (x86dec-inc-eip d) (x86dec-set-pfss     (pfx d)))
  (ite (= #x3E b) (x86dec-update-pfx (x86dec-inc-eip d) (x86dec-set-pfds     (pfx d)))
  (ite (= #x26 b) (x86dec-update-pfx (x86dec-inc-eip d) (x86dec-set-pfes     (pfx d)))
  (ite (= #x64 b) (x86dec-update-pfx (x86dec-inc-eip d) (x86dec-set-pffs     (pfx d)))
  (ite (= #x65 b) (x86dec-update-pfx (x86dec-inc-eip d) (x86dec-set-pfgs     (pfx d)))
  d)))))))))))
)

(define-fun x86dec-eat-pfxs ((d X86Decoder)) X86Decoder 
  (let ((d1  (x86dec-eat-pfx d)  ))
  (let ((d2  (x86dec-eat-pfx d1) ))
  (let ((d3  (x86dec-eat-pfx d2) ))
  (let ((d4  (x86dec-eat-pfx d3) ))
  (let ((d5  (x86dec-eat-pfx d4) ))
  (let ((d6  (x86dec-eat-pfx d5) ))
  (let ((d7  (x86dec-eat-pfx d6) ))
  (let ((d8  (x86dec-eat-pfx d7) ))
  (let ((d9  (x86dec-eat-pfx d8) ))
  (x86dec-eat-pfx d9))))))))))
)

(define-fun x86dec-modrm16 ((d X86Decoder)) X86Decoder )
(define-fun x86dec-modrm32 ((d X86Decoder)) X86Decoder 
  (let ((b0 (x86dec-peek d)) (d0 (x86dec-inc-eip d)))
  (let ((mod ((_ extract 7 6) b0)) (gpart ((_ extract 5 3) b0)) (epart ((_ extract 2 0) b0)) 
  (ite (and (= #b00 mod) (= #b101 epart)) (); result is a plain DWORD address in the segment)
  
  (ite (and (not (= #b11 mod)) (= epart #b100))
    ; SIB required
    (let ((b1 (x86dec-peek d0)) (d1 (x86dec-peek d0)))
      (let ((ss ((_ extract 7 6) b0)) (idx ((_ extract 5 3) b0)) (sreg ((_ extract 2 0) b0)))
      (ite (and (= sreg #b101) (= #b00 mod)) 
        ; add dword, but don't add EBP
      (ite (= sreg #b100) 
        ; no scaled component
        () ; otherwise, result is [sreg+idx*ss+(mod constant)]
      ))))
    ; SIB not required

      
    )
)

(define-fun x86dec-modrm ((d X86Decoder)) X86Decoder (ite (addrsize (pfx d)) (x86dec-modrm32 d) (x86dec-modrm16 d)))

(define-fun x86-disasm ((d X86Decoder)) (Option (Pair X86Decoder X86Instr))
  (let ((b0 (x86dec-peek d) ))
  (ite (= b0 #x00) (
  
  )
)
