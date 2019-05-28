open X86
open X86InternalOperand

(* This array exists in two places, which is obviously bad.  Should refactor this out of
   X86Random and this file, and into a common file. *)
let x86_mnem_arr = [|
  Aaa;
  Aad;
  Aam;
  Aas;
  Adc;
  Add;
  Addpd;
  Addps;
  Addsd;
  Addss;
  Addsubpd;
  Addsubps;
  And;
  Andnpd;
  Andnps;
  Andpd;
  Andps;
  Arpl;
  Blendpd;
  Blendps;
  Blendvpd;
  Blendvps;
  Bound;
  Bsf;
  Bsr;
  Bswap;
  Bt;
  Btc;
  Btr;
  Bts;
  Call;
  CallF;
  Cbw;
  Cdq;
  Clc;
  Cld;
  Clflush;
  Cli;
  Clts;
  Cmc;
  Cmova;
  Cmovae;
  Cmovb;
  Cmovbe;
  Cmovg;
  Cmovge;
  Cmovl;
  Cmovle;
  Cmovno;
  Cmovnp;
  Cmovns;
  Cmovnz;
  Cmovo;
  Cmovp;
  Cmovs;
  Cmovz;
  Cmp;
  Cmppd;
  Cmpps;
  Cmpsb;
  Cmpsd;
  Cmpss;
  Cmpsw;
  Cmpxchg;
  Cmpxchg8b;
  Comisd;
  Comiss;
  Cpuid;
  Crc32;
  Cvtdq2pd;
  Cvtdq2ps;
  Cvtpd2dq;
  Cvtpd2pi;
  Cvtpd2ps;
  Cvtpi2pd;
  Cvtpi2ps;
  Cvtps2dq;
  Cvtps2pd;
  Cvtps2pi;
  Cvtsd2si;
  Cvtsd2ss;
  Cvtsi2sd;
  Cvtsi2ss;
  Cvtss2sd;
  Cvtss2si;
  Cvttpd2dq;
  Cvttpd2pi;
  Cvttps2dq;
  Cvttps2pi;
  Cvttsd2si;
  Cvttss2si;
  Cwd;
  Cwde;
  Daa;
  Das;
  Dec;
  Div;
  Divpd;
  Divps;
  Divsd;
  Divss;
  Dppd;
  Dpps;
  Emms;
  Enter;
  Extractps;
  F2xm1;
  Fabs;
  Fadd;
  Faddp;
  Fbld;
  Fbstp;
  Fchs;
  Fclex;
  Fcmovb;
  Fcmovbe;
  Fcmove;
  Fcmovnb;
  Fcmovnbe;
  Fcmovne;
  Fcmovnu;
  Fcmovu;
  Fcom;
  Fcomi;
  Fcomip;
  Fcomp;
  Fcompp;
  Fcos;
  Fdecstp;
  Fdiv;
  Fdivp;
  Fdivr;
  Fdivrp;
  Ffree;
  Fiadd;
  Ficom;
  Ficomp;
  Fidiv;
  Fidivr;
  Fild;
  Fimul;
  Fincstp;
  Finit;
  Fist;
  Fistp;
  Fisttp;
  Fisub;
  Fisubr;
  Fld;
  Fld1;
  Fldcw;
  Fldenv;
  Fldl2e;
  Fldl2t;
  Fldlg2;
  Fldln2;
  Fldpi;
  Fldz;
  Fmul;
  Fmulp;
  Fnop;
  Fpatan;
  Fprem;
  Fprem1;
  Fptan;
  Frndint;
  Frstor;
  Fsave;
  Fscale;
  Fsin;
  Fsincos;
  Fsqrt;
  Fst;
  Fstcw;
  Fstenv;
  Fstp;
  Fstsw;
  Fsub;
  Fsubp;
  Fsubr;
  Fsubrp;
  Ftst;
  Fucom;
  Fucomi;
  Fucomip;
  Fucomp;
  Fucompp;
  Fxam;
  Fxch;
  Fxrstor;
  Fxsave;
  Fxtract;
  Fyl2x;
  Fyl2xp1;
  Getsec;
  Haddpd;
  Haddps;
  Hlt;
  Hsubpd;
  Hsubps;
  Icebp;
  Idiv;
  Imul;
  In;
  Inc;
  Insb;
  Insd;
  Insertps;
  Insw;
  Int;
  Int3;
  Into;
  Invd;
  Invlpg;
  Iretd;
  Iretw;
  Ja;
  Jae;
  Jb;
  Jbe;
  Jcxz;
  Jecxz;
  Jg;
  Jge;
  Jl;
  Jle;
  Jmp;
  JmpF;
  Jno;
  Jnp;
  Jns;
  Jnz;
  Jo;
  Jp;
  Js;
  Jz;
  Lahf;
  Lar;
  Lddqu;
  Ldmxcsr;
  Lds;
  Lea;
  Leave;
  Les;
  Lfence;
  Lfs;
  Lgdt;
  Lgs;
  Lidt;
  Lldt;
  Lmsw;
  Lodsb;
  Lodsd;
  Lodsw;
  Loop;
  Loopnz;
  Loopz;
  Lsl;
  Lss;
  Ltr;
  Maskmovdqu;
  Maskmovq;
  Maxpd;
  Maxps;
  Maxsd;
  Maxss;
  Mfence;
  Minpd;
  Minps;
  Minsd;
  Minss;
  Monitor;
  Mov;
  Movapd;
  Movaps;
  Movd;
  Movddup;
  Movdq2q;
  Movdqa;
  Movdqu;
  Movhlps;
  Movhpd;
  Movhps;
  Movlhps;
  Movlpd;
  Movlps;
  Movmskpd;
  Movmskps;
  Movntdq;
  Movntdqa;
  Movnti;
  Movntpd;
  Movntps;
  Movntq;
  Movq;
  Movq2dq;
  Movsb;
  Movsd;
  Movshdup;
  Movsldup;
  Movss;
  Movsw;
  Movsx;
  Movupd;
  Movups;
  Movzx;
  Mpsadbw;
  Mul;
  Mulpd;
  Mulps;
  Mulsd;
  Mulss;
  Mwait;
  Neg;
  Nop;
  Not;
  Or;
  Orpd;
  Orps;
  Out;
  Outsb;
  Outsd;
  Outsw;
  Pabsb;
  Pabsd;
  Pabsw;
  Packssdw;
  Packsswb;
  Packusdw;
  Packuswb;
  Paddb;
  Paddd;
  Paddq;
  Paddsb;
  Paddsw;
  Paddusb;
  Paddusw;
  Paddw;
  Palignr;
  Pand;
  Pandn;
  Pause;
  Pavgb;
  Pavgw;
  Pblendvb;
  Pblendw;
  Pcmpeqb;
  Pcmpeqd;
  Pcmpeqq;
  Pcmpeqw;
  Pcmpestri;
  Pcmpestrm;
  Pcmpgtb;
  Pcmpgtd;
  Pcmpgtq;
  Pcmpgtw;
  Pcmpistri;
  Pcmpistrm;
  Pextrb;
  Pextrd;
  Pextrw;
  Phaddd;
  Phaddsw;
  Phaddw;
  Phminposuw;
  Phsubd;
  Phsubsw;
  Phsubw;
  Pinsrb;
  Pinsrd;
  Pinsrw;
  Pmaddubsw;
  Pmaddwd;
  Pmaxsb;
  Pmaxsd;
  Pmaxsw;
  Pmaxub;
  Pmaxud;
  Pmaxuw;
  Pminsb;
  Pminsd;
  Pminsw;
  Pminub;
  Pminud;
  Pminuw;
  Pmovmskb;
  Pmovsxbd;
  Pmovsxbq;
  Pmovsxbw;
  Pmovsxdq;
  Pmovsxwd;
  Pmovsxwq;
  Pmovzxbd;
  Pmovzxbq;
  Pmovzxbw;
  Pmovzxdq;
  Pmovzxwd;
  Pmovzxwq;
  Pmuldq;
  Pmulhrsw;
  Pmulhuw;
  Pmulhw;
  Pmulld;
  Pmullw;
  Pmuludq;
  Pop;
  Popad;
  Popaw;
  Popcnt;
  Popfd;
  Popfw;
  Por;
  Prefetchnta;
  Prefetcht0;
  Prefetcht1;
  Prefetcht2;
  Psadbw;
  Pshufb;
  Pshufd;
  Pshufhw;
  Pshuflw;
  Pshufw;
  Psignb;
  Psignd;
  Psignw;
  Pslld;
  Pslldq;
  Psllq;
  Psllw;
  Psrad;
  Psraw;
  Psrld;
  Psrldq;
  Psrlq;
  Psrlw;
  Psubb;
  Psubd;
  Psubq;
  Psubsb;
  Psubsw;
  Psubusb;
  Psubusw;
  Psubw;
  Ptest;
  Punpckhbw;
  Punpckhdq;
  Punpckhqdq;
  Punpckhwd;
  Punpcklbw;
  Punpckldq;
  Punpcklqdq;
  Punpcklwd;
  Push;
  Pushad;
  Pushaw;
  Pushfd;
  Pushfw;
  Pxor;
  Rcl;
  Rcpps;
  Rcpss;
  Rcr;
  Rdmsr;
  Rdpmc;
  Rdtsc;
  Ret;
  Retf;
  Rol;
  Ror;
  Roundpd;
  Roundps;
  Roundsd;
  Roundss;
  Rsm;
  Rsqrtps;
  Rsqrtss;
  Sahf;
  Sal;
  Salc;
  Sar;
  Sbb;
  Scasb;
  Scasd;
  Scasw;
  Seta;
  Setae;
  Setb;
  Setbe;
  Setg;
  Setge;
  Setl;
  Setle;
  Setno;
  Setnp;
  Setns;
  Setnz;
  Seto;
  Setp;
  Sets;
  Setz;
  Sfence;
  Sgdt;
  Shl;
  Shld;
  Shr;
  Shrd;
  Shufpd;
  Shufps;
  Sidt;
  Sldt;
  Smsw;
  Sqrtpd;
  Sqrtps;
  Sqrtsd;
  Sqrtss;
  Stc;
  Std;
  Sti;
  Stmxcsr;
  Stosb;
  Stosd;
  Stosw;
  Str;
  Sub;
  Subpd;
  Subps;
  Subsd;
  Subss;
  Syscall;
  Sysenter;
  Sysexit;
  Sysret;
  Test;
  Ucomisd;
  Ucomiss;
  Ud2;
  Unpckhpd;
  Unpckhps;
  Unpcklpd;
  Unpcklps;
  Verr;
  Verw;
  Vmcall;
  Vmclear;
  Vmlaunch;
  Vmptrld;
  Vmptrst;
  Vmread;
  Vmresume;
  Vmwrite;
  Vmxoff;
  Vmxon;
  Wait;
  Wbinvd;
  Wrmsr;
  Xadd;
  Xlat;
  Xchg;
  Xor;
  Xorpd;
  Xorps;|]

let number_mnem = function
| Aaa ->                   1
| Aad ->                   2
| Aam ->                   3
| Aas ->                   4
| Adc ->                   5
| Add ->                   6
| Addpd ->                 7
| Addps ->                 8
| Addsd ->                 9
| Addss ->                10
| Addsubpd ->             11
| Addsubps ->             12
| And ->                  13
| Andnpd ->               14
| Andnps ->               15
| Andpd ->                16
| Andps ->                17
| Arpl ->                 18
| Blendpd ->              19
| Blendps ->              20
| Blendvpd ->             21
| Blendvps ->             22
| Bound ->                23
| Bsf ->                  24
| Bsr ->                  25
| Bswap ->                26
| Bt ->                   27
| Btc ->                  28
| Btr ->                  29
| Bts ->                  30
| Call ->                 31
| CallF ->                32
| Cbw ->                  33
| Cdq ->                  34
| Clc ->                  35
| Cld ->                  36
| Clflush ->              37
| Cli ->                  38
| Clts ->                 39
| Cmc ->                  40
| Cmova ->                41
| Cmovae ->               42
| Cmovb ->                43
| Cmovbe ->               44
| Cmovg ->                45
| Cmovge ->               46
| Cmovl ->                47
| Cmovle ->               48
| Cmovno ->               49
| Cmovnp ->               50
| Cmovns ->               51
| Cmovnz ->               52
| Cmovo ->                53
| Cmovp ->                54
| Cmovs ->                55
| Cmovz ->                56
| Cmp ->                  57
| Cmppd ->                58
| Cmpps ->                59
| Cmpsb ->                60
| Cmpsd ->                61
| Cmpss ->                62
| Cmpsw ->                63
| Cmpxchg ->              64
| Cmpxchg8b ->            65
| Comisd ->               66
| Comiss ->               67
| Cpuid ->                68
| Crc32 ->                69
| Cvtdq2pd ->             70
| Cvtdq2ps ->             71
| Cvtpd2dq ->             72
| Cvtpd2pi ->             73
| Cvtpd2ps ->             74
| Cvtpi2pd ->             75
| Cvtpi2ps ->             76
| Cvtps2dq ->             77
| Cvtps2pd ->             78
| Cvtps2pi ->             79
| Cvtsd2si ->             80
| Cvtsd2ss ->             81
| Cvtsi2sd ->             82
| Cvtsi2ss ->             83
| Cvtss2sd ->             84
| Cvtss2si ->             85
| Cvttpd2dq ->            86
| Cvttpd2pi ->            87
| Cvttps2dq ->            88
| Cvttps2pi ->            89
| Cvttsd2si ->            90
| Cvttss2si ->            91
| Cwd ->                  92
| Cwde ->                 93
| Daa ->                  94
| Das ->                  95
| Dec ->                  96
| Div ->                  97
| Divpd ->                98
| Divps ->                99
| Divsd ->               100
| Divss ->               101
| Dppd ->                102
| Dpps ->                103
| Emms ->                104
| Enter ->               105
| Extractps ->           106
| F2xm1 ->               107
| Fabs ->                108
| Fadd ->                109
| Faddp ->               110
| Fbld ->                111
| Fbstp ->               112
| Fchs ->                113
| Fclex ->               114
| Fcmovb ->              115
| Fcmovbe ->             116
| Fcmove ->              117
| Fcmovnb ->             118
| Fcmovnbe ->            119
| Fcmovne ->             120
| Fcmovnu ->             121
| Fcmovu ->              122
| Fcom ->                123
| Fcomi ->               124
| Fcomip ->              125
| Fcomp ->               126
| Fcompp ->              127
| Fcos ->                128
| Fdecstp ->             129
| Fdiv ->                130
| Fdivp ->               131
| Fdivr ->               132
| Fdivrp ->              133
| Ffree ->               134
| Fiadd ->               135
| Ficom ->               136
| Ficomp ->              137
| Fidiv ->               138
| Fidivr ->              139
| Fild ->                140
| Fimul ->               141
| Fincstp ->             142
| Finit ->               143
| Fist ->                144
| Fistp ->               145
| Fisttp ->              146
| Fisub ->               147
| Fisubr ->              148
| Fld ->                 149
| Fld1 ->                150
| Fldcw ->               151
| Fldenv ->              152
| Fldl2e ->              153
| Fldl2t ->              154
| Fldlg2 ->              155
| Fldln2 ->              156
| Fldpi ->               157
| Fldz ->                158
| Fmul ->                159
| Fmulp ->               160
| Fnop ->                161
| Fpatan ->              162
| Fprem ->               163
| Fprem1 ->              164
| Fptan ->               165
| Frndint ->             166
| Frstor ->              167
| Fsave ->               168
| Fscale ->              169
| Fsin ->                170
| Fsincos ->             171
| Fsqrt ->               172
| Fst ->                 173
| Fstcw ->               174
| Fstenv ->              175
| Fstp ->                176
| Fstsw ->               177
| Fsub ->                178
| Fsubp ->               179
| Fsubr ->               180
| Fsubrp ->              181
| Ftst ->                182
| Fucom ->               183
| Fucomi ->              184
| Fucomip ->             185
| Fucomp ->              186
| Fucompp ->             187
| Fxam ->                188
| Fxch ->                189
| Fxrstor ->             190
| Fxsave ->              191
| Fxtract ->             192
| Fyl2x ->               193
| Fyl2xp1 ->             194
| Getsec ->              195
| Haddpd ->              196
| Haddps ->              197
| Hlt ->                 198
| Hsubpd ->              199
| Hsubps ->              200
| Icebp ->               201
| Idiv ->                202
| Imul ->                203
| In ->                  204
| Inc ->                 205
| Insb ->                206
| Insd ->                207
| Insertps ->            208
| Insw ->                209
| Int ->                 210
| Int3 ->                211
| Into ->                212
| Invd ->                213
| Invlpg ->              214
| Iretd ->               215
| Iretw ->               216
| Ja ->                  217
| Jae ->                 218
| Jb ->                  219
| Jbe ->                 220
| Jcxz ->                221
| Jecxz ->               222
| Jg ->                  223
| Jge ->                 224
| Jl ->                  225
| Jle ->                 226
| Jmp ->                 227
| JmpF ->                228
| Jno ->                 229
| Jnp ->                 230
| Jns ->                 231
| Jnz ->                 232
| Jo ->                  233
| Jp ->                  234
| Js ->                  235
| Jz ->                  236
| Lahf ->                237
| Lar ->                 238
| Lddqu ->               239
| Ldmxcsr ->             240
| Lds ->                 241
| Lea ->                 242
| Leave ->               243
| Les ->                 244
| Lfence ->              245
| Lfs ->                 246
| Lgdt ->                247
| Lgs ->                 248
| Lidt ->                249
| Lldt ->                250
| Lmsw ->                251
| Lodsb ->               252
| Lodsd ->               253
| Lodsw ->               254
| Loop ->                255
| Loopnz ->              256
| Loopz ->               257
| Lsl ->                 258
| Lss ->                 259
| Ltr ->                 260
| Maskmovdqu ->          261
| Maskmovq ->            262
| Maxpd ->               263
| Maxps ->               264
| Maxsd ->               265
| Maxss ->               266
| Mfence ->              267
| Minpd ->               268
| Minps ->               269
| Minsd ->               270
| Minss ->               271
| Monitor ->             272
| Mov ->                 273
| Movapd ->              274
| Movaps ->              275
| Movd ->                276
| Movddup ->             277
| Movdq2q ->             278
| Movdqa ->              279
| Movdqu ->              280
| Movhlps ->             281
| Movhpd ->              282
| Movhps ->              283
| Movlhps ->             284
| Movlpd ->              285
| Movlps ->              286
| Movmskpd ->            287
| Movmskps ->            288
| Movntdq ->             289
| Movntdqa ->            290
| Movnti ->              291
| Movntpd ->             292
| Movntps ->             293
| Movntq ->              294
| Movq ->                295
| Movq2dq ->             296
| Movsb ->               297
| Movsd ->               298
| Movshdup ->            299
| Movsldup ->            300
| Movss ->               301
| Movsw ->               302
| Movsx ->               303
| Movupd ->              304
| Movups ->              305
| Movzx ->               306
| Mpsadbw ->             307
| Mul ->                 308
| Mulpd ->               309
| Mulps ->               310
| Mulsd ->               311
| Mulss ->               312
| Mwait ->               313
| Neg ->                 314
| Nop ->                 315
| Not ->                 316
| Or ->                  317
| Orpd ->                318
| Orps ->                319
| Out ->                 320
| Outsb ->               321
| Outsd ->               322
| Outsw ->               323
| Pabsb ->               324
| Pabsd ->               325
| Pabsw ->               326
| Packssdw ->            327
| Packsswb ->            328
| Packusdw ->            329
| Packuswb ->            330
| Paddb ->               331
| Paddd ->               332
| Paddq ->               333
| Paddsb ->              334
| Paddsw ->              335
| Paddusb ->             336
| Paddusw ->             337
| Paddw ->               338
| Palignr ->             339
| Pand ->                340
| Pandn ->               341
| Pause ->               342
| Pavgb ->               343
| Pavgw ->               344
| Pblendvb ->            345
| Pblendw ->             346
| Pcmpeqb ->             347
| Pcmpeqd ->             348
| Pcmpeqq ->             349
| Pcmpeqw ->             350
| Pcmpestri ->           351
| Pcmpestrm ->           352
| Pcmpgtb ->             353
| Pcmpgtd ->             354
| Pcmpgtq ->             355
| Pcmpgtw ->             356
| Pcmpistri ->           357
| Pcmpistrm ->           358
| Pextrb ->              359
| Pextrd ->              360
| Pextrw ->              361
| Phaddd ->              362
| Phaddsw ->             363
| Phaddw ->              364
| Phminposuw ->          365
| Phsubd ->              366
| Phsubsw ->             367
| Phsubw ->              368
| Pinsrb ->              369
| Pinsrd ->              370
| Pinsrw ->              371
| Pmaddubsw ->           372
| Pmaddwd ->             373
| Pmaxsb ->              374
| Pmaxsd ->              375
| Pmaxsw ->              376
| Pmaxub ->              377
| Pmaxud ->              378
| Pmaxuw ->              379
| Pminsb ->              380
| Pminsd ->              381
| Pminsw ->              382
| Pminub ->              383
| Pminud ->              384
| Pminuw ->              385
| Pmovmskb ->            386
| Pmovsxbd ->            387
| Pmovsxbq ->            388
| Pmovsxbw ->            389
| Pmovsxdq ->            390
| Pmovsxwd ->            391
| Pmovsxwq ->            392
| Pmovzxbd ->            393
| Pmovzxbq ->            394
| Pmovzxbw ->            395
| Pmovzxdq ->            396
| Pmovzxwd ->            397
| Pmovzxwq ->            398
| Pmuldq ->              399
| Pmulhrsw ->            400
| Pmulhuw ->             401
| Pmulhw ->              402
| Pmulld ->              403
| Pmullw ->              404
| Pmuludq ->             405
| Pop ->                 406
| Popad ->               407
| Popaw ->               408
| Popcnt ->              409
| Popfd ->               410
| Popfw ->               411
| Por ->                 412
| Prefetchnta ->         413
| Prefetcht0 ->          414
| Prefetcht1 ->          415
| Prefetcht2 ->          416
| Psadbw ->              417
| Pshufb ->              418
| Pshufd ->              419
| Pshufhw ->             420
| Pshuflw ->             421
| Pshufw ->              422
| Psignb ->              423
| Psignd ->              424
| Psignw ->              425
| Pslld ->               426
| Pslldq ->              427
| Psllq ->               428
| Psllw ->               429
| Psrad ->               430
| Psraw ->               431
| Psrld ->               432
| Psrldq ->              433
| Psrlq ->               434
| Psrlw ->               435
| Psubb ->               436
| Psubd ->               437
| Psubq ->               438
| Psubsb ->              439
| Psubsw ->              440
| Psubusb ->             441
| Psubusw ->             442
| Psubw ->               443
| Ptest ->               444
| Punpckhbw ->           445
| Punpckhdq ->           446
| Punpckhqdq ->          447
| Punpckhwd ->           448
| Punpcklbw ->           449
| Punpckldq ->           450
| Punpcklqdq ->          451
| Punpcklwd ->           452
| Push ->                453
| Pushad ->              454
| Pushaw ->              455
| Pushfd ->              456
| Pushfw ->              457
| Pxor ->                458
| Rcl ->                 459
| Rcpps ->               460
| Rcpss ->               461
| Rcr ->                 462
| Rdmsr ->               463
| Rdpmc ->               464
| Rdtsc ->               465
| Ret ->                 466
| Retf ->                467
| Rol ->                 468
| Ror ->                 469
| Roundpd ->             470
| Roundps ->             471
| Roundsd ->             472
| Roundss ->             473
| Rsm ->                 474
| Rsqrtps ->             475
| Rsqrtss ->             476
| Sahf ->                477
| Sal ->                 478
| Salc ->                479
| Sar ->                 480
| Sbb ->                 481
| Scasb ->               482
| Scasd ->               483
| Scasw ->               484
| Seta ->                485
| Setae ->               486
| Setb ->                487
| Setbe ->               488
| Setg ->                489
| Setge ->               490
| Setl ->                491
| Setle ->               492
| Setno ->               493
| Setnp ->               494
| Setns ->               495
| Setnz ->               496
| Seto ->                497
| Setp ->                498
| Sets ->                499
| Setz ->                500
| Sfence ->              501
| Sgdt ->                502
| Shl ->                 503
| Shld ->                504
| Shr ->                 505
| Shrd ->                506
| Shufpd ->              507
| Shufps ->              508
| Sidt ->                509
| Sldt ->                510
| Smsw ->                511
| Sqrtpd ->              512
| Sqrtps ->              513
| Sqrtsd ->              514
| Sqrtss ->              515
| Stc ->                 516
| Std ->                 517
| Sti ->                 518
| Stmxcsr ->             519
| Stosb ->               520
| Stosd ->               521
| Stosw ->               522
| Str ->                 523
| Sub ->                 524
| Subpd ->               525
| Subps ->               526
| Subsd ->               527
| Subss ->               528
| Syscall ->             529
| Sysenter ->            530
| Sysexit ->             531
| Sysret ->              532
| Test ->                533
| Ucomisd ->             534
| Ucomiss ->             535
| Ud2 ->                 536
| Unpckhpd ->            537
| Unpckhps ->            538
| Unpcklpd ->            539
| Unpcklps ->            540
| Verr ->                541
| Verw ->                542
| Vmcall ->              543
| Vmclear ->             544
| Vmlaunch ->            545
| Vmptrld ->             546
| Vmptrst ->             547
| Vmread ->              548
| Vmresume ->            549
| Vmwrite ->             550
| Vmxoff ->              551
| Vmxon ->               552
| Wait ->                553
| Wbinvd ->              554
| Wrmsr ->               555
| Xadd ->                556
| Xlat ->                557
| Xchg ->                558
| Xor ->                 559
| Xorpd ->               560
| Xorps ->               561
  
let make_canonical_map () =
  let map = Hashtbl.create (Array.length x86_mnem_arr * 2) in
  let list = 
    Array.fold_left 
     (fun acc mnem -> List.fold_left (fun acc (aol,(_,_)) -> (mnem,aol)::acc) 
      acc 
     (let list = try X86Encode.mnem_to_encodings mnem with _ -> [] in list)) [] x86_mnem_arr in
  (* Since the encoder doesn't handle these encodings (due to their relative addressing),
     we include them manually. *)
  let extra_encodings =
   [(Jo ,[OJb]);
    (Jno,[OJb]);
    (Jb ,[OJb]);
    (Jae,[OJb]);
    (Jz ,[OJb]);
    (Jnz,[OJb]);
    (Jbe,[OJb]);
    (Ja ,[OJb]);
    (Js ,[OJb]);
    (Jns,[OJb]);
    (Jp ,[OJb]);
    (Jnp,[OJb]);
    (Jl ,[OJb]);
    (Jge,[OJb]);
    (Jle,[OJb]);
    
    (Jo ,[OJz]);
    (Jno,[OJz]);
    (Jb ,[OJz]);
    (Jae,[OJz]);
    (Jz ,[OJz]);
    (Jnz,[OJz]);
    (Jbe,[OJz]);
    (Ja ,[OJz]);
    (Js ,[OJz]);
    (Jns,[OJz]);
    (Jp ,[OJz]);
    (Jnp,[OJz]);
    (Jl ,[OJz]);
    (Jge,[OJz]);
    (Jle,[OJz]);
    (Jg ,[OJz]);
    
    (Loopnz,[OJb]);
    (Loopz,[OJb]);
    (Loop,[OJb]);
    (Jcxz,[OJb]);
    (Jecxz,[OJb]);
    
    (Call,[OJz]);
    (Jmp,[OJz]);
    (JmpF,[OAp]);
    (Jmp,[OJb])]
  in
  let num_encodings = List.fold_left (fun i e -> Hashtbl.replace map e i; i+1) 1 (extra_encodings@list) in
  (num_encodings-1,map)
  
