# PandemicML
Pandemic binary program analysis framework in OCaml (abandoned)

This repository contains the OCaml binary program analysis framework that I developed and used from roughly 2008 to 2014. It helped me develop and refine many techniques that I still use to this day. In 2013, I switched to developing a Python version of this framework -- not because Python is a better language than OCaml for these kinds of tasks (it isn't), but because I created a commercial training course on SMT-based binary analysis, and figured I needed to use a mainstream language.

Since I have a working replacement, and since the hassle of maintaining two separate frameworks proved prohibitive, I don't intend to develop or maintain the code any further, nor to support it. I doubt I'd even be able to compile this without putting in serious effort into it. I used to build OCaml from source with MSVC, and ran everything inside of IDA Pro (via my IDAOCaml plugin). The code relies upon outdated versions of IDA, OCaml, and Z3. I wish you the best of luck if you want to build it yourself, but I can't offer support.

Mostly I'm publishing this for the people out there, like me, who are content with merely reading the source code to things rather than running them. If you do, you may develop a deep appreciation for OCaml, and particularly how simple it makes program analysis-related development. You also might be interested in the Projects directory, which contains working (if early) versions of some of my past publications.
