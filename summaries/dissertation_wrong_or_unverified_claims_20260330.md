# Draft Claims To Revisit

These are the original draft claims that were factually wrong, mismatched to the project, or too strong to leave unqualified.

1. [Dissertation_Template__Copy_/Design.tex](Dissertation_Template__Copy_/Design.tex): "There currently is a gap in the literature looking at integrating Haskell into existing databases, especially the combination of Haskell and python."
Reason: the dissertation is not about databases, and the claim is broader than the sources support.

2. [Dissertation_Template__Copy_/Design.tex](Dissertation_Template__Copy_/Design.tex): "the method of refactoring/ translating a program within a larger system ... has not been covered in a major research paper."
Reason: this is too broad to verify from the current draft and should be treated as unsupported unless you can cite it properly.

3. [Dissertation_Template__Copy_/Design.tex](Dissertation_Template__Copy_/Design.tex): "haskells compiler requires completeness so having a non comprehensive match function isnt even possible."
Reason: Haskell can still compile incomplete pattern matches unless warnings are treated as errors, so the current wording is too strong.

4. [Dissertation_Template__Copy_/StateOfTheArt.tex](Dissertation_Template__Copy_/StateOfTheArt.tex): the claim that lazy evaluation in this project allows very large test suites to be generated without running out of memory.
Reason: the current implementation and evaluation do not demonstrate that claim.

5. [Dissertation_Template__Copy_/StateOfTheArt.tex](Dissertation_Template__Copy_/StateOfTheArt.tex): the claim that the project uses an abstract "test action" with concrete implementations in multiple languages.
Reason: the repository and evaluation here are centred on the existing C-oriented pipeline, not a demonstrated multi-backend typeclass design.

6. [Dissertation_Template__Copy_/StateOfTheArt.tex](Dissertation_Template__Copy_/StateOfTheArt.tex): the suggestion that the workflow maps model-checker traces to concrete code in C, Ada, or Rust.
Reason: the repository material used in this dissertation only verifies the C path.