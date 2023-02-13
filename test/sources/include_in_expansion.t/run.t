Checking that source parents are kept, using include.

  $ odoc compile --child module-a root.mld
  $ ocamlc -c -o b.cmo b.ml -bin-annot -I .
  $ ocamlc -c -o main__A.cmo a.ml -bin-annot -I .
  $ ocamlc -c main.ml -bin-annot -I .

  $ odoc compile --source-name b.ml --source-parent-file page-root.odoc -I . b.cmt
  $ odoc compile --source-name a.ml --source-parent-file page-root.odoc -I . main__A.cmt
  $ odoc compile --source-name main.ml --source-parent-file page-root.odoc -I . main.cmt

  $ odoc link -I . main.odoc
  $ odoc link -I . main__A.odoc

  $ odoc html-generate --source main.ml --indent -o html main.odocl
  $ odoc html-generate --source a.ml --hidden --indent -o html main__A.odocl

In Main.A, the source parent of value x should be to Main__A, while the
source parent of value y should be left to B.

  $ grep source_link html/Main/A/index.html -C 1
     <h1>Module <code><span>Main.A</span></code>
      <a href="../../root/a.ml.html" class="source_link">Source</a>
     </h1>
  --
         <a href="#val-y" class="anchor"></a>
         <a href="../../root/b.ml.html#def-0" class="source_link">Source</a>
         <code><span><span class="keyword">val</span> y : int</span></code>
  --
       <a href="#val-x" class="anchor"></a>
       <a href="../../root/a.ml.html#def-0" class="source_link">Source</a>
       <code><span><span class="keyword">val</span> x : int</span></code>